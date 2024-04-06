;;; mowie.el --- Cycle Through Point-Moving Commands -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author:                  Mekeor Melire <mekeor@posteo.de>
;; Created:                 2024
;; Homepage:                https://codeberg.org/mekeor/emacs-mowie
;; Keywords:                convenience
;; Maintainer:              Mekeor Melire <mekeor@posteo.de>
;; Package-Requires:        ((emacs "28.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version:                 0.0.3

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `mowie' let's you define smart commands that cycle through the
;; result of point-moving commands by consecutive repetitions. It also
;; offers a few point-moving commands.

;; For example, let's define an smart alternative commands to
;; `beginning-of-line' and `end-of-line':
;;
;;   (require 'mowie)
;;
;;   (defun my-mowie-beginning-of-line ()
;;     "Cycle through alternatives of `beginning-of-line'."
;;     (interactive "^")
;;     (mowie
;;       #'beginning-of-line
;;       #'move-beginning-of-line
;;       #'beginning-of-visual-line
;;       #'mowie-beginning-of-code
;;       #'mowie-beginning-of-comment
;;       #'mowie-beginning-of-comment-text))
;;
;;   (defun my-mowie-end-of-line ()
;;     "Cycle through alternatives of `end-of-line'."
;;     (interactive "^")
;;     (mowie
;;       #'end-of-line
;;       #'move-end-of-line
;;       #'end-of-visual-line))
;;
;;   (keymap-substitute (current-global-map)
;;     #'move-beginning-of-line #'my-mowie-beginning-of-line)
;;
;;   (keymap-substitute (current-global-map)
;;     #'move-end-of-line #'my-mowie-end-of-line)

;; `mowie' differs from Alex Kost's `mwim' package and from Adam
;; Porter's `mosey' package by being minimalistic rather than complex;
;; by being opinionated rather than maximizing flexibility; and by not
;; providing any macros.

;;; Roadmap:

;; Introduce a user option determining if last-position should be
;; avoided while repeating.

;;; Code:

;;;; Internal State

(defvar mowie--index nil)
(defvar mowie--point nil)

;;;; Point-Moving Commands

;; pass `^' code-letter to `interactive' so that if
;; "`shift-select-mode' is non-nil, emacs first calls the function
;; `handle-shift-selection'" (describe-function 'interactive).

(defun mowie-beginning-of-code ()
  "Move point to first character of line that is not whitespace."
  (interactive "^")
  (beginning-of-line)
  (skip-syntax-forward " " (line-end-position)))

(defun mowie-beginning-of-comment ()
  "Move point to first character of line that is comment."
  (interactive "^")
  (if-let*
    ((pos (nth 8 (save-excursion (syntax-ppss (line-end-position))))))
    (goto-char pos)))

(defun mowie-beginning-of-comment-text ()
  "Move point to first character of line that is text inside comment."
  (interactive "^")
  (if-let
    ((pos
       (save-excursion
         (beginning-of-line)
         (when (comment-search-forward (line-end-position) t)
           (point)))))
    (goto-char pos)))

(defun mowie-end-of-code ()
  "Move point to right behind last character of line that is code."
  (interactive "^")
  (if-let
    ((pos
       (save-excursion
         (end-of-line)
         (when-let
           ((pos
              (save-excursion
                (comment-search-backward
                  (line-beginning-position) t))))
           (goto-char pos))
         (skip-syntax-backward " " (line-beginning-position))
         (point))))
    (goto-char pos)))

;;;; Mowie

(defun mowie (&rest funs)
  "Cycle through list of point-moving commands by repetition."
  (let ((repeating (eq last-command this-command)))
    (unless repeating
      (setq mowie--point (point))
      (setq mowie--index 0))
    (mowie--cycle funs (and repeating (list (point))))))

(defun mowie--cycle (funs &optional last-point)
  (let
    ( (loop-length 0)
      (loop-max-length (length funs))
      (condition t)
      (excluded-positions (cons mowie--point last-point)))
    (while condition
      (setq mowie--index (% (1+ mowie--index) (length funs)))
      ;; reset point to where it was before the series.
      (goto-char mowie--point)
      ;; call the function; interactively, when appropriate.
      (let ((fun (nth mowie--index funs)))
        (if (interactive-form fun)
          (call-interactively fun)
          (funcall fun)))
      ;; keep track of number of iterations.
      (setq loop-length (1+ loop-length))
      (setq condition
        (and
          (< loop-length loop-max-length)
          ;; by checking against `mowie--point', assume that we do not
          ;; want to go back where point was when the series was
          ;; started:
          (member (point) excluded-positions))))))

(provide 'mowie)

;;; mowie.el ends here
