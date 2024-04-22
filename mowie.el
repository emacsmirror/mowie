;;; mowie.el --- Cycle Through Point-Moving Commands -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author:                  Mekeor Melire <mekeor@posteo.de>
;; Created:                 2024
;; Homepage:                https://codeberg.org/mekeor/emacs-mowie
;; Keywords:                convenience
;; Maintainer:              Mekeor Melire <mekeor@posteo.de>
;; Package-Requires:        ((emacs "28.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version:                 0.0.4

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
;; result of point-moving commands by consecutive repetitions. The
;; package also offers a few point-moving commands.

;; While repeating the command that you defined using `mowie', it will
;; skip those point-moving commands that do not provide a point which
;; has not been visited in the running repetition so far. In a similar
;; manner, it currently also avoids the point as it was right before
;; the repetition started.

;; `mowie' strives to be minimalistic rather than being generalizable.
;; If you want something different, consider copying and modifying the
;; code to your liking, or using an alternative package.

;; `mowie' differs from Alex Kost's `mwim' package and from Adam
;; Porter's `mosey' package by being minimalistic rather than complex;
;; by being opinionated rather than maximizing flexibility; and by not
;; providing any (rather opaque) macros.

;;;; Examples:

;; Cycle through alternatives of `beginning-of-line' and
;; `end-of-line'!
;;
;;   (defun my-beginning-of-line ()
;;     (interactive "^")
;;     (mowie
;;       #'beginning-of-line
;;       #'beginning-of-visual-line
;;       #'mowie-beginning-of-code
;;       #'mowie-beginning-of-comment
;;       #'mowie-beginning-of-comment-text))
;;
;;   (defun my-end-of-line ()
;;     (interactive "^")
;;     (mowie
;;       #'end-of-line
;;       #'end-of-visual-line
;;       #'mowie-end-of-code))
;;
;;   (keymap-substitute (current-global-map)
;;     #'move-beginning-of-line #'my-beginning-of-line)
;;
;;   (keymap-substitute (current-global-map)
;;     #'move-end-of-line #'my-end-of-line)

;; Cycle through alternatives of `beginning-of-buffer' and
;; `end-of-buffer', e.g. in `message-mode'!
;;
;;   (defun my-beginning-of-message ()
;;     (interactive "^")
;;     (mowie
;;       #'message-goto-body
;;       #'beginning-of-buffer))
;;
;;   (defun my-end-of-message ()
;;     (interactive "^")
;;     (mowie
;;       #'message-goto-eoh
;;       #'end-of-buffer))
;;
;;   (keymap-set message-mode-map "M-<" #'my-beginning-of-message)
;;   (keymap-set message-mode-map "M->" #'my-end-of-message)

;;;; TODO:

;; Consider introducing a user option determining if last-position
;; should be avoided while repeating.

;; Write tests.

;;; Code:

;;;; Internal State

(defvar mowie--index nil)
(defvar mowie--point nil)

;;;; Point-Moving Commands

;; Pass `^' code-letter to `interactive' so that if
;; "`shift-select-mode' is non-nil, Emacs first calls the function
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
    ( (line-beg-pos (line-beginning-position))
      (pos
        (save-excursion
          (end-of-line)
          (when-let
            ((pos
               (save-excursion
                 ;; Catch the error that `comment-search-backward'
                 ;; throws when point is not inside a comment.
                 (condition-case nil
                   (comment-search-backward
                     line-beg-pos t)
                   (error nil)))))
            (goto-char pos))
          (skip-syntax-backward " " (line-beginning-position))
          (unless (equal (point) line-beg-pos) (point)))))
    (goto-char pos)))

;;;; Mowie

(defun mowie (&rest funs)
  "Cycle through list of point-moving functions by repetition."
  (let*
    ( (repetition (eq last-command this-command))
      (loop-amount 0)
      (fun-amount (length funs))
      (excluded
        (cons
          ;; Excluding (point) ensures that we do not stay where we
          ;; started this command, if possible.
          (point)
          ;; Excluding `mowie--point' assumes we do not want to go
          ;; back where point was right before this repetition.
          (when repetition (list mowie--point)))))
    (unless repetition
      (setq mowie--point (point))
      (setq mowie--index 0))
    (while (and (< loop-amount fun-amount) (member (point) excluded))
      ;; Reset point to where it was before the repetition.
      (goto-char mowie--point)
      ;; Call the function; interactively, when appropriate.
      (let ((fun (nth mowie--index funs)))
        (if (interactive-form fun)
          (call-interactively fun)
          (funcall fun)))
      ;; Prepare next loop.
      (setq mowie--index (% (1+ mowie--index) fun-amount))
      (setq loop-amount (1+ loop-amount)))))

(provide 'mowie)

;;; mowie.el ends here
