;;; mowie-extra.el --- Mowie-Based Extra Commands  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mekeor Melire

;; SPDX-License-Identifier: GPL-3.0-or-later

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

;;; Code:

(require 'message)
(require 'mowie)

(defun mowie-extra-beginning-of-message ()
  "Cycle through alternatives of `beginning-of-buffer'.

Meant to be used in `message-mode'."
  (interactive "^")
  (mowie #'message-goto-body
         #'beginning-of-buffer))

(defun mowie-extra-end-of-message ()
  "Cycle through alternatives of `end-of-buffer'.

Meant to be used in `message-mode'."
  (interactive "^")
  (mowie #'end-of-buffer
         #'message-goto-eoh))

(defun mowie-extra-beginning-of-line ()
  "Cycle through alternatives of `beginning-of-line'."
  (interactive "^")
  (mowie #'mowie-beginning-of-code
         #'beginning-of-line
         #'beginning-of-visual-line
         #'mowie-beginning-of-comment-text))

(defun mowie-extra-end-of-line ()
  "Cycle through alternatives of `end-of-line'."
  (interactive "^")
  (mowie #'end-of-line
         #'end-of-visual-line
         #'mowie-end-of-code))

(provide 'mowie-extra)

;;; mowie-extra.el ends here
