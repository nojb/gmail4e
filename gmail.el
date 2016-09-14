;;; gmail.el -- part of gmail4e, a Gmail client for Emacs
;;
;; Copyright (C) 2016 Nicolas Ojeda Bar

;; Author: Nicolas Ojeda Bar <n.oje.bar@gmail.com>
;; Maintainer: Nicolas Ojeda Bar <n.oje.bar@gmail.com>

;; This file is not part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO

;;; Code:

(defconst gmail-main-buffer-name " *gmail*"
  "Name of the Gmail main buffer.")

(defvar gmail-main-mode-map
  (let ((map (make-sparse-keymap)))
    map))

(define-derived-mode gmail-main-mode special-mode "Gmail"
  "Major mode for the Gmail main screen.

\\{gmail-main-mode-map}."
  (use-local-map gmail-main-mode-map)
  (setq truncate-lines t))

(defvar gmail-cache-threads nil)

(defun gmail-main-revert ()
  (let ((buf (get-buffer-create gmail-main-buffer-name))
        (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (apply 'insert (mapcar (lambda (json)
                        (concat (cdr (assoc 'id json)) "\n"))
                      (cdr (assoc 'messages gmail-messages-cache))))
      (gmail-main-mode))))

(defun gmail-main-view ()
  "Create the Gmail main view, and switch to it."
  (gmail-main-revert)
  (switch-to-buffer gmail-main-buffer-name)
  (goto-char (point-min)))
