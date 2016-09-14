;;; gmail-request.el -- part of gmail4e, a Gmail client for Emacs
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

(defun gmail-request-threads-get ()
  (request
   "https://www.googleapis.com/gmail/v1/users/me/threads"
   :type "GET"
   :headers `(("Authorization" . ,(concat gmail-auth-token-type " " gmail-auth-access-token)))
   :parser 'json-read
   :success (function*
             (lambda (&key data &allow-other-keys)
               (pp data)
               (setq gmail-cache-threads data)))))

(defvar gmail-messages-cache nil)

(defun gmail-messages-list ()
  (request
   "https://www.googleapis.com/gmail/v1/users/me/messages"
   :type "GET"
   :headers `(("Authorization" . ,(concat gmail-auth-token-type " " gmail-auth-access-token)))
   :parser 'json-read
   :success (function*
             (lambda (&key data &allow-other-keys)
               (pp data)
               (setq gmail-messages-cache data)))))

(defun gmail-messages-get (id)
  (request
   (concat "https://www.googleapis.com/gmail/v1/users/me/messages/" id)
   :type "GET"
   :headers `(("Authorization" . ,(concat gmail-auth-token-type " " gmail-auth-access-token)))
   :parser 'json-read
   :success (function*
             (lambda (&key data &allow-other-keys)
               (pp data)
               (setq gmail-messages-cache data)))))
