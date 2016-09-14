;;; gmail-auth.el -- part of gmail4e, a Gmail client for Emacs
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

(defconst gmail-auth-endpoint "https://accounts.google.com/o/oauth2/v2/auth"
  "Google OAuth2 authorization endpoint.")

(defconst gmail-auth-token-endpoint "https://www.googleapis.com/oauth2/v4/token"
  "Google OAuth2 token endpoint.")

(defconst gmail-auth-client-id "252168474907-1d09if0ph0dfapfe10ibr8vrqoqrrtb0.apps.googleusercontent.com"
  "Google OAuth2 client ID")

(defvar gmail-auth-port (+ (random 8000) 8000))

(defvar gmail-auth-code nil)

(defvar gmail-auth-access-token nil)

(defvar gmail-auth-refresh-token nil)

(defvar gmail-auth-expires-in nil)

(defvar gmail-auth-token-type nil)

(defun gmail-auth-handle-token (access-token refresh-token expires-in token-type)
  (setq gmail-auth-access-token access-token)
  (setq gmail-auth-refresh-token refresh-token)
  (setq gmail-auth-expires-in expires-in)
  (setq gmail-auth-token-type token-type)
  (gmail-reload))

(defun gmail-auth-request-token ()
  (request
   gmail-auth-token-endpoint
   :type "POST"
   :data `(("code" . ,gmail-auth-code)
           ("client_id" . ,gmail-auth-client-id)
           ("client_secret" . ,gmail-auth-client-secret)
           ("redirect_uri" . ,(format "http://127.0.0.1:%d" gmail-auth-port))
           ("grant_type" . "authorization_code"))
   :parser 'json-read
   :success (function*
             (lambda (&key data &allow-other-keys)
               (pp data)
               (let ((access-token (cdr (assoc 'access_token data)))
                     (refresh-token (cdr (assoc 'refresh_token data)))
                     (expires-in (cdr (assoc 'expires_in data)))
                     (token-type (cdr (assoc 'token_type data))))
                 (gmail-auth-handle-token access-token refresh-token expires-in token-type))))))

(defun gmail-auth-handle-code (code)
  (gmail-auth-request-token code))

(defun gmail-auth-handler (httpcon)
  "Demonstration function"
  (progn
    (pp (elnode-http-params httpcon))
    (let ((code (elnode-http-param httpcon "code")))
      (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
      (elnode-http-return httpcon "<html><b>HELLO!</b></html>")
      (if gmail-auth-code
          (elnode-stop gmail-auth-port)
          (progn
            (setq gmail-auth-code code)
            (gmail-auth-request-token))))))

(defun gmail-auth-request-code ()
  (elnode-start 'gmail-auth-handler :port gmail-auth-port :host "localhost")
  (browse-url
   (concat
    gmail-auth-endpoint "?"
    (url-build-query-string
     `(("client_id" ,gmail-auth-client-id)
       ("prompt" "consent")
       ("scope" ,(mapconcat 'identity gmail-auth-scopes " "))
       ("response_type" "code")
       ("redirect_uri" ,(format "http://127.0.0.1:%d" gmail-auth-port))
       ("login_hint" ,gmail-address))))))

(defconst gmail-auth-scopes
  '("https://www.googleapis.com/auth/gmail.readonly"))

(provide 'gmail-auth)
