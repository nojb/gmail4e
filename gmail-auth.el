(defconst gmail-auth-endpoint "https://accounts.google.com/o/oauth2/v2/auth"
  "Google OAuth2 authorization endpoint.")

(defconst gmail-auth-token-endpoint "https://www.googleapis.com/oauth2/v4/token"
  "Google OAuth2 token endpoint.")

(defconst gmail-auth-client-id "252168474907-1d09if0ph0dfapfe10ibr8vrqoqrrtb0.apps.googleusercontent.com"
  "Google OAuth2 client ID")

(defvar gmail-auth-port (+ (random 8000) 8000))

(defvar gmail-auth-code nil)

(defun gmail-auth-handle-token (access-token refresh-token expires-in token-type)
  (message "access-token: %s" access-token)
  (message "refresh-token: %s" refresh-token)
  (message "expires-in: %s" expires-in)
  (message "token-type: %s" token-type))

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
               (print data)
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
    (print (elnode-http-params httpcon))
    (let ((code (elnode-http-param httpcon "code")))
      (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
      (elnode-http-return httpcon "<html><b>HELLO!</b></html>")
      ;; (elnode-stop gmail-auth-port)
      (if gmail-auth-code
          (progn
            (setq gmail-auth-code code)
            (gmail-auth-request-token))
        (elnode-stop gmail-auth-port)))))

(defun gmail-auth-request-code (scopes login_hint)
  (elnode-start 'gmail-auth-handler :port gmail-auth-port :host "localhost")
  (browse-url
   (concat
    gmail-auth-endpoint "?"
    (url-build-query-string
     `(("client_id" ,gmail-auth-client-id)
       ("prompt" "consent")
       ("scope" ,(mapconcat 'identity scopes " "))
       ("response_type" "code")
       ("redirect_uri" ,(format "http://127.0.0.1:%d" gmail-auth-port))
       ("login_hint" ,login_hint))))))
