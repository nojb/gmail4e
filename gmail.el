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

(defun gmail-main-view-real ()
  (let ((buf (get-buffer-create gmail-main-buffer-name))
        (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (insert "Hello, Gmail!")
      (gmail-main-mode))))

(defun gmail-main-view ()
  "Create the Gmail main view, and switch to it."
  (gmail-main-view-real)
  (switch-to-buffer gmail-main-buffer-name)
  (goto-char (point-min)))
