;;; signel.el --- Signal client for Emacs via signal-cli JSON-RPC  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Keenan Salandy <keenan@salandy.dev>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: comm, signal, chat, external
;; URL: https://github.com/keenban/signel

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; Sign.el provides a lightweight, text-based interface for Signal
;; within Emacs. It communicates with a running 'signal-cli' daemon
;; via JSON-RPC.

;; Prerequisites:
;; You must have 'signal-cli' installed and your account linked/registered.
;; Start the daemon outside Emacs or let Signel manage the process.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'notifications)
(require 'button)
(require 'image)

;; -----------------------------------------------------------------------------
;; 1. CONFIGURATION
;; -----------------------------------------------------------------------------

(defgroup signel nil
  "Signal client for Emacs using signal-cli."
  :group 'comm
  :prefix "signel-")

(defcustom signel-account nil
  "The registered Signal phone number (e.g. +15550000000).
This must match the account registered with signal-cli."
  :type '(choice (const :tag "Not Set" nil) string)
  :group 'signel)

(defcustom signel-cli-program "signal-cli"
  "Path to the signal-cli executable.
If signal-cli is not in your $PATH, provide the absolute path here."
  :type 'string
  :group 'signel)

(defcustom signel-prompt "> "
  "The prompt string displayed in chat buffers."
  :type 'string
  :group 'signel)

(defcustom signel-auto-open-buffer t
  "If non-nil, automatically display the chat buffer when a message arrives."
  :type 'boolean
  :group 'signel)

;; Faces

(defface signel-my-msg-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face applied to your own messages."
  :group 'signel)

(defface signel-other-msg-face
  '((t :inherit font-lock-variable-name-face :weight bold))
  "Face applied to messages from other users."
  :group 'signel)

(defface signel-timestamp-face
  '((t :inherit shadow))
  "Face for message timestamps."
  :group 'signel)

(defface signel-error-face
  '((t :inherit error))
  "Face for error messages."
  :group 'signel)

;; Internal State

(defvar signel-process-name "signal-rpc"
  "Internal name for the signal-cli process.")

(defvar signel-rpc-id-counter 0
  "Counter for JSON-RPC request IDs.")

(defvar signel-request-buffer-map (make-hash-table :test 'equal)
  "Mapping of RPC ID to buffer name for error reporting.")

(defvar signel-contact-map (make-hash-table :test 'equal)
  "Cache of phone numbers to display names.")

(defvar signel-active-chats (make-hash-table :test 'equal)
  "Set of currently active chat IDs.")

;; -----------------------------------------------------------------------------
;; 2. LOGGING & DEBUGGING
;; -----------------------------------------------------------------------------

(defun signel-log (fmt &rest args)
  "Log debug info to *signel-log*."
  (let ((buf (get-buffer-create "*signel-log*")))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert (format-time-string "[%H:%M:%S] "))
      (insert (apply #'format fmt args))
      (insert "\n"))))

(defun signel-toggle-log ()
  "Show the debug log."
  (interactive)
  (display-buffer (get-buffer-create "*signel-log*")))

;; -----------------------------------------------------------------------------
;; 3. PROCESS INFRASTRUCTURE
;; -----------------------------------------------------------------------------

;;;###autoload
(defun signel-start ()
  "Start the signal-cli JSON-RPC process."
  (interactive)
  (unless signel-account
    (user-error "Variable `signel-account' is not set. Please configure it"))

  (when (get-process signel-process-name)
    (delete-process signel-process-name))

  (let ((proc (make-process
               :name signel-process-name
               :buffer " *signel-stderr*"
               :command (list signel-cli-program "-a" signel-account "jsonRpc")
               :filter #'signel-process-filter
               :sentinel #'signel-process-sentinel
               :coding 'utf-8-unix)))
    (set-process-query-on-exit-flag proc nil)
    (signel-log "Signel service started.")
    (message "Signel service started.")))

(defun signel-stop ()
  "Stop the Signel service."
  (interactive)
  (when (get-process signel-process-name)
    (delete-process signel-process-name)
    (message "Signel service stopped.")))

(defun signel-send-rpc (method params &optional target-buffer)
  "Send a JSON-RPC payload."
  (unless (get-process signel-process-name)
    (error "Signel service not running. M-x signel-start"))

  (let* ((id (cl-incf signel-rpc-id-counter))
         (req `((jsonrpc . "2.0")
                (method . ,method)
                (params . ,params)
                (id . ,id)))
         (json-str (json-encode req)))

    (when target-buffer
      (puthash id (buffer-name target-buffer) signel-request-buffer-map))

    (signel-log "SEND: %s" json-str)
    (process-send-string signel-process-name (concat json-str "\n"))
    id))

;; -----------------------------------------------------------------------------
;; 4. PARSING & DISPATCH
;; -----------------------------------------------------------------------------

(defvar signel-partial-line "")

(defun signel-process-filter (proc string)
  "Accumulate output from PROC and parse complete JSON objects from STRING."
  (setq signel-partial-line (concat signel-partial-line string))
  (let ((lines (split-string signel-partial-line "\n")))
    (if (string-suffix-p "\n" signel-partial-line)
        (setq signel-partial-line "")
      (setq signel-partial-line (car (last lines)))
      (setq lines (butlast lines)))

    (dolist (line lines)
      (setq line (string-trim line))
      ;; signal-cli may emit non-JSON log lines; ignore them
      (when (and (not (string-empty-p line)) (string-prefix-p "{" line))
        (signel-log "RECV: %s" line)
        (condition-case err
            (let ((json (json-read-from-string line)))
              (signel-dispatch json))
          (error (signel-log "JSON Error: %s" err)))))))

(defun signel-process-sentinel (_proc event)
  (signel-log "Process Event: %s" event))

(defun signel-dispatch (json)
  "Dispatch JSON object to appropriate handler."
  (let ((method (alist-get 'method json))
        (error-obj (alist-get 'error json))
        (id (alist-get 'id json))
        (params (alist-get 'params json)))
    (cond
     ((string= method "receive") (signel-handle-receive params))
     (error-obj (signel-handle-error id error-obj)))))

(defun signel-handle-error (id error-obj)
  "Handle RPC errors."
  (let* ((buf-name (gethash id signel-request-buffer-map))
         (msg (alist-get 'message error-obj)))
    (signel-log "RPC Error [%s]: %s" id msg)
    (when (and buf-name (get-buffer buf-name))
      (with-current-buffer buf-name
        (signel-insert-system-msg (format "ERROR: %s" msg) 'signel-error-face)))))

(defun signel-handle-receive (params)
  "Handle new messages, attachments, stickers, and sync events."
  (let* ((envelope (alist-get 'envelope params))
         (source (or (alist-get 'sourceNumber envelope) (alist-get 'source envelope)))
         (source-name (alist-get 'sourceName envelope))
         (data (alist-get 'dataMessage envelope))
         (sync (alist-get 'syncMessage envelope))
         (typing (alist-get 'typingMessage envelope))
         (group-info (alist-get 'groupInfo data)))

    (when (and source source-name)
      (puthash source source-name signel-contact-map))

    (let ((chat-id (if group-info (alist-get 'groupId group-info) source)))
      (when chat-id
        (puthash chat-id t signel-active-chats)
        (signel-dashboard-refresh)

        ;; 1. Incoming Data (Text OR Attachments)
        (when data
          (let ((msg-text (alist-get 'message data))
                (attachments (alist-get 'attachments data))
                (sticker (alist-get 'sticker data))
                (sender (or source-name source)))

            (when (or msg-text attachments sticker)
              (signel-insert-msg chat-id sender msg-text attachments sticker nil)

              (let ((notify-body (cond (msg-text msg-text)
                                       (sticker "[Sticker]")
                                       (attachments "[Attachment]")
                                       (t "New Message"))))
                (notifications-notify :title (format "Signel: %s" sender)
                                      :body notify-body))

              (when signel-auto-open-buffer
                (display-buffer (signel-get-buffer chat-id))))))

        ;; 2. Sync (My sent messages)
        (when sync
          (let* ((sent (alist-get 'sentMessage sync))
                 (dest (alist-get 'destinationNumber sent))
                 (msg-text (alist-get 'message sent))
                 (attachments (alist-get 'attachments sent))
                 (sticker (alist-get 'sticker sent)))
            (when (and dest (or msg-text attachments sticker))
              (signel-insert-msg dest "Me" msg-text attachments sticker t))))

        ;; 3. Typing
        (when (and typing (string= "STARTED" (alist-get 'action typing)))
          (with-current-buffer (signel-get-buffer chat-id)
            (setq mode-line-process (format " [%s...]" (or source-name source)))))))))

;; -----------------------------------------------------------------------------
;; 4. BUFFER & UI MANAGEMENT
;; -----------------------------------------------------------------------------

(defvar-local signel-chat-id nil)

(defun signel-guard-cursor ()
  "Ensure cursor stays in the editable prompt area."
  (let ((prompt-start (signel-prompt-start-pos)))
    (when (< (point) prompt-start)
      (goto-char prompt-start))))

(defun signel-prompt-start-pos ()
  "Return the position where the user input area begins."
  (save-excursion
    (goto-char (point-max))
    (forward-line 0)
    (if (looking-at (regexp-quote signel-prompt))
        (+ (point) (length signel-prompt))
      (point-max))))

(define-derived-mode signel-chat-mode fundamental-mode "Signel"
  "Major mode for Signal chats."
  (setq-local paragraph-start (regexp-quote signel-prompt))
  (visual-line-mode 1)
  (add-hook 'post-command-hook #'signel-guard-cursor nil t)
  (local-set-key (kbd "RET") #'signel-send-input)
  (local-set-key (kbd "C-c C-c") #'signel-send-input))

(defun signel-get-buffer (id)
  "Get or create a chat buffer for ID."
  (let* ((buf-name (format "*Signel: %s*" id))
         (buffer (get-buffer buf-name)))
    (unless buffer
      (setq buffer (get-buffer-create buf-name))
      (with-current-buffer buffer
        (signel-chat-mode)
        (setq signel-chat-id id)
        (signel-draw-prompt)))
    buffer))

(defun signel-draw-prompt ()
  "Draw the input prompt with sticky read-only properties."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert (propertize signel-prompt
                        'read-only t
                        'face 'minibuffer-prompt
                        'rear-nonsticky '(read-only face)
                        'front-sticky '(read-only face)))))

(defun signel-insert-media (attachments sticker)
  "Insert buttons or inline images for media."
  (when sticker
    (insert (propertize "[Sticker]" 'face 'font-lock-comment-face)))

  (when attachments
    (dolist (att attachments)
      (let* ((path (alist-get 'storedFilename att))
             (type (alist-get 'contentType att))
             (name (or (alist-get 'filename att) "attachment")))

        (insert " ")
        (cond
         ((and path (string-prefix-p "image/" type) (file-exists-p path))
          (let ((image (create-image path nil nil :max-width 400)))
            (insert-image image)))
         (t
          (insert-button (format "[File: %s]" name)
                         'action (lambda (_) (if path
                                                 (browse-url-of-file path)
                                               (message "File not available locally")))
                         'help-echo (format "Type: %s\nPath: %s" type path))))
        (insert "\n")))))

(defun signel-insert-msg (id name text attachments sticker is-me)
  "Insert text and media into the buffer."
  (with-current-buffer (signel-get-buffer id)
    (let ((inhibit-read-only t)
          (name-face (if is-me 'signel-my-msg-face 'signel-other-msg-face)))
      (save-excursion
        (goto-char (point-max))
        (forward-line 0)
        (when (looking-at (regexp-quote signel-prompt))
          (delete-region (point) (point-max)))

        ;; Header
        (insert (propertize (format-time-string "[%H:%M] " ) 'face 'signel-timestamp-face))
        (insert (propertize (concat "<" name "> ") 'face name-face))

        ;; Body
        (when text (insert text))
        (when (or attachments sticker)
          (when text (insert "\n"))
          (signel-insert-media attachments sticker))

        (insert "\n")
        (signel-draw-prompt)))
    (let ((win (get-buffer-window (signel-get-buffer id))))
      (when win (set-window-point win (point-max))))))

(defun signel-insert-system-msg (text face)
  "Insert a system/error message."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-max))
      (forward-line 0)
      (when (looking-at (regexp-quote signel-prompt))
        (delete-region (point) (point-max)))
      (insert (propertize (concat "*** " text "\n") 'face face))
      (signel-draw-prompt))))

;; -----------------------------------------------------------------------------
;; 5. USER COMMANDS
;; -----------------------------------------------------------------------------

(defun signel-send-input ()
  "Read input, clear buffer, send RPC."
  (interactive)
  (let ((start (signel-prompt-start-pos))
        (end (point-max))
        (text ""))

    (setq text (string-trim (buffer-substring-no-properties start end)))

    (unless (string-empty-p text)
      (let ((inhibit-read-only t))
        (delete-region start end))

      (let ((is-group (not (string-prefix-p "+" signel-chat-id)))
            (params `((message . ,text))))

        (if is-group
            (push `(groupId . ,signel-chat-id) params)
          (push `(recipient . [,signel-chat-id]) params))

        (signel-send-rpc "send" params (current-buffer)))

      ;; Insert local echo
      (signel-insert-msg signel-chat-id "Me" text nil nil t))))

;;;###autoload
(defun signel-attach-file (file-path)
  "Send a file attachment to the current chat."
  (interactive "fAttachment: ")
  (unless signel-chat-id
    (user-error "Not in a Signal chat buffer"))

  (let ((is-group (not (string-prefix-p "+" signel-chat-id)))
        (params `((attachments . [,file-path]))))

    (if is-group
        (push `(groupId . ,signel-chat-id) params)
      (push `(recipient . [,signel-chat-id]) params))

    (signel-send-rpc "send" params (current-buffer))
    (signel-insert-msg signel-chat-id "Me" (format "[Sending: %s]" (file-name-nondirectory file-path)) nil nil t)))

;;;###autoload
(defun signel-chat (recipient)
  "Open a chat buffer."
  (interactive "sSignal Recipient (+Phone): ")
  (let ((buffer (signel-get-buffer recipient)))
    (switch-to-buffer buffer)
    (message "Chat opened.")))

;; -----------------------------------------------------------------------------
;; 6. DASHBOARD
;; -----------------------------------------------------------------------------

(defvar signel-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'signel-dashboard-open-entry)
    (define-key map (kbd "n") #'forward-button)
    (define-key map (kbd "p") #'backward-button)
    (define-key map (kbd "g") #'signel-dashboard-refresh)
    map))

(define-derived-mode signel-dashboard-mode special-mode "Signel List"
  "List of active Signal chats.")

(defun signel-dashboard-draw ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "Active Chats:\n")
    (insert "-------------\n")
    (maphash (lambda (id _)
               (let ((name (gethash id signel-contact-map id)))
                 (insert-button (format "%s (%s)" name id)
                                'action #'signel-dashboard-open-entry
                                'signel-id id
                                'follow-link t)
                 (insert "\n")))
             signel-active-chats)))

(defun signel-dashboard-refresh ()
  (interactive)
  (let ((buf (get-buffer "*Signel List*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((line (line-number-at-pos)))
          (signel-dashboard-draw)
          (goto-char (point-min))
          (forward-line (1- line)))))))

;;;###autoload
(defun signel-dashboard ()
  "Open the Signel Dashboard."
  (interactive)
  (let ((buf (get-buffer-create "*Signel List*")))
    (with-current-buffer buf
      (signel-dashboard-mode)
      (signel-dashboard-draw))
    (switch-to-buffer buf)))

(defun signel-dashboard-open-entry (&optional btn)
  "Open chat for the button at point."
  (interactive)
  (let* ((button (or btn (button-at (point))))
         (id (and button (button-get button 'signel-id))))
    (if id
        (signel-chat id)
      (user-error "No chat entry at point"))))

(provide 'signel)
;;; signel.el ends here
