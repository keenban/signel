;;; sign.el --- Signal client for Emacs via signal-cli JSON-RPC  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Keenan Salandy <keenan@salandy.dev>
;; Maintainer: Keenan Salandy <keenan@salandy.dev>
;; URL: https://github.com/keenban/signel
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT a part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Sign.el provides a lightweight, text-based interface for Signal
;; within Emacs. It communicates with a running `signal-cli' daemon
;; via JSON-RPC.
;;
;; Features:
;; - Strict chat buffers (read-only history, guarded prompt).
;; - Inline image rendering.
;; - Sticker support (APNG->GIF conversion for animation).
;; - Auto-refreshing dashboard of active chats.
;; - Native Emacs desktop notifications.
;;
;; Prerequisites:
;; 1. signal-cli installed and in $PATH.
;; 2. A registered/linked Signal account.
;; 3. For animated stickers: `imagemagick' (specifically the `convert' command)
;;    must be available in your PATH.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'notifications)
(require 'button)
(require 'image)

;;; Configuration

(defgroup signel nil
  "Signal client for Emacs using signal-cli."
  :group 'comm
  :prefix "signel-")

(defcustom signel-account nil
  "The registered Signal phone number (e.g. +15550000000).
This must match the account registered with signal-cli."
  :type '(choice (const :tag "Not Set" nil) string))

(defcustom signel-cli-program "signal-cli"
  "Path to the signal-cli executable.
If signal-cli is not in your `exec-path', provide the absolute path here."
  :type 'string)

(defcustom signel-prompt "> "
  "The prompt string displayed in chat buffers."
  :type 'string)

(defcustom signel-auto-open-buffer t
  "If non-nil, automatically display the chat buffer when a message arrives."
  :type 'boolean)

;;; Faces

(defface signel-my-msg-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face applied to your own messages.")

(defface signel-other-msg-face
  '((t :inherit font-lock-variable-name-face :weight bold))
  "Face applied to messages from other users.")

(defface signel-timestamp-face
  '((t :inherit shadow))
  "Face for message timestamps.")

(defface signel-error-face
  '((t :inherit error))
  "Face for error messages.")

;;; Internal State

(defconst signel-process-name "signal-rpc"
  "Internal name for the signal-cli process.")

(defconst signel-buffer-stderr " *signel-stderr*"
  "Name of the hidden buffer used for process stderr.")

(defvar signel-rpc-id-counter 0
  "Counter for JSON-RPC request IDs.")

(defvar signel-request-buffer-map (make-hash-table :test 'equal)
  "Mapping of RPC ID to buffer name for error reporting.")

(defvar signel-contact-map (make-hash-table :test 'equal)
  "Cache of phone numbers to display names.")

(defvar signel-active-chats (make-hash-table :test 'equal)
  "Set of currently active chat IDs.")

(defvar signel-partial-line ""
  "Buffer string for incomplete JSON lines received from the process.")

;;; Logging

(defun signel-log (fmt &rest args)
  "Log debug info to *signel-log* using FMT and ARGS."
  (with-current-buffer (get-buffer-create "*signel-log*")
    (goto-char (point-max))
    (insert (format-time-string "[%H:%M:%S] "))
    (insert (apply #'format fmt args))
    (insert "\n")))

(defun signel-toggle-log ()
  "Display the debug log buffer."
  (interactive)
  (display-buffer (get-buffer-create "*signel-log*")))

;;; Process Infrastructure

;;;###autoload
(defun signel-start ()
  "Start the signal-cli JSON-RPC process."
  (interactive)
  (unless signel-account
    (user-error "Variable `signel-account' is not set"))

  (when (get-process signel-process-name)
    (delete-process signel-process-name))

  ;; State cleanup
  (setq signel-partial-line "")
  (clrhash signel-request-buffer-map)

  (let ((proc (make-process
               :name signel-process-name
               :buffer signel-buffer-stderr
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
  "Send a JSON-RPC payload with METHOD and PARAMS.
If TARGET-BUFFER is non-nil, map the request ID to that buffer for error handling."
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

;;; Parsing & Dispatch

(defun signel-process-filter (_proc string)
  "Accumulate output from _PROC and parse complete JSON objects from STRING."
  (setq signel-partial-line (concat signel-partial-line string))
  ;; DoS Protection: Reset buffer if it gets suspiciously large without a newline
  (when (> (length signel-partial-line) 100000)
    (setq signel-partial-line "")
    (signel-log "WARNING: Buffer overflow protection triggered. Dropped data."))

  (let ((lines (split-string signel-partial-line "\n")))
    (if (string-suffix-p "\n" signel-partial-line)
        (setq signel-partial-line "")
      (setq signel-partial-line (car (last lines)))
      (setq lines (butlast lines)))

    (dolist (line lines)
      (setq line (string-trim line))
      (when (and (not (string-empty-p line)) (string-prefix-p "{" line))
        (signel-log "RECV: %s" line)
        (condition-case err
            (let ((json (json-read-from-string line)))
              (signel-dispatch json))
          (error (signel-log "JSON Error: %s" err)))))))

(defun signel-process-sentinel (_proc event)
  "Log process EVENT for debugging."
  (signel-log "Process Event: %s" event)
  (when (string-prefix-p "exited" event)
    (message "Signel process exited.")))

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
  "Handle RPC errors for request ID using ERROR-OBJ."
  (let* ((buf-name (gethash id signel-request-buffer-map))
         (msg (alist-get 'message error-obj)))
    (signel-log "RPC Error [%s]: %s" id msg)
    (when (and buf-name (get-buffer buf-name))
      (with-current-buffer buf-name
        (signel-insert-system-msg (format "ERROR: %s" msg) 'signel-error-face)))))

(defun signel-handle-receive (params)
  "Handle new messages, attachments, stickers, and sync events from PARAMS."
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

              ;; Notify
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

;;; Media & Stickers

(defun signel-find-sticker (pack-id sticker-id)
  "Find the local sticker file for PACK-ID and STICKER-ID using manifest.json."
  (let* ((base-dir (expand-file-name "~/.local/share/signal-cli/stickers/"))
         (pack-dir (expand-file-name pack-id base-dir))
         (manifest-file (expand-file-name "manifest.json" pack-dir)))
    (if (file-exists-p manifest-file)
        ;; METHOD 1: Use manifest.json (Accurate)
        (condition-case nil
            (let* ((json-object-type 'alist)
                   (json-array-type 'list)
                   (manifest (json-read-file manifest-file))
                   (stickers (alist-get 'stickers manifest))
                   (sticker-info (seq-find (lambda (s) (= (alist-get 'id s) sticker-id)) stickers))
                   (file-name (alist-get 'file sticker-info)))
              (when file-name
                (expand-file-name file-name pack-dir)))
          (error nil))
      ;; METHOD 2: Fallback (Guessing)
      (let* ((path-no-ext (expand-file-name (number-to-string sticker-id) pack-dir))
             (path-webp (concat path-no-ext ".webp")))
        (cond
         ((file-exists-p path-webp) path-webp)
         ((file-exists-p path-no-ext) path-no-ext)
         (t nil))))))

(defun signel-guess-image-type (file)
  "Read the first few bytes of FILE to determine image type (png, webp, gif)."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file nil 0 12)
    (goto-char (point-min))
    (cond
     ((looking-at "\x89PNG") 'png)
     ((looking-at "GIF8") 'gif)
     ((and (looking-at "RIFF")
           (ignore-errors (forward-char 8) (looking-at "WEBP")))
      'webp)
     (t nil))))

(defun signel-convert-apng-to-gif (file)
  "Convert APNG FILE to a temporary GIF using ImageMagick `convert'.
Returns the path to the temporary GIF.  Uses `unwind-protect' to ensure cleanup."
  (let ((tmp-gif (make-temp-file "signel-sticker-" nil ".gif")))
    (if (executable-find "convert")
        (with-temp-buffer
          ;; Run: convert apng:INPUT -coalesce gif:OUTPUT
          (if (eq 0 (call-process "convert" nil nil nil
                                  (concat "apng:" file)
                                  "-coalesce"
                                  tmp-gif))
              tmp-gif
            (signel-log "Failed to convert APNG to GIF. `convert' exit code non-zero.")
            ;; Cleanup failed file if it was created
            (when (file-exists-p tmp-gif) (delete-file tmp-gif))
            nil))
      (signel-log "ImageMagick `convert' not found. Cannot animate APNG.")
      (when (file-exists-p tmp-gif) (delete-file tmp-gif))
      nil)))

(defun signel-insert-media (attachments sticker)
  "Insert buttons or inline images for ATTACHMENTS and STICKER with animation support."
  ;; 1. Handle Stickers
  (when sticker
    (let* ((pack-id (alist-get 'packId sticker))
           (sticker-id (alist-get 'stickerId sticker))
           (emoji (or (alist-get 'emoji sticker) "🧩"))
           (file (if (and pack-id sticker-id)
                     (signel-find-sticker pack-id sticker-id)
                   nil)))
      (insert "\n")
      (cond
       ((and file (file-exists-p file))
        (let* ((type (signel-guess-image-type file))
               (final-file file)
               (final-type type)
               (converted nil))

          ;; CONVERSION LOGIC: If PNG (APNG), try to convert to GIF
          (when (and (eq type 'png) (executable-find "convert"))
            (let ((gif (signel-convert-apng-to-gif file)))
              (when gif
                (setq final-file gif)
                (setq final-type 'gif)
                (setq converted t))))

          ;; Load the image
          (let ((image (create-image final-file final-type nil :max-width 150)))
            ;; Clean up temp file if we created one
            (when converted
              (run-at-time "5 sec" nil (lambda (f) (when (file-exists-p f) (delete-file f))) final-file))

            (if image
                (progn
                  (insert-image image)
                  (when (fboundp 'image-animate)
                    (image-animate image nil t))) ;; Loop forever
              ;; Render failed
              (insert (propertize (format "[Sticker %s (Render Failed)]" emoji)
                                  'face 'font-lock-warning-face))))))
       ;; File not found
       (t
        (insert (propertize (format "[Sticker %s]" emoji)
                            'face 'font-lock-constant-face))))))

  ;; 2. Handle Attachments
  (when attachments
    (let ((att-list (if (vectorp attachments) (append attachments nil) attachments)))
      (dolist (att att-list)
        (let* ((path (alist-get 'storedFilename att))
               (type (alist-get 'contentType att))
               (name (or (alist-get 'filename att) "attachment")))
          (insert "\n")
          (cond
           ;; A: Inline Image
           ((and path (string-prefix-p "image/" type) (file-exists-p path))
            (let ((image (create-image path nil nil :max-width 400)))
              (insert-image image)
              (when (and (fboundp 'image-animate) (image-multi-frame-p image))
                (image-animate image nil t))))
           ;; B: File Button
           ((and path (file-exists-p path))
            (insert-button (format "[File: %s]" name)
                           'action (lambda (_) (browse-url-of-file path))
                           'face 'link
                           'help-echo (format "Type: %s\nPath: %s" type path)))
           ;; C: Missing/Not Downloaded
           (t
            (let* ((std-path (expand-file-name (format "~/.local/share/signal-cli/attachments/%s" (alist-get 'id att))))
                   (exists (file-exists-p std-path)))
              (if exists
                  (if (string-prefix-p "image/" type)
                      (let ((image (create-image std-path nil nil :max-width 400)))
                        (insert-image image)
                        (when (and (fboundp 'image-animate) (image-multi-frame-p image))
                          (image-animate image nil t)))
                    (insert-button (format "[File: %s]" name)
                                   'action (lambda (_) (browse-url-of-file std-path))
                                   'face 'link))
                (insert-button (format "[File: %s (Not Downloaded)]" name)
                               'action (lambda (_) (message "File not found at %s" std-path))
                               'face 'font-lock-comment-face))))))))))

;;; Buffer & UI Management

(defvar-local signel-chat-id nil
  "The Signal recipient ID associated with the current buffer.")

(defvar-local signel-input-marker nil
  "Marker indicating the start of the editable input area.")

(defun signel-guard-cursor ()
  "Ensure cursor stays in the editable prompt area."
  (let ((limit (if signel-input-marker (marker-position signel-input-marker) (point-min))))
    (when (< (point) limit)
      (goto-char limit))))

(define-derived-mode signel-chat-mode fundamental-mode "Signel"
  "Major mode for Signal chats."
  (setq-local paragraph-start (regexp-quote signel-prompt))
  (visual-line-mode 1)

  ;; Setup Input Marker
  (setq signel-input-marker (make-marker))

  (add-hook 'post-command-hook #'signel-guard-cursor nil t)
  (local-set-key (kbd "RET") #'signel-send-input)
  (local-set-key (kbd "C-c C-c") #'signel-send-input)
  (local-set-key (kbd "C-c C-a") #'signel-attach-file))

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
  "Draw the input prompt and update the input marker."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert (propertize signel-prompt
                        'read-only t
                        'face 'minibuffer-prompt
                        'rear-nonsticky '(read-only face)
                        'front-sticky '(read-only face)))
    ;; Update the marker to the end of the prompt
    (set-marker signel-input-marker (point))))

(defun signel-insert-msg (id name text attachments sticker is-me)
  "Insert text and media into the buffer for chat ID.
NAME is the sender, TEXT is the content, ATTACHMENTS and STICKER contain
media data, and IS-ME is non-nil if the message is from the user."
  (with-current-buffer (signel-get-buffer id)
    (let ((inhibit-read-only t)
          (name-face (if is-me 'signel-my-msg-face 'signel-other-msg-face)))
      (save-excursion
        ;; Move to just before the prompt
        (goto-char (marker-position signel-input-marker))
        (forward-line 0) ;; Ensure we are at start of prompt line (usually empty above)

        ;; If the previous line isn't a newline, insert one
        (unless (bolp) (insert "\n"))

        ;; Delete the prompt from the view momentarily (optional, but cleaner)
        (delete-region (point) (point-max))

        ;; Insert Message
        (insert (propertize (format-time-string "[%H:%M] " ) 'face 'signel-timestamp-face))
        (insert (propertize (concat "<" name "> ") 'face name-face))
        (when text (insert text))
        (when (or attachments sticker)
          (when text (insert "\n"))
          (signel-insert-media attachments sticker))
        (insert "\n")

        ;; Redraw Prompt at the new bottom
        (signel-draw-prompt)))

    (let ((win (get-buffer-window (signel-get-buffer id))))
      (when win (set-window-point win (point-max))))))

(defun signel-insert-system-msg (text face)
  "Insert a system message with TEXT using FACE."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (if signel-input-marker (marker-position signel-input-marker) (point-max)))
      (forward-line 0)
      (delete-region (point) (point-max))
      (insert (propertize (concat "*** " text "\n") 'face face))
      (signel-draw-prompt))))

;;; Interactive Commands

(defun signel-send-input ()
  "Send the input from the prompt to the current chat."
  (interactive)
  (let* ((start (marker-position signel-input-marker))
         (end (point-max))
         (text (string-trim (buffer-substring-no-properties start end))))
    (unless (string-empty-p text)
      (let ((inhibit-read-only t))
        (delete-region start end))

      (let ((is-group (not (string-prefix-p "+" signel-chat-id)))
            (params `((message . ,text))))
        (if is-group
            (push `(groupId . ,signel-chat-id) params)
          (push `(recipient . [,signel-chat-id]) params))
        (signel-send-rpc "send" params (current-buffer)))

      (signel-insert-msg signel-chat-id "Me" text nil nil t))))

;;;###autoload
(defun signel-attach-file (file-path)
  "Send FILE-PATH as an attachment to the current chat."
  (interactive "fAttachment: ")
  (unless signel-chat-id
    (user-error "Not in a Signal chat buffer"))
  (let* ((full-path (expand-file-name file-path))
         (is-group (not (string-prefix-p "+" signel-chat-id)))
         (params `((attachments . [,full-path]))))
    (if is-group
        (push `(groupId . ,signel-chat-id) params)
      (push `(recipient . [,signel-chat-id]) params))
    (signel-send-rpc "send" params (current-buffer))
    (signel-insert-msg signel-chat-id "Me"
                       (format "[Sending: %s]" (file-name-nondirectory full-path))
                       nil nil t)))

;;;###autoload
(defun signel-chat (recipient)
  "Open a chat buffer for RECIPIENT (phone number or group ID)."
  (interactive "sSignal Recipient (+Phone): ")
  (let ((buffer (signel-get-buffer recipient)))
    (switch-to-buffer buffer)
    (message "Chat opened.")))

;;; Dashboard

(defvar signel-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'signel-dashboard-open-entry)
    (define-key map (kbd "n") #'forward-button)
    (define-key map (kbd "p") #'backward-button)
    (define-key map (kbd "g") #'signel-dashboard-refresh)
    map)
  "Keymap for `signel-dashboard-mode'.")

(define-derived-mode signel-dashboard-mode special-mode "Signel List"
  "Major mode for the list of active Signal chats.")

(defun signel-dashboard-draw ()
  "Redraw the dashboard content."
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
  "Refresh the dashboard buffer."
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
  "Open chat for the button at point or BTN."
  (interactive)
  (let* ((button (or btn (button-at (point))))
         (id (and button (button-get button 'signel-id))))
    (if id
        (signel-chat id)
      (user-error "No chat entry at point"))))

(provide 'signel)
;;; signel.el ends here
