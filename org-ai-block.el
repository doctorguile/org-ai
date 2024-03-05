;;; org-ai-block.el --- org-ai special block helpers -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;; org-ai.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; org-ai.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with org-ai.el.
;; If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Defines functions for dealing with #+begin_ai..#+end_ai special blocks

;;; Code:

(require 'org)
(require 'org-element)
(require 'org-macs)

(when (and (boundp 'org-protecting-blocks) (listp org-protecting-blocks))
  (add-to-list 'org-protecting-blocks "ai"))

;; `org-element-with-disabled-cache' is not available pre org-mode 9.6.6, i.e.
;; emacs 28 does not ship with it
(defmacro org-ai--org-element-with-disabled-cache (&rest body)
  "Run BODY without active org-element-cache."
  (declare (debug (form body)) (indent 0))
  `(cl-letf (((symbol-function #'org-element--cache-active-p) (lambda (&rest _) nil)))
     ,@body))

(defun org-ai-special-block ()
  "Are we inside a #+begin_ai...#+end_ai block?"
  (org-ai--org-element-with-disabled-cache ;; with cache enabled we get weird Cached element is incorrect warnings
    (cl-loop with context = (org-element-context)
             while (and context
                        (not (equal 'special-block (org-element-type context)))
                        (not (string-equal "ai" (org-element-property :type context))))
             do (setq context (org-element-property :parent context))
             finally return context)))

(defun org-ai-get-block-info (&optional context)
  "Parse the header of #+begin_ai...#+end_ai block.
`CONTEXT' is the context of the special block. Return an alist of
key-value pairs."
  (let* ((context (or context (org-ai-special-block)))
         (header-start (org-element-property :post-affiliated context))
         (header-end (org-element-property :contents-begin context)))
    (if (or (not header-start) (not header-end))
        (error "Error: org-ai was not able to extract the beginning/end of the org-ai block")
      (save-match-data
        (let* ((string (string-trim (buffer-substring-no-properties header-start header-end)))
               (string (string-trim-left (replace-regexp-in-string "^#\\+begin_ai" "" string))))
          (org-babel-parse-header-arguments string))))))

(defun org-ai--string-equal-ignore-case (string1 string2)
  "Helper for backwards compat.
STRING1 and STRING2 are strings. Return t if they are equal
ignoring case."
  (eq 't (compare-strings string1 0 nil string2 0 nil t)))

(defun org-ai-get-block-content (&optional context)
  "Extracts the text content of the #+begin_ai...#+end_ai block.
`CONTEXT' is the context of the special block.

Will expand noweb templates if an 'org-ai-noweb' property or
'noweb' header arg is \"yes\""

  (let* ((context (or context (org-ai-special-block)))
         (content-start (org-element-property :contents-begin context))
         (content-end (org-element-property :contents-end context))
         (unexpanded-content (string-trim (buffer-substring-no-properties content-start content-end)))
         (info (org-ai-get-block-info context))
         (noweb-control (or (alist-get :noweb info nil)
                            (org-entry-get (point) "org-ai-noweb" 1)
                            "no"))
         (content (if (org-ai--string-equal-ignore-case "yes" noweb-control)
                      (org-babel-expand-noweb-references (list "markdown" unexpanded-content))
                      unexpanded-content)))
    content))

(defun org-ai--request-type (info)
  "Look at the header of the #+begin_ai...#+end_ai block.
returns the type of request. `INFO' is the alist of key-value
pairs from `org-ai-get-block-info'."
  (cond
   ((not (eql 'x (alist-get :chat info 'x))) 'chat)
   ((not (eql 'x (alist-get :completion info 'x))) 'completion)
   ((not (eql 'x (alist-get :image info 'x))) 'image)
   ((not (eql 'x (alist-get :sd-image info 'x))) 'sd-image)
   ((not (eql 'x (alist-get :local info 'x))) 'local-chat)
   (t 'chat)))

(defun org-ai--chat-role-regions ()
  "Splits the special block by role prompts."
  (if-let* ((context (org-ai-special-block))
            (content-start (org-element-property :contents-begin context))
            (content-end (org-element-property :contents-end context)))
      (let ((result (save-match-data
                      (save-excursion
                        (goto-char content-start)
                        (cl-loop with result
                                 while (search-forward-regexp "\\[SYS\\]:\\|\\[ME\\]:\\|\\[AI\\]:" content-end t)
                                 do (push (match-beginning 0) result)
                                 finally return result)))))
        (if result
            (cl-concatenate 'list (list content-start) (reverse result) (list content-end))
          (list content-start content-end)))))


(defun org-ai-mark-last-region ()
  "Marks the last prompt in an org-ai block."
  (interactive)
  (when-let* ((regions (reverse (org-ai--chat-role-regions)))
              (last-region-end (pop regions))
              (last-region-start (pop regions)))
        (goto-char last-region-end)
        (push-mark last-region-start t t)))

(defun org-ai-mark-region-at-point ()
  "Marks the prompt at point."
  (interactive)
  (when-let* ((regions (org-ai--chat-role-regions))
              (start (cl-find-if (lambda (x) (>= (point) x)) (reverse regions)))
              (end (cl-find-if (lambda (x) (<= (point) x)) regions)))
    (when (= start end)
      (setq end (cl-find-if (lambda (x) (< start x)) regions)))
    (when (not end)
      (setq end start)
      (setq start (cl-find-if (lambda (x) (> end x)) (reverse regions))))
    (when (and start end)
      (goto-char start)
      (push-mark end t t)
      (cons start end))))

(defun org-ai-kill-region-at-point (&optional arg)
  "Kills the prompt at point.
The numeric `ARG' can be used for killing the last n."
  (interactive "P")
  (cl-loop repeat (or arg 1)
           do (when-let ((region (org-ai-mark-region-at-point)))
                (cl-destructuring-bind (start . end) region
                  (kill-region end start)))))

(defun is-jpeg-file (filename)
  "Return t if FILENAME is a JPEG file."
  (let ((case-fold-search t))
    (string-match "\\.jpe?g\\'" filename)))

;; OpenAI: For low res mode, we expect a 512px x 512px image.
;; For high res mode, the short side of the image should be less than 768px and the long side should be less than 2,000px
(defun resize-image-for-openai-vision (image-path)
  "Convert/Resize the image to jpg, if it isn't jpg or doesn't meet size constraints, saving the resized image as a new temp jpg file.
Resize such that the short side is no more than 768px, while the long side is no more than 2000px, maintaining aspect ratio."
  (interactive "fImage Path: ")
  (-let* ((image-path (expand-file-name image-path))
          ((width . height) (image-size (create-image image-path) t))
          (short-side (min width height))
          (long-side (max width height))
          (landscape-mode (= long-side width))
          (resize-needed (or (> short-side 768) (> long-side 2000) (not (is-jpeg-file image-path)))))
    (when resize-needed
      (let* ((new-short-side (if (> short-side 768) 768 short-side))
             (new-long-side (if (> long-side 2000) 2000 long-side))
             (supported-aspect-ratio (/ 2000.0 768.0))
             (aspect-ratio (/ (float long-side) short-side))
             (temp-file-path (concat (file-name-sans-extension image-path) "_resized.jpg")))
        (unless (and (<= short-side 768) (<= long-side 2000))
          (if (> aspect-ratio supported-aspect-ratio)
              (setq new-short-side (max 1 (floor (/ new-long-side aspect-ratio))))
            (setq new-long-side (max 1 (floor (* new-short-side aspect-ratio))))))
        ;; Use ImageMagick's convert tool to resize and save to temp-file-path
        (shell-command (format "convert %s -resize %dx%d! %s"
                               (shell-quote-argument image-path)
                               (if landscape-mode new-long-side new-short-side)
                               (if landscape-mode new-short-side new-long-side)
                               (shell-quote-argument temp-file-path)))
        (message "Image resized to: %dx%d, saved as %s" new-long-side new-short-side temp-file-path)
        temp-file-path))))

(defun extract-org-image-link-file-path (filelink)
  "Extract and return the path after 'file:'
If the file is not an image or doesn't exist, return nil."
  (when (string-prefix-p "file:" filelink)
    (let ((file-extension-re (image-file-name-regexp))
          (path (expand-file-name (substring filelink (length "file:")))))
      (when (and path (string-match-p file-extension-re path) (file-exists-p path))
        path))))

(defun base64-jpg-data-block (filepath)
  "Encode file as base64 data"
  (concat "data:image/jpeg;base64,"
          (with-temp-buffer
            (insert-file-contents-literally filepath)
            (base64-encode-region (point-min) (point-max) t)
            (buffer-string))))

(defun org-link-to-openai-message-block (link-matched full-matched)
  (if (string-prefix-p "http" link-matched)
      `((type . "image_url")
        (image_url . ((url . ,link-matched))))
    (if-let* ((local-imgfile (extract-org-image-link-file-path link-matched)))
        (let* ((converted-temp-imgfile (resize-image-for-openai-vision local-imgfile))
               (base64-data (base64-jpg-data-block (or converted-temp-imgfile local-imgfile))))
          (when converted-temp-imgfile (delete-file converted-temp-imgfile))
          `((type . "image_url")
            (image_url . ((url . ,base64-data)))))
      `((type . "text")
        (text . ,full-matched)))))

(defun parse-user-content-as-openai-vision (&optional content)
  "Parse the content into a vector of text and image links.
If no image found, return NIL"
  ;; https://platform.openai.com/docs/guides/vision?lang=curl
  (interactive)
  (unless content (setq content (buffer-string)))
  ;; matching org links [[file:...][description]]
  (let ((regex "\\[\\[\\(\\(?:file:\\|https?://\\)[^]]*?\\)\\]\\(?:\\[[^]]*?\\]\\)?\\]")
        (pos 0)  ; Tracking position within the string.
        segments  ; List to hold the parsed segments.
        start
        match-end
        valid-image-url-found)
    ;; Loop over the content string, searching for matches to the regex.
    (while (string-match regex content pos)
      (setq start (match-beginning 0)
            match-end (match-end 0))
      ;; If there's text before the current match, add it as a text segment.
      (when (> start pos)
        (push `((type . "text")
                (text . ,(substring content pos start)))
              segments))
      (let ((msgblk (org-link-to-openai-message-block
                     (match-string-no-properties 1 content)
                     (match-string-no-properties 0 content))))
        ;; Add the current match as a link segment.
        (push msgblk segments)
        (-let ((((typekey . typeval) . ignore) msgblk))
          (when (string= typeval "image_url")
            (setq valid-image-url-found t))))
      ;; Update pos to the end of the current match, to search for the next one.
      (setq pos match-end))

    ;; If there's text after the last match, add it as a text segment.
    (when (< pos (length content))
      (push `((type . "text") (text . ,(substring content pos))) segments))

    (setq segments (nreverse segments))

    ;; For debugging or interactive use: print the structured list.
    (when (called-interactively-p 'any)
      (with-output-to-temp-buffer "*temp-buffer*"
        (princ (if valid-image-url-found segments content))))

    ;; Return the structured list of segments if there are image embedded
    ;; othewise if it's just text keep it as is
    (when valid-image-url-found
      (apply #'vector segments))))

(defun org-ai--collect-chat-messages (content-string &optional default-system-prompt persistant-sys-prompts)
  "Takes `CONTENT-STRING' and splits it by [SYS]:, [ME]: and [AI]: markers.
If `PERSISTANT-SYS-PROMPTS' is non-nil, [SYS] prompts are
intercalated. The [SYS] prompt used is either
`DEFAULT-SYSTEM-PROMPT' or the first [SYS] prompt found in
`CONTENT-STRING'."
  (with-temp-buffer
    (erase-buffer)
    (insert content-string)
    (goto-char (point-min))

    (let* (;; collect all positions before [ME]: and [AI]:
           (sections (cl-loop while (search-forward-regexp "\\[SYS\\]:\\|\\[ME\\]:\\|\\[AI\\]:" nil t)
                              collect (save-excursion
                                        (goto-char (match-beginning 0))
                                        (point))))

           ;; make sure we have from the beginning if there is no first marker
           (sections (if (not sections)
                         (list (point-min))
                       (if (not (= (car sections) (point-min)))
                           (cons (point-min) sections)
                         sections)))

           (parts (cl-loop for (start end) on sections by #'cdr
                           collect (string-trim (buffer-substring-no-properties start (or end (point-max))))))

           ;; if no role is specified, assume [ME]
           (parts (if (and
                       (not (string-prefix-p "[SYS]:" (car parts)))
                       (not (string-prefix-p "[ME]:" (car parts)))
                       (not (string-prefix-p "[AI]:" (car parts))))
                      (progn (setf (car parts) (concat "[ME]: " (car parts)))
                             parts)
                    parts))

           ;; create (:role :content) list
           (messages (cl-loop for part in parts
                              collect (cl-destructuring-bind (type &rest content) (split-string part ":")
                                        (let ((type (string-trim type))
                                              (content (string-trim (string-join content ":"))))
                                          (list :role (cond ((string= type "[SYS]") 'system)
                                                            ((string= type "[ME]") 'user)
                                                            ((string= type "[AI]") 'assistant)
                                                            (t 'assistant))
                                                :content content)))))

           (messages (cl-remove-if-not (lambda (x) (not (string-empty-p (plist-get x :content)))) messages))

           ;; merge messages with same role
           (messages (cl-loop with last-role = nil
                              with result = nil
                              for (_ role _ content) in messages
                              if (eql role last-role)
                              do (let ((last (pop result)))
                                   (push (list :role role :content (string-join (list (plist-get last :content) content) "\n")) result))
                              else
                              do (push (list :role role :content content) result)
                              do (setq last-role role)
                              finally return (reverse result)))

           ;; TODO: model should be available at this point,
           ;; and we determine whether search for image based on model == "gpt-4-vision-preview"
           ;; however currently the code first extracted messages before determining what model to use

           ;; check if there's any org image links [[file:/path/to/some.jpg][description]]
           (messages (cl-loop with result = nil
                              for (_ role _ content) in messages
                              if (eql role 'user)
                              do (push (list :role role :content (or (parse-user-content-as-openai-vision content) content)) result)
                              else
                              do (push (list :role role :content content) result)
                              finally return (reverse result)))

           (starts-with-sys-prompt-p (and messages (eql (plist-get (car messages) :role) 'system)))

           (sys-prompt (if starts-with-sys-prompt-p
                           (plist-get (car messages) :content)
                         default-system-prompt))

           (messages (if persistant-sys-prompts
                         (cl-loop with result = nil
                                  for (_ role _ content) in messages
                                  if (eql role 'assistant)
                                  do (push (list :role 'assistant :content content) result)
                                  else if (eql role 'user)
                                  do (progn
                                       (push (list :role 'system :content sys-prompt) result)
                                       (push (list :role 'user :content content) result))
                                  finally return (reverse result))
                       (if (or starts-with-sys-prompt-p (not sys-prompt))
                           messages
                         (cons (list :role 'system :content sys-prompt) messages)))))

      (apply #'vector messages))))

;; deal with unspecified prefix
(cl-assert
 (equal
  (let ((test-string "\ntesting\n  [ME]: foo bar baz zorrk\nfoo\n[AI]: hello hello[ME]: "))
    (org-ai--collect-chat-messages test-string))
  '[(:role user :content "testing\nfoo bar baz zorrk\nfoo")
    (:role assistant :content "hello hello")]))

;; sys prompt
(cl-assert
 (equal
  (let ((test-string "[SYS]: system\n[ME]: user\n[AI]: assistant"))
    (org-ai--collect-chat-messages test-string))
  '[(:role system :content "system")
    (:role user :content "user")
    (:role assistant :content "assistant")]))

;; sys prompt intercalated
(cl-assert
 (equal
  (let ((test-string "[SYS]: system\n[ME]: user\n[AI]: assistant\n[ME]: user"))
    (org-ai--collect-chat-messages test-string nil t))
  '[(:role system :content "system")
    (:role user :content "user")
    (:role assistant :content "assistant")
    (:role system :content "system")
    (:role user :content "user")]))

;; merge messages with same role
(cl-assert
 (equal
  (let ((test-string "[ME]: hello [ME]: world")) (org-ai--collect-chat-messages test-string))
  '[(:role user :content "hello\nworld")]))

;; (comment
;;   (with-current-buffer "org-ai-mode-test.org"
;;    (org-ai--collect-chat-messages (org-ai-get-block-content))))

(cl-defun org-ai--stringify-chat-messages (messages &optional &key
                                                    default-system-prompt
                                                    (system-prefix "[SYS]: ")
                                                    (user-prefix "[ME]: ")
                                                    (assistant-prefix "[AI]: "))
  "Converts a chat message to a string.
`MESSAGES' is a vector of (:role :content) pairs. :role can be
'system, 'user or 'assistant. If `DEFAULT-SYSTEM-PROMPT' is
non-nil, a [SYS] prompt is prepended if the first message is not
a system message. `SYSTEM-PREFIX', `USER-PREFIX' and
`ASSISTANT-PREFIX' are the prefixes for the respective roles
inside the assembled prompt string."
  (let ((messages (if (and default-system-prompt
                           (not (eql (plist-get (aref messages 0) :role) 'system)))
                      (cl-concatenate 'vector (vector (list :role 'system :content default-system-prompt)) messages)
                    messages)))
    (cl-loop for (_ role _ content) across messages
             collect (cond ((eql role 'system) (concat system-prefix content))
                           ((eql role 'user) (concat user-prefix content))
                           ((eql role 'assistant) (concat assistant-prefix content)))
             into result
             finally return (string-join result "\n\n"))))

(cl-assert
 (equal
  (org-ai--stringify-chat-messages '[(:role system :content "system")
                                     (:role user :content "user")
                                     (:role assistant :content "assistant")])
  "[SYS]: system\n\n[ME]: user\n\n[AI]: assistant"))

(cl-assert
 (equal
  (org-ai--stringify-chat-messages '[(:role user :content "user")
                                     (:role assistant :content "assistant")]
                                   :default-system-prompt "system")
  "[SYS]: system\n\n[ME]: user\n\n[AI]: assistant"))

(cl-assert
 (equal
  (org-ai--stringify-chat-messages '[(:role user :content "user")
                                     (:role assistant :content "assistant")]
                                   :user-prefix "You: "
                                   :assistant-prefix "Assistant: ")
  "You: user\n\nAssistant: assistant"))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(provide 'org-ai-block)

;;; org-ai-block.el ends here
