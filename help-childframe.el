;;; help-childframe.el --- display help text in a childframe -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defgroup help-childframe nil
  "Group for customize help childframe."
  :prefix "help-childframe-")

(defcustom help-childframe-width 50
  "The width for the childframe, in unit of character (not pixel)"
  :type 'integer
  :group 'help-childframe)

(defcustom help-childframe-max-height 20
  "Max height for the childframe, in unit of lines (not pixel)"
  :type 'integer
  :group 'help-childframe)

(defcustom help-childframe-frame-transient-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "J") 'help-childframe-next-three-lines)
    (define-key map (kbd "K") 'help-childframe-prev-three-lines)
    (define-key map (kbd "C-u") 'help-childframe-scroll-up)
    (define-key map (kbd "C-d") 'help-childframe-scroll-down)
    (define-key map (kbd "C-e") 'help-childframe-next-xref)
    (define-key map (kbd "C-y") 'help-childframe-prev-xref)
    (define-key map (kbd "<return>") 'help-childframe-pop-source)
    (define-key map (kbd "<C-g>") 'help-childframe-hide)
    (define-key map (kbd "<escape>") 'help-childframe-hide)
    map)
  "Keymap for controlling the help childframe."
  :type 'keymap
  :group 'help-childframe)

(defcustom help-childframe-backend-alist
  `((emacs-lisp-mode . ,(if (featurep 'helpful) 'help-childframe--helpful-backend
                          'help-childframe--help-backend))
    (python-mode . help-childframe--eglot-backend))
  "List of (major-mode . backend) where \"backend\" is a function that takes a symbol as augment and return the help text for that symbol. See see `help-childframe--eglot-backend' for example."
  :type '(repeat (symbol function))
  :group 'help-childframe)

(defconst help-childframe--init-parameters
  `((parent-frame . ,(selected-frame))

    (left . -1)
    (top . -1)
    (width  . ,(* help-childframe-width (default-font-width)))
    (height  . 0)

    (no-accept-focus . t)
    (no-focus-on-map . t)
    (internal-border-width . 3)
    (vertical-scroll-bars . right)
    (scroll-bar-width . 10)
    (horizontal-scroll-bars . nil)
    (left-fringe . 0)
    (right-fringe . 0)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (line-spacing . 0)
    (unsplittable . t)
    (undecorated . t)
    (visibility . nil)
    (mouse-wheel-frame . nil)
    (no-other-frame . t)
    (cursor-type . nil)
    (inhibit-double-buffering . t)
    (drag-internal-border . t)
    (no-special-glyphs . t)
    (desktop-dont-save . t)
    (skip-taskbar . t)
    (minibuffer . nil))
  "The initial frame parameters for `help-childframe--frame'.")

(defvar help-childframe--frame nil)

(defvar help-childframe--buffer " *help-childframe-buffer*")

(defvar-local help-childframe--restore-keymap-fn nil)

(defun help-childframe-position-fn ()
  "Return the pixel position on where the childframe should pop up."
  (let ((edge (frame-pixel-width))
        (window-edge (nth 2 (window-pixel-edges))))
    (if (= edge window-edge)
        ;; display on the top left corner
        '(0 . 0)
      `(,(- edge (* help-childframe-width (default-font-width))) . 0))))

;;; ===============================
;;  backends
;;  ----------
;;  the backend should take a single augment SYMBOL
;;  and return a help string.
;;
;;  see `help-childframe--eglot-backend' for example.
;;; ===============================

(defun help-childframe--help-backend (symbol)
  "Return the buffer string from `describe-*' commands."
  )

(defun help-childframe--helpful-backend (symbol)
  "Return the buffer string from `helpful-symbol' command."
  )

(defun help-childframe--eglot-backend (_symbol)
  "Return the buffer string from `eglot-help-at-point' command."
  (when (and (featurep 'eglot) eglot--managed-mode)
    (eglot--dbind ((Hover) contents range)
        (jsonrpc-request (eglot--current-server-or-lose) :textDocument/hover
                         (eglot--TextDocumentPositionParams))
      (when (seq-empty-p contents) (eglot--error "No hover info here"))
      (eglot--hover-info contents range))))

;;; ===============================
;;  minor-mode
;;; ===============================

(defun help-childframe--set-frame-size ()
  "Return the pixel size of `help-childframe--frame' to fit `help-childframe--buffer'. Honoring settings in `help-childframe-width' and `help-childframe-max-height'."
  (let* ((text-length
          (with-current-buffer (get-buffer-create help-childframe--buffer)
            (length (buffer-string))))
         (text-height (min help-childframe-max-height
                           (+ 2 (ceiling (/ text-length help-childframe-width)))))
         (text-pixel-height (* text-height (default-font-height))))
    `(,(* (default-font-width) help-childframe-width) ,text-pixel-height)))

(defvar-local help-childframe--backend nil)
(defun help-childframe--determine-backend (&optional current-major-mode)
  "Determine which backend function to use according to `help-childframe-backend-alist'."
  (let ((current-major-mode (or current-major-mode major-mode)))
    (cdr (assq current-major-mode help-childframe-backend-alist))))

;;;###autoload
(defun help-childframe-show (symbol)
  "Show help for SYMBOL."
  (interactive (list (symbol-at-point)))

  ;; prepare buffer content
  (let ((content (funcall help-childframe--backend symbol)))
    (with-current-buffer (get-buffer-create help-childframe--buffer)
      (erase-buffer)
      (insert content)
      (setq-local cursor-type nil)
      (setq-local cursor-in-non-selected-windows nil)
      (setq-local mode-line-format nil)
      (setq-local header-line-format nil)

      ;; setup transient keymap
      (setq help-childframe--restore-keymap-fn
            (set-transient-map
             help-childframe-frame-transient-map t #'help-childframe-hide))))
  
  ;; Then create frame if needed
  (unless (and help-childframe--frame (frame-live-p help-childframe--frame))
    (setq help-childframe--frame
          (make-frame help-childframe--init-parameters)))

  (with-selected-frame help-childframe--frame
    (delete-other-windows)
    (switch-to-buffer help-childframe--buffer))

  ;; move frame to desirable position
  (apply 'set-frame-size
         `(,help-childframe--frame ,@(help-childframe--set-frame-size) t))
  (apply 'set-frame-position
         `(,help-childframe--frame ,@(help-childframe-position-fn)))
  (set-face-background 'internal-border "gray80" help-childframe--frame)

  ;; display buffer
  (redirect-frame-focus help-childframe--frame
                        (frame-parent help-childframe--frame))

  (make-frame-visible help-childframe--frame))

;;;###autoload
(define-minor-mode help-childframe-mode
  "Use childframe to display help text."
  nil nil nil
  (if help-childframe-mode
      (setq-local help-childframe--backend nil)
    (setq help-childframe--backend (help-childframe--determine-backend))))

;;; ===============================
;;  commands
;;  -----------
;;  use `help-childframe-command' to define command
;;  and bind it in `help-childframe-transient-map'.
;;; ===============================

(defun help-childframe-hide ()
  "Hide childframe."
  (interactive)
  ;; deactivate transient keymap
  (when help-childframe--restore-keymap-fn
    (let ((fn help-childframe--restore-keymap-fn))
      (setq help-childframe--restore-keymap-fn nil)
      (funcall fn)))
  (when (frame-visible-p help-childframe--frame)
    (set-frame-parameter help-childframe--frame 'visibility nil)))

(defmacro help-childframe-command (&rest body)
  "Run BODY in `help-childframe--frame' and `help-childframe--buffer'."
  (declare (indent 1))
  `(with-selected-frame help-childframe--frame
     (with-current-buffer (get-buffer-create help-childframe--buffer)
       ,@body)))

(defun help-childframe-next-three-lines ()
  "Move down three lines."
  (interactive)
  (help-childframe-command (scroll-up 3)))

(defun help-childframe-prev-three-lines ()
  "Move up three lines."
  (interactive)
  (help-childframe-command (scroll-down 3)))

(defun help-childframe-scroll-up ()
  "Move up half page."
  (interactive)
  (help-childframe-command
    (scroll-down (max 1 (/ (1- (window-height (selected-window))) 2)))))

(defun help-childframe-scroll-down ()
  "Move down half page."
  (interactive)
  (help-childframe-command
    (scroll-up (max 1 (/ (1- (window-height (selected-window))) 2)))))

(defun help-childframe-pop-help ()
  "Hide the childframe, pop to `help-childframe--current-help' in another window."
  (interactive)
  )

(provide 'help-childframe)
;;; help-childframe.el ends here
