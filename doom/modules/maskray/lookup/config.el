;;; maskray/lookup/config.el -*- lexical-binding: t; -*-

;; flycheck
(after! flycheck
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  (global-flycheck-mode -1))

;; flymake
(after! flymake-proc
  (setq-default flymake-diagnostic-functions nil))
(defvar flymake-posframe-delay 0.5)
(defvar flymake-posframe-buffer "*flymake-posframe*")
(defvar flymake-posframe--last-diag nil)
(defvar flymake-posframe--timer nil)

(defun flymake-posframe-hide ()
  (posframe-hide flymake-posframe-buffer))

(defun flymake-posframe-display ()
  (when flymake-mode
    (if-let (diag (and flymake-mode
                       (get-char-property (point) 'flymake-diagnostic)))
        (unless (and (eq diag flymake-posframe--last-diag)
                     (frame-visible-p (buffer-local-value 'posframe--frame (get-buffer flymake-posframe-buffer))))
          (setq flymake-posframe--last-diag diag)
          (posframe-show
           flymake-posframe-buffer
           :string (propertize (concat "? " (flymake--diag-text diag))
                               'face
                               (case (flymake--diag-type diag)
                                 (:error 'error)
                                 (:warning 'warning)
                                 (:note 'info)))))
      (flymake-posframe-hide))))

(defun flymake-posframe-set-timer ()
  (when flymake-posframe--timer
    (cancel-timer flymake-posframe--timer))
  (setq flymake-posframe-timer
        (run-with-idle-timer flymake-posframe-delay nil #'flymake-posframe-display)))

;; lsp
(defvar +my-use-eglot nil) ;; TODO: try eglot?
(defun +maskray/toggle-eglot ()
  (interactive)
  (setq +my-use-eglot (not +my-use-eglot))
  (message "use: %s" (if +my-use-eglot "eglot" "lsp-mode")))

(setq lsp-keymap-prefix "M-q")
(use-package! lsp-mode
  :commands lsp
  :hook (nim-mode . lsp)
  :config
  (setq lsp-lens-enable nil) ;; Very slow
  (setq lsp-auto-guess-root t lsp-eldoc-prefer-signature-help nil)
  (setq lsp-semantic-tokens-enable t)
  (setq lsp-enable-links nil)
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-keep-workspace-alive nil)
  (add-hook 'evil-insert-state-entry-hook (lambda () (setq-local lsp-hover-enabled nil)))
  (add-hook 'evil-insert-state-exit-hook (lambda () (setq-local lsp-hover-enabled t))))

(use-package! lsp-ui
  :commands lsp-ui-mode
  :config
  (setq
   lsp-ui-sideline-enable nil
   lsp-ui-sideline-ignore-duplicate t
   lsp-ui-doc-header nil
   lsp-ui-doc-include-signature nil
   lsp-ui-doc-background (doom-color 'base4)
   lsp-ui-doc-border (doom-color 'fg)

   lsp-ui-peek-force-fontify nil
   lsp-ui-peek-expand-function (lambda (xs) (mapcar #'car xs)))

  (custom-set-faces
   '(ccls-sem-global-variable-face ((t (:underline t :weight extra-bold))))
   '(lsp-face-highlight-read ((t (:background "sea green"))))
   '(lsp-face-highlight-write ((t (:background "brown4"))))
   '(lsp-ui-sideline-current-symbol ((t (:foreground "grey38" :box nil))))
   '(lsp-ui-sideline-symbol ((t (:foreground "grey30" :box nil)))))

  (map! :after lsp-ui-peek
        :map lsp-ui-peek-mode-map
        "h" #'lsp-ui-peek--select-prev-file
        "j" #'lsp-ui-peek--select-next
        "k" #'lsp-ui-peek--select-prev
        "l" #'lsp-ui-peek--select-next-file))
