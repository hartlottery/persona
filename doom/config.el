;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; face
(setq doom-theme 'doom-earl-grey)
(setq doom-font (font-spec :family "Ubuntu Mono" :size 12.0)
      doom-variable-pitch-font (font-spec :family "Sarasa Mono SC" :size 12.0)
      doom-unicode-font (font-spec :family "Sarasa Mono SC" :size 12.0))
(setq all-the-icons-scale-factor 0.8)

;; avy
(after! avy
  (setq avy-timeout-seconds 0.2))

;; company
(after! company
  (setq company-minimum-prefix-length 2
        company-show-quick-access t))

;; eglot
(use-package! eglot)

;; flycheck
(after! flycheck
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  (global-flycheck-mode -1))

;; maskray/flymake
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

;; maskray/lsp
(setq lsp-keymap-prefix "M-q")
(use-package! lsp-mode
  ;; :load-path "~/Dev/Emacs/lsp-mode"
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

;; maskray/xref
(defun +advice/xref-set-jump (&rest args)
  (require 'lsp-ui)
  (lsp-ui-peek--with-evil-jumps (evil-set-jump)))
(advice-add '+lookup/definition :before #'+advice/xref-set-jump)
(advice-add '+lookup/references :before #'+advice/xref-set-jump)

(after! xref
  ;; This is required to make `xref-find-references' not give a prompt.
  ;; `xref-find-references' asks the identifier (which has no text property)
  ;; and then passes it to `lsp-mode', which requires the text property at
  ;; point to locate the references.
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=29619
  (setq xref-prompt-for-identifier '(not xref-find-definitions
                                         xref-find-definitions-other-window
                                         xref-find-definitions-other-frame
                                         xref-find-references)))

;; which-key
(setq which-key-delay-functions 0)

;; which-func
(use-package! which-func
  :config
  (which-function-mode 1))

;; lookup
(when (modulep! :tools lookup)
  (defun +lookup/references-at-function (&optional arg)
    "Show a list of usages of current line's caller, one less jump to function start.

Due to the limitation of LSP, we can only search references _at point_ :("
    ;; +lookup/references
    (interactive "^P")
    (beginning-of-defun)
    (search-forward (concat (which-function) "("))  ; TODO: other language that is not "func(" style
    (sp-backward-symbol)  ; TODO: ugly, hmm...
    (+lookup--jump-to :references (doom-thing-at-point-or-region) nil arg))

  (defun +lookup/project-symbol-at-function (dir)
    "Show a list of searched symbols (in project) of current line's caller"
    ;; +default/search-project-for-symbol-at-point
    (interactive
     (list (let ((projectile-project-root nil))
             (if current-prefix-arg
                 (if-let (projects (projectile-relevant-known-projects))
                     (completing-read "Search project: " projects nil t)
                   (user-error "There are no known projects"))
               (doom-project-root default-directory)))))
    (+vertico/project-search nil (which-function) dir))

  ;; bind; TODO: add-hook prog-mode-hook?
  (meow-normal-define-key
   ;; jump
   '(";" . +maskray/avy-goto-definitions)
   '("j;" . +maskray/avy-goto-symbol)
   '("jr" . +maskray/avy-goto-references)
   '("jx" . +lookup/references-at-function)
   '("js" . +lookup/project-symbol-at-function)
   '("ji" . lsp-ui-imenu)
   '("jb" . lsp-ui-peek-jump-backward)
   '("jf" . lsp-ui-peek-jump-forward)
   '("ja" . +maskray/workspace-symbol)
   '("jA" . +maskray/workspace-symbol-alt)
   '("jF" . +maskray/ffap)))

;; utf-8
(set-language-environment "UTF-8")

;; pyim
(use-package! pyim-basedict)
(after! pyim
  (pyim-default-scheme 'zhinengabc-shuangpin)
  (defun pyim-probe-meow-mode ()
    (not (eq meow--current-state 'insert)))
  (setq pyim-page-length 9
        pyim-cloudim 'baidu
        pyim-punctuation-dict
                '(("'" "‘" "’")
                  ("\"" "“" "”")
                  ("^" "…")
                  ("?" "？")
                  (">" "》")
                  ("<" "《")
                  (";" "；")
                  (":" "：")
                  ("/" "、")
                  ("." "。")
                  ("-" "－")
                  ("," "，")
                  (")" "）")
                  ("(" "（")
                  ("!" "！"))
        pyim-english-input-switch-functions
                '(pyim-probe-auto-english
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template
                  pyim-probe-meow-mode)
        pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation)
        pyim-process-run-delay 1)

  ;; load dict from ~/.eim/*.pyim; redguardtoo/emacs.d/lisp/init-chinese.el
  (let* ((files (and (file-exists-p "~/.eim")
                     (directory-files-recursively "~/.eim" "\.pyim$")))
         disable-basedict)
    (when (and files (> (length files) 0))
      (setq pyim-dicts
            (mapcar (lambda (f)
                      (list :name (file-name-base f) :file f))
                    files))
      (dolist (f files)
        (when (member (file-name-base f) '("pyim-another-dict"
                                           "pyim-tsinghua-dict.pyim"
                                           "pyim-bigdict"
                                           "pyim-greatdict"))
          (setq disable-basedict t))))
    (unless disable-basedict (pyim-basedict-enable)))
  (map! :map pyim-mode-map
        "." #'pyim-page-next-page
        "," #'pyim-page-previous-page))

;; org-download
(use-package! org-download
  :config
  (add-hook! 'dired-mode-hook 'org-download-enable)
  ;; https://github.com/abo-abo/org-download/issues/195#issuecomment-1109410866
  (defun my-org-download-method (link)
    (let ((filename
           (file-name-nondirectory
            (car (url-path-and-query
                  (url-generic-parse-url link)))))
          ;; place images into customized directory
          (dirname (concat "./Attach/" (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))))
      (setq org-download-image-dir dirname)
      (make-directory dirname t)
      ;; https://github.com/abo-abo/org-download/issues/195#issuecomment-1408073395
      (expand-file-name (funcall org-download-file-format-function filename) dirname)))
  (setq org-download-method 'my-org-download-method))

;; ccls
(after! ccls
  (setq ccls-executable "~/.local/bin/ccls"))

;; mind-wave, with `setq mind-wave-api-base' and `setq mind-wave-api-key-path'
(use-package! mind-wave
  :init
  (when (file-exists-p ".mind-wave-config.el")
    (load! ".mind-wave-config.el")))

;; proxy with user/passwd
(let ((proxy (getenv "http_proxy"))
      (re (pcre-to-elisp "http://(\\w+:\\w+)@([\\w\\.]+:\\d+)")))
  (when (and proxy (string-match re proxy))
    ;; proxy is set from env, we only need the auth
    ;; https://stackoverflow.com/a/18697223
    (setq url-http-proxy-basic-auth-storage
        (list (list (match-string 2 proxy)
                    (cons "Input your LDAP UID !"
                          (base64-encode-string (match-string 1 proxy))))))))
