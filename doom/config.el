;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; font
(setq doom-font (font-spec :family "Ubuntu Mono" :size 12.0)
      doom-variable-pitch-font (font-spec :family "Sarasa Mono SC" :size 12.0)
      doom-unicode-font (font-spec :family "Sarasa Mono SC" :size 12.0))
(setq all-the-icons-scale-factor 0.8)

;; meow
(meow-global-mode)

;; which
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

  ;; bind
  (add-hook 'prog-mode-hook
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
             '("jF" . +maskray/ffap))))

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
