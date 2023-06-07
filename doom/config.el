;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; face; https://github.com/tam5/util-font-patcher, tweaked to ensure both the
;;   (size, height, ascent, descent) are the same.
;; https://docs.oracle.com/javase/tutorial/2d/text/fontconcepts.html
(setq doom-theme 'doom-earl-grey)
(setq doom-font (font-spec :family "Ubuntu Mono 1.25" :size 12.0)
      doom-variable-pitch-font (font-spec :family "Sarasa Mono SC")
      doom-unicode-font (font-spec :family "Sarasa Mono SC"))
(setq all-the-icons-scale-factor 0.8)

;; annotate, C-c C-a
(use-package! annotate)

;; maskray/frog-jump-buffer
(use-package! frog-jump-buffer
  :config
  (dolist (regexp '("^\\*"))
    (push regexp frog-jump-buffer-ignore-buffers))
  (map! :leader "," #'frog-jump-buffer))

;; maskray/tldr
(use-package! tldr
  :commands (tldr)
  :config
  (setq tldr-directory-path (concat doom-data-dir "tldr/"))
  (set-popup-rule! "^\\*tldr\\*" :side 'right :select t :quit t))

;; avy
(after! avy
  (setq avy-timeout-seconds 0.2))
(after! ace-pinyin
  (ace-pinyin-global-mode -1))

;; company
(after! company
  (setq company-minimum-prefix-length 2
        company-show-quick-access t))

;; consult
(map! "C-s" #'consult-line)

;; eglot
(use-package! eglot)

;; which-key
(setq which-key-idle-delay 0
      which-key-popup-type 'minibuffer)

;; which-func
(use-package! which-func
  :config
  (which-function-mode 1))

;; redguardtoo/modeline
(defconst ml-default-color (cons (face-background 'mode-line)
                                 (face-foreground 'mode-line)))

(defun +redguardtoo/show-meow-state ()
  "Change modeline color to notify user meow's current state"
  (let ((color (cond
                ;; https://m2.material.io/design/color/the-color-system.html
                ((minibufferp)
                 ml-default-color)
                ;; ((and current-input-method (meow-insert-mode-p))
                ;;  '("#a7ffeb" . nil))
                ((meow-insert-mode-p)
                 '("#ffe57f" . nil))
                ((meow-motion-mode-p)
                 '("#ccff90" . nil))
                ((buffer-modified-p)
                 '("#ff8a80" . nil))
                (t
                 ml-default-color))))
    (set-face-background 'mode-line (car color))
    (when (cdr color)
      (set-face-foreground 'mode-line (cdr color)))))
(add-hook 'post-command-hook #'+redguardtoo/show-meow-state)

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

  (defun ccls-callee-hierarchy ()
    (interactive)
    (ccls-call-hierarchy t))

  ;; bind; TODO: add-hook prog-mode-hook?
  (meow-normal-define-key
   ;; jump
   '(";" . +maskray/avy-goto-definitions)
   '("j;" . +maskray/avy-goto-symbol)
   '("jr" . +maskray/avy-goto-references)
   '("jx" . +lookup/references-at-function)
   '("js" . +lookup/project-symbol-at-function)
   '("jD" . +lookup/references)
   '("jb" . lsp-ui-peek-jump-backward)
   '("jf" . lsp-ui-peek-jump-forward)
   '("ja" . +maskray/workspace-symbol)
   '("jA" . +maskray/workspace-symbol-alt)
   '("jF" . +maskray/ffap)
   ;; display; TODO: popups
   '("di" . lsp-ui-imenu)
   '("dc" . ccls/caller)
   '("dC" . ccls/callee)
   '("de" . ccls-call-hierarchy)
   '("dE" . ccls-callee-hierarchy)))

;; utf-8
(set-language-environment "UTF-8")

;; pyim
(use-package! pyim-basedict)
(after! pyim
  (pyim-default-scheme 'zhinengabc-shuangpin)
  (defun pyim-probe-meow-mode ()
    (not (eq meow--current-state 'insert)))
  (setq pyim-page-length 5
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

  ;; add a duplicate word in 2nd place, as placeholder for cloudim
  (advice-add 'pyim-candidates-create :filter-return
              (lambda (result)
                (when (car result)
                  `(,(car result)
                    ,"…"
                    ,@(cdr result)))))
  ;; restrict only one cloud result
  (advice-add 'pyim-cloudim--parse-baidu-buffer-string :filter-return
              (lambda (result)
                (if (> (length result) 0)
                    (list (car result))
                  result)))
  ;; then removes it when cloud returns; TODO: the placeholder may splash
  (advice-add 'pyim-process--merge-candidates :filter-args
              (lambda (args)
                (let ((new (nth 0 args))
                      (old (nth 1 args)))
                  ;; TODO: remove duplicates? the list `new' is expected to be small
                  (if (member (car new) old)
                      (setf (nth 0 args) '())
                    (setf (nth 1 args) (delete "…" old))))
                args))

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
        "," #'pyim-page-previous-page)
  (setq default-input-method "pyim"))

;; ace-window; TODO: fix warn
(after! (:and ace-window posframe)
  (ace-window-posframe-mode)
  (setq aw-posframe-position-handler #'posframe-poshandler-window-bottom-left-corner
        aw-background nil)
  (custom-set-faces
   '(aw-leading-char-face ((t (:background "#424242" :foreground "#ECEFF1" :weight bold
                               :width ultra-expanded :height 4.0))))))

;; maskray/company
(map! :after company :map company-active-map
      "C-v" #'company-next-page
      "M-v" #'company-previous-page
      "C-i" #'company-complete-selection
      [tab] #'company-complete-selection
      "RET" nil
      [return] nil
      "SPC" nil)

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
