;;; party/maskray.el -*- lexical-binding: t; -*-
;; https://github.com/MaskRay/Config

;;;###autoload
(defun +maskray/avy-document-symbol (all)
  (interactive)
  (let ((line 0) (col 0) (w (selected-window))
        (ccls (memq major-mode '(c-mode c++-mode objc-mode)))
        (start-line (1- (line-number-at-pos (window-start))))
        (end-line (1- (line-number-at-pos (window-end))))
        ranges point0 point1
        candidates)
    (save-excursion
      (goto-char 1)
      (cl-loop for loc in
               (lsp--send-request (lsp--make-request
                                   "textDocument/documentSymbol"
                                   `(:textDocument ,(lsp--text-document-identifier)
                                                   ,@(when all '(:excludeRole 0))
                                                   :startLine ,start-line :endLine ,end-line)))
               for range = (if ccls loc (->> loc (gethash "location") (gethash "range")))
               for range_start = (gethash "start" range)
               for range_end = (gethash "end" range)
               for l0 = (gethash "line" range_start)
               for c0 = (gethash "character" range_start)
               for l1 = (gethash "line" range_end)
               for c1 = (gethash "character" range_end)
               while (<= l0 end-line)
               when (>= l0 start-line)
               do
               (forward-line (- l0 line))
               (forward-char c0)
               (setq point0 (point))
               (forward-line (- l1 l0))
               (forward-char c1)
               (setq point1 (point))
               (setq line l1 col c1)
               (push `((,point0 . ,point1) . ,w) candidates)))
    (require 'avy)
    (avy-with avy-document-symbol
      (avy--process candidates
                    (avy--style-fn avy-style)))))

;;;###autoload
(defun +maskray/ffap ()
  (interactive)
  (-if-let (filename (ffap-guess-file-name-at-point))
      (find-file filename)
    (user-error "No file at point")))

;;;###autoload
(defun +maskray/find-definitions ()
  (interactive)
  (if lsp-mode (lsp-ui-peek-find-definitions) (call-interactively #'+lookup/definition)))

;;;###autoload
(defun +maskray/find-references (&optional extra)
  (interactive)
  (if lsp-mode (lsp-ui-peek-find-references nil) (call-interactively #'+lookup/references)))

;;;###autoload
(defun +maskray/workspace-symbol (pattern)
  (interactive (list (read-string "workspace/symbol: " nil 'xref--read-pattern-history)))
  (let ((symbols (lsp-request
                  "workspace/symbol"
                  `(:query ,pattern :folders ,(if current-prefix-arg (vector (projectile-project-root)) [])))))
    (unless symbols
      (user-error "No symbol found for: %s" pattern))
    (xref--show-xrefs
     (lambda () (mapcar #'lsp--symbol-information-to-xref symbols)) nil)))

(defun +maskray/avy-goto-definitions ()
  (interactive)
  (if lsp-mode
      (progn (+maskray/avy-document-symbol t)
             (+maskray/find-definitions))
    (avy-goto-word-0 nil)))

(defun +maskray/avy-goto-symbol ()
  (interactive)
  (+maskray/avy-document-symbol nil))

(defun +maskray/avy-goto-references ()
  (interactive)
  (+maskray/avy-document-symbol '+maskray/find-references))

(defun +maskray/workspace-symbol-alt ()
  (interactive)
  (setq current-prefix-arg t)
  (call-interactively #'+maskray/workspace-symbol))
