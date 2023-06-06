;;; editor/meow/config.el -*- lexical-binding: t; -*-

;; we need evil, but don't be evil
(use-package! evil)

;; ace-pinyin
(use-package! ace-pinyin)
(setq ace-pinyin--jump-word-timeout 0.3)
(defun ace-pinyin--read-wait (string)
  (if string
      (read-char nil nil ace-pinyin--jump-word-timeout)
    (read-char nil nil nil)))
(defun ace-pinyin-jump-word-wait (arg)
  "Fork from `ace-pinyin-jump-word', but wait on the first char (like avy)"
  (interactive "P")
  (if arg
      ;; Read input from minibuffer
      (ace-pinyin--jump-word-1 (read-string "Query word: "))
    ;; Read input by using timer
    (message "Query word: ")
    (let (char string)
      (while (and (setq char (ace-pinyin--read-wait string))
                  (not (char-equal char ?)))
        (setq string (concat string (char-to-string char)))
        (message (concat "Query word: " string)))
      (ace-pinyin--jump-word-1 string))))

;; meow save the world!
(use-package! meow
  :after evil
  :hook (doom-after-modules-config . meow-global-mode)
  :demand t
  :config
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

  ;; use domm leader key, instead of meow
  (map! :map meow-normal-state-keymap
    doom-leader-key doom-leader-map)
  (map! :map meow-motion-state-keymap
    doom-leader-key doom-leader-map)
  (map! :map meow-beacon-state-keymap
    doom-leader-key doom-leader-map)

  ;; unmap for keypad mode; TODO: just prefix of C-x, do not inherit
  (map! "C-x C-0" nil)
  (map! "C-x C-o" nil)

  ;; keys for motion
  (meow-motion-overwrite-define-key
   ;; move
   '("b" . meow-left)
   '("n" . meow-next)
   '("p" . meow-prev)
   '("f" . meow-right)
   ;; nav
   '("," . "C-x C-SPC")
   ;; god-mode
   '("\\ x" . "H-x")
   '("x" . "C-x")
   ;; actions
   '(":" . "M-x"))

  ;; keys for normal; TODO: key hq undefined
  (meow-normal-define-key
   ;; expand
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   ;; move
   '("b" . meow-left)
   '("B" . meow-back-word)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("p" . meow-prev)
   '("P" . meow-prev-expand)
   '("f" . meow-right)
   '("F" . meow-next-word)
   ;; evil-z
   '("zo" . evil-open-fold)
   '("zO" . evil-open-fold-rec)
   '("zc" . evil-close-fold)
   '("za" . evil-toggle-fold)
   '("zr" . evil-open-folds)
   '("zm" . evil-close-folds)
   '("z=" . ispell-word)
   '("z <return>" . evil-scroll-line-to-top-first-non-blank)
   '("zz" . evil-scroll-line-to-center)
   '("z." . evil-scroll-line-to-center-first-non-blank)
   '("zf" . evil-scroll-column-right)
   '("zb" . evil-scroll-column-left)
   '("z/" . meow-comment)
   ;; nav
   '("v" . evil-scroll-down)
   '("V" . evil-scroll-up)
   '("," . "C-x C-SPC")
   '(".." . bookmark-jump)
   '(".m" . bookmark-set)
   '(".d" . bookmark-delete)
   ;; jumps
   '("t" . meow-find)
   '("T" . meow-till)
   '("s" . meow-visit)
   '("Sb" . meow-left-expand)
   '("Sf" . meow-right-expand)
   '("\\ <return>" . "RET")
   '("<return>" . meow-search)
   '("jj" . ace-pinyin-jump-word-wait)
   '("jn" . avy-goto-word-0-below)
   '("jp" . avy-goto-word-0-above)
   '("jd" . "M-.")
   ;; marks
   '("m" . meow-mark-symbol)
   '("M" . meow-mark-word)
   '("[" . meow-inner-of-thing)
   '("]" . meow-bounds-of-thing)
   '("{" . meow-beginning-of-thing)
   '("}" . meow-end-of-thing)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("l" . meow-line)
   '("L" . meow-goto-line)
   '("o" . meow-block)
   '("O" . meow-to-block)
   ;; edit+marks
   '("e" . meow-change)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("w" . meow-kill)
   '("W" . meow-save)
   '("y" . meow-yank)
   '("Y" . meow-yank-pop)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("/" . meow-undo)
   '("?" . undo-redo)
   '("u" . meow-pop-selection)
   '("<" . evil-shift-left)
   '(">" . evil-shift-right)
   ;; god-mode
   '("x" . "C-x")
   '("c" . "C-c")
   ;; actions
   '(":" . "M-x")
   '("'" . meow-reverse)
   '("\"" . repeat)
   '("<escape>" . ignore)))
