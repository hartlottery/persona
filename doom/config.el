;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(use-package! meow
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

  ;; unmap C-x C-0 for keypad mode of x0; TODO: just prefix of C-x, do not inherit
  (map! "C-x C-0" nil)

  ;; keys for motion
  (meow-motion-overwrite-define-key
   ;; move
   '("b" . meow-left)
   '("n" . meow-next)
   '("p" . meow-prev)
   '("f" . meow-right)
   ;; god-mode
   '("x" . "C-x")
   '("C-x C-x" . "H-x") ;; xx to run the original x
   ;; actions
   '(":" . "M-x"))

  ;; keys for normal
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
   '("b" . meow-back-symbol)
   '("B" . meow-back-word)
   '("C-S-b" . meow-left-expand)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("p" . meow-prev)
   '("P" . meow-prev-expand)
   '("f" . meow-next-symbol)
   '("F" . meow-next-word)
   '("C-S-f" . meow-right-expand)
   ;; jumps
   '("t" . meow-find)
   '("T" . meow-till)
   '("/" . meow-visit)
   '("s" . meow-search)
   ;; marks
   '("m" . meow-mark-symbol)
   '("M" . meow-mark-word)
   '("[" . meow-inner-of-thing)
   '("]" . meow-bounds-of-thing)
   '("{" . meow-beginning-of-thing)
   '("}" . meow-end-of-thing)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("v" . meow-line)
   '("V" . meow-goto-line)
   ;; edit+marks
   '("e" . meow-change)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("w" . meow-kill)
   '("W" . meow-save)
   '("y" . meow-yank)
   '("Y" . meow-sync-grab)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("q" . meow-pop-selection)
   ;; god-mode
   '("x" . "C-x")
   '("c" . "C-c")
   ;; actions
   '(":" . "M-x")
   '("'" . meow-reverse)
   '("\"" . repeat)
   '("<escape>" . ignore))

  ;; meow!
  (meow-global-mode))
