;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! meow)

(package! pyim-basedict)

(package! markdown-mode)
(package! mind-wave
  :recipe (:host github
           :repo "manateelazycat/mind-wave"
           :files ("*.el" "*.py")))

(package! org-download)
