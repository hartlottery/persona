;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! modus-themes)

(package! pyim-basedict)

(package! markdown-mode)
(package! mind-wave
  :recipe (:host github
           :repo "manateelazycat/mind-wave"
           :files ("*.el" "*.py")))

(package! org-download)

(package! spinner)
(package! eglot)

(package! annotate)

(package! frog-jump-buffer)

(package! tldr)
