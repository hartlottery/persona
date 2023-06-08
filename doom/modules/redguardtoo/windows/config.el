;;; redguardtoo/config.el -*- lexical-binding: t; -*-

;; init-utils.el
(defun my-setup-extra-keymap (extra-fn-list hint fn &rest args)
  "Map EXTRA-FN-LIST to new keymap and show HINT after calling FN with ARGS."
  (let ((echo-keystrokes nil))
    (when fn (apply fn args))
    (message hint)
    (set-transient-map
     (let ((map (make-sparse-keymap))
           cmd)
       (dolist (item extra-fn-list)
         (setq cmd (nth 1 item))
         (setq cmd (cond
                    ((commandp cmd)
                     cmd)
                    (t
                     `(lambda ()
                        (interactive)
                        (if (functionp ,cmd) (funcall ,cmd) ,cmd)))))
         (define-key map (kbd (nth 0 item)) cmd))
       map)
     t)))

;; https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-windows.el
(defun my-toggle-two-split-window ()
  "Toggle two window layout vertically or horizontally."
  (interactive)
  (when (= (count-windows) 2)
    (let* ((this-win-buffer (window-buffer))
           (next-win-buffer (window-buffer (next-window)))
           (this-win-edges (window-edges (selected-window)))
           (next-win-edges (window-edges (next-window)))
           (this-win-2nd (not (and (<= (car this-win-edges)
                                       (car next-win-edges))
                                   (<= (cadr this-win-edges)
                                       (cadr next-win-edges)))))
           (splitter
            (if (= (car this-win-edges)
                   (car (window-edges (next-window))))
                'split-window-horizontally
              'split-window-vertically)))
      (delete-other-windows)
      (let* ((first-win (selected-window)))
        (funcall splitter)
        (if this-win-2nd (other-window 1))
        (set-window-buffer (selected-window) this-win-buffer)
        (set-window-buffer (next-window) next-win-buffer)
        (select-window first-win)
        (if this-win-2nd (other-window 1))))))

(defun my-rotate-windows ()
  "Rotate windows in clock-wise direction."
  (interactive)
  (cond
   ((not (> (count-windows)1))
    (message "You can't rotate a single window!"))
   (t
    (let* ((i 1)
           (num-windows (count-windows)))
      (while (< i num-windows)
        (let* ((w1 (elt (window-list) i))
               (w2 (elt (window-list) (+ (% i num-windows) 1)))

               (b1 (window-buffer w1))
               (b2 (window-buffer w2))

               (s1 (window-start w1))
               (s2 (window-start w2)))
          (set-window-buffer w1 b2)
          (set-window-buffer w2 b1)
          (set-window-start w1 s2)
          (set-window-start w2 s1)
          (setq i (1+ i))))))))

(defun my-windows-setup ()
  "Setup windows."
  (interactive)
  (my-setup-extra-keymap '(("p" enlarge-window)
                           ("n" shrink-window)
                           ("f" enlarge-window-horizontally)
                           ("b" shrink-window-horizontally)
                           ("a" balance-windows)
                           ("t" my-toggle-two-split-window)
                           ("c" (kill-buffer (current-buffer)))
                           ("r" my-rotate-windows))
                         "Window: [r]oate [t]oggle-split [bnpf]resize b[a]lance [c]lose [q]uit"
                         nil))

;; meow integrate
(when (modulep! :editor meow)
  (meow-normal-define-key
   '("h h" . my-windows-setup)))
