(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-by-copying t)
 '(backup-directory-alist '((".*" . "/home/allen/emacs_backup")))
 '(package-selected-packages
   '(company yasnippet lsp-ui lsp-metals lsp-mode flycheck sbt-mode scala-mode use-package))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 84 :width normal :foundry "PfEd" :family "DejaVu Sans Mono")))))

(windmove-default-keybindings)

(load-file (concat (getenv "HOME") "/.emacs.d/init.el"))

;;;(provide .emacs)
;;; .emacs ends here
