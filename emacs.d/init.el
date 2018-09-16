;;; Init --- startup starts here
;;; Commentary:
;;;   If you want to add something specific to a particular mode, put it in modes.el, not here.
;;; Code:

;;;; Set paths
(defvar root-dir "~/.emacs.d/")
(add-to-list 'load-path (concat root-dir "lisp"))

;;;; Package management
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
;; simple way to synchronize a bunch of packages across installs.
;;   (there are more sophisticated ways to do this, like pallet
;;    or el-get, but this seems to work fine for now)
(defvar my-packages '(add-node-modules-path
                      ag
                      applescript-mode
                      auto-complete
                      coffee-mode
                      css-mode
                      ;; dropdown-list
                      expand-region
                      flycheck
                      flycheck-flow
                      graphql-mode
                      highlight-indentation
                      js2-mode
                      json-mode
                      magit
                      markdown-mode
                      multiple-cursors
                      prettier-js
                      projectile
                      rainbow-mode
                      robe
                      rvm
                      slim-mode
                      string-inflection
                      web-mode
                      yaml-mode
                      yasnippet))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;;; Setup
(menu-bar-mode 0)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(setq initial-major-mode (quote text-mode))
(setq initial-scratch-message nil)
(setq-default indent-tabs-mode nil)
;; Save squiggle files somewhere out of the way
(defconst emacs-tmp-dir "~/.emacs_saves/")
(make-directory emacs-tmp-dir t)
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))

;;;; Server Mode
(server-mode)

;;;; Hooks
(add-hook 'before-save-hook 'whitespace-cleanup)

;;;; Don't mess with our Tmux config
(global-unset-key (kbd "C-]"))

;;;; Shortcuts
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (quote [27 up]) (quote scroll-down))
(global-set-key (quote [27 down]) (quote scroll-up))
(global-set-key (kbd "M-p") (lambda () (interactive) (scroll-down 1)))
(global-set-key (kbd "M-n") (lambda () (interactive) (scroll-up 1)))
(global-set-key (kbd "M-#") 'linum-mode)
(global-set-key (kbd "M-+") 'ido-mode)
(global-set-key (kbd "M-s")     'fixup-whitespace) ; best function ever
(savehist-mode 1) ;; save your minibuffer history
(global-set-key (kbd "C-c r") 'revert-buffer) ;; not quite the same as c-x c-v RET, but close
(put 'upcase-region 'disabled nil)





;;;; DO NOT ADD ANYTHING BELOW THIS LINE




;; mode-specific initialization
(load (concat root-dir "modes.el"))

;; stuff added by Custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; DO THIS LAST - load local user config
;; load .emacs_local.el, if present
(if (file-exists-p (setq local-init-file "~/.emacs_local.el"))
    (load local-init-file))

(provide 'init)
;;; init.el ends here
