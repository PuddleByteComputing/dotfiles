;;; Modes --- mode-specific initialization and configuration
;;; Commentary:
;;;   Try to keep things in here alphabetized by mode
;;; Code:

;; ag (ag > ack)
(global-set-key (kbd "C-c k") 'ag)

;; auto-complete
(ac-config-default)
;; (setq ac-ignore-case nil)
;; (add-to-list 'ac-modes 'enh-ruby-mode)
(add-to-list 'ac-modes 'web-mode)

;; buffer selection
(require 'bs)
(global-set-key (kbd "C-x C-b") 'bs-show) ; better buffer listings

;; cnf files (e.g. mysql config files)
(add-to-list 'auto-mode-alist '("\\.cnf$" . conf-mode))

;; coffee
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(custom-set-variables '(coffee-tab-width 2))

;; elpy (python meta-package-- jedi, flake8, autopep8
(elpy-enable)

;; eslint
(eval-after-load "compile"
  '(progn
     (add-to-list 'compilation-error-regexp-alist
                  '("^\\(/[^\":\n]+\\)\n *\\([0-9]+\\):[0-9]+ +\\(error\\|warning\\) +" 1 2))))

;; expand-region
(require 'expand-region)
(global-set-key (kbd "C-c o") 'er/expand-region)

;; flycheck
(require 'flycheck)
(require 'flycheck-flow)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'js-mode-hook (lambda () (flycheck-mode t)))
(add-hook 'scss-mode-hook 'flycheck-mode)
;; disable jshint
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))
;; pick up project/local install of eslint
(eval-after-load 'web-mode
  '(add-hook 'web-mode-hook #'add-node-modules-path))
;; use flow with web-mode for js/jsx files
(flycheck-add-mode 'javascript-flow 'web-mode)
;; use eslint after flow
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-next-checker 'javascript-flow 'javascript-eslint)

;; js-mode, js2-mode
(setq js-indent-level 2)

;; magit
;; (global-set-key (kbd "C-x g") 'magit-status)
(setq vc-handled-backends (delq 'Git vc-handled-backends))
;; (add-hook 'after-save-hook 'magit-after-save-refresh-status t)

;; multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;; markdown mode for *.md files.
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; prettier-js - format js/jsx on save
(require 'prettier-js)
(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
      (funcall (cdr my-pair)))))
(add-hook 'web-mode-hook #'(lambda ()
                             (enable-minor-mode
                              '("\\.[jt]sx?\\'" . prettier-js-mode))))

;; projectile
(require 'projectile)

;; rainbow mode - in css, display color specifiers in the color they specify
;; (autoload 'rainbow-mode "rainbow-mode")
(setq rainbow-html-colors 'auto)

;; robe
;;   first tell inf-ruby to always use the 'development' Rails environment, instead of prompting.
(setq inf-ruby-console-environment "development")
(add-hook 'ruby-mode-hook 'robe-mode)
(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rvm-activate-corresponding-ruby))

;; ruby

(require 'ruby-mode)
(define-key ruby-mode-map (kbd "C-c C-z") 'xmp)
(setq ruby-align-chained-calls t)
(autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
(setq auto-mode-alist (append '(("\\.rb$"   . ruby-mode)
                                ("\\.rake$"   . ruby-mode)
                                ("\\.builder$" . ruby-mode)
                                ("\\.jbuilder$" . ruby-mode)
                                ("\\.gemspec$" . ruby-mode)
                                ("\\.rabl$"    . ruby-mode)
                                ("Rakefile"   . ruby-mode)
                                ("Capfile"   . ruby-mode)
                                ("Gemfile"   . ruby-mode)
                                ) auto-mode-alist))
;; rvm
(require 'rvm)
(rvm-autodetect-ruby)

;; css
(setq cssm-indent-level 2)
(setq cssm-newline-before-closing-bracket t)
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq css-indent-offset 2)

;; sass - don't compile on save
(add-hook 'scss-mode-hook
          '(lambda ()
             (setq scss-compile-at-save nil)
             (setq css-indent-offset 2)))

;; string-inflection - cycle between snake case, camel case, etc.
(require 'string-inflection)
(global-set-key (kbd "C-c i") 'string-inflection-cycle)
(global-set-key (kbd "C-c c") 'string-inflection-camelcase)        ;; Force to CamelCase
(global-set-key (kbd "C-c l") 'string-inflection-lower-camelcase)  ;; Force to lowerCamelCase
(global-set-key (kbd "C-c j") 'string-inflection-java-style-cycle) ;; Cycle through Java styles

;; tide (typescript)
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq typescript-indent-level 2)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; use eslint instead of tslint
  (flycheck-add-next-checker 'typescript-tide 'javascript-eslint 'append)
  (flycheck-add-next-checker 'tsx-tide 'javascript-eslint 'append)
  (append flycheck-disabled-checkers '(json-jsonlist)))
  ;; company is an optional dependency. You have to install it separately via package-install
  ;; (company-mode +1))

;; aligns annotation to the right hand side
;; (setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; web-mode
(add-to-list 'auto-mode-alist '("\\(html?\\|te?xt\\)\\(\\.erb\\)?$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[jt]sx?\\(\\.erb\\)?$" . web-mode))

;; use web-mode for jsx - https://truongtx.me/2014/03/10/emacs-setup-jsx-mode-and-jsx-syntax-checking
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

;; highlight jsx in .js files
(setq web-mode-content-types-alist
  '(("jsx" . "\\.js\\'")))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-ac-sources-alist
        '(("css" . (ac-source-words-in-buffer ac-source-css-property))
          ("html" . (ac-source-words-in-buffer ac-source-abbrev))
          ("jsx" . (ac-source-words-in-buffer ac-source-words-in-same-mode-buffers))))
  (when (equal web-mode-content-type "html")
    (setq flycheck-disabled-checkers
          (append flycheck-disabled-checkers
                  '(javascript-eslint)
                  '(typescript-tslint)
                  '(javascript-flow))))
  (when (equal web-mode-content-type "jsx")
    (setq flycheck-disabled-checkers
          (append flycheck-disabled-checkers '(json-jsonlist)))
    (if (string-equal "tsx" (file-name-extension buffer-file-name))
        (progn
          (setup-tide-mode)
;;          (flycheck-select-checker 'typescript-tslint)
          (setq flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-flow))))
      (progn
        (flycheck-select-checker 'javascript-flow)
        (setq flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(typescript-tslint)))))))

(add-hook 'web-mode-hook 'my-web-mode-hook)

;; Yasnippet
(require 'yasnippet)
;; use the nice dropdown-list widget, instead of selecting snippets in the minibuffer
;; (require 'dropdown-list)
;; (setq yas-prompt-functions '(yas-dropdown-prompt
;;                             yas-ido-prompt
;;                             yas-completing-prompt))

;; pick up custom snippets in dicksonlabs/snippets
(add-to-list 'yas-snippet-dirs
             (concat root-dir "dicksonlabs/snippets/") t)
;; use default html snippets in web-mode
(add-hook 'web-mode-hook
          #'(lambda () (yas-activate-extra-mode 'html-mode)))
;; do this to make the web-mode-hook stick
(yas-global-mode 1)
;; remap yasnippet expansion key to ctrl-tab, to prevent interference with auto-complete package
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)


(provide 'modes)
;;; modes.el ends here
