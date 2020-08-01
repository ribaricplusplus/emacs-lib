(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(company-require-match nil)
 '(confirm-kill-processes nil)
 '(custom-enabled-themes (quote (deeper-blue)))
 '(display-line-numbers (quote relative))
 '(electric-pair-mode nil)
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(js2-strict-missing-semi-warning nil)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (php-mode company tide ido-completing-read+ rjsx-mode eyebrowse ace-window web-mode auto-rename-tag emmet-mode projectile helm expand-region smex xref-js2 js2-refactor js2-mode smartparens org)))
 '(register-preview-delay nil)
 '(show-paren-mode t)
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(typescript-indent-level 2)
 '(which-function-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Stuff added by me
(add-to-list 'load-path "/home/bruno/emacs-lib")
(add-to-list 'exec-path "/home/bruno/emacs-external/bin")
(setq visible-bell t)
(setq make-backup-files nil)
;; Display things in selected window rather than in some new, unexpected one.
(setq display-buffer-overriding-action '(display-buffer-same-window . nil))

;; Fixes package installation 'bad request' problem
;; No idea why it works. 
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")


;; Custom packages
(smartparens-global-mode t)
(semantic-mode 1)
(require 'smartparens-config)
(require 'init-smartparens)

;; JavaScript
(defun js2-open-block ()
  "Open curly brace blocks."
  (interactive)
  (newline 2)
  (indent-for-tab-command)
  (forward-line -1)
  (indent-for-tab-command)
  ) 
(require 'rjsx-mode)
(require 'js2-mode)
(require 'rjsx-mode) ;; For JSX
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
(require 'js2-refactor)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js-mode-map (kbd "M-.") nil)
(add-hook 'js2-mode-hook (lambda ()
                           (company-mode 1)
                           (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)
                           ))
(define-key js-mode-map (kbd "<C-return>") 'js2-open-block)
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . rjsx-mode))

;; Windmove
;;(when (fboundp 'windmove-default-keybindings)
;;  (windmove-default-keybindings))

;; Expand region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Helm
;; (helm-mode 1)
;; (require 'helm-config)
;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "C-x C-f") 'helm-find-files)

;; Notes
(require 'bno/notes "bno-notes")
(add-hook 'text-mode-hook (lambda ()
                            (let (filename matchresult)
                              (setq filename (buffer-file-name))
                              (setq matchresult (string-match ".txt$" filename))
                              (unless (equal matchresult nil)
                                (bno-notes-mode)
                                (message "Bruno notes mode activated."))
                              )))
(require 'bno-funcs)

;; Emmet and HTML
(require 'emmet-mode)
(require 'auto-rename-tag)
(require 'web-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook (lambda ()
                           (company-mode 1)))
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'sgml-mode-hook (lambda ()
                            (auto-rename-tag-mode t)))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . web-mode))
(global-set-key (kbd "<C-return>") 'js2-open-block)
(define-key emmet-mode-keymap (kbd "<C-return>") 'js2-open-block)

;; Ace window => Jumping between windows
(require 'ace-window)
(global-set-key (kbd "M-o") 'ace-window)

;; Kill buffer unconditionally
(defvar shell-mode-active nil)
(defun set-shell-mode-active ()
  (make-local-variable 'shell-mode-active)
  (setq shell-mode-active  t))
(add-hook 'shell-mode-hook 'set-shell-mode-active)
(add-hook 'term-mode-hook 'set-shell-mode-active)
(defun volatile-kill-buffer ()
   "Kill current buffer unconditionally."
   (interactive)
   (let ((buffer-modified-p nil))
     (if shell-mode-active
         (progn
           (kill-process (current-buffer))
           (sleep-for 0.3)
           (kill-buffer))
       (kill-buffer (current-buffer)))
     ))
(global-set-key (kbd "C-x k") 'volatile-kill-buffer) 


;; Eyebrowse
(eyebrowse-mode t)

;; Yasnippet
(require 'yasnippet)
(require 'init-yasnippet)

;; Ido
(require 'ido)
(require 'ido-completing-read+)
(ido-mode t)
(ido-everywhere t)
(ido-ubiquitous-mode t)
(setq ido-auto-merge-work-directories-length -1)

;; Projectile
(require 'projectile)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode 1)

;; Tide

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  )

(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'typescript-mode-hook (lambda ()
                                  (define-key typescript-mode-map (kbd "<C-return>") 'js2-open-block)))


;; Company
(global-set-key (kbd "S-SPC") 'company-complete)
