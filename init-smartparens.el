(provide 'init-smartparens)
(message "%s" "Smartparens init file loaded.")
(sp-use-smartparens-bindings)
(define-key smartparens-mode-map (kbd "C-S-a") 'sp-beginning-of-sexp)
(define-key smartparens-mode-map (kbd "C-S-e") 'sp-end-of-sexp)
(define-key smartparens-mode-map (kbd "C-c c") 'sp-change-enclosing)
(define-key smartparens-mode-map (kbd "C-c s r") 'sp-rewrap-sexp)
