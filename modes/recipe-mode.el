;; syntax highlighting for custom SLiM recipe format (.slim.recipe and .eidos.recipe)
;; requires package highlight-numbers for proper number colouring
;; (gets janky when number directly follows after equal sign, but otherwise works well enough)
(defvar recipe-mode-hook nil)

(defvar recipe-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    (define-key map "\r" 'newline-and-indent)
    (define-key map "\t" 'self-insert-command)
    map)
  "Keymap for Recipe major mode")

(defvar recipe-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?/ ". 12" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for `recipe-mode'.")

(defvar sample-font-lock-keywords
  '(("function \\(\\sw+\\)" (1 font-lock-function-name-face)))
  "Keyword highlighting specification for `recipe-mode'.")

;; (defvar sample-imenu-generic-expression
;;   ...)

;; (defvar sample-outline-regexp
;;   ...)

;;;###autoload
(define-derived-mode recipe-mode fundamental-mode "Recipe"
  "A major mode for editing Recipe files."
  :syntax-table recipe-mode-syntax-table
  ;; (setq-local comment-start "// ")
  ;; (setq-local comment-end "")
  ;; (setq-local comment-start-skip "#+\\s-*")
  (setq-local font-lock-defaults
       '(recipe-font-lock-keywords))
  ;; (setq-local indent-line-function 'recipe-indent-line)
  (setq-local indent-line-function (quote insert-tab))
  (setq-local tab-width 4)
  (setq-local imenu-generic-expression
       recipe-imenu-generic-expression)
  (setq-local outline-regexp recipe-outline-regexp)
  )
  ;; ...)

(add-to-list 'auto-mode-alist '("\\.eidos\\.recipe\\'" . recipe-mode))
(add-to-list 'auto-mode-alist '("\\.slim\\.recipe\\'" . recipe-mode))
(defconst recipe-font-lock-keywords-1
  (list
   '("\\(\\[[^]]*\\]\\|=\\||\\)" . font-lock-keyword-face)
   '("\\<\\(NULL\\|[FT]\\)\\>" . font-lock-constant-face))
  "Minimal highlighting expressions for recipe mode")

(defconst recipe-font-lock-keywords-2
  (append recipe-font-lock-keywords-1
          (list
           '("\\<\\(\\^\\[\\)\\>" . font-lock-function-name-face)))
  "Additional keywords (functions) to highlight in Recipe mode")

(defvar recipe-font-lock-keywords recipe-font-lock-keywords-1
  "Default highlighting expressions for Recipe mode")


;; to generate pattern for keywords, type '(regexp-opt '("keyword1" "keyword2" ... "keywordn") t)' in a blank buffer, use M-x lisp-interaction-mode, navigate the end of the regexp-opt expression and hit C-j. Then copy over the printed output, prefixing it with '\\<' and suffixing with '\\>' so that emacs will only highlight the keywords if they aren't flanked by alphanumeric characters (I think. at the very least, if it sees whitespace around a keyword it'll highlight it)

(add-hook 'recipe-mode-hook 'highlight-numbers-mode)

(add-hook 'recipe-mode-hook
          '(lambda ()
             (electric-indent-mode 0)
             (setq indent-tabs-mode t)
             (setq tab-width 4)
             (set (make-local-variable 'comment-start) "// ")
             (set (make-local-variable 'comment-end) "")
             ))

(defun recipe-mode ()
  "Major mode for editing Recipe files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table recipe-mode-syntax-table)
  (use-local-map recipe-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(recipe-font-lock-keywords))
  ;; (set (make-local-variable 'indent-line-function) 'recipe-indent-line)
  (setq major-mode 'recipe-mode)
  (setq mode-name "Recipe")
  (run-hooks 'recipe-mode-hook))

(provide 'recipe-mode)

