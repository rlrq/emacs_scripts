;; syntax highlighting for Eidos language (.slim and .eidos)
;; requires package highlight-numbers for proper number colouring
(defvar eidos-mode-hook nil)

(defvar eidos-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    (define-key map "\r" 'newline-and-indent)
    (define-key map "\t" 'self-insert-command)
    map)
  "Keymap for Eidos major mode")

(defvar eidos-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?/ ". 12" st)
    ;; (modify-syntax-entry ?/ ". 12 4b" st)
    ;; (modify-syntax-entry ?* ". 23" st)
    ;; (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for `eidos-mode'.")

(defvar sample-font-lock-keywords
  '(("function \\(\\sw+\\)" (1 font-lock-function-name-face)))
  "Keyword highlighting specification for `eidos-mode'.")

;; (defvar sample-imenu-generic-expression
;;   ...)

;; (defvar sample-outline-regexp
;;   ...)

;;;###autoload
;; (define-derived-mode eidos-mode fundamental-mode "Eidos"
(define-derived-mode eidos-mode fundamental-mode "Eidos"
  "A major mode for editing Eidos files."
  :syntax-table eidos-mode-syntax-table
  ;; (setq-local comment-start "// ")
  ;; (setq-local comment-end "")
  ;; (setq-local comment-start-skip "#+\\s-*")
  (setq-local font-lock-defaults
       '(eidos-font-lock-keywords))
  ;; (setq-local indent-line-function 'eidos-indent-line)
  (setq-local indent-line-function (quote insert-tab))
  (setq-local tab-width 4)
  (setq-local imenu-generic-expression
       eidos-imenu-generic-expression)
  (setq-local outline-regexp eidos-outline-regexp)
  )
  ;; ...)

(add-to-list 'auto-mode-alist '("\\.eidos\\'" . eidos-mode))
(add-to-list 'auto-mode-alist '("\\.slim\\'" . eidos-mode))
(defconst eidos-font-lock-keywords-1
  (list
   '("\\<\\(break\\|do\\|e\\(?:arly\\|lse\\)\\|f\\(?:or\\|unction\\)\\|i[fn]\\|late\\|next\\|return\\|while\\)\\>" . font-lock-keyword-face)
   '("\\<\\(NULL\\|[FT]\\)\\>" . font-lock-constant-face))
  "Minimal highlighting expressions for Eidos mode")

(defconst eidos-font-lock-keywords-2
  (append eidos-font-lock-keywords-1
          (list
           '("\\<\\(a\\(?:bs\\|cos\\|ll\\|ny\\|pply\\|rray\\|s\\(?:Float\\|Integer\\|Logical\\|String\\|in\\)\\|tan2?\\)\\|beep\\|c\\(?:atn?\\|bind\\|eil\\|itation\\|lock\\|mColors\\|o\\(?:lor2rgb\\|[rsv]\\)\\|reateDirectory\\|um\\(?:Product\\|Sum\\)\\)\\|d\\(?:ate\\|e\\(?:fine\\(?:Constant\\|Global\\)\\|leteFile\\)\\|im\\|mvnorm\\|norm\\|oCall\\|rop\\)\\|e\\(?:lementType\\|x\\(?:ecuteLambda\\|ists\\|p\\)\\)\\|f\\(?:i\\(?:lesAtPath\\|tness\\)\\|lo\\(?:at\\|or\\)\\|ormat\\|unctionSignature\\)\\|get\\(?:\\(?:See\\|w\\)d\\)\\|h\\(?:eatColors\\|sv2rgb\\)\\|i\\(?:dentical\\|felse\\|n\\(?:itialize\\(?:Gen\\(?:eConversion\\|omicElement\\(?:Type\\)?\\)\\|InteractionType\\|Mutation\\(?:\\(?:Rat\\|Typ\\)e\\)\\|RecombinationRate\\|S\\(?:LiMOptions\\|ex\\)\\|TreeSeq\\)?\\|te\\(?:ger\\(?:Dev\\|Mod\\)?\\|raction\\)\\)\\|s\\(?:F\\(?:inite\\|loat\\)\\|In\\(?:finite\\|teger\\)\\|Logical\\|N\\(?:AN\\|ULL\\)\\|Object\\|String\\)\\)\\|l\\(?:ength\\|icense\\|og\\(?:10\\|2\\|ical\\)?\\|s\\)\\|m\\(?:a\\(?:t\\(?:ch\\|eChoice\\|rix\\(?:Mult\\)?\\)\\|x\\)\\|e\\(?:an\\|thodSignature\\)\\|in\\)\\|n\\(?:c\\(?:har\\|ol\\)\\|row\\)\\|o\\(?:bject\\|rder\\)\\|p\\(?:aste0?\\|m\\(?:ax\\|in\\)\\|r\\(?:int\\|o\\(?:duct\\|pertySignature\\)\\)\\)\\|r\\(?:a\\(?:inbow\\|nge\\)\\|bin\\(?:d\\|om\\)\\|cauchy\\|dunif\\|e\\(?:adFile\\|combination\\|p\\(?:Each\\|roduction\\)\\|xp\\|[pv]\\)\\|g\\(?:amma\\|b2\\(?:color\\|hsv\\)\\|eom\\)\\|lnorm\\|m\\(?:vnorm\\)?\\|norm\\|ound\\|pois\\|unif\\|weibull\\)\\|s\\(?:a\\(?:mple\\|pply\\)\\|d\\|e\\(?:q\\(?:Along\\|Len\\)?\\|t\\(?:Difference\\|Intersection\\|S\\(?:eed\\|ymmetricDifference\\)\\|Union\\|wd\\)\\)\\|i\\(?:n\\|ze\\)\\|o\\(?:rt\\(?:By\\)?\\|urce\\)\\|qrt\\|t\\(?:op\\|r\\(?:ing\\|split\\)?\\)\\|u\\(?:bstr\\|m\\(?:Exacttan\\)?\\|ppressWarnings\\)\\|ystem\\)\\|t\\(?:errainColors\\|ime\\|runc\\|test\\|ype\\)\\|u\\(?:\\(?:niqu\\|sag\\)e\\)\\|v\\(?:ar\\|ersion\\)\\|w\\(?:hich\\(?:M\\(?:ax\\|in\\)\\)?\\|rite\\(?:\\(?:Temp\\)?File\\)\\)\\|[ct]\\)\\>" . font-lock-function-name-face)
           '("\\<\\([gimps][0-9]+\\|sim\\)\\>" . font-lock-constant-face)))
  "Additional keywords (functions) to highlight in Eidos mode")

(defvar eidos-font-lock-keywords eidos-font-lock-keywords-2
  "Default highlighting expressions for Eidos mode")


;; to generate pattern for keywords, type '(regexp-opt '("keyword1" "keyword2" ... "keywordn") t)' in a blank buffer, use M-x lisp-interaction-mode, navigate the end of the regexp-opt expression and hit C-j. Then copy over the printed output, prefixing it with '\\<' and suffixing with '\\>' so that emacs will only highlight the keywords if they aren't flanked by alphanumeric characters (I think. at the very least, if it sees whitespace around a keyword it'll highlight it)

;; (setq eidos-font-lock-keywords
;;       (let* (
;;              ;; define several categories of keywords
;;              (x-keywords '("if" "else" "while" "do" "for" "in" "next" "break" "return" "function"))
;;              (x-functions '("abs" "acos" "asin" "atan" "atan2" "ceil" "cos" "cumProduct" "cumSum" "exp" "floor" "integerDev" "integerMod" "isFinite" "isInfinite" "isNAN" "log" "log2" "log10" "product" "round" "setDifference" "setIntersection" "setSymmetricDifference" "setUnion" "sin" "sqrt" "sum" "sumExacttan" "trunc" "cor" "cov" "max" "mean" "min" "pmax" "pmin" "range" "sd" "ttest" "var" "dmvnorm" "dnorm" "rbinom" "rcauchy" "rdunif" "rexp" "rgamma" "rgeom" "rlnorm" "rmvnorm" "rnorm" "rpois" "runif" "rweibull" "c" "float" "integer" "logical" "object" "rep" "repEach" "sample" "seq" "seqAlong" "seqLen" "string" "all" "any" "cat" "catn" "format" "identical" "ifelse" "match" "nchar" "order" "paste" "paste0" "print" "rev" "size" "sort" "sortBy" "str" "strsplit" "substr" "unique" "which" "whichMax" "whichMin" "asFloat" "asInteger" "asLogical" "asString" "elementType" "isFloat" "isInteger" "isLogical" "isNULL" "isObject" "isString" "type" "apply" "array" "cbind" "dim" "drop" "matrix" "matrixMult" "ncol" "nrow" "rbind" "t" "createDirectory" "deleteFile" "filesAtPath" "getwd" "readFile" "setwd" "writeFile" "writeTempFile" "cmColors" "color2rgb" "heatColors" "hsv2rgb" "rainbow" "rgb2color" "rgb2hsv" "terrainColors" "beep" "citation" "clock" "date" "defineConstant" "defineGlobal" "doCall" "executeLambda" "exists" "functionSignature" "getSeed" "license" "ls" "rm" "sapply" "setSeed" "source" "stop" "suppressWarnings" "system" "time" "usage" "version" "str" "length" "methodSignature" "propertySignature" "size" "initializeGeneConversion" "initializeGenomicElement" "initializeGenomicElementType" "initializeInteractionType" "initializeMutationRate" "initializeMutationType" "initializeRecombinationRate" "initializeSex" "initializeSex" "initializeSLiMOptions" "initializeTreeSeq" "initialize" "fitness" "mateChoice" "mateChoice" "recombination" "interaction" "reproduction"))
;;              (x-constants '("T" "F" "NULL"))

;;              ;; generate regex string for each category of keywords
;;              (x-keywords-regexp (regexp-opt x-keywords 'words))
;;              (x-functions-regexp (regexp-opt x-functions 'words))
;;              (x-constants-regexp (regexp-opt x-constants 'words))

;;              ;; prepend \\< and append \\> to regex strings (okay this doesn't work. and i don't know enough about lisp or emacs lisp to fix this)
;;              (x-keywords-regexp2 (concat "\\<" ,x-keywords-regexp "\\>"))
;;              (x-functions-regexp2 (concat "\\<" ,x-functions-regexp "\\>"))
;;              (x-constants-regexp2 (concat "\\<" ,x-constants-regexp "\\>"))

;;         `(
;;           (,x-keywords-regexp2 . 'font-lock-keyword-face)
;;           (,x-functions-regexp2 . 'font-lock-function-name-face)
;;           (,x-constants-regexp2 . 'font-lock-constant-face)
;;           ;; order matters, because once coloured, that part won't change.
;;           ;; in general, put longer words first
;;           ))))


;;; Indentation

;; (defun eidos-indent-line ()
;;   "Indent current line of Eidos code."
;;   (interactive)
;;   (let ((savep (> (current-column) (current-indentation)))
;;         (indent (condition-case nil (max (eidos-calculate-indentation) 0)
;;                   (error 0))))
;;     (if savep
;;         (save-excursion (indent-line-to indent))
;;       (indent-line-to indent))))

;; (defun eidos-calculate-indentation ()
;;   "Return the column to which the current line should be indented."
;;   ...)


(add-hook 'eidos-mode-hook 'highlight-numbers-mode)

(add-hook 'eidos-mode-hook
          '(lambda ()
             ;; (setq indent-line-function (quote insert-tab))
             ;; (setq indent-line-function (indent-relative-maybe))
             ;; (setq indent-line-function (electric-indent-inhibit))
             ;; (setq indent-line-function (newline-and-indent-same-level))
             ;; (setq indent-line-function (indent-relative-new))
             ;; (setq indent-line-function (my-newline-and-indent))
             ;; (setq indent-relative 1)
             (electric-indent-mode 0)
             ;; (smart-tabs-mode 1)
             ;; (setq indent-tabs-mode nil)
             (setq indent-tabs-mode t)
             (setq tab-width 4)
             (set (make-local-variable 'comment-start) "// ")
             (set (make-local-variable 'comment-end) "")
             ))

(defun eidos-mode ()
  "Major mode for editing Eidos files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table eidos-mode-syntax-table)
  (use-local-map eidos-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(eidos-font-lock-keywords))
  ;; (set (make-local-variable 'indent-line-function) 'eidos-indent-line)
  (setq major-mode 'eidos-mode)
  (setq mode-name "Eidos")
  (run-hooks 'eidos-mode-hook))

(provide 'eidos-mode)

