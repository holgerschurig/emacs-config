;; -*- mode: lisp -*-
;; see: https://github.com/minad/tempel#template-syntax
;; see: https://www.emacswiki.org/emacs/tempo-c-cpp.el
;; see: https://www.emacswiki.org/emacs/tempo-c-cpp.el
;;
;; "string"                      Inserts a string literal.
;; p                             Inserts an unnamed placeholder field.
;; n                             Inserts a newline.
;; >                             Indents with indent-according-to-mode.
;; r                             Inserts the current region.
;; r>                            The region, but indented.
;; n>                            Inserts a newline and indents.
;; &                             Insert newline if there is only whitespace between line start and point.
;; %                             Insert newline if there is only whitespace between point and line end.
;; o                             Like % but leaves the point before newline.
;; (s NAME)                      Inserts a named field.
;; (p PROMPT <NAME> <NONINS>)    Insert an optionally named field with a prompt.
;;                               The PROMPT is displayed directly in the buffer
;;                               as default value. If NOINSERT is non-nil, no
;;                               field is inserted. Then the minibuffer is used
;;                               for prompting and the value is bound to NAME.
;; (r PROMPT <NAME> <NOINSERT>)  Insert region or act like (p ...).
;; (r> PROMPT <NAME> <NOINSERT>) Act like (r ...), but indent region.
;; (p FORM <NAME> <NONINS>)      Like p described above, but FORM is evaluated.
;; (FORM ...)                    Other Lisp forms are evaluated. Named fields are lexically bound.



fundamental-mode ;; Available everywhere

(today (format-time-string "%Y-%m-%d"))
(calc (p "taylor(sin(x),x=0,3)" formula) n "----" n (format "%s" (calc-eval formula)))



prog-mode

(fixme (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "FIXME ")
(todo (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "TODO ")
(bug (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "BUG ")
(hack (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "HACK ")




latex-mode

(begin "\\begin{" (s env) "}" r> n> "\\end{" (s env) "}")
(frac "\\frac{" p "}{" q "}")
(enumerate "\\begin{enumerate}\n\\item " r> n> "\\end{enumerate}")
(itemize "\\begin{itemize}\n\\item " r> n> "\\end{itemize}")



emacs-lisp-mode

(autoload ";;;###autoload")
(pt "(point)")
(lambda "(lambda (" p ")" n> r> ")")
(var "(defvar " p "\n  \"" p "\")")
(local "(defvar-local " p "\n  \"" p "\")")
(const "(defconst " p "\n  \"" p "\")")
(custom "(defcustom " p "\n  \"" p "\"" n> ":type '" p ")")
(face "(defface " p " '((t :inherit " p "))\n  \"" p "\")")
(group "(defgroup " p " nil\n  \"" p "\"" n> ":group '" p n> ":prefix \"" p "-\")")
(macro "(defmacro " p " (" p ")\n  \"" p "\"" n> r> ")")
(alias "(defalias '" p " '" p ")")
(fun "(defun " p " (" p ")\n  \"" p "\"" n> r> ")")
(iflet "(if-let (" p ")" n> r> ")")
(whenlet "(when-let (" p ")" n> r> ")")
(iflet* "(if-let* (" p ")" n> r> ")")
(whenlet* "(when-let* (" p ")" n> r> ")")
(andlet* "(and-let* (" p ")" n> r> ")")
(cond "(cond" n "(" q "))" >)
(pcase "(pcase " (p "scrutinee") n "(" q "))" >)
(let "(let (" p ")" n> r> ")")
(let* "(let* (" p ")" n> r> ")")
(rec "(letrec (" p ")" n> r> ")")
(dotimes "(dotimes (" p ")" n> r> ")")
(dolist "(dolist (" p ")" n> r> ")")
(loop "(cl-loop for " p " in " p " do" n> r> ")")
(command "(defun " p " (" p ")\n  \"" p "\"" n> "(interactive" p ")" n> r> ")")
(advice "(defun " (p "adv" name) " (&rest app)" n> p n> "(apply app))" n>
        "(advice-add #'" (p "fun") " " (p ":around") " #'" (s name) ")")
(provide "(provide '" (file-name-base (or (buffer-file-name) (buffer-name))) ")" n
         ";;; " (file-name-nondirectory (or (buffer-file-name) (buffer-name))) " ends here" n)



eshell-mode

(for "for " (p "i") " in " p " { " q " }")
(while "while { " p " } { " q " }")
(until "until { " p " } { " q " }")
(if "if { " p " } { " q " }")
(ife "if { " p " } { " p " } { " q " }")
(unl "unless { " p " } { " q " }")
(unle "unless { " p " } { " p " } { " q " }")



text-mode

(cut "--8<---------------cut here---------------start------------->8---" n r n
     "--8<---------------cut here---------------end--------------->8---" n)
(asciibox "+-" (make-string (length str) ?-) "-+" n
          "| " (s str)                       " |" n
          "+-" (make-string (length str) ?-) "-+" n)



rst-mode

(title (make-string (length title) ?=) n (p "Title: " title) n (make-string (length title) ?=) n)



org-mode

(elisp "#+begin_src emacs-lisp" n> r> n "#+end_src")
(c "#+begin_src C" n> r> n "#+end_src")
(cpp "#+begin_src cpp" n> r> n "#+end_src")
(python "#+begin_src python" n> r> n "#+end_src")
(js "#+begin_src js" n> r> n "#+end_src")
(bash "#+begin_src bash" n> r> n "#+end_src")
(latex "#+begin_src latex" n> r> n "#+end_src")
(+q "#+begin_quote" n> r> n> "#+end_quote\n")
(+e "#+begin_example" n> r> n> "#+end_example\n")
(+c "#+begin_center" n> r> n> "#+end_center\n")
(+s "#+begin_src" n> r> n> "#+end_src\n")
(+o "#+begin_comment" n> r> n> "#+end_comment\n")
(+v "#+begin_verse" n> r> n> "#+end_verse\n")
(+l "#+begin_src emacs-lisp" n> r> n "#+end_src\n"
    :post (progn (tempel-done) (org-edit-src-code)))



c-mode :condition (re-search-backward "^\\w*$" (line-beginning-position) 'noerror)

;; (inc "#include <" (p (concat (file-name-base (or (buffer-file-name) (buffer-name))) ".h")) ">")
;; (incc "#include \"" (p (concat (file-name-base (or (buffer-file-name) (buffer-name))) ".h")) "\"")

(dd "/**\n * @brief " p "\n */")
(ii "#include \"" p ".h\"")
(is "#include <" p ".h>")
(head "#ifndef " (p (concat (upcase (file-name-base (buffer-file-name))) "_H") clause) > n "#define " (s clause) n> p n "#endif" n>)
(main > "int main(int argc, char *argv[])" n> "{" >  n> > r n "return 0;" > n "}" > n>)
;; Doxygen documentation
