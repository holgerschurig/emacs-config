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

(begin "\\begin{" (s env) "}" > n> r> "\\end{" (s env) "}")
(frac "\\frac{" p "}{" p "}")
(enumerate "\\begin{enumerate}\n\\item " r> n> "\\end{enumerate}")
(itemize "\\begin{itemize}\n\\item " r> n> "\\end{itemize}")



emacs-lisp-mode

(lamb "(lambda (" p ")" n> r> ")")
(var "(defvar " p "\n  \"" p "\")")
(local "(defvar-local " p "\n  \"" p "\")")
(const "(defconst " p "\n  \"" p "\")")
(custom "(defcustom " p "\n  \"" p "\"" n> ":type '" p ")")
(face "(defface " p " '((t :inherit " p "))\n  \"" p "\")")
(group "(defgroup " p " nil\n  \"" p "\"" n> ":group '" p n> ":prefix \"" p "-\")")
(macro "(defmacro " p " (" p ")\n  \"" p "\"" n> r> ")")
(alias "(defalias '" p " '" p ")")
(fun "(defun " p " (" p ")\n  \"" p "\"" n> r> ")")
(cmd "(defun " p " (" p ")\n  \"" p "\"" n> "(interactive)" n> r> ")")
(iflet "(if-let (" p ")" n> r> ")")
(whenlet "(when-let (" p ")" n> r> ")")
(cond "(cond" n "(" p "))" >)
(pcase "(pcase " (p "scrutinee") n "(" p "))" >)
(let "(let (" p ")" n> r> ")")
(star "(let* (" p ")" n> r> ")")
(rec "(letrec (" p ")" n> r> ")")
(dotimes "(dotimes (" p ")" n> r> ")")
(dolist "(dolist (" p ")" n> r> ")")
(loop "(cl-loop for " p " in " p " do" n> r> ")")
(adv "(defun " (p "adv" name) " (&rest app)" n> p n> "(apply app))" n>
        "(advice-add #'" (p "fun") " " (p ":around") " #'" (s name) ")")
(prov "(provide '" (file-name-base (or (buffer-file-name) (buffer-name))) ")" n n)



eshell-mode

(for "for " (p "i") " in " p " { " p " }")
(while "while { " p " } { " p " }")
(until "until { " p " } { " p " }")
(if "if { " p " } { " p " }")
(if-else "if { " p " } { " p " } { " p " }")
(unless "unless { " p " } { " p " }")
(unless-else "unless { " p " } { " p " } { " p " }")



text-mode

(cut "--8<---------------cut here---------------start------------->8---" n r n
     "--8<---------------cut here---------------end--------------->8---" n)
(asciibox "+-" (make-string (length str) ?-) "-+" n
          "| " (s str)                       " |" n
          "+-" (make-string (length str) ?-) "-+" n)



rst-mode

(title (make-string (length title) ?=) n (p "Title: " title) n (make-string (length title) ?=) n)



org-mode

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

(inch "#include \"" p ".h\"")
(incs "#include <" p ".h>")
(head "#ifndef " (p (concat (upcase (file-name-base (buffer-file-name))) "_H") clause) > n "#define " (s clause) n> p n "#endif" n>)
(main > "int main(int argc, char *argv[])" n> "{" >  n> > r n "return 0;" > n "}" > n>)
