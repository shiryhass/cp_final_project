

(load "pc.scm")

(load "pattern-matcher.scm")

    

        

(define <0-9>

  (range #\0 #\9))



(define <1-9>

  (range #\1 #\9))



(define <a-f>

  (range-ci #\a #\f))



(define <a-z>

  (range-ci #\a #\z))



(define <small-a-z>

  (range #\a #\z))



(define <boolean>

  (new (*parser (word-ci "#t"))

       (*pack

	(lambda (_) #t))



       (*parser (word-ci "#f"))

       (*pack

	(lambda (_) #f))



       (*disj 2)

       done))



(define <char-prefix>

  (new (*parser (char #\"))

       done))



(define <visibleSimpleChar>

  (range #\! #\~))



(define <vschar>

  (new (*parser <visibleSimpleChar>)

       done))





(define ^<meta-char>

  (lambda (str ch)

    (new (*parser (word str))

	 (*pack (lambda (_) ch))

	 done)))





(define <NameChar>

  (new 

   (*parser (word-ci "lambda"))

   (*pack (lambda (_) (integer->char 955)))

   (*parser (word-ci "newline"))

   (*pack (lambda (_) #\newline))

   (*parser (word-ci "nul"))

   (*pack (lambda (_) #\nul)) 

   (*parser (word-ci "page"))

   (*pack (lambda (_) #\page))

   (*parser (word-ci "return"))

   (*pack (lambda (_) #\return))

   (*parser (word-ci "space"))

   (*pack (lambda (_) #\space))

   (*parser (word-ci "tab"))

   (*pack (lambda (_) #\tab))

   (*disj 7)

   done))









(define <hex-digit>

  (let ((zero (char->integer #\0))

	(lc-a (char->integer #\a))

	(uc-a (char->integer #\A)))

    (new (*parser (range #\0 #\9))

	 (*pack

	  (lambda (ch)

	    (- (char->integer ch) zero)))



	 (*parser (range #\a #\f))

	 (*pack

	  (lambda (ch)

	    (+ 10 (- (char->integer ch) lc-a))))



	 (*parser (range #\A #\F))

	 (*pack

	  (lambda (ch)

	    (+ 10 (- (char->integer ch) uc-a))))



	 (*disj 3)

	 done)))





(define hexa->digit 

  (lambda (lst)

    (if (null? lst) 0

	(+ (* (car lst) (expt 16 (sub1 (length lst))))  (hexa->digit(cdr lst)))

        )))



(define <hex-char>

  (new 

   (*parser (char-ci #\x))

   (*parser <hex-digit>) *plus

   (*caten 2)

   (*pack-with (lambda (_ rest)

		 (if (and (>= (hexa->digit rest) 0) (< (hexa->digit rest)  1114112))

		     (integer->char (hexa->digit rest))

		     'failed-to-convert)))

   done))



(define <char>

  (new 

   (*parser (word "#\\"))



   (*parser <hex-char>)

   (*parser <NameChar>)

   (*parser <vschar>)

   (*disj 3)

   (*caten 2)

   (*pack-with (lambda (prefix char) char))

   done))





(define <op>

  (new

   (*parser (char #\+))

   (*parser (char #\-))

   (*disj 2)

   (*pack (lambda (op-char)

	    (string->symbol (string op-char))))

   done))







(define <nat>

  (new



   

   (*parser <0-9>)

   (*parser <0-9>) *star

   (*caten 2)

   (*pack-with

    (lambda (x xs)

      (string->number (list->string `(,x ,@xs)))))

   

   

   done))















(define <int>

  (new (*parser (char #\+))

       (*parser <nat>)

       (*caten 2)

       (*pack-with (lambda (op nat)

		     nat))



       (*parser (char #\-))

       (*parser <nat>)

       (*caten 2)

       (*pack-with (lambda (op nat)

		     (- nat)))



       (*parser <nat>)



       (*disj 3)



       done))





(define <frac>

  (new (*parser <int>)

       (*parser (char #\/))

       (*parser  <nat>)

       (*guard (lambda (n) (not (zero? n))))

       (*caten 3)

       (*pack-with (lambda (int op nat)

		     ( / int nat)))

       done))





(define <num>

  (new 

   (*parser <frac>)

   (*parser <int>)

   (*disj 2)

   done))







(define <string-meta-char>

  (new (*parser <hex-char>)

       (*parser (^<meta-char> "\\\\" #\\))

       (*parser (^<meta-char> "\\\"" #\"))

       (*parser (^<meta-char> "\\n" #\newline))

       (*parser (^<meta-char> "\\r" #\return))

       (*parser (^<meta-char> "\\t" #\tab))

       (*parser (^<meta-char> "\\f" #\page)) 

       (*parser (^<meta-char> "\\{lambda}" (integer->char 955)))

       (*parser (^<meta-char> "\\{alef}" (integer->char 1488)))

       (*parser (^<meta-char> "\\{bismillah}" (integer->char 65021)))

       (*parser (^<meta-char> "\\{smiley}" (integer->char 9786)))



       (*disj 11)

       done))





(define <string-char>

  (new (*parser <string-meta-char>)



       (*parser <any-char>)



       (*parser (char #\"))

       (*parser (char #\\))

       (*disj 2)



       *diff

       (*disj 2)

       done))







(define <hex-rep>

  (new

   (*parser <0-9>)

   (*parser <a-f>)

   (*disj 2)

   done))





(define <stringhexchar>

  (new

   (*parser (word-ci "\\x"))

   (*parser <hex-rep>) *plus

   (*parser (char #\;))

   (*caten 3)

   

   (*pack-with (lambda (pre hex post) (integer->char(string->number(list->string hex)16))))  

   

   done))



(define <string>

  (new (*parser (char #\"))

       (*parser <stringhexchar>)

       (*parser <string-char>)

       (*disj 2)

       *star

       (*parser (char #\"))

       (*caten 3)



       (*pack-with

	(lambda (open-delim chars close-delim)

	  (list->string chars)))



       done))





(define <symbolchar>

  (new 

   (*parser <0-9>)

   



   

   

   

   

   (*parser <a-z>)

   

   (*parser (char #\!))

   (*parser (char #\$))

   (*parser (char #\^))

   (*parser (char #\*))

   (*parser (char #\-))

   (*parser (char #\_))

   (*parser (char #\=))

   (*parser (char #\+))

   (*parser (char #\<))

   (*parser (char #\>))

   (*parser (char #\?))

   (*parser (char #\/))



   (*disj 14)

   done))







(define <symbol>

  (new 

   (*parser <symbolchar>)

   *plus

   (*pack (lambda (x) (string->symbol (string-downcase(list->string x) )) ))

   done))





(define <whitespace>

  (const

   (lambda (ch)

     (char<=? ch #\space))))





(define <properlist>

  (new

   (*parser (char #\()) 

   

   (*parser <whitespace>)

   *star

   (*delayed (lambda() <sexpr>))

   (*parser <whitespace>)

   *star

   (*caten 3)

   (*pack-with (lambda(ls sexp rs) sexp )) 

   *star

   (*parser (char #\) )) 

   (*caten 3)

   (*pack-with (lambda(lb sexp rb) sexp))



   

   done))





(define <improperlist>

  (new

   (*parser (char #\()) 

   

   (*parser <whitespace>)*star

   (*delayed (lambda() <sexpr>))

   (*parser <whitespace>)*star

   (*caten 3)

   (*pack-with (lambda(ls sexp rs) sexp ))*plus

   

   (*parser <whitespace>)*star

   (*parser (char #\.))

   (*parser <whitespace>)*star

   (*caten 3)

   (*pack-with (lambda (a b c) b))

   

   (*delayed (lambda() <sexpr>))

   (*parser <whitespace>)*star

   (*caten 2)

   (*pack-with (lambda (a b) a))

   

   (*parser (char #\) )) 

   (*caten 5)

   (*pack-with (lambda(lb sexp dot sexp2 rb) `(,@sexp ,@sexp2)))

   

   

   done))  







(define <vector>

  (new

   (*parser (char #\#))

   (*parser (char #\())

   

   (*parser <whitespace>)

   *star

   (*delayed (lambda() <sexpr>))

   (*parser <whitespace>)

   *star

   (*caten 3)

   (*pack-with (lambda(ls sexp rs) sexp )) 

   *star

   (*parser (char #\) )) 

   (*caten 4)

   (*pack-with (lambda(hash lb sexp rb) (list->vector sexp)))

   

   done))





(define <quoted>

  (new

   (*parser (char #\'))

   

   

   (*parser <whitespace>)

   *star

   (*delayed (lambda() <sexpr>))

   (*parser <whitespace>)

   *star

   (*caten 3)

   (*pack-with (lambda(ls sexp rs) sexp )) 

   

   (*caten 2)

   (*pack-with (lambda(qut sexp) (list 'quote sexp)))

   

   done))



(define <quasiquoted>

  (new

   (*parser (char #\`))

   

   

   (*parser <whitespace>)

   *star

   (*delayed (lambda() <sexpr>))

   (*parser <whitespace>)

   *star

   (*caten 3)

   (*pack-with (lambda(ls sexp rs) sexp )) 

   

   (*caten 2)

   (*pack-with (lambda(qut sexp) (list 'quasiquote sexp)))

   

   done))





(define <unquoted>

  (new

   (*parser (char #\,))

   

   

   (*parser <whitespace>)

   *star

   (*delayed (lambda() <sexpr>))

   (*parser <whitespace>)

   *star

   (*caten 3)

   (*pack-with (lambda(ls sexp rs) sexp )) 

   

   (*caten 2)

   (*pack-with (lambda(qut sexp) (list 'unquote sexp)))

   

   done))













(define <unquoteandspliced>

  (new

   (*parser (char #\,))

   (*parser (char #\@))

   

   

   

   (*parser <whitespace>)

   *star

   (*delayed (lambda() <sexpr>))

   (*parser <whitespace>)

   *star

   (*caten 3)

   (*pack-with (lambda(ls sexp rs) sexp )) 

   

   (*caten 3)

   (*pack-with (lambda(qut at sexp) (list 'unquote-splicing sexp)))

   

   done))













(define <line-comment>

  (let ((<end-of-line-comment>

	 (new (*parser (char #\newline))

	      (*parser <end-of-input>)

	      (*disj 2)

	      done)))

    (new (*parser (char #\;))

	 

	 (*parser <any-char>)

	 (*parser <end-of-line-comment>)

	 *diff *star



	 (*parser <end-of-line-comment>)

	 (*caten 3)

	 done)))



(define <sexpr-comment>

  (new (*parser (word "#;"))

       (*delayed (lambda () <InfixExpression>))

       (*delayed (lambda () <sexpr>))

       (*disj 2)

       (*caten 2)

       done))



(define <comment>

  (disj <line-comment>

	<sexpr-comment>))



(define <skip>

  (disj <comment>

	<whitespace>))



(define ^^<wrapped>

  (lambda (<wrapper>)

    (lambda (<p>)

      (new (*parser <wrapper>)

	   (*parser <p>)

	   (*parser <wrapper>)

	   (*caten 3)

	   (*pack-with

	    (lambda (_left e _right) e))

	   done))))



(define ^<skipped*> (^^<wrapped> (star <skip>)))













(define <InfixPrefixExtensionPrefix>

  (new 

   (*parser <whitespace>)*star

   

   (*parser( word "##"))

   (*parser( word "#%"))

   (*disj 2)

   

   (*parser <whitespace>)*star

   (*caten 3)

   (*pack-with (lambda ( ls ext rs) ext))



   

   done))



(define <otherSymbols>

  (new

   (*parser( char #\+))

   (*parser( char #\-))

   (*parser( char #\*))

   (*parser( char #\/))

   (*parser( word "**"))

   (*parser( char #\^))

   (*disj 6)

   done))



(define <InfixSymbol>

  (new

   (*parser <whitespace>)*star

   

   

   (*parser <symbolchar>)

   (*parser <otherSymbols>)

   *diff

   *plus

   (*pack (lambda (x) (string->symbol (string-downcase(list->string x) )) ))

   

   (*parser <whitespace>)*star

   (*caten 3)

   (*pack-with (lambda ( ls infSym rs) infSym))

   

   done))







(define <PowerSymbol>

  (new 





   (*parser <whitespace>)*star

   (*parser (^<meta-char> "**" #\^))

   (*parser( char #\^))

   (*disj 2)

   (*parser <whitespace>)*star

   (*caten 3)

   (*pack-with (lambda ( ls powSym rs) powSym))

   

   done))

(define <mulSymbol>

  (new 

   (*parser (char #\*))

   (*parser (char #\/))

   (*disj 2)

   done))





(define <addSymbol>

  (new 

   (*parser (char #\+))

   (*parser (char #\-))

   (*disj 2)



   

   

   done))







(define <atom>

  (new

   

   (*parser (char #\())

   (*delayed (lambda() <InfixExpression>))

   (*parser (char #\)))

   (*caten 3)

   (*pack-with (lambda (lb exp rb)  exp ))

   

   

   (*parser <whitespace>)*star

   (*parser <num>)

   (*parser <whitespace>)*star

   (*caten 3)

   (*pack-with (lambda ( ls numb rs) numb))



   

   (*parser <skip>)*star

   (*parser (char #\-))

   (*delayed (lambda() <InfixExpression>))

   (*caten 3)

   (*pack-with (lambda (a minus numb )

		 `( ,(string->symbol(string minus)) ,numb)))

   

   

   (*delayed (lambda() <InfixSexprEscape>))



   

   

   

   (*parser <whitespace>)*star

   (*parser <InfixSymbol>)

   (*parser <whitespace>)*star

   (*caten 3)

   (*pack-with (lambda ( ls symb rs) symb))

   

   (*disj 5)

   done))



(define <InfixArrayGet>

  (new

   (*parser <atom>)

   

   

   (*parser (char #\[))

   (*delayed(lambda() <InfixExpression>))

   (*parser (char #\]))

   (*caten 3)

   (*pack-with (lambda(lbr expr rbr) (lambda (exp1) `(vector-ref ,exp1 ,expr))))

   *star

   

   (*caten 2)

   (*pack-with (lambda(firstExp lst) (fold-left (lambda (e lmb) (lmb e))  firstExp lst)))

   done))



(define <InfixArgList>   

  (new

   

   (*delayed(lambda() <InfixExpression>))

   (*delayed(lambda() <help>))

   (*disj 2)

   

   (*parser (char #\,))

   (*delayed(lambda() <InfixExpression>))

   (*caten 2)

   

   (*pack-with(lambda(comma iexp) iexp))

   *star



   

   (*caten 2)

   (*pack-with (lambda (a b) `( ,a ,@b)))

   

   (*parser (char #\( ))

   (*parser (char #\) ))

   (*caten 2)

   (*pack-with (lambda (O P) '() ))

   (*disj 2)

   





   done))







(define <InfixFuncall>

  (new

   

   (*parser (char #\( ))

   (*parser <InfixArgList>)

   (*parser (char #\)))

   (*caten 3)

   (*pack-with (lambda (lbr expr rbr) (lambda (exp1) `(,exp1 ,@expr))))

   

   

   (*parser (char #\( ))

   (*parser <skip>)*star

   (*caten 2)

   (*pack-with (lambda (a b) a))

   (*parser (char #\)))

   (*caten 2)

   (*pack-with (lambda (op cl) (lambda (a ) `(,a) )))

   

   (*disj 2)

   done))







(define <InfixArrayGet>

  (new

   (*parser (char #\[))

   (*delayed(lambda() <InfixExpression>))

   (*parser (char #\]))

   (*caten 3)

   (*pack-with (lambda(lbr expr rbr) (lambda (exp1) `(vector-ref ,exp1 ,expr))))

   done))



(define <help>

  (new

   

   (*parser <atom>)   

   (*parser <InfixFuncall>)

   (*parser <InfixArrayGet>)

   (*disj 2)

   *star

   

   (*caten 2)

   (*pack-with (lambda( firstExp lst) 

		 (fold-left (lambda (e lmb) (lmb e))  firstExp  lst)))



   (*delayed (lambda() <InfixSexprEscape>))

   (*disj 2)

   

   done))





(define <InfixPow>

  (new 

   (*parser <skip>) *star

   (*parser <help>)

   (*parser <skip>) *star

   (*caten 3)

   (*pack-with (lambda (leftSkip exp rightSkip) exp))

   

   (*parser <PowerSymbol>)

   

   (*parser <skip>) *star

   (*delayed (lambda () <InfixPow>))

   (*parser <help>)

   (*disj 2)

   (*parser <skip>) *star

   (*caten 3)

   (*pack-with (lambda( leftSkip exp rightSkip) exp))

   

   (*caten 2)

   (*pack-with (lambda (add mu) (lambda (ele)  `(expt ,ele ,mu ))))

   *star

   (*caten 2)

   (*pack-with (lambda(atm poList) (fold-left (lambda (e lmb) (lmb e)) atm  poList)))

   done))















(define <InfixMul>   

  (new

   (*parser <InfixPow>)

   

   

   (*parser <whitespace>)*star

   (*parser <mulSymbol>)

   (*parser <whitespace>)*star

   (*caten 3)

   (*pack-with (lambda ( ls mulymb rs) mulymb))

   

   

   

   (*parser <InfixPow>)

   (*caten 2)

   (*pack-with (lambda (add mu) (lambda (ele) `(,(string->symbol (string add)) ,ele ,mu) )))

   

   *star

   (*caten 2)

   (*pack-with (lambda(pw muList) (fold-left (lambda (e lmb) (lmb e)) pw muList)))



   done))



(define <InfixAdd>   

  (new

   (*parser <InfixMul>)

   

   (*parser <whitespace>)*star

   (*parser <addSymbol>)

   (*parser <whitespace>)*star

   (*caten 3)

   (*pack-with (lambda ( ls addymb rs) addymb))



   (*parser <InfixMul>)

   (*caten 2)

   (*pack-with (lambda (add mu) (lambda (ele) `(,(string->symbol (string add)) ,ele ,mu))))

   *star



   (*caten 2)

   (*pack-with (lambda(mu addList) (fold-left (lambda (e lmb) (lmb e)) mu addList)))





   done))



























(define <InfixSexprEscape>

  (new (*parser <skip>)*star

       (*parser <InfixPrefixExtensionPrefix>)

       (*parser <skip>)*star

       (*caten 3)

       (*pack-with (lambda (ls exp rs) exp))

       (*delayed (lambda () <sexpr>))

       (*parser <skip>)

       *star

       (*caten 2)

       (*pack-with (lambda (firstExp rest) firstExp))

       (*caten 2)

       (*pack-with (lambda (exp1 rest) rest))

       done))











(define <InfixExpression>

  (new

   



   (*parser <InfixSexprEscape>)

   (*parser <InfixAdd>)

   (*disj 2)



   done))



(define <InfixExtension>

  (new 

   (*parser <InfixPrefixExtensionPrefix>)

   (*parser <InfixExpression>)

   (*caten 2)

   (*pack-with (lambda (a b) b))

   done))





(define <sexpr>

  (^<skipped*> (new

		

		(*parser <boolean>)

		

                (*parser <char>)

		

		(*parser <num>)

		

		

		

		

		(*parser <symbol>)

		(*parser <0-9>)

		*diff

		*not-followed-by

		(*parser <symbol>)

		

		(*parser <string>)

		(*parser <properlist>)

		(*parser <improperlist>)

		(*parser <vector>)

		(*parser <quoted>)

		(*parser <quasiquoted>)

		(*parser <unquoted>)

		(*parser <unquoteandspliced>)

		(*parser <InfixExtension>)

		



		(*disj 13)



		done)))











					;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

					;\\\\\\\\\\\\\\\WORK-2\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\





(define *void-object* (if #f #f))





(define simple-const?

  (let ((type (list number? char?  string? vector? boolean?)))

    (lambda (expr)

      (ormap (lambda (t?) (t? expr)) type))))





(define *reserved-words*

  '(and begin cond define do else if lambda

	let let* letrec or quasiquote unquote

	unquote-splicing quote set!))



(define is-reserved?

  (lambda (e)

    (ormap (lambda (word) (eq? e word)) *reserved-words*)))

(define not-reserved

  (lambda (e)

    (not (is-reserved? e))))



(define var?

  (lambda (expr) (and (symbol? expr) (not (is-reserved? expr)))))







(define beginify

  (lambda (s)

    (cond ((null? s) *void-object*)

	  ((null? (cdr s)) (car s))

	  (else `(begin ,@s)))))















(define argument-list?

  (letrec ((loop

	    (lambda (e seen)

	      (or (null? e)

		  (and (pair? e)

		       (var? (car e))

		       (not (ormap (lambda (seen-var) (eq? (car e) seen-var)) seen))

		       (loop (cdr e) (cons (car e) seen)))))))

    (lambda (e) 

      (take-apart$ e

		   (lambda () (loop e '()))

		   (lambda (s a) (loop (cons a s) '()))

		   (lambda () #t)))))





;;lambda-auxilary

(define (take-apart$ exp on-proper on-improper on-atom)

  (cond 

   ((pair? exp) (take-apart$ (cdr exp) 

			     on-proper 

			     (lambda(cdr-res last-param) (on-improper (cons (car exp) cdr-res) last-param))

			     (lambda()(on-improper (list (car exp)) (cdr exp)))))

   ((null? exp) (on-proper))

   (else (on-atom))))







(define okList?

  (lambda (lst)

    (and (not(null? lst)) (list? lst))))



					;\\\\\\\\\\\\\\\\\\\\\\mayer code\\\\\\\\\\\\\\\\\\\\\\





(define ^quote?

  (lambda (tag)

    (lambda (e)

      (and (pair? e)

	   (eq? (car e) tag)

	   (pair? (cdr e))

	   (null? (cddr e))))))



(define quote? (^quote? 'quote))

(define unquote? (^quote? 'unquote))

(define unquote-splicing? (^quote? 'unquote-splicing))



(define const?

  (let ((simple-sexprs-predicates

	 (list boolean? char? number? string?)))

    (lambda (e)

      (or (ormap (lambda (p?) (p? e))

		 simple-sexprs-predicates)

	  (quote? e)))))



(define quotify

  (lambda (e)

    (if (or (null? e)

	    (pair? e)

	    (symbol? e)

	    (vector? e))

	`',e

	e)))



(define unquotify

  (lambda (e)

    (if (quote? e)

	(cadr e)

	e)))



(define const-pair?

  (lambda (e)

    (and (quote? e)

	 (pair? (cadr e)))))



(define expand-qq

  (letrec ((expand-qq

	    (lambda (e)

	      (cond ((unquote? e) (cadr e))

		    ((unquote-splicing? e)

		     (error 'expand-qq

			    "unquote-splicing here makes no sense!"))

		    ((pair? e)

		     (let ((a (car e))

			   (b (cdr e)))

		       (cond ((unquote-splicing? a)

			      `(append ,(cadr a) ,(expand-qq b)))

			     ((unquote-splicing? b)

			      `(cons ,(expand-qq a) ,(cadr b)))

			     (else `(cons ,(expand-qq a) ,(expand-qq b))))))

		    ((vector? e) `(list->vector ,(expand-qq (vector->list e))))

		    ((or (null? e) (symbol? e)) `',e)

		    (else e))))

	   (optimize-qq-expansion (lambda (e) (optimizer e (lambda () e))))

	   (optimizer

	    (compose-patterns

	     (pattern-rule

	      `(append ,(? 'e) '())

	      (lambda (e) (optimize-qq-expansion e)))

	     (pattern-rule

	      `(append ,(? 'c1 const-pair?) (cons ,(? 'c2 const?) ,(? 'e)))

	      (lambda (c1 c2 e)

		(let ((c (quotify `(,@(unquotify c1) ,(unquotify c2))))

		      (e (optimize-qq-expansion e)))

		  (optimize-qq-expansion `(append ,c ,e)))))

	     (pattern-rule

	      `(append ,(? 'c1 const-pair?) ,(? 'c2 const-pair?))

	      (lambda (c1 c2)

		(let ((c (quotify (append (unquotify c1) (unquotify c2)))))

		  c)))

	     (pattern-rule

	      `(append ,(? 'e1) ,(? 'e2))

	      (lambda (e1 e2)

		(let ((e1 (optimize-qq-expansion e1))

		      (e2 (optimize-qq-expansion e2)))

		  `(append ,e1 ,e2))))

	     (pattern-rule

	      `(cons ,(? 'c1 const?) (cons ,(? 'c2 const?) ,(? 'e)))

	      (lambda (c1 c2 e)

		(let ((c (quotify (list (unquotify c1) (unquotify c2))))

		      (e (optimize-qq-expansion e)))

		  (optimize-qq-expansion `(append ,c ,e)))))

	     (pattern-rule

	      `(cons ,(? 'e1) ,(? 'e2))

	      (lambda (e1 e2)

		(let ((e1 (optimize-qq-expansion e1))

		      (e2 (optimize-qq-expansion e2)))

		  (if (and (const? e1) (const? e2))

		      (quotify (cons (unquotify e1) (unquotify e2)))

		      `(cons ,e1 ,e2))))))))

    (lambda (e)

      (optimize-qq-expansion

       (expand-qq e)))))



					;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\up to here\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\



(define parse

  (let ((run

	 (compose-patterns					;;simple-const    	  

	  (pattern-rule (? 'c simple-const?) ;simple-const?     is a "guard"

			(lambda (c) `(const ,c)))

					;quote                        

	  (pattern-rule `(quote ,(? 'c)) 

			(lambda (c) `(const ,c)))





	  ;;variable

	  (pattern-rule (? 'v var?) ;var? is a "guard"

			(lambda (v) `(var  ,v)))





	  ;;if

	  (pattern-rule `(if ,(? 'test) ,(? 'consq) ,(? 'alter))

			(lambda (t c alter)  `(if3 ,(parse t) ,(parse c) ,(parse alter))))



	  (pattern-rule `(if ,(? 'test) ,(? 'consq))

			(lambda (t c)  `(if3 ,(parse t) ,(parse c) (const ,*void-object*) )))



	  ;;cond                



    	  (pattern-rule `(cond ,(? 'firstCondList okList?))

			(lambda (firstCondList ) (parse (expand-cond (cons firstCondList void)))))

	  

	  

	  (pattern-rule `(cond ,(? 'firstCondList okList?) . ,(? 'restCondList okList?))

			(lambda (firstCondList restCondList) (parse (expand-cond (cons firstCondList restCondList)))))







	  ;;or          

	  (pattern-rule `(or)

			(lambda () (parse #f)))



	  (pattern-rule `(or  ,( ? 'test))

			(lambda (t) (parse t)))





	  (pattern-rule `(or . ,(? 'conditions list?))  ; list? is a "guard"

			(lambda (conditions) `(or ,(map parse conditions))))

					;and          

	  (pattern-rule `(and . ,(? 'conditions list?))  ; list? is a "guard"

			(lambda (condi) (parse (expand-and condi))))







					;lambda	          





	  (pattern-rule `(lambda ,(? 'argl argument-list?) . ,(? 'bodies)) ;argument-list?  is a "guard"

			(lambda (argl bodies)

			  (let ((bodies (parse (beginify bodies))))

			    (take-apart$ argl

					 (lambda () `(lambda-simple ,argl ,bodies))

					 (lambda (s a) `(lambda-opt ,s ,a ,bodies))

					 (lambda () `(lambda-var ,argl ,bodies))))))  













	  ;;let  



	  (pattern-rule `(let ,(? 'pairs ) . ,(? 'bodies))

			(lambda ( a b)   (expand-let  (cons a b))))



	  ;;let*

	  (pattern-rule `(let* () ,(? 'expr) . ,(? 'exprs list?))

			(lambda (expr exprs) 

			  (if (eq? exprs '() ) (parse `((lambda () ,expr)))

			      (parse `((lambda () ,(beginify (cons expr exprs))))))))





	  (pattern-rule `(let* ((,(? 'var var?) ,(? 'val)) . ,(? 'rest)) . ,(? 'exprs))

			(lambda (var val rest exprs) 

			  (if (null? rest) 

			      (parse `(let ((,var ,val)) ,@exprs)) 

			      (parse `(let ((,var ,val)) (let* ,rest . ,exprs))))))

	  

					;letrec     

	  (pattern-rule `(letrec . ,(? 'rest list?))

			(lambda (lst)

			  (parse (expand-letrec `(,@lst)))))

	  

	  

	  

	  



	  ;;define



	  (pattern-rule `(define ,(? 'def var?) . ,(? 'value)) ;var?  is a "guard"

			(lambda (defin value)

			  `(def ,(parse defin) ,(parse (beginify value)))))

					;<MIT style define>

	  (pattern-rule `(define (,(? 'def var?) . ,(? 'arglist)) . ,(? 'body)) ; var?  is a "guard"

			(lambda (defin arglist body)

			  (parse `(define ,defin (lambda(,@arglist) ,@body)       ))))

	  

	  

					;set!

	  (pattern-rule `(set! ,(? 'ass var?), (? 'val)) ;var?  is a "guard"

			(lambda (assi valu)

			  `(set ,(parse assi) ,(parse valu))))



	  

	  

	  ;;applications

	  (pattern-rule `( ,(? 'app not-reserved) . ,(? 'varlist))

			(lambda (b v)  `(applic ,(parse b) ,(map parse v))))





					;QQ

	  (pattern-rule `(quasiquote . ,(? 'lst) )

			(lambda (lst) (parse (expand-qq (car lst)))))







	  ;;sequance

	  (pattern-rule `(begin)

			(lambda () `(const ,*void-object*)))



	  (pattern-rule `(begin  ,(? 'beg))

			(lambda (beg) (parse beg)))



	  (pattern-rule `(begin . ,(? 'seq list?))  ;list? is a "guard"

			(lambda (seq) `(seq ,(map parse (deleteBegin seq)))))



	  

	  

	  



	  









	  )))

    (lambda (sexpr)

      (run sexpr (lambda () '(this is what happens when the tag

				   parser fails to match the input))))))



;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

;\\\\\\\\\\\\\\\\\\\\\\\\\ASSIGNMENT 3\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\





;\\\\\\\\\\\\\\\\\\\\\\\\\QUESTION 3\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\



(define pickLambda

  (lambda (parse-exp ret-ds+es)

    (if (null? parse-exp) (ret-ds+es '() '())

        (pickLambda (cdr parse-exp)

		    (lambda (ds es)

		      (cond ((eq? (caar parse-exp) 'def) 

			     (ret-ds+es (cons (cdar parse-exp) ds) es))

			    ((eq? (caar parse-exp) 'seq)

			     (pickLambda (cadar parse-exp)

					 (lambda (ds1 es1)(ret-ds+es

							   (append ds1 ds)

							   (append es1 es)))))

			    (else  (ret-ds+es ds (cons (car parse-exp) es)))))))))





(define eliminate-nested-defines

 (lambda (pexp)	  



   (letrec (

             (run0 (lambda (pe) (if (not(lambda? pe)) (if (list? pe) (map run pe) pe) (run pe))))

	     (run (lambda (pes) 

               

		(cond ((null? pes) '())

                      

                      ((not (list? pes)) pes)

		      

		      ((list? (car pes))  `( ,(run (car pes)) ,@(run (cdr pes))))

		      

		      ((or (eq? (car pes) 'seq) (eq? (car pes) 'def)) 

		       (help   (pickLambda (list pes) (lambda (def body) (run def))) (pickLambda (list pes) (lambda (def body) (run body))) pes ))



		      (else `(,(car pes) ,@(run (cdr pes)))) )))

	     

	     (help (lambda (def body pes)

                                (if (null? def)

                                     `(,(car pes) ,@(run (cdr pes)))

                                    (lambda-sim def body))))

                

	     

	     (lambda-sim

	      (lambda (def body)

		(let ((names `(,(map cadar def)))

		      (sequenceList `(seq ,`( ,@(map (lambda (e) `(set   ,@e)) def) ,@body)))

		      (falses-app (make-list (length def) (list 'const #f))))

 		  `(applic ,`(lambda-simple ,@names  ,@`(,sequenceList)) ,falses-app))))

		  )

   

   (run0  pexp)  )))



      

;\\\\\\\\\\\\\\\\\\\\\\\\\QUESTION 4\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\



(define remove-applic-lambda-nil

  (lambda (expr)

    (cond ((or (null? expr) (not(list? expr)))  expr)

	  ((and (eq? (car expr) 'applic) (eq? (caadr expr) 'lambda-simple) (eq? (cadadr expr) '() ) (eq? (caddr expr) '() ))    (car (remove-applic-lambda-nil (cddadr  expr)) ))

	  (else (cons (remove-applic-lambda-nil (car expr)) (remove-applic-lambda-nil (cdr expr) ))))))







	  

;\\\\\\\\\\\\\\\\\\\\\\\\\QUESTION 5\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\





(define lam-sim? (lambda(lst) (and (list? lst) (not (null? lst)) (equal? (car lst) 'lambda-simple))))

(define lam-opt? (lambda(lst) (and (list? lst) (not (null? lst)) (equal? (car lst) 'lambda-opt))))

(define lam-var? (lambda(lst) (and (list? lst) (not (null? lst)) (equal? (car lst) 'lambda-var))))





          

          

(define bound?

  (lambda(expr varName1)

    (letrec ((varNam varName1)

        (run11 (lambda (expr) (ormap run22 expr)))

        (run22 (lambda (lstt) (if (or (null? lstt) (not (list? lstt))) #f   ;;reshima lo reyka lesader  

                        (cond ((lam-sim? lstt)

                            (if (member varNam (cadr lstt)) #f

                                (if (inside-var? (cddr lstt) varNam) #t

                                    (run11 (cddr lstt)))))

                                ((lam-opt? lstt)

                            (if (or (member varNam (cadr lstt)) (equal? varNam (caddr lstt))) #f

                                (if (inside-var? (cdddr lstt) varNam) #t

                                    (run11 (cdddr lstt)))))

                                ((lam-var? lstt)

                            (if (equal? (cadr lstt) varNam) #f

                                (if (inside-var? (cddr lstt) varNam) #t

                                    (run11 (cddr lstt)))))

                                    (else (run11 lstt)))))))

     (run11 expr))))

          

          



                              

  

(define insideSetMap? 

 (lambda (expr nameVar)

    (letrec (

            (run1 (lambda (expr nameVar) (if (not(list? expr)) #f (run2 expr nameVar))))

            

            

            (run2 (lambda (expr nameVar) 

             (if (and (list? expr) (not (null? expr)) (equal? (car expr) 'set) (not (null? (cdr expr))) (equal? (cadr expr) `(var ,nameVar))) 

             #t

             (or (ormap (lambda(ex) (and (list? ex) (not (null? ex)) (equal? (car ex) 'set) (not (null? (cdr ex))) (equal? (cadr ex) `(var ,nameVar)))) expr) 

                 (ormap (lambda(ex) (if (and (list? ex) (not(null? ex)) (lambda? ex))

                                    #f 

                                    (run1 ex nameVar))) expr))))))

    (run1 expr nameVar))))



            



          

(define needBoxSet?

    (lambda(expr varName1)

        (letrec ((varNam varName1)

                (run1 (lambda (pe) (or (insideSetMap? pe varNam) (run2 pe))))

                (run2 (lambda (pe) (ormap run3 pe)))

                (run3 (lambda(ls) (if  (not (list? ls)) 

                                    #f 

                                    (cond ((lam-sim? ls) (if (member varNam (cadr ls)) #f

                                            (if (insideSetMap? (cddr ls) varNam) #t

                                                (run1 (cddr ls)))))

                                        ((lam-opt? ls)

                                        (if (or (member varNam (cadr ls)) (equal? varNam (caddr ls))) #f

                                            (if (insideSetMap? (cdddr ls) varNam) #t

                                                (run1 (cdddr ls)))))

                                        ((lam-var? ls)

                                        (if (equal? (cadr ls) varNam) #f

                                            (if (insideSetMap? (cddr ls) varNam) #t

                                                (run1 (cddr ls)))))

                                                (else (run1 ls)))))))

        (run1 expr))))

    

    

    

                                                                                                                                                                                                                                

                              

(define insideSetVar?

(letrec ((run (lambda (pe varName)   

            (if (not (list? pe)) #f

             (or (ormap (lambda(expr)  (and (list? expr) (not (null? expr)) (equal? (car expr) 'set) (not (null? (cdr expr))) (inside-var? (cadr (cdr expr)) varName)))  pe)

             (ormap (lambda(expr) (if (and (list? expr) (not (null? expr)) (member (car expr) `(lambda-simple lambda-var lambda-opt)) (member varName (getBoxParam expr))) #f (run expr varName))) pe))))))

              

              

  (lambda (lst varName) (run lst varName))))

  

                                

(define needBoxRead?

        (lambda (pe varName)

            (letrec ((run (lambda (expr var) (or (and (inside-var? expr var) (not (insideSetMap? expr var))) (insideSetVar? expr var) (run1 expr var))))

                     (run1 (lambda (expr var) (if (sett? expr) (run (caddr expr) var) (run2 expr var))))

                     (run2 (lambda (expr var) (ormap (lambda(lst) (if (and (list? lst) (not (null? lst)) (not (null? (cdr lst))))  

                                            (cond ((lambda? lst)

                                                (cond ((and (inside-var? (getBoxBody lst) var) (not (insideSetMap? (getBoxBody lst) var)))  #t)

                                                      ((insideSetVar? (getBoxBody lst) var)  #t)

                                                      (else (run (getBoxBody lst) var))))

                                               (else(run lst var))) #f))

                                expr))))

                            (run pe varName))))



(define shouldBoxPar?

    (lambda(pe varName)

        (not(or (not(bound? pe varName))

                (not (needBoxSet? pe varName))

                (not(needBoxRead? pe varName))))))

 

(define sett?

    (lambda (expr)

        (and (list? expr)(eq? (car expr) 'set))))





                      

(define boxGet-Set

     (lambda (pars body)

         (letrec ((run (lambda (pars body)

                 (if (null? pars) body (boxGet body (car pars)))))  

              (boxGet (lambda (box-body nameVar)

                 (cond ((or (not (list? box-body)) (null? box-body)) box-body)

                                       ((set? box-body nameVar) `(box-set (var ,nameVar) ,(boxGet (caddr box-body) nameVar)))

                                       ((equal? box-body `(var ,nameVar)) `(box-get (var ,nameVar)))

                                       ((lam-sim? box-body)

                                                 (if (varMem nameVar box-body) box-body 

                                                     `(lambda-simple , (cadr box-body) ,(boxGet (caddr box-body) nameVar))))

                                       ((lam-opt? box-body)

                                                 (if (or (varMem nameVar box-body) (equal? nameVar (caddr box-body))) box-body

                                                     `(lambda-opt ,(cadr box-body) ,(caddr box-body) ,(boxGet (cadddr box-body) nameVar))))

                                     ((lam-var? box-body)

                                                 (if (equal? (cadr box-body) nameVar) box-body

                                                  `(lambda-var ,(cadr box-body) ,(boxGet (caddr box-body) nameVar))))

                                       (else (map2pars boxGet box-body nameVar))))))

                                      

                                       

                        (run (cdr pars) (boxGet body (car pars))))))

                       

 (define map2pars

     (lambda (func lst name) 

         (cond ((null? lst) lst)

         (else (cons (func (car lst) name) (map2pars func (cdr lst) name))))

     ))

     


            
                        

                        

 (define set? 

 (lambda (lambdaBody varName)

 (and (list? lambdaBody)(equal? (car lambdaBody) 'set) (not (null? (cdr lambdaBody))) (equal? (cadr lambdaBody) `(var ,varName)) )))

       

(define varMem

(lambda (var body) (member var (cadr body))))

      

(define delSeq 

    (lambda(expr)

        (cond ((null? expr) expr)

              ((null? (cdr expr)) (car expr))

              (else `(seq ,(buildSeq  expr '()))))))



           

(define buildSeq 

(lambda ( lst exp)

(fold-left (lambda(accum rest) 

                (if (and (list? rest) (not (null? rest)) (equal? (car rest) 'seq)) (append accum (cadr rest)) ;maybe quasi

                    (append accum `(,rest) )))  exp lst)))      

      

      



                                      							

                

(define doBox 

    (lambda (lst)

      (if (and (list? lst) (not (null? lst)) (member (car lst) '(lambda-simple lambda-var lambda-opt)))

      

      (let* (

           (body (getBoxBody lst))            

           (pars (getBoxParam lst))               

           (shoudBox (filter (lambda(par) (shouldBoxPar? body par)) pars))

           (afterBox (map (lambda (par)  `(set (var ,par) (box (var ,par)))) shoudBox))) ;need to change

            (if (null? shoudBox) lst

                (build lst pars body shoudBox afterBox)

           ))

           lst)))

           

(define build 

(lambda (lst pars body shoudBox afterBox)

(cond ((lam-sim? lst)

           `(lambda-simple ,pars  ,(buildHelpr body shoudBox afterBox)))

                ((lam-opt? lst)

            `(lambda-opt ,(cadr lst) ,(caddr lst) ,(buildHelpr body shoudBox afterBox)))

                ((lam-var? lst)

            `(lambda-var ,(cadr  lst) ,(buildHelpr body shoudBox afterBox))))))

    

(define buildHelpr 

    (lambda (body shoudBox afterBox)

     (delSeq `(,@afterBox ,@(boxGet-Set shoudBox body )))))

           

           

(define getBoxParam

    (lambda (lamda) (cond

                        ((lam-sim? lamda) (cadr lamda))

                        ((lam-var? lamda) (list (cadr lamda)))

                        ((lam-opt? lamda) (cons (caddr lamda) (cadr lamda)))

                        )))	  

	  

	  

(define getBoxBody 

    (lambda (lamda) (cond

                        ((or (lam-var? lamda) (lam-sim? lamda)) (cddr lamda))

                        

                        ((lam-opt? lamda)    (cdddr lamda))

                        )))



           

           

(define inside-map?

    (letrec (

             (run (lambda (pe op)

                (cond ((not (list? pe)) (op pe))

                        (else (op(help pe op))))))

             (help  (lambda (expr op) (map (lambda(exprs) (run exprs op)) expr)))        

                        

                        )

(lambda (pe op) (run pe op) )))



            

(define inside-var? 

    (letrec (

                (run (lambda (pes varName)

                                    (cond 

                                        ((list? pes) (or 

                                                        (member `(var ,varName) pes) (equal? `(var ,varName) pes)

                                                        (help pes varName)))

                                        (else #f))))

                (help (lambda (pees nameVar) (ormap (lambda (expr) (if (and (list? expr) (not (null? expr)) (lambda? expr))  #f 

                                    (run expr nameVar))) pees))))

(lambda (pe var) (run pe var))))





(define lambda? (lambda (exp)

                    (and (list? exp) (or (lam-sim? exp) (lam-var? exp) (lam-opt? exp)))))



       

(define box-set

    (lambda(lst)  

      (inside-map? lst doBox)))

      











;\\\\\\\\\\\\\\\\\\\\\\\\\QUESTION 6\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\







(define pe->lex-pe

  (letrec((run (lambda (pe db env)

		 (cond 

		  ((null? pe) '())

		  

		  ((list? (car pe)) (cons (run (car pe) db env) (run (cdr pe) db env)))

		  

		  ((eq? 'var (car pe))  (search-in-rib (cadr pe) db (lambda (mino) `(pvar ,(cadr pe) ,mino))

							  (lambda ()

							    (search-in-ribs (cadr pe) env (lambda (maj mino) `(bvar ,(cadr pe) ,maj ,mino ) )

									    (lambda() `(fvar ,(cadr pe)))))))

		  

		  ((eq? (car pe) 'lambda-simple) (with pe (lambda (x argl body) `(lambda-simple ,argl ,(run body argl (cons db env))))))

		  

		  ((eq? (car pe) 'lambda-var) `(lambda-var ,(cadr pe) ,(run (caddr pe) (list (cadr pe)) (cons db env))))

                  

		  ((eq? (car pe) 'lambda-opt) (with pe  (lambda(x argl opt body) `(lambda-opt ,argl ,opt ,(run body `(,@argl ,opt) (cons db env))))))

		  

		  (else (cons (car pe) (run (cdr pe) db env)))))))

    (lambda (pe) (run pe '() '() ))))







    

(define search-in-rib

  (lambda (cadrPe parmList ret-mino ret-nf)

    (begin 

      (cond ((null? parmList) (ret-nf))

            ((eq? (car parmList) cadrPe) (ret-mino 0))

            (else (search-in-rib cadrPe (cdr parmList) (lambda (mino) (ret-mino (add1 mino))) ret-nf))))))

 

(define search-in-ribs

  (lambda (cadrPe env ret-maj+min ret-nf)

    (if (null? env) (ret-nf)

        (search-in-rib cadrPe   (car env)

                       (lambda (mino) (ret-maj+min 0 mino))

                       (lambda () (search-in-ribs cadrPe (cdr env)

                                                  (lambda (maj mino) (ret-maj+min (add1 maj) mino))

                                                  ret-nf))))))









;\\\\\\\\\\\\\\\\\\\\\\\\\QUESTION 7\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\







(define annotate-tc

  (letrec ((annotate (lambda (pe tp?)

      (cond 

      ((or (eq? (car pe) 'fvar) (eq? (car pe) 'pvar) (eq? (car pe) 'bvar) )  pe)

      

      ((eq? (car pe) 'const) pe)

      

      ((eq? (car pe) 'or)   `(or ,(last (cadr pe) tp?)))

      

      ((eq? (car pe) 'seq)  `(seq ,(last (cadr pe) tp?)))

      

      ((eq? (car pe) 'if3)

        (let* ((test (cadr pe))

               (doIfTrue (caddr pe))

               (doIfFalse (cadddr pe)))

                `(if3 ,(annotate test #f) ,(annotate doIfTrue tp?) ,(annotate doIfFalse tp?))))

      ((eq? (car pe) 'def) `(def ,(cadr pe) ,(annotate (caddr pe) #f)))

      

      ((eq? (car pe) 'lambda-simple) `(lambda-simple ,(cadr pe) ,(annotate (caddr pe) #t)))

		  

      ((eq? (car pe) 'lambda-var) `(lambda-var ,(cadr pe) ,(annotate (caddr pe) #t)))

                  

      ((eq? (car pe) 'lambda-opt) `(lambda-opt ,(cadr pe) ,(caddr pe) ,(annotate (cadddr pe) #t)))

      

      ((eq? (car pe) 'applic) (if tp? `(tc-applic,(annotate (cadr pe) #f) ,(map (lambda (x) (annotate x #f)) (caddr pe) ))

                                      `(applic ,(annotate (cadr pe) #f) ,(map (lambda (x) (annotate x #f)) (caddr pe) ))))

      

      ((or (eq? (car pe) 'set) (eq? (car pe) 'box-set)) `(,(car pe) ,(cadr pe) ,(annotate (caddr pe) #f)))

      

      ((eq? (car pe) 'box-get) pe)

            

      ((eq? (car pe) 'box) pe)

      

                                      

        (else `(Wrong Parsed Expression)

      

      ))))

      

      (last (lambda (pe tp?)    

      

      (begin

		(if (null? (cdr pe)) 

		(cons (annotate (car pe) tp?) '())

                (cons (annotate (car pe) #f) (last (cdr pe) tp?))))))

                

      (rev (lambda (lst)

      (if (not (null? (cdr lst))) (reverse(cdr (reverse lst))) lst)))

                

                

                )

      

      

      (lambda (pe) (annotate pe #f))))





;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

;\\\\\\\\\\\\\\\\\\\END OF ASSIGNMENT 3\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\			   



;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\HELPER FUNCTION FOR BEGIN PATTERN\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\



(define deleteBegin

  (lambda (lst)

    (cond

     ((null? lst) '())

     ((and (list? (car lst)) (equal? 'begin (caar lst))) (deleteBegin `( ,@(cdar lst) ,@(cdr lst))))

     (else (cons (car lst) (deleteBegin (cdr lst)))))))



				





;;;;;;;;;;;;;;;;;;;;;;;;;;;;expand-macros;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;expand and

(define expand-and

  (letrec ((loop (lambda (carList cdrList)

		   (if (null? cdrList)

		       carList

		       `(if ,carList ,(loop (car cdrList) (cdr cdrList)) #f)))))

    (lambda (expList) 

      (if (null? expList)

	  #t

	  (loop (car expList) (cdr expList))))))



;;expand cond

(define expand-cond 

  (lambda (cList)

    (let* (

	   (firstTest (caar cList))

	   (doIfTrueCond  (if (null? (cdr (car cList))) firstTest (beginify (cdr (car cList)))))

	   (rest   (cdr cList)))



      (if (okList? rest)

	  `(if ,firstTest ,doIfTrueCond ,(expand-cond rest))

	  (if (eq? 'else firstTest) 

	      doIfTrueCond

	      `(if ,firstTest ,doIfTrueCond))))))







(define expand-let

  (lambda (allLet)

    (let* 	((pairs (car allLet)) 

		 (body 	(cdr allLet)) 

		 (vars 	(map car pairs)) 

		 (vals 	(map cadr pairs)))

      `,(parse `((lambda ,vars ,@(list (beginify body))) ,@vals)))))









(define expand-letrec

  (lambda (letrec-exp)

    (let* 	((pairs (car letrec-exp)) 

		 (body 	(cdr letrec-exp)) 

		 (vars 	(map car pairs))  

		 (vals 	(map cadr pairs))) 



      (cond 

       ((null? vals) `(let() ((lambda() ,@body))))

       

       (else 

	`(let(,@(map (lambda(var) (list var #f)) vars))

	   ,@(map (lambda(var val) `(set! ,var ,val)) vars vals) ((lambda () ,@body )))

	)))))



	

	

	

	

	

	

	

	

	

	

	

	

	

	

	

	

	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;FINALPROJECT START HERE!!!!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define void?

    (lambda(lst) (equal? lst *void-object*)))



(define const?

(lambda (lst)

    (equal? (car lst) 'const)))



(define or?

(lambda (lst)

    (equal? (car lst) 'or)))



(define seq?)

(lambda (lst)

    (equal? (car lst) 'seq))

    

(define def?

(lambda (lst)

    (equal? (car lst) 'def)))



(define isSet?

(lambda (lst)

    (equal? (car lst) 'set)))



(define box-set?

(lambda (lst)

    (equal? (car lst) 'box-set)))



(define box?

(lambda (lst)

(equal? (car lst) 'box)))



(define if3?

(lambda (lst)

    (equal? (car lst) 'if3)))



(define if3test (lambda (pe) (cadr pe)))



(define doIfTrue (lambda (pe) (caddr pe)))



(define doIfFalse (lambda (pe) (cadddr pe)))









(define header1 

"

#include <stdio.h>

#include <stdlib.h>



#define DO_SHOW 1



#include \"cisc.h\"



int main()

{

  START_MACHINE;



  JUMP(CONTINUE);



#include \"char.lib\"

#include \"io.lib\"

#include \"math.lib\"

#include \"string.lib\"

#include \"system.lib\"

#include \"scheme.lib\"

"

)



(define prim

(lambda() 

(string-append

"   #define SOB_VOID " (number->string(lookUpConstanTable *void-object*))

"\n #define SOB_NIL " (number->string(lookUpConstanTable '()))

"\n #define SOB_FALSE " (number->string(lookUpConstanTable #f))

"\n #define SOB_TRUE " (number->string(lookUpConstanTable #t))


"\n #include \"primitives.asm\"

"

(str->sym)
)))







(define header2 



"

 CONTINUE:\n"

 )

 

 (define footer



(string-append


"JUMP(FINISH_ALL);\n"



"L_ERROR_NOT_A_CLOS:\n"

"SHOW(\"L_ERROR_NOT_A_CLOS\",R0);\n"

"JUMP(FINISH_ALL);\n"



"L_ERROR_UNBOUND_VAR:\n"

"SHOW(\"L_ERROR_UNBOUND_VAR\",R0);\n"

"JUMP(FINISH_ALL);\n



L_ERROR_NOT_FRACTION:\n

SHOW(\"L_ERROR_NOT_FRACTION\",R0);\n

JUMP(FINISH_ALL);\n



L_ERROR_INCORRECT_INDEX:\n

SHOW(\"L_ERROR_INCORRECT_INDEX\",R0);\n

JUMP(FINISH_ALL);\n



L_ERROR_INCORRECT_TYPE:\n

SHOW(\"L_ERROR_INCORRECT_TYPE\",R0);\n

JUMP(FINISH_ALL);\n



L_ERROR_NOT_AN_INT:\n

SHOW(\"L_ERROR_NOT_AN_INT\",R0);\n

JUMP(FINISH_ALL);\n



L_ERROR_NOT_A_BOOLEAN:\n

SHOW(\"L_ERROR_NOT_A_BOOLEAN\",R0);\n

JUMP(FINISH_ALL);\n



L_ERROR_INCORRECT_NUM_OF_ARGS:\n

MOV(R0,FPARG(1));\n

SHOW(\"L_ERROR_INCORRECT_NUM_OF_ARGS\",R0);\n

MOV(R0,SOB_VOID);

JUMP(FINISH_ALL);\n



L_ERROR_NOT_A_PAIR:\n

SHOW(\"L_ERROR_NOT_A_PAIR\",R0);\n

JUMP(FINISH_ALL);\n



L_SECOND_ARG_CANT_BE_ZERO:\n

SHOW(\"L_SECOND_ARG_CANT_BE_ZERO\",R0);\n

JUMP(FINISH_ALL);\n



L_ERROR_NOT_NUMBER22:\n

SHOW(\"L_ERROR_NOT_NUMBER22\",R0);\n

JUMP(FINISH_ALL);\n


L_ERROR_NOT_NUMBER1:\n

SHOW(\"L_ERROR_NOT_NUMBER1\",R0);\n

JUMP(FINISH_ALL);\n


L_ERROR_INCORRECT_TYPE_IN_REM:\n

SHOW(\"L_ERROR_INCORRECT_TYPE_IN_REM\",R0);\n

JUMP(FINISH_ALL);\n







FINISH_ALL:

CMP(R0,SOB_VOID);

JUMP_EQ(L_STOP_MACHINE);



L_STOP_MACHINE:

STOP_MACHINE;

return 0;

}"

)

)





 

;;  (define g

;;     (lambda (code)

;;         (string-append

;;             header1

;;             (prim)

;;             header2

;;             

;;             code"\n"

;;             footer)))



(define applyAll

    (lambda (lst)

       ;(annotate-tc

            (pe->lex-pe

                (box-set

                (remove-applic-lambda-nil

                    (eliminate-nested-defines lst))))));)

                                    

                





                



 (define listOfExps

  (lambda (parser string)

    (parser (string->list string)

	    (lambda (e s)

	      (append (list e)

		(listOfExps <sexpr> (list->string s))))

	    (lambda (w) `()))))   

    
(define ^^label
  (lambda (name)
    (let ((n 0))
      (lambda ()
	(set! n (+ n 1))           
                
                
	(string-append name
		       (number->string n))))))
            



(define label-shai-exit (^^label "L__shai_exit_"))

            
(define shai (lambda () (let ((exit-l (label-shai-exit)))
    
            (string-append 
            "CMP(R0,SOB_VOID);\n"
            "JUMP_EQ (" exit-l ");\n" 

            "PUSH(R0)\n;
            CALL(WRITE_SOB)\n;
            DROP(1)\n;
            PUSH(IMM('\\n'));\n
            CALL(PUTCHAR);\n
            DROP(1);\n"
            exit-l ":"
            ))))
            

            

            

   (define compile-scheme-file

  (lambda (fromScheme toCisc)
   ;(let ((out-port (open-output-file (symbol->string toCisc) 'truncate)))
   
  (let ((out-port (open-output-file toCisc 'truncate)))

        (display

        (string-append 

                header1

                (prim)

                header2

                    (printConstantTableToMemory)

                    (printFvarTableToMemory)

                    (writeNil) 

                    (printSymbolTableToMemory)

                    "ADD(IND(0)," (number->string (+ (length symbolTable) count 20)) ");"

                    (lstrings->string (mapOverPrim))

                    (let* (

                            (stringFromFile (string-append (file->string "schemeLib.scm") (file->string fromScheme)))
                            (lstOfExps (listOfExps <sexpr> stringFromFile))

                            (mapParsed (map parse lstOfExps))

                            (applyAllParsed (map applyAll mapParsed))

                            (createAllTabels (allFvarCain (allConstChain applyAllParsed)))

                            (symTab (findStringInConstantTable))

                            (mapCodeGen (fold-left (lambda (acc x)(string-append acc (code-gen x 0) (shai) )) "" applyAllParsed)))

                        mapCodeGen 

                        )                               
                    footer) 
       out-port)

        (close-output-port out-port))))

                

                

          

(define file->string

  (lambda (in-file)

    (let ((in-port (open-input-file in-file)))

      (letrec ((run

		(lambda ()

		  (let ((ch (read-char in-port)))

		    (if (eof-object? ch)

			(begin

			  (close-input-port in-port)

			  '())

			(cons ch (run)))))))

	(list->string (run))))))





	











(define ^^label

  (lambda (name)

    (let ((n 0))

      (lambda ()

	(set! n (+ n 1))           

	(string-append name

		       (number->string n))))))

		       

		       





(define nl (list->string (list #\newline)))

	       	       

(define primitiveConst

`(,*void-object* #t #f () ))



(define constlist

'())





;;this function recives a List and retrive this list in a topologic order to the @constlist.

(define topoSort 

    (lambda (lst1)
    (set! constlist constlist)
    
    (letrec ((tmpLst '())

        (topologicSort 

            (lambda (lst)

                (cond ((null? lst) '())

                      ((or (list? lst) (pair? lst))

                            (if (list? (car lst))

                                (begin 

                                    (topologicSort (car lst))

                                    (set! tmpLst (cons (topologicSort (cdr lst)) tmpLst))

                                        lst)

                                (begin 

                                    (if (symbol? (car lst))
                                    (set! tmpLst (cons  (car lst) (cons (symbol->string (car lst)) tmpLst) ))
                                    
                                    (set! tmpLst (cons (car lst) tmpLst)))

                                    (set! tmpLst (cons (topologicSort (cdr lst)) tmpLst))

                                        lst)))

                    (else `(,lst ,@tmpLst))))))

            (begin (topologicSort lst1) (set! tmpLst (cons lst1 tmpLst)) (set! tmpLst  (reverse tmpLst)) (set! constlist `(,@constlist ,@ tmpLst)) ))))  


  (define cMap

    (lambda(proc lst)

        (fold-left

            (lambda(acc el)

                (append acc (list (proc el))))

            '()

            lst)))                      

                        

                        

                        

                        

                        

                        

(define findConst

    (lambda (pe) 

        (cond 

            ((null? pe) '())

            ((not (list? pe)) pe)
            

            ((and (list? pe) (equal? (car pe) 'const))  (cond 

                                                               ((symbol? (cadr pe))
                                            

                                                                    (begin (update-const-lst-vec (symbol->string (cadr pe))) (update-const-lst-vec (cadr pe))))

                                                               ((vector? (cadr pe))
                                                
                                                                    (begin  
                                                                    
                                                                    (cMap (lambda(x) (begin (findConst `(const ,x)))) (vector->list(cadr pe))) 

                                                                        (update-const-lst-vec  (cadr pe))))                                                                

                                                               ((list? (cadr pe))  
                                                 

                                                                    (begin (update-const-lst (topoSort (cadr pe))) (findConst (cdr pe))))

                                                               ((pair? (cadr pe))

                                                                        (begin (update-const-lst-vec(findConst `(const ,(car(cadr pe))))) 
                                                                                (update-const-lst-vec(findConst `(const ,(cdr(cadr pe))))) 
                                                                                (update-const-lst-vec (cadr pe))))



                                                            (else (update-const-lst (cadr pe)))))

                                                       

            (else (cMap findConst pe)))))

             

            

 (define update-const-lst (lambda(lst) (set! constlist (cons lst constlist))))

 

 (define update-const-lst-vec (lambda(lst) (set! constlist (reverse(cons lst constlist)))))





 

 (define count 100)

 

(define constanTable `())   

        

(define addToConstanTable

    (lambda (constList)

    

        (let* (

                (carConst  constList)

                (oldcount count)

                (rep   (cond

                            ((integer? carConst)        `(T_INTEGER ,carConst))

                            ((rational? carConst)       `(T_FRAC ,(numerator carConst) ,(denominator carConst)))

                            ((char? carConst)           `(T_CHAR ,(char->integer carConst)))

                            ((void? carConst)           '(T_VOID))

                            ((boolean? carConst)        `(T_BOOL ,(if carConst 1 0)))

                            ((null? carConst)           '(T_NIL))

                            ((pair? carConst)           `(T_PAIR ,(lookUpConstanTable (car carConst) ) ,(lookUpConstanTable (cdr carConst)) ))

                            ((vector? carConst)         `(T_VECTOR ,(vector-length carConst) ,@(cMap (lambda(el) (lookUpConstanTable el )) (vector->list carConst))))

                            ((symbol? carConst)         `(T_SYMBOL ,(lookUpConstanTable (symbol->string carConst) )))

                            ((string? carConst)         `(T_STRING ,(string-length carConst) ,@(map (lambda(x) (char->integer x)) (string->list carConst))))

                            (else 'YOU_R_A_DUMBASS))

         ))

             (begin (set! count (if (list? rep) (+ count (length rep))

                                                 (+ count 1)))

             (set! constanTable (append constanTable `((,oldcount ,carConst ,rep))))))))

             

         



 

(define (represent lst) (cadar lst))

(define (address lst) (caar lst))

(define lookUpConstanTable

    (lambda(elm )

        (letrec ((lookUp (lambda(table)

                            (cond 

                                ((null? table) 'something_isnt_right)

                                ((equal? (represent table) elm) (address table))

                                (else (lookUp (cdr table)))))))

                            (lookUp constanTable))))                      

                            

                            

                            

(define removeDup  

    (lambda (lst)

        (if (null? lst)

            '()

            (if (member (car lst) (cdr lst))  

                (removeDup (cdr lst)) 

                (cons (car lst)

                        (removeDup (cdr lst))))))) 





(define addMap

(lambda (lst func)

(cMap (lambda(x) (func x)) lst)))

        

(define allConstChain

    (lambda (pe)

                (begin  

                        (findConst pe)

                        (let* ((newl (reverse (removeDup (reverse `(,@primitiveConst ,@constlist))))))

                        (addMap newl addToConstanTable)

                        ))

                        pe))

(define printConstantTableToMemory

    (lambda()

        (table-to-mem constanTable ele-to-mem)))

;;;;;;;;;;fvar;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        



(define fvar-count 200)

 

(define fvarTable `()) 



(define primFvarlist '( cons car cdr not null? string? integer? boolean? char? symbol? pair? vector? procedure? 

                        numerator denominator string-length char->integer list + - * / < > =  zero? set-car! set-cdr! 

                        remainder apply map append integer->char vector-length 

                        number? rational? string-ref vector-ref string-set! vector-set! make-string make-vector 

                        vector symbol->string string->symbol eq? string-compare))



(define pirmFvarBody `("L_CONS" "L_CAR" "L_CDR" "L_NOT" "L_IS_SOB_NIL" "L_IS_SOB_STRING" "L_IS_SOB_INTEGER" "L_IS_SOB_BOOL" "L_IS_SOB_CHAR" "L_IS_SOB_SYMBOL" "L_IS_SOB_PAIR" "L_IS_VECTOR" "L_IS_SOB_CLOSURE"

                       "L_NUMERATOR" "L_DENOMINATOR" "L_STRING_LENGTH" "L_CHAR_TO_INTEGER" "L_LIST" "L_PLUS" "L_MINUS" "L_MUL" "L_DIV" "L_LT" "L_GT" "L_SHAVE" "L_IS_ZERO" "L_SET_CAR" "L_SET_CDR" "L_REMANIDER"

                       "L_APPLY" "L_MAP" "L_APPEND" "L_INTEGER_TO_CHAR" "L_VECTOR_LENGTH" "L_IS_NUMBER" "L_IS_RATIONAL" "L_STRING_REF" "L_VECTOR_REF" "L_STRING_SET" "L_VECTOR_SET" "L_MAKE_STRING" "L_MAKE_VECTOR" 

                       "L_VECTOR" "L_SYMBOL_TO_STRING" "L_STRING_TO_SYMBOL" "L_EQ" "L_STRING_COMPARE"))



(define fvarlist primFvarlist) 



(define update-fvar-lst (lambda(lst) (set! fvarlist (cons lst fvarlist))))







(define findFvar

    (lambda (pe)

        (cond 

            ((null? pe) '())

            ((not (list? pe)) pe) 

            ((and (list? pe) (equal? (car pe) 'fvar)) (update-fvar-lst (cadr pe)))

            (else (cMap findFvar pe)))))





(define addToFvarTable

    (lambda (fvarList)

        (let* ((carFvar (cond ((null? fvarList)        '())

                              ((not (list? fvarList)) fvarList)

                              (else                   (car fvarList))))

                (oldcount count))

                (begin (set! count (add1 count)) 

                       (set! fvarTable (append fvarTable `((,oldcount ,carFvar 0xDEF)))))))) 

                       

(define allFvarCain (lambda (pe) (begin (findFvar pe)

                                (let ((newl (removeDup fvarlist)))

                                 (addMap newl addToFvarTable))

                                 

                                 )

                                 pe))



                                 

                                 

(define alocateClosureToPrimitive

    (lambda (label funcName)(string-append

                               "PUSH(IMM(3));

                                CALL(MALLOC);

                                DROP(1);

                                MOV(INDD(R0, 0), IMM(T_CLOSURE));

                                MOV(INDD(R0, 1), IMM(0));

                                MOV(INDD(R0, 2), LABEL("label"));

                                MOV(IND("(number->string(lookUpFvarTable funcName)) "),R0);\n"

  )))

  

(define mapOverPrim

(lambda()

        (map (lambda (x y) (alocateClosureToPrimitive x y)) pirmFvarBody primFvarlist)))

        

        

(define printFvarTableToMemory

    (lambda()

        (table-to-mem fvarTable ele-to-mem)))

  

;;;;;;;;;;;; add constant table to memory ;;;;;;;;;;
    





;we use it to get the fvar table   ;call it w ele to mem

(define table-to-mem       ;;;maybe need to be run everything and then set constanTable to be the updated and the get it here :)

    (lambda (table func)

        (string-append "" (fold-left 

                            (lambda (acc ele) (string-append acc (func ele))) 

                            "" table))))   

        



(define ele-to-mem

    (lambda (ele)

        (let* ((add (car ele))    

                (i 0)

                (rep (caddr ele))

                )

            (if (list? rep)  

                      (string-append "MOV(IND(" (number->string add) "),"  (symbol->string (car rep))");\n "

                        (fold-left (lambda (acc el) (begin (set! i (+ i 1)) (string-append acc "MOV(IND(" (number->string (+ add i)) "),"  (number->string el)");\n ") )) "" (cdr rep)))

                      (string-append  "MOV(IND(" (number->string add) "),"  (symbol->string rep)");\n ")))))



                      



    

;;;;;;;;;;;; fvar table to memory





(define (frepresent lst) (cadar lst))

(define faddress (lambda (lst) (caar lst)))





(define lookUpFvarTable

    (lambda(elm)

        (letrec ((lookUp (lambda(table)

                            (cond 

                                ((null? table) "error_var_not_found")

                                ((equal? (frepresent table) elm) (faddress table))

                                (else (lookUp (cdr table)))))))

                            (lookUp fvarTable)))) 





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;CREATE-SYMBOL-TABLE;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                           
(define flag #t)
(define startSymTab 0)
(define symbolTable '())

(define findStringInConstantTable
    (lambda()
        (map 
            (lambda (x) (if (equal? (caaddr x) 'T_STRING)
                             (set! symbolTable `(,(car x) ,@symbolTable))
                             symbolTable))
            constanTable)))                         
                                               
                         

(define symAddressToMem
    (lambda (ele)
    (begin (set! count (+ count 2))
            (if flag (begin (set! startSymTab count) (set! flag #f)) (set! sta #f))
                (string-append  
                                "MOV(IND(" (number->string count) "),"  (number->string ele)");\n "
                                "MOV(IND(" (number->string (+ count 1)) "),"  (number->string (+ count 2))");\n " 
                                )
                    )))
                         
(define writeNil
(lambda ()
(begin 
    (string-append  "MOV(IND(" (number->string (+ count 2)) "),"  (number->string (lookUpConstanTable '()) )");\n " )))) 

                         
                         
                         
(define printSymbolTableToMemory 
    (lambda()
    (table-to-mem symbolTable symAddressToMem)
  ))
       
       
 (define str->sym
 (lambda()
(string-append
        "L_STRING_TO_SYMBOL:\n"
        "PUSH(FP);\n"
    "MOV(FP,SP);\n"
    "MOV(R1,"(number->string startSymTab) ");\n" ;in R1 adress of chain strings
    "MOV(R5, FPARG(2));\n"
    "MOV(R6, INDD(R1,0));\n"
    "L_STRING_TO_SYMBOL_BEGIN:\n"
    "CMP(INDD(R6,1),INDD(R5,1));\n" ;
    "JUMP_NE(L_STRING_TO_SYMBOL_RET_FALSE);\n"
    "MOV(R11, IMM(1));\n" ;
    "MOV(R13, INDD(R6,1));\n" ;
    "L_STRING_TO_SYMBOL_LOOP_CHAR:\n"
    "CMP(R11,R13);\n"
    "JUMP_GT(L_STRING_TO_SYMBOL_RET_TRUE);\n"
    "CMP(INDD(R6,1+R11),INDD(R5,1+R11));\n"
    "MOV(R15, INDD(R6,R11+1));\n"
    "MOV(R9, INDD(R5,R11+1));\n"
    "JUMP_NE(L_STRING_TO_SYMBOL_RET_FALSE);\n"
    "INCR(R11);\n"
    "JUMP(L_STRING_TO_SYMBOL_LOOP_CHAR);\n"
    "L_STRING_TO_SYMBOL_RET_FALSE:\n"
    "MOV(R11,0);\n"
    "JUMP(L_STRING_TO_SYMBOL_RETURN);\n"
    "L_STRING_TO_SYMBOL_RET_TRUE:\n"
    "MOV(R11,1);\n"
    "JUMP(L_STRING_TO_SYMBOL_RETURN);\n"

    "L_STRING_TO_SYMBOL_RETURN:\n"
    "CMP(R11,1);\n"
    "JUMP_EQ(L_STRING_TO_SYMBOL_MAKE_SYM);\n"
    "CMP(INDD(R1,1),IMM(0));\n"
    "JUMP_EQ(L_STRING_TO_SYMBOL_ADD);\n"
    "MOV(R1,INDD(R1,1));\n"
    "JUMP(L_STRING_TO_SYMBOL_BEGIN);\n"
    "L_STRING_TO_SYMBOL_ADD:\n"
    "PUSH(2);\n"
    "CALL(MALLOC);\n"
    "DROP(1);\n"
    "MOV(INDD(R0,0),FPARG(2));\n"
    "MOV(INDD(R0,1),IMM(0));\n"
    "MOV(INDD(R1,0),IND(R0));\n"
    "PUSH(FPARG(2));\n"
    "CALL(MAKE_SOB_SYMBOL);\n"
    "DROP(1);\n"
    "JUMP(L_STRING_TO_SYMBOL_EXIT);\n"
    "L_STRING_TO_SYMBOL_MAKE_SYM:\n"
    "PUSH(INDD(R1,0));\n"
    "CALL(MAKE_SOB_SYMBOL);\n"
    "DROP(1);\n"
    "L_STRING_TO_SYMBOL_EXIT:\n"
    "POP(FP);\n"
  "RETURN;\n"
    
        )))   

                            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;CODE-GEN-START-HERE;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define get-const-pos

  (lambda (c)

    (let	((pos (lookUpConstanTable c)))

      (if pos pos))))



(define code-gen-const

  (lambda (pe)

    

    (let ((c (list-ref pe 1))) ;c is the number 

      (string-append

       "MOV(R0," 

       (number->string(get-const-pos c)) ;;;;;;;;;

       ");/*code-gen-const*/" nl))))



(define code-gen-fvar

  (lambda (pe)

    (let	((pos (lookUpFvarTable (list-ref pe 1))) 

                                                        )

      (if (not (equal? pos "errorrrr_var_not_found"))

	  (string-append

	   "MOV(R0, IND("  (number->string pos) "));" nl

	   "CMP(R0, IMM(0));		/* check if unbounddddddddddddd var */" nl

	   "JUMP_EQ(L_ERROR_UNBOUND_VAR);" nl

	   )

	  (string-append

	   "	/* ERROR: variable " (symbol->string (cdr pe)) " is not bound! */" nl

	   "	JUMP(L_ERROR_UNBOUND_VAR);" nl)))))

  

  

(define get-pvar caddr)

(define code-gen-pvar 

  (lambda (pe)

    (let ((pos (get-pvar pe)))

      (string-append



        

       "	MOV(R0, FPARG(" (number->string (+ 2 pos)) "));" nl))))



(define get-bvar-level caddr)

(define get-bvar-pos cadddr)

(define code-gen-bvar

  (lambda (pe)

    (let ((mino (get-bvar-pos pe))

	  (maj (get-bvar-level pe)))

      (string-append 

       "	MOV(R0, FPARG(0));" nl

       "	MOV(R0, INDD(R0," (number->string  maj) "));" nl

       "	MOV(R0, INDD(R0," (number->string mino) "));" nl)))) 





(define ^label-or-is-true (^^label "L_Or_Is_True")) ;i think we can remove this line

(define ^label-or-exit (^^label "L_Or_Exit"))

(define code-gen-or

  (lambda (e maj)

    (let	((label-true (^label-or-is-true)) ;i think we can remove this line too

		 (label-exit (^label-or-exit)))

      (letrec	((foo			; could change to CPS

		  (lambda (or-args) ;(display " here ") (display or-args)

		    (if (null? or-args)

			(string-append

			 "	MOV(R0, IMM(SOB_FALSE));" nl

			 "	JUMP(" label-exit ");" nl

			 label-true ":" nl   ;i think we can remove this line too

			 label-exit ":" nl)

			(string-append

			 (code-gen (car or-args) maj) nl

			 "	CMP(R0, SOB_FALSE);" nl

			 "	JUMP_NE(" label-exit ");" nl

			 (foo (cdr or-args)))))))

	(foo (cadr e))))))

  

(define ^label-if3else (^^label "Lif3else"))



(define ^label-if3exit (^^label "Lif3exit"))



;; @mayer code

(define code-gen-if3

   (lambda (e maj)

     (with e

 	  (lambda (if3 test do-if-true do-if-false)

 	    (let ((code-test (code-gen test maj))

 		  (code-dit (code-gen do-if-true maj))

 		  (code-dif (code-gen do-if-false maj))

 		  (label-else (^label-if3else))

 		  (label-exit (^label-if3exit)))

 	      (string-append

 	       code-test nl ; when run, the result of the test will be in R0

 	       "CMP(R0, SOB_FALSE);" nl

 	       "JUMP_EQ(" label-else ");" nl

 	       code-dit nl

 	       "JUMP(" label-exit ");" nl

 	       label-else ":" nl

 	       code-dif nl

 	       label-exit ":"))))))

 	       



               

               

        

              

  (define code-gen-box

    (lambda(pexpr)



        (string-append

        "PUSH(1);\n"

        "CALL(MALLOC);\n"

        "DROP(1);\n"

        "MOV(IND(R0),FPARG(" (number->string (+ 2 (list-ref (list-ref pexpr 1) 2 ))) "));\n"

        "MOV(FPARG(" (number->string (+ 2 (list-ref (list-ref pexpr 1) 2 ))) "),R0);\n"

        )))

        

(define code-gen-box-get

    (lambda(pexpr maj)

        (string-append (code-gen (list-ref pexpr 1) maj)

        "MOV(R0,IND(R0));\n"

        )

        ))      

        

(define code-gen-box-set

    (lambda(pexpr maj)

        (string-append (code-gen (list-ref pexpr 2) maj)

        "MOV(R1,R0);\n"

        (code-gen (list-ref pexpr 1) maj)

        "MOV(IND(R0),R1);\n"

        "MOV(R0,SOB_VOID);\n"

        )

        ))             



               

                      

 	       

 	       

 	       



(define get-sequence cadr) 	       

(define code-gen-seq

  (lambda (pe maj)

    (lstrings->string

     (map (lambda(e) 

	    (string-append (code-gen e maj) nl)) 

	  (get-sequence pe)))))


	  
	  
;(define code-gen-seq2
 ;   (lambda(pe maj)
  ;     ((lambda(x) (fold-left string-append "" x))
   ;         (map code-gen (get-sequence pe) maj))))
            
            
;(define code-gen-seq3
 ;   (lambda(pe maj)
  ;     (fold-str (map2pars code-gen (get-sequence pe) maj))))
;(define fold-str
 ;   (lambda(x) (fold-left string-append "" x)))



(define lstrings->string 

  (lambda (l)

    (cond

     ((null? l) "")

     ((null? (cdr l)) (car l))

     (else (string-append (car l) (lstrings->string (cdr l))))

     )

    )

  )

(define get-define-def caddr)

	  

(define get-define-name cadr)	  

(define code-gen-def

  (lambda (pe maj)

    (string-append

     "	/* DEBUG */" nl

     "	/* DEBUG */" nl

     (code-gen (get-define-def pe) maj) nl

     "	/* value is now in R0 */" nl

     "	MOV(IND(" (number->string (lookUpFvarTable (cadr (get-define-name pe)))) "), R0); /* place value in mem coresponding with \"" (symbol->string (cadr (get-define-name pe))) "\" */" nl

     "	MOV(R0, IMM(SOB_VOID));		/* leave no tracks, like a ninja! */" nl nl)))

            





            

            

            

            

            

            

            

(define get-applic-func cadr)

(define get-applic-args caddr)            

(define ^label-tc-stack-fix-loop (^^label "L_TC_Stack_Fix_Loop"))

(define ^label-tc-stack-fix-exit (^^label "L_TC_Stack_Fix_Exit"))



(define code-gen-tc-applic

  (lambda (pe maj)

    (let ((proc (get-applic-func pe))

	  (args (get-applic-args pe))

	  (args-num (length (get-applic-args pe)))

	  (label-stack-fix-loop (^label-tc-stack-fix-loop))

	  (label-stack-fix-exit (^label-tc-stack-fix-exit)))

      (string-append

       "	POP(FP);	/* retrive old FP */" nl

       (lstrings->string						; for calculating the arguments 

	(map (lambda(e) 

	       (string-append 

		(code-gen e maj) nl

		"	PUSH(R0);" nl ))		; and pushing them to the stack

	     (reverse args)))				; the reverse way

       "	PUSH(" (cg-num args-num) ");" nl 	; then pushing number of arguments

       (code-gen proc maj) nl							; relevant closure will be in R0

       "	MOV(R1, R0); " nl

       "	PUSH(R0); " nl

       "	CALL(L_IS_SOB_CLOSURE); " nl

       "	DROP(1); " nl

       "	CMP(R0,IMM(0)); " nl

       "	JUMP_EQ(L_ERROR_NOT_A_CLOS);" nl nl

       

       "	/* DEBUG */" nl

       ;"	/* SHOW(\"closure is in\", R1) */" nl

       "	/*PRINT_MEM(R1-9,R1+5)*/" nl

       "	/* DEBUG */" nl nl

       

       "	PUSH(INDD(R1, 1));" nl 				; push env

       "	PUSH(STARG(" (number->string (+ 1 args-num)) "));	/* copy old-ret from argnum+1 deep to top of stack */" nl nl

       

       "	/* now we need to fix the stack */" nl

       "	MOV(R2, STARG(" (number->string (+ 4 args-num)) "));		/* R2 <- old-argnum */" nl

       "	ADD(R2, IMM(3));		/* take into account argnum, env & ret */" nl

       "	MOV(R3, R2);			/* R3 <- buttom pointer (copy-to pointer) */" nl

       "	MOV(R4, " (cg-num (+ 1 args-num)) ");		/* R4 <- new-argnum + 1 top-pointer (copy-from pointer) */" nl

       "	ADD(R3, R4);			/* R3 <- old-argnum + new-argnum + 4 */" nl

       "	/* everyting is ready, now lets fix it */" nl nl

       

       label-stack-fix-loop ":" nl

       "	CMP(R4, IMM(-2));		/* stop when copy-from pointer is bellow -1 (STARG(-1)=ret=last thing to reposition) */" nl

       "	JUMP_EQ(" label-stack-fix-exit ");" nl

       "	MOV(STARG(R3), STARG(R4));		/* copy from R4 to R3 */" nl

       "	DECR(R3);" nl

       "	DECR(R4);" nl

       "	JUMP(" label-stack-fix-loop ");" nl

       label-stack-fix-exit ":" nl nl

       

       "	/* repositioned everything on stack, need to fix SP */" nl

       "	SUB(SP, R2);" nl nl

       

       "	/* now we can \"call\" the proc */" nl

       "	JUMPA(INDD(R1, 2));"

       )

      )

    )

  )



(define cg-num

  (lambda (n)

    (string-append "IMM("  (number->string n) ")")

    )

  )





(define code-gen-applic

  (lambda (pe maj)

    (let ((proc (get-applic-func pe)) 

	  (args (get-applic-args pe))

	  (args-num (length (get-applic-args pe))))  ;;fparg[1]

      (string-append

              ;  "PUSH("(cg-num (lookUpConstanTable '()))");"nl		; nil, for lambda-opt and lambda-var  ;;;dont know about that

       (lstrings->string						; for calculating the arguments 

	(cMap (lambda(e) 

	       (string-append 

		(code-gen e maj) nl

		"PUSH(R0);/*code-gen-applic1*/" nl ))		; and pushing them to the stack

	     (reverse args)))

	     

                

       "	PUSH(IMM(" (number->string args-num ) "));/*code-gen-applic2*/" nl 

	; then pushing number of arguments

                (code-gen proc maj) nl							; relevant closure will be in R0


                "CMP(IND(R0),IMM(T_CLOSURE));\n"

           ;  

       "	JUMP_NE(L_ERROR_NOT_A_CLOS);" nl nl

       

       "	/* DEBUG */" nl


       "	/*PRINT_MEM(R1-9,R1+5)*/" nl

       "	/* DEBUG */" nl nl

       

          "	/* fffff */" nl nl

                

                

       "	PUSH(INDD(R0, 1));" nl				; push env

       "	CALLA(INDD(R0, 2));" nl				; and call label

       "	DROP(1);" nl

       "	POP(R1);" nl						; R1 <- num of args

       "	DROP(R1);" nl)						; updating the sp

      )

    )

  )

  

  

  



  

;;;;;;;;;;;;;;;shared for lambda;;;;;;;;;;;;;;;;;



(define allocate-memory-for-clos-env (lambda (maj)		

(string-append

"MOV(R1,FPARG(0)); /*env - copy the old env*/\n"

  		  	"PUSH(IMM(" (number->string maj) "));\n"

 			"CALL(MALLOC);\n"

  	    	"DROP(1);\n"

  		  	"MOV(R2,R0);\n"	;R2 <-the new env



    )))



(define create-label-lambda-loop-exit (^^label "lambda_loop_exit"))

(define create-label-lambda-loop (^^label "lambda_loop_"))



(define copy-vectors-args

	(lambda (major)

		(let* ((label-loop-start (create-label-lambda-loop))

			   (label-loop-exit (create-label-lambda-loop-exit)))

		(string-append



  			"MOV(R4,IMM(0)); /*i*/\n"

  			"MOV(R5,IMM(1)); /*j*/\n"


  			label-loop-start ":\n"

  			"CMP(R4,IMM("(number->string major) "));\n"

  			"JUMP_EQ("label-loop-exit");\n"

  			"MOV(INDD(R2, R5),INDD(R1, R4));\n"

  			"INCR(R5);\n"

  			"INCR(R4);\n"

 

  			"JUMP("label-loop-start");\n"

			label-loop-exit ":\n"


				))))







(define allocat-mem4-args-cloure

	(lambda ()

	(string-append

		 "MOV(R3,FPARG(1));\n /*n*/"


         "PUSH(IMM(R3));\n"

 	 	 "CALL(MALLOC);\n"

  	     "DROP(1);\n"

  	     "MOV(INDD(R2, 0),R0);\n")))









(define create-label-loop-args (^^label "loop_copy_args_"))

(define create-label-loop-args-exit (^^label "loop_copy_args_exit_"))





(define args-to-mem

	(lambda (major)

		(let* ((label-loop-start (create-label-loop-args))

					 (label-loop-exit (create-label-loop-args-exit)))

		(string-append 

  			"MOV(R4,IMM(0));/*i*/\n"

  			"MOV(R5,IMM(2));/*j*/\n"

  			"MOV(R6,IND(R2));\n"

  			;"SHOW(\"args-to-mem\",R0);\n"


  			label-loop-start ":\n"

  			"CMP(R4,FPARG(1));\n"

  			"JUMP_EQ("label-loop-exit");\n"

  			"MOV(INDD(R6, R4),(FPARG(IMM(R5))));\n"

  			"INCR(R5);\n"

  			"INCR(R4);\n"

  			;"SHOW(\" i \",R14);\n"

  			;"SHOW(\" j \",R15);\n"

  			"JUMP("label-loop-start");\n"

		    label-loop-exit ":\n\n"

		))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;lambda simple;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define create-label-simple-clos-exit (^^label "L_clos_simple_exit_"))

(define create-label-simple-clos-body (^^label "L_clos_simple_body_"))



(define lam-sim-clos-create

	(lambda (label-clos-body label-clos-exit expression major)

			(string-append

				label-clos-body ":\n"

				"PUSH(FP);\n"

				"MOV(FP, SP);\n"

				;"CMP(FPARG(1),IMM(3));"

				;"JUMP_NE(L_error_lambda_args_count);"

				(code-gen (cadr expression) major) ;extract body from lambda expression

              ;  "SHOW(\"THE NUM\",FP);\n"

              ;  "SHOW(\"THE NUM\",SP);\n"



				"POP(FP);\n"

				"RETURN;\n"

		 		label-clos-exit ":\n"

			)))





(define make-cisc-create-clos-lam-sim

	(lambda (expression major)

	(let  ((label-clos-body (create-label-simple-clos-body))

				 (label-clos-exit (create-label-simple-clos-exit)))

	(string-append

			"/*creating T_CLOSURE\n*/"

		;	"SHOW(\"befor creating T_CLOSURE\",R2);\n"

			 "PUSH(IMM(3));\n"

 			 "CALL(MALLOC);\n"

  		 	; "SHOW(\"MALLOC RETURNED \", R0);\n"

  		     "DROP(1);\n"

			 "MOV(INDD(R0,IMM(0)), T_CLOSURE);\n"

		     "MOV(INDD(R0,IMM(1)), R2);//ext. env\n"

		     "MOV(INDD(R0,IMM(2)),LABEL("label-clos-body"));\n"

		    ; "INFO;\n"

		     "JUMP("label-clos-exit");\n"

		     (lam-sim-clos-create label-clos-body label-clos-exit expression major)

		))))

		





(define make-cisc-lam-sim

	(lambda (expression major)

				(string-append 

					"/* lambda-simple */\n"

				;	"SHOW(\"make-cisc-lambda-simple \",R0);\n"


				 (allocate-memory-for-clos-env major) 

  				 (copy-vectors-args major)

  				 (allocat-mem4-args-cloure)

  				 (args-to-mem major)

  				 (make-cisc-create-clos-lam-sim expression major))

				))



(define code-gen-lam-sim (lambda (pe maj) (make-cisc-lam-sim (cdr pe) maj)))



				

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;lambda var;;;;;;;;;;;;;;;;







(define create-label-var-clos-exit (^^label "L_clos_var_exit_"))

(define create-label-var-clos-entry (^^label "L_clos_var_entry_"))



(define lam-var-clos-create

	(lambda (expression major)

				(string-append 

					"/* lambda-simple */\n"

				;	"SHOW(\"lam-var-clos-create \",R0);\n"

				 (allocate-memory-for-clos-env major) 

  				 (copy-vectors-args major)

  				 (allocat-mem4-args-cloure)

  				 (args-to-mem major)

                                 (make-cisc-create-clos-lam-var expression major))))



(define get-var-body cadr )



(define create-labal-var-clos-body-exit (^^label "L_clos_var_clos_body_exit_"))

(define create-labal-var-clos-body-entry (^^label "L_clos_var_clos_body_entry_"))



(define make-cisc-create-clos-lam-var

	(lambda (expression major)

	(let  ((var-fix-start (create-label-var-clos-entry))

               (var-fix-end (create-label-var-clos-exit))

               (label-clos-body (create-labal-var-clos-body-entry))

               (label-clos-exit (create-labal-var-clos-body-exit))

               )

                

                    

      (string-append

			"/*creating T_CLOSURE\n*/"

			;"SHOW(\"befor creating T_CLOSURE\",R2);\n"



                         

                         

                         

                         "/*creating T_CLOSURE\n*/"

			;"SHOW(\"befor creating T_CLOSURE\",R2);\n"

			 "PUSH(IMM(3));\n"

 			 "CALL(MALLOC);\n"

  		 	

  		     "DROP(1);\n"

			 "MOV(INDD(R0,IMM(0)), T_CLOSURE);\n"

		     "MOV(INDD(R0,IMM(1)), R2);//ext. env\n"

		     "MOV(INDD(R0,IMM(2)),LABEL("label-clos-body"));\n"


		     "JUMP("label-clos-exit");\n"

                        

                        label-clos-body ":" nl

                			

			"PUSH(FP);\n"

                         "MOV(FP, SP);\n"  

                         

                "MOV(R1,SOB_NIL);\n"


               "MOV(R15,FPARG(1));\n" ;number of args 15 1


               "MOV(R13,IMM(0));\n" ; i = 0  13 0

              "MOV(R14,IMM(1));\n"   

               "ADD(R14,R15);\n"  ; R4 next arg

               var-fix-start":\n"

               "CMP(R13,R15);\n"

               "JUMP_EQ("var-fix-end");\n"

               

               "PUSH(R1);\n"

               "PUSH(FPARG(R14));\n"

               "CALL(MAKE_SOB_PAIR);\n"



               "DROP(2);"

               "MOV(R1,R0);\n"

               "ADD(R13,IMM(1));\n"

               "SUB(R14,IMM(1));\n"

               "JUMP("var-fix-start");\n"

               var-fix-end":\n"

               "MOV(R5,FPARG(0));\n" ;R5 env

               "MOV(R6,FPARG(-1));\n" ; R6 ret

              "MOV(R7,FPARG(-2));\n" ; R7 old fp

               "ADD(R15,4);\n"

               "DROP(R15);\n"

              "PUSH(R1);\n"

               "PUSH(IMM(1));\n"

              "PUSH(R5);\n"

               "PUSH(R6);\n"

               "PUSH(R7);\n"

               "MOV(FP,SP);\n"

               

                (code-gen (get-var-body expression) major)



               	"POP(FP);\n"

                "RETURN;\n"

                label-clos-exit ":\n"

               ))

               ))
		



;;;;;;;;;;;;;;;;;;;;lambda opt;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define get-opt-args car)

(define get-opt-rest cadr)

(define get-opt-body caddr)



(define create-label-opt-opt-entry (^^label "L_opt_pair_entry_"))

(define create-label-opt-opt-exit (^^label "L_opt_pair_end_"))



(define create-labal-opt-clos-body-exit (^^label "L_clos_opt_clos_body_exit_"))

(define create-labal-opt-clos-body-entry (^^label "L_clos_opt_clos_body_entry_"))



(define create-label-opt-clos-entry (^^label "L_clos_opt_entry_"))

(define create-label-opt-clos-exit (^^label "L_clos_opt_exit_"))



(define create-label-opt-clos-entry (^^label "L_clos_opt_entry_"))

(define create-label-opt-clos-exit (^^label "L_clos_opt_exit_"))







(define get-opt-args car)                                







(define lam-opt-clos-create

	(lambda (expression major)

	(let  ((opt-pair-start (create-label-opt-opt-entry))

               (opt-pair-end (create-label-opt-opt-exit))

               (label-clos-body (create-labal-opt-clos-body-entry))

               (label-clos-exit (create-labal-opt-clos-body-exit))

               (var-fix-start (create-label-opt-clos-entry))

               (var-fix-end (create-label-opt-clos-exit))

               (unused-pars-entry (create-label-opt-clos-entry))

               (unused-pars-exit (create-label-opt-clos-exit))

               

               

               )

                

                    

      (string-append

                         "/*creating T_CLOSURE\n*/"

                ;"SHOW(\"befor creating T_CLOSURE\",R2);\n"

                "PUSH(IMM(3));\n"

                "CALL(MALLOC);\n"

                

                "DROP(1);\n"

                "MOV(INDD(R0,IMM(0)), T_CLOSURE);\n"

                "MOV(INDD(R0,IMM(1)), R2);//ext. env\n"

                "MOV(INDD(R0,IMM(2)),LABEL("label-clos-body"));\n"

		   

                "JUMP("label-clos-exit");\n"

                        

                label-clos-body ":" nl

                			

                "PUSH(FP);\n"

                "MOV(FP, SP);\n"  

                         

                "MOV(R1,SOB_NIL);\n" ;nil for list

                "MOV(R15, IMM(FPARG(1)));\n" ;number of args in 15

                "MOV(R14, R15); " nl ;;number of args in 15

                "ADD(R14, IMM(1));" nl;number of args + 1 in 14



                "MOV(R11, R15); " nl ;;number of args in 11

                "SUB(R11, IMM(1));" nl;number of args - 1 in 11

                

                "MOV(R12," (cg-num (length (get-opt-args expression)))");\n"  ;num of before dot args

                "SUB(R15,R12);\n"    ;the correct number of loop in 15





               opt-pair-start":\n"

               "CMP(IMM(0),R15);\n"

               "JUMP_EQ("opt-pair-end");\n"

               

               "PUSH(R1);\n"

               "PUSH(FPARG(R14));\n"

               "CALL(MAKE_SOB_PAIR);\n"



               "DROP(2);"

               "MOV(R1,R0);\n"

               "SUB(R15,IMM(1));\n"

               "SUB(R14,IMM(1));\n"



               "JUMP("opt-pair-start");\n"

               opt-pair-end":\n"

               

               "PUSH(R1);\n"

               "ADD(R12, IMM(1));" nl

               

               "MOV(R3,FPARG(1));\n" ;

                "ADD(R3,IMM(1));\n"

                

                "MOV(R4,FPARG(1));\n"

                "ADD(R4,IMM(4));\n"

                

                "MOV(FPARG(1), R12);\n" ;

                "INCR(FPARG(1));"nl

                

                var-fix-start ":" nl

                "CMP(R12, IMM(-3));\n"



          

                 "JUMP_EQ("var-fix-end");\n"

               "PUSH(FPARG(R12));"nl

               "SUB(R12, IMM(1));" nl

                  "JUMP( " var-fix-start ");\n"

                var-fix-end ":" nl

               unused-pars-entry ":" nl

               "CMP(R3, IMM(-4));\n"

                       

               "JUMP_EQ("unused-pars-exit");\n"



               "MOV(FPARG(R3),FPARG(R12));\n" ;

               "SUB(R12,IMM(1));"nl

                "SUB(R3,IMM(1));" nl

                

                "JUMP( " unused-pars-entry ");\n"

                unused-pars-exit ":" nl

                  

                "DROP(R4);\n"       

                "MOV(FP,SP);\n"



                          

                (code-gen (get-opt-body expression) major)



               	"POP(FP);\n"

                "RETURN;\n"

                label-clos-exit ":\n"

     

               ))

               ))



(define make-cisc-lam-opt

	(lambda (pe major)

				(string-append 

					"/* lambda-simple */\n"

				 (allocate-memory-for-clos-env major) 

  				 (copy-vectors-args major)

  				 (allocat-mem4-args-cloure)

  				 (args-to-mem major)

                 (lam-opt-clos-create pe major))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





 (define code-gen-set2

    (lambda(pexpr maj) (display (car (cadr pexpr)))

        (string-append (code-gen (caddr pexpr) maj)

            (cond 
            
            ((eq? (car (cadr pexpr)) 'pvar) (display 1)

            (string-append "MOV(FPARG( " (number->string (+ 2 (caddr (cadr pexpr)))) "),R0);\n"))

            ((eq? (car (cadr pexpr)) 'fvar) (display 2)

            (string-append "MOV(IND("(number->string(cdr (assoc (cadr (cadr pexpr)) fvarTable ))) "),R0);\n")) (display 2.5)

            ((eq? (car (cadr pexpr)) 'bvar) 
            (display 3)

            (string-append 

            "MOV(R1,FPARG(0));\n"

            "MOV(R1,INDD(R1," (number->string (caddr (cadr pexpr)))"));\n"

            

            "MOV(INDD(R1," (number->string (cadddr (cadr pexpr))) "),R0);\n"))

            (else "SET! ERROR\n"))        

        "MOV(R0,SOB_VOID);\n")
        (display 4)

        ))
(define get-var cadr)
(define get-type car)

(define code-gen-set
    (lambda (pe major)
     
        (let ((type (get-var pe))
                  (ele (caddr pe)))
        (cond ((equal? 'fvar (get-type type))
                (let ((add (lookUpFvarTable (cadadr pe))))
                    (string-append
                        (code-gen ele major)
                        "MOV(IND("(number->string add) " ), R0);\n"
                      "MOV(R0,IMM(SOB_VOID));\n"
                        )))
                    ((equal? 'pvar (get-type type))
                        (let ((add (+ 2 (caddr (get-var pe)))))
                    (string-append
                        (code-gen ele major)
                        "MOV(FPARG("(number->string add )"), R0);\n"
                        "MOV(R0,IMM(SOB_VOID));\n"
                        )))
                    ((equal? 'bvar (get-type type))
                        (let ((maj (number->string (car (cddadr pe))))
                                    (min (number->string (cadr (cddadr pe)))))
                    (string-append
                        (code-gen ele major)
                        "MOV(R1,FPARG(0));\n//env"
                        "MOV(R1,INDD(R1 , "  maj "));\n"
                        "MOV(INDD(R1 , " min ") , R0);\n"
                    "MOV(R0,IMM(SOB_VOID));\n"
                        )))
                    (else "set! error\n")
            ))))





(define code-gen-lam-opt (lambda (pe maj) (make-cisc-lam-opt (cdr pe) maj)))

  				 

(define code-gen-lam-var (lambda (pe maj) (lam-var-clos-create (cdr pe) maj)))



(define code-gen 

    (lambda (pe maj)

        (cond   ((eq? (car pe) 'const)(code-gen-const pe))

                ((eq? (car pe) 'fvar) (code-gen-fvar pe ))

                ((eq? (car pe) 'pvar) (code-gen-pvar pe ))

                ((eq? (car pe) 'bvar) (code-gen-bvar pe ))

                ((eq? (car pe) 'or) (code-gen-or pe maj))

                ((eq? (car pe) 'seq) (code-gen-seq pe maj))

                ((eq? (car pe) 'if3) (code-gen-if3 pe maj))

                ((eq? (car pe) 'def) (code-gen-def pe maj))

                ((eq? (car pe) 'lambda-simple) (code-gen-lam-sim pe (+ maj 1)))

                ((eq? (car pe) 'lambda-var) (code-gen-lam-var pe (+ maj 1)))

                ((eq? (car pe) 'lambda-opt) (code-gen-lam-opt pe (+ maj 1)))

                ((eq? (car pe) 'applic) (code-gen-applic pe maj))

                ((eq? (car pe) 'tc-applic) (code-gen-tc-applic pe maj))

                ((eq? (car pe) 'set) (code-gen-set pe maj))

                ((eq? (car pe) 'box-get) (code-gen-box-get pe maj))

                ((eq? (car pe) 'box-set) (code-gen-box-set pe maj))

                ((and (list? pe) (eq? (car pe) 'box)) (code-gen-box pe))

                

                (else "CODE GEN ERROR WRONG..."))))

                

        

        















                       



