; Variation on The Little Schemer Interpreter - Lazy Evaluation
; Students: Aashna Shah, Gurpreet Singh, James Chou

; Top Level of the Interpreter
(define value
  (lambda (e)
    (meaning e (quote () ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; .......... STREAMS ..............
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (try a b)
  (if (= a 0) 1 b))

(define value
  (lambda (e)
    (meaning e (quote () ))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 (quote ())))))

(define first car)
(define second cadr)
(define third caddr)

; verify if the given expression is an atom?
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; seach the table for the value of the given symbol
(define lookup-in-table
  (lambda (name table table-f)
    (cond 
      ((null? table) (table-f name))
      (else (lookup-in-entry name
                             (car table)
                             (lambda (name)
                               (lookup-in-table name
                                                (cdr table)
                                                table-f)))))))

(define extend-table cons)

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (names entry)
                          (vals entry)
                          entry-f)))


; when searching the enviornment the symbol may return a thunk
; in which case we must force it.
(define lookup-in-entry-help
  (lambda (name names vals entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names) name)
       (cond ((eq? 'thunk (car (car vals))) (force-it (car vals)))    
             ((eq? 'evaluated-thunk (car (car vals))) (force-it (car vals)))
             (else (car vals))
             ))
      (else (lookup-in-entry-help name
                                  (cdr names)
                                  (cdr vals)
                                  entry-f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; simple cons
(define new-entry build)

; extract names from the entry
(define names
  (lambda (entry) (car entry)))

; extract values from the entry
(define vals
  (lambda (entry) (cadr entry)))

; call atom to action or expression to action
(define expression-to-action
  (lambda (e)
    (cond 
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

; check if e is an identifier, a number, #t, #f, cons....
(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e (quote cons)) *const)
      ((eq? e (quote car)) *const)
      ((eq? e (quote cdr)) *const)
      ((eq? e (quote null?)) *const)
      ((eq? e (quote eq?)) *const)
      ((eq? e (quote atom?)) *const)
      ((eq? e (quote zero?)) *const)
      ((eq? e (quote add1)) *const)
      ((eq? e (quote mul)) *const)
      ((eq? e (quote sub1)) *const)
      ((eq? e (quote sub1)) *const)
      ((eq? e (quote number?)) *const)
      ((eq? e (quote plus)) *const)
      (else *identifier))))

; check if the given expression is a quote, lambda, cond or an application
(define list-to-action
  (lambda (e)
    (cond
      ((null? e) '())
      ((atom? (car e))
       (cond 
         ((eq? (car e) (quote quote))
          *quote)
         ((eq? (car e) (quote lambda))
          *lambda)
         ((eq? (car e) (quote cond))
          *cond)
         (else *application)))
      (else *application))))

; returns a #, #t, #f, or a primitive ....
(define *const
  (lambda (e table)
    (cond 
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build (quote primitive) e)))))

; simply return the text after quote
(define *quote
  (lambda (e table)
    (text-of e)))

; cdr
(define text-of second)

; look for the identifier in the table
(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

; note that as (car (quote ())) throws an error, this definition
; amounts to saying that looking anything up in the initial table
; is impossible.
(define initial-table
  (lambda (name)
    (car (quote ()))))

; the *lambda function simply creates the closures
(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
           (cons table (cdr e)))))

(define table-of first)
(define formals-of second)
(define body-of third)

; cond is a special form that takes any number of 
; cond-lines ...  if it sees an else-line, it treats
; that cond-line as if its question part were true.
(define evcon
  (lambda (lines table)
    (cond 
      ((else? (question-of (car lines)))
       (cond ((atom? (meaning (answer-of (car lines)) table))
              (meaning (answer-of (car lines)) table))
             (else (if (or (eq? (car (meaning (answer-of (car lines)) table)) 'thunk)
                           (eq? (car (meaning (answer-of (car lines)) table)) 'evaluated-thunk))
                       (force-it (meaning (answer-of (car lines)) table))
                       (meaning (answer-of (car lines))table)))))
      ((meaning (question-of (car lines)) table)
       (cond ((atom? (meaning (answer-of (car lines)) table))
              (meaning (answer-of (car lines)) table))
             (else (if (or (eq? (car (meaning (answer-of (car lines)) table)) 'thunk)
                           (eq? (car (meaning (answer-of (car lines)) table)) 'evaluated-thunk))
                       (force-it (meaning (answer-of (car lines)) table))
                       (meaning (answer-of (car lines))table)))))
      (else (evcon (cdr lines) table)))))

(define else?
  (lambda (x)
    (cond 
      ((atom? x) (eq? x (quote else)))
      (else #f))))

; car and cadr
(define question-of first)
(define answer-of second)

(define *cond 
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

; NOT BEING USED ANYMORE
(define evlis
  (lambda (args table)
    (cond 
      ((null? args) (quote ()))
      (else
       (cons (meaning (car args) table)
             (evlis (cdr args) table))))))

; checks if the object is a thunk or a evaluated thunk or neither
(define (evaluated-thunk? obj)
 (if (and (list? obj) (not (null? obj)))
     (eq? (car obj) 'evaluated-thunk)
     #f))

; thunk structure (thunk exp env)
(define (thunk-value e)
  (cadr e))

; force-it check if the object passed is a thunk or an evaluted thunk
; if it is a thunk then we call actual-value on the expression of the thunk with
; the thunk enviornment, and save it as the result and then we update the thunk 
; to an evaluated thunk. If object is evaluated-thunk, then we simply return
; the result that was stored when the thunk was evaluated.
(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)
           (set-cdr! (cdr obj) '())
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

; NEW: actual-value takes in expression an the table, and calls the method 
; force-it (meaning e table)..so then whatever meaning returns get passedd in to force-t
(define (actual-value e table)
  (force-it (meaning e table))
  )

; CHANGED: just calls myapply with, (actual-value of the (car e)), (cdr e) and table.
(define *application
  (lambda (e table)
    (myapply (actual-value (car e) table)
             (cdr e) 
             table)))

(define function-of car)
(define arguments-of cdr)

(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))

(define non-primitive?
  (lambda (l)
    (eq? (first l) (quote non-primitive))))

;NEW: (ki nd of immitates the old myapply )
;taks in the arguments and the table, if the args is empty, returns empty list
;else it cons (actural-value of (car args *so that would be the function)) w/ (list-of-args (cdr args) table)
(define (list-of-args-value arguments table)
  (if (null? arguments)
      '()
      (cons (actual-value (car arguments) table)
            (list-of-args-value (cdr arguments) table)
            )))

;NEW
;takes in expression and environment. Uses delay-it to create all the thunks possible in the expression
;If expression is null returns empty list. 
(define (list-of-delayed-args exp env)
  (if (null? exp)
      '()
      (cons (delay-it (car exp) env)
            (list-of-delayed-args (cdr exp) env))))


;NEW
;Creates a thunk! 
(define (delay-it exp env)
  (list (quote thunk) exp env))


;NEW: thunk predicate! 
(define (thunk? obj)
  (if (and (list? obj) (not (null? obj)))
      (eq? (car obj) (quote thunk))
  #f))


;NEW - identifies thunk expression 
(define (thunk-exp thunk) (cadr thunk))


;NEW - idefnties thunk environment 
(define (thunk-env thunk) (caddr thunk) )


;CHANGED: So if it is primitive, it calls myapply-primitive, with the name of the function
;(second procedure), (list of args arguments table)
;if its is not a primitive it calls (myapply-closure) (second procedure) vals

; NEW
; returns the procedure body from the closure
(define (proc-body p)
  (cdr (cdr (car (cdr p))))
  )


; NEW
; returns the procedure env from the closure
(define (proc-env p)
  (car (car (cdr p)))
  )


; NEW
; returns the procedure params from the closure
(define (proc-params p)
  (car (cdr (car (cdr p))))
  )

; NEW: extends new entries to the enviornment
(define (extend-env vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (cons vars (list vals)) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

; NEW
(define (last-exp? seq)
  (null? (cdr seq)))

(define first-exp car)

; Goes through each expression in the body of the procedure
; and evalutes it by calling actual-value on it.
(define (eval-seq exps env)
  (cond ((last-exp? exps) (meaning (first-exp exps) env))
        (else (actual-value (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))


; CHANGED: takes in a procedure, the arguments and a table
; primitives: no changes
; non-primitive: call eval-seq with the body of the procedure 
; and extend the enviornment to include the thunks, which are created
; using list-of-delayed-args.
(define myapply
  (lambda (procedure arguments table)
    (cond
      ((primitive? procedure)   (myapply-primitive
                                (second procedure)     ; (primitive add1) 
                                (list-of-args-value arguments table)))
      ((non-primitive? procedure)
       (eval-seq (proc-body procedure)
                 (extend-env (proc-params procedure)
                             (list-of-delayed-args arguments table)
                             (proc-env procedure)
                             ))))))

(define myapply-primitive
  (lambda (name vals)
    (cond
      ((eq? name (quote cons))
       (cons (first vals) (second vals)) )
      ((eq? name (quote car))
       (car (first vals)))
      ((eq? name (quote cdr))
       (cdr (first vals)))
      ((eq? name (quote null?))
       (null? (first vals)))
      ((eq? name (quote eq?))
       (eq? (first vals) (second vals)))
      ((eq? name (quote atom?))
       (:atom? (first vals)))
      ((eq? name (quote zero?))
       (zero? (first vals)))
      ((eq? name (quote add1))
       ((lambda (x) (+ x 1)) (first vals)))
      ((eq? name (quote mul))
       (* (first vals) (second vals)))
      ((eq? name (quote plus))
       (+ (first vals) (second vals)))
      ((eq? name (quote sub1))
       (- (first vals) 1))
      ((eq? name (quote number?))
       (number? (first vals))))))

(define :atom?
  (lambda (x)
    (cond 
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (car x) (quote primitive))
       #t)
      ((eq? (car x) (quote non-primitive))
       #t)
      (else #f))))

(define myapply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
              (new-entry
               (formals-of closure)
               vals)
              (table-of closure)))))



;(value '(lambda (x) (add1 x)) 2))
;(meaning '(lambda (x) (add1 x)) 2) '())
;(expression-to-action '(lambda (x) (add1 x)) 2) '())
;(list-to-action '(lambda (x) (add1 x)) 2) '())
;(*application '(lambda (x) (add1 x)) 2) '())
;(myapply (actual-value '(lambda (x) (add1 x)) '())
;          '(2)
;           '()))
;
;(myapply (forceit (meaning '(lambda (x) (add1 x)) '()))
;          '(2)
;           '()))
;

;(myapply (forceit (non-primtive '( '() (x) (add1 x))))
;          '(2)
;          '()))

;(myapply (non-primitive '( '() (x) (add1 x)))
;         '(2)
;         '()))


; ((non-primitive?(non-primitive '( '() (x) (add1 x)))) 
;                                  (myapply-closure '( '() (x) (add1 x))  '(2)))

;(myapply-closure  (second ))




;exp = e
;env = table 



;(define (eval-sequence exps env)
 ; (cond (last? exps) (eval) (f)

;(value '((lambda (x) x) 2))
;
;(value '1)
;(value '((lambda(x) (add1 x)) 1))
;(value '(add1 (add1 (add1 1))))
;(value '(quote a))
;(value '(cond ((eq? 1 2) 2) ((eq? 1 1) 3)))
;(value '((lambda (x) x) 2))
;(value '((lambda (x) (add1 x)) 1))
;(value '((lambda (y x) (add1 ((lambda (x) (add1 y)) 3) )) 1 2))
;(value '((lambda (a b) (cond ((eq? a 0) 1000000) (else b))) 0 (cons 2 3)))
;(value '((lambda (a b) (cond ((eq? a 0) a) (else b))) 0 ((lambda (a b) (cond ((eq? a 1) 44) (else b))) 1 2 )))
;(value '((lambda (a b) (cond ((eq? a 1) 44) (else b))) 1 2 ))
;(value '((cons 'a '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; things you can do with lazy tls scheme - specifically, some examples adapted from Section 4.2
; of Abelson and Sussman


; all examples require your working lazy tls to have been loaded first - you will see from these 
; examples that my lazy-tls follows tls quite closely in many ways.




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; an example displaying the lazy form of the y-combinator and its use in defining recursive functions
;
;(value '((lambda (y)
;           ((y (lambda (length)
;                 (lambda (l)
;                   (cond ((null? l) 0)
;                         (else (add1 (length (cdr l))))))))
;            (quote (a b c d e f)))
;           )
;         
;         (lambda (f)
;           ((lambda (x) (f (x x)))
;            (lambda (x) (f (x x)))))
;         ))
;
;; (value '((lambda (y) (cons-stream y (+ 1 y))) 5))
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;(value '((lambda (lcons lcar lcdr)
;          (lcar (lcdr (lcons 0 (lcons 1 (div 1 0)))))
;          )
;        
;        (lambda (x y)
;          (lambda (m) (m x y)))
;        
;        (lambda (z)
;          (z (lambda (p q) p)))
;        
;        (lambda (z)
;          (z (lambda (p q) q)))
;        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; WE HAVE FIGURED OUT STREAMS

; SEE BELOW THE EXAMPLES THAT WE CAME UP WITH.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;--------------------------------------------------------
; stream of fibonacci numbers
;--------------------------------------------------------
; change the lcdr's to get the nth fibonacci's

(value '((lambda (y lcons lcar lcdr)
          (lcar
           (lcdr
            (lcdr
             (lcdr
              (lcdr
               (lcdr
                (lcdr
                 ((y (lambda (cs)
                       (lambda (a b) 
                         (lcons a (cs b (plus a b))))))
                  0 1)
                 ))))))))
        
        (lambda (f)
          ((lambda (x) (f (x x)))
           (lambda (x) (f (x x)))))
        
        
        (lambda (x y)
          (lambda (m) (m x y)))
        
        (lambda (z)
          (z (lambda (p q) p)))
        
        (lambda (z)
          (z (lambda (p q) q)))
        ))

;------------------------------------------------
; factorial using the Y-COMBINATOR
;------------------------------------------------

(display
 (value '((lambda (y)
          ((y (lambda (fact)
                (lambda (n)
                  (cond ((eq? n 0) 1)
                        (else (mul n (fact (sub1 n))))))))
           5)
          )
        
        (lambda (f)
          ((lambda (x) (f (x x)))
           (lambda (x) (f (x x)))))
        )))

(display "\n")

;--------------------------------------------------------
; stream of Integers
;--------------------------------------------------------
; change the lcdr's to get the nth integer in a stream of infinite numbers

(value '((lambda (y lcons lcar lcdr)
          (lcar
           (lcdr
            (lcdr
             (lcdr
              (lcdr
               (lcdr
                (lcdr
                 ((y (lambda (cs)
                       (lambda (a) 
                         (lcons a (cs (add1 a))))))
                  0)
                 ))))))))
        
        (lambda (f)
          ((lambda (x) (f (x x)))
           (lambda (x) (f (x x)))))
        
        (lambda (x y)
          (lambda (m) (m x y)))
        
        (lambda (z)
          (z (lambda (p q) p)))
        
        (lambda (z)
          (z (lambda (p q) q)))
        
        ))

;--------------------------------------------------------
; stream of Integers multiplied by 7
;--------------------------------------------------------
(value '((lambda (y lcons lcar lcdr)
          ((lambda (ones add-lists list-ref map)
             ((lambda (integers)
                
                (list-ref (map 
                           (lambda (x) (mul x 2))
                           integers)
                          7)
                
                )
              
              ((y (lambda (integers-starting-from)
                    (lambda (n)
                      (lcons n (add-lists ones (integers-starting-from n))))))
               0)
              
              )
             
             )
           
           ((y (lambda (cs)
                 (lambda (c) 
                   (lcons c (cs c)))))
            1)
           
           
           (y (lambda (add-lists)
                (lambda (list1 list2)
                  (cond ((null? list1) list2)
                        ((null? list2) list1)
                        (else (lcons (plus (lcar list1) (lcar list2))
                                     (add-lists (lcdr list1) (lcdr list2))))))))
           
           
           (y (lambda (list-ref)
                (lambda (items n)
                  (cond ((zero? n)
                         (lcar items))
                        (else (list-ref (lcdr items) (sub1 n)))))))
           
           
           (y (lambda (map)
                (lambda (proc items)
                  (cond ((null? items) (quote ()))
                        (else (lcons (proc (lcar items))
                                     (map proc (lcdr items))))))))
                 
           
           )
                    
          )
        
        (lambda (f)
          ((lambda (x) (f (x x)))
           (lambda (x) (f (x x)))))
        
        
        (lambda (x y)
          (lambda (m) (m x y)))
        
        (lambda (z)
          (z (lambda (p q) p)))
        
        (lambda (z)
          (z (lambda (p q) q)))
        
        ))