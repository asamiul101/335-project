;;; Names: Ida Touray & Samiul Ahmed
;;; Emails: Itouray000@citymail.cuny.edu, sahmed051@citymail.cuny.edu

;;; --------------------------------------------------------------------------------------------------------------
;;; PART 1

;;; STRUCTURAL INDUCTION PROOF:
;;; BASIS:
;;; A symbol contains no operators so the result is vacuously true

;;; INDUCTION HYPOTHESIS (IH):
;;; Assume that for each component in the proposition we are considering we have already re-written the
;;; component into a logically equivalent proposition using just OR (v) and NOT (~) operators

;;; INDUCTION STEP (IS):
;;; Consider a proposition P. There are 4 cases:

;;; case 1: P = (A v B); A and B are components
;;; By the IH, A' is the re-written proposition that is logically equivalent to A, where A' is expressed using only
;;; the OR (v) and NOT (~) operators. Similarly, B' is the re-written proposition that is logically equivalent to B,
;;; where B' is expressed using only the OR (v) and NOT (~) operators.
;;; (A v B) is then logically equivalent to (A' v B') so all we need to do is call the make-and constructor on A' and
;;; B' to re-write (A v B).

;;; case 2: P = (A ^ B)
;;; (A ^ B) is logically equivalent to (A' ^ B'), where by the IH, A' and B' are re-written propositions that are
;;; logically equivalent to A and B, respectively, using only OR (^) and NOT (~) operators.
;;; Now, all that's needed to be done is to call convert-or function that will convert (A' ^ B') to the logically
;;; equivalent proposition ~(~A' v ~B').

;;; case 3: P = ~A
;;; To reiterate, by the IH, A' is the re-written proposition that is logically equivalent to A, where A' is
;;; expressed using only the OR (v) and NOT (~) operators. So, ~A is logically equivalent to ~A'.
;;; Then, all that's left to do is to call the make-not constructor on A'.

;;; case 4: P = (A => B)
;;; (A => B) is logically equivalent to (A' => B'), where by the IH, A' and B' are re-written propositions that are
;;; logically equivalent to A and B, respectively, using only OR (v) and NOT (~) operators. All that's left now to
;;; do is to call convert-imply function on (A' => B') to convert it to the logically equivalent proposition
;;; (~A' v B').



;;; -------------------------------------------------------------------------------------------------------------
;;; Selectors, Constructors & Classifiers
;;; --------------------------------------------------------------------------------------------------------------
;;; CHECK PREFIX or INFIX
;;; Pre-condition: inputs an expression representing a proposition
;;; Post-condition: returns boolean true if the input structure is infix and returns false if the input is prefix
(define (check-infix e)
  (cond ((eq? (car e) '=>) #f)  ;;;;; (=> p q) => #f
        ((eq? (car e) '^) #f)
        ((eq? (car e) 'v) #f)
        ((eq? (car e) '~) #f)
        (else #t)))             ;;;; (p => q) => #t


;;; SELECTORS
;;; --------------------------------------------------------------------------------------------------------------
;;; FIRST-OPERAND FUNCTION
;;; Pre-condition: inputs an expression representing a proposition
;;; Post-condition: returns the expression representing the first operand of the inputed expression

;;; CODE
(define (first-operand e)
  (cond ((eq? (check-infix e) #f) (cadr e))     ;;; (=> p q)
        (else (car e))))                        ;;; (p ^ q)

;;; --------------------------------------------------------------------------------------------------------------

;;; --------------------------------------------------------------------------------------------------------------
;;; SECOND-OPERAND FUNCTION
;;; Pre-condition: inputs an expression representing either a AND, OR, IMPLY proposition
;;; Post-condition: returns the expression representing the second operand of the inputed expression

;;; CODE
(define (second-operand e)
  (cond ((eq? (check-infix e) #f) (caddr e))
        (else (caddr e))))                      ;;; (p ^ q)

;;; --------------------------------------------------------------------------------------------------------------

;;; --------------------------------------------------------------------------------------------------------------
;;; OPERATOR FUNCTION
;;; Pre-condition: inputs an expression representing either a AND, OR, IMPLY proposition
;;; Post-condition: returns the operator of the inputed expression

;;; CODE
(define (operator e)
  (cond ((eq? (check-infix e) #f) (car e))
        (else (cadr e))))

;;; --------------------------------------------------------------------------------------------------------------

;;; Infix CONSTRUCTORS
;;; --------------------------------------------------------------------------------------------------------------
;;; MAKE-AND FUNCTION
;;; Pre-condition: inputs two expressions representing propositions
;;; Post-condition: returns the expression obtained from performing the AND (^) operator on the inputed expressions

;;; CODE
(define (make-and e1 e2)
  (list e1 '^ e2))

;;; --------------------------------------------------------------------------------------------------------------

;;; --------------------------------------------------------------------------------------------------------------
;;; MAKE-OR FUNCTION
;;; Pre-condition: inputs two expressions representing propositions
;;; Post-condition: returns the expression obtained from performing the OR (v) operator on the inputed expressions

;;; CODE
(define (make-or e1 e2)
  (list e1 'v e2))

;;; --------------------------------------------------------------------------------------------------------------

;;; --------------------------------------------------------------------------------------------------------------
;;; MAKE-NOT FUNCTION
;;; Pre-condition: inputs an expressions representing a proposition
;;; Post-condition: returns the expression obtained from performing the NOT (~) operator on the inputed expression

;;; CODE
(define (make-not e)
  (cond ((and (not (symbol? e)) (not? e)) (first-operand e))
        (else (list '~ e ))))

;;; --------------------------------------------------------------------------------------------------------------

;;; --------------------------------------------------------------------------------------------------------------
;;; MAKE-IMPLY FUNCTION
;;; Pre-condition: inputs two expressions representing propositions
;;; Post-condition: returns the expression obtained from performing the IMPLY (=>) operator on the inputed
;;;                 expressions

;;; CODE
(define (make-imply e1 e2)
  (list e1 '=> e2))

;;; --------------------------------------------------------------------------------------------------------------


;;; Prefix CONSTRUCTORS
;;; --------------------------------------------------------------------------------------------------------------
;;; MAKE-AND FUNCTION
;;; Pre-condition: inputs two expressions representing propositions
;;; Post-condition: returns the expression obtained from performing the AND (^) operator on the input expressions

;;; CODE
(define (make-and-pre e1 e2)
  (list '^ e1 e2))

;;; --------------------------------------------------------------------------------------------------------------

;;; --------------------------------------------------------------------------------------------------------------
;;; MAKE-OR FUNCTION
;;; Pre-condition: inputs two expressions representing propositions
;;; Post-condition: returns the expression obtained from performing the OR (v) operator on the input expressions

;;; CODE
(define (make-or-pre e1 e2)
  (list 'v e1 e2))

;;; --------------------------------------------------------------------------------------------------------------

;;; --------------------------------------------------------------------------------------------------------------
;;; MAKE-NOT FUNCTION
;;; Pre-condition: inputs an expressions representing a proposition
;;; Post-condition: returns the expression obtained from performing the NOT (~) operator on the input expression

;;; CODE
(define (make-not-pre e)
  (cond ((and (not (symbol? e)) (not? e)) (first-operand e))
        (else (list '~ e))))

;;; --------------------------------------------------------------------------------------------------------------

;;; --------------------------------------------------------------------------------------------------------------
;;; MAKE-IMPLY FUNCTION
;;; Pre-condition: inputs two expressions representing propositions
;;; Post-condition: returns the expression obtained from performing the IMPLY (=>) operator on the input expressions

;;; CODE
(define (make-imply-pre e1 e2)
  (list '=> e1 e2))




;;; CLASSIFIERS
;;; --------------------------------------------------------------------------------------------------------------
;;; AND? FUNCTION
;;; Pre-condition: inputs an expression representing a proposition
;;; Post-condition: returns #t if the input expression is an AND expression, #f if not

;;; CODE
(define (and? e)
  (eq? (operator e) '^))

;;; --------------------------------------------------------------------------------------------------------------

;;; --------------------------------------------------------------------------------------------------------------
;;; OR? FUNCTION
;;; Pre-condition: inputs an expression representing a proposition
;;; Post-condition: returns #t if the input expression is an OR expression, #f if not

;;; CODE
(define (or? e)
  (eq? (operator e) 'v))

;;; --------------------------------------------------------------------------------------------------------------

;;; --------------------------------------------------------------------------------------------------------------
;;; NOT? FUNCTION
;;; Pre-condition: inputs an expression representing a proposition
;;; Post-condition: returns #t if the inputed expression is an NOT expression, #f if not

;;; CODE
(define (not? e)
  (eq? (operator e) '~))

;;; --------------------------------------------------------------------------------------------------------------

;;; --------------------------------------------------------------------------------------------------------------
;;; IMPLY? FUNCTION
;;; Pre-condition: inputs an expression representing a proposition
;;; Post-condition: returns #t if the inputed expression is an IMPLY expression, #f if not

;;; CODE
(define (imply? e)
  (eq? (operator e) '=>))

;;; --------------------------------------------------------------------------------------------------------------


;;; -------------------------------------------------------------------------------------------------------------
;;; PART 2.1
;;; -------------------------------------------------------------------------------------------------------------
;;; CONVERT-AND FUNCTION

;;; SPECIFICATION
;;; Pre-Condition: inputs a AND (^) proposition e that was created using the constructors
;;; Post-Condition: returns a proposition that is equivalent to e but using just NOTs (~) and ORs (v)
;;; NOTE: (p ^ q) is logically equivalent to ~(~p v ~q)

(define (convert-and e)
  (let ((first-op (first-operand e)) (second-op (second-operand e)))
    (cond ((eq? (check-infix e) #f) (let ((or-exp (make-or-pre (make-not-pre first-op) (make-not-pre second-op))))
      (make-not-pre or-exp)))
          (else (let ((or-exp (make-or (make-not first-op) (make-not second-op))))
      (make-not or-exp))))))

;;; --------------------------------------------------------------------------------------------------------------


;;; --------------------------------------------------------------------------------------------------------------
;;; CONVERT-IMPLY FUNCTION

;;; SPECIFICATION
;;; Pre-Condition: inputs an IMPLY (=>) proposition e that was created using the constructors
;;; Post-Condition: returns a proposition that is equivalent to e but using just NOTs (~) and ORs (v)
;;; NOTE: (p => q) is logically equivalent to (~p v q)

(define (convert-imply e)
  (let ((first-op (first-operand e)) (second-op (second-operand e)))
    (cond ((eq? (check-infix e) #f) (make-or-pre (make-not-pre first-op) second-op))
          (else (make-or (make-not first-op) second-op)))))

;;; --------------------------------------------------------------------------------------------------------------



;;; -------------------------------------------------------------------------------------------------------------
;;; PART 2.2
;;; -------------------------------------------------------------------------------------------------------------
;;; TRANSLATOR FUNCTION

;;; SPECIFICATION
;;; Pre-Condition: inputs a proposition e that was created using the constructors
;;; Post-Condition: returns a proposition that is equivalent to e but using just NOTs (~) and ORs (v)

(define (translator e)
  (cond ((symbol? e) e)  
        ((not (not? e))
         (let ((first-op (first-operand e)) (second-op (second-operand e)))
           (cond ((and? e) (convert-and (cond ((eq? (check-infix e) #f) (make-and-pre (translator first-op) (translator second-op)))   
                                               (else (make-and (translator first-op) (translator second-op))))))
                 ((imply? e) (convert-imply (cond ((eq? (check-infix e) #f) (make-imply-pre (translator first-op) (translator second-op)))
                                                   (else (make-imply (translator first-op) (translator second-op))))))
                 (else (cond ((eq? (check-infix e) #f) (make-or-pre (translator first-op) (translator second-op)))
                             (else (make-or (translator first-op) (translator second-op))))))))
        (else (let ((first-op (first-operand e)))
                (cond ((eq? (check-infix e) #f) (make-not-pre (translator first-op)))  ;;; ~p
                      (else (make-not (translator first-op))))))))


;;; TEST CASES -----------------------
;;;(translator '((p => q) ^ (q => p)))
(translator '((p ^ q) ^ (q ^ p)))
(translator '(^ (^ p q) (^ q p)))
;;; (translator '(^ (=> p q) (=> q p)))


;;; STRUCTURAL INDUCTION PROOF:
;;; BASIS:
;;; A symbol contains no operators so the result is vacuously true

;;; INDUCTION HYPOTHESIS (IH):
;;; Assume that for each component in the proposition currently under consideration, we have already
;;; re-written the component into a logically equivalent proposition using just OR (v) and NOT (~) operators

;;; INDUCTION STEP (IS):
;;; Consider a proposition P. There are 4 cases:

;;; 1. P = (R v S); R and S are components
;;; By the IH, R' is the re-written proposition that is logically equivalent to R, where R' is expressed using only
;;; the OR (v) and NOT (~) operators. Similarly, S' is the re-written proposition that is logically equivalent to S,
;;; where S' is expressed using only the OR (v) and NOT (~) operators.
;;; (R v S) is then logically equivalent to (R' v S') so all we need to do is call the make-and constructor on R' and
;;; S' to re-write (R v S).

;;; 2. P = (R ^ S)
;;; (R ^ S) is logically equivalent to (R' v S'), where by the IH, R' and S' are re-written propositions that are
;;; logically equivalent to R and S, respectively, using only OR (^) and NOT (~) operators.
;;; Now, all that's needed to be done is to call convert-or function that will convert (R' ^ S') to the logically
;;; equivalent proposition ~(~R' v ~S').

;;; 3. P = ~R
;;; To reiterate, by the IH, R' is the re-written proposition that is logically equivalent to R, where R' is
;;; expressed using only the OR (v) and NOT (~) operators. So, ~R is logically equivalent to ~R'.
;;; Then, all that's left to do is to call the make-not constructor on R'.

;;; 4. P = (R => S)
;;; (R => S) is logically equivalent to (R' => S'), where by the IH, R' and S' are re-written propositions that are
;;; logically equivalent to R and S, respectively, using only OR (v) and NOT (~) operators. All that's left now to
;;; do is to call convert-imply function on (R' => S') to convert it to the logically equivalent proposition
;;; (~R' v S').


;;; -------------------------------------------------------------------------------------------------------------
;;; PART 2.3
;;; -------------------------------------------------------------------------------------------------------------
;;; LOOKUP-VALUE FUNCTION

;;; SPECIFICATION
;;; Pre-condition: input a symbol x, and a association list alist
;;; Post-condition: if x is in the association list, then it will output its value,
;;;   if x is not in the  association list, then it will return a null list '()

;;; DESIGN IDEA
;;; iterative procedure
;;; Check every pair of association list, say current pair = (car alist), for the termination, if x is the first 
;;; element of current pair, then it will terminate and return the value of second elements of current pair, 
;;; and if we check through the list, the x does not appear in any pair of the association list

;;; orignal ALIST
;;; ------------------------------------------
;;; already checked pairs | not checked yet
;;; ------------------------------------------
;;;                     (alist head ....     )

(define (lookup-value x alist)
  (cond ((eq? (caar alist) x) (cadar alist)) ;; if x is in current pair        ;;;; caar means car on top of car  car(car) ;;; cadar means cdr on top of car (car(cdr)) 
        ((eq? (cdr alist) '()) '())          ;; if x is not in the list
        (else (lookup-value x (cdr alist)))))

;;; PROOF
;;; we let original association list to be ALIST, 
;;; guess invariant: x appears in any pair of original ALIST iff x appears in alist

;;; Strong enough?: when the program start, alist = ALIST, then our GI is true.
;;; Weak enough?: when the program terminates, as we mentioned in design idea, there are two cases cause the program terminates
;;;   1. x was found in current head pair of alist, x must appears in the same pair of ALIST, then our GI is true
;;;   2. alist contains only an element of a pair and x is not appear in the pair, x is not in the orignal ALIST, our GI is true
;;; Preservable?: We assume that GI is true before each iterative call, while the program does not terminates, that means x is not in current head pair,
;;;   and alist does contains at least 2 pairs, then for the next call new alist becomes (cdr alist), then our GI: x appears in any pair of ALIST iff x
;;;   appears in alist still maintained, since previous head pair does not contains x.


;;; TEST DATA
(lookup-value 'x '((x #f) (y #t) (z #f))); => #f
;;;(lookup-value 'y '((x #f) (y #t) (z #f))); => #t
;;;(lookup-value 'z '((x #f) (y #t) (z #f))); => #f
;;;(lookup-value 'w '((x #f) (y #f) (z #f))); => ()


;;; IMPLY FUNCTION

;;; SPECIFICATION
;;; Pre-condition: input 2 boolean value p and q
;;; Post-condition: returns the boolean value of p implies q

;;; USE p=>q <=> ~pvq
(define (imply p q)
  (or (not p) q))

;;; TEST INPUTS
;;; (imply #t #t) => #t
;;; (imply #t #f) => #f
;;; (imply #f #t) => #t
;;; (imply #f #f) ;=> #f

;;; INTERPRETOR FUNCTION

;;; SPECIFICATION:
;;; Pre-Condition: inputs a proposition e that was created using the constructors and a valid association list alist
;;;   (every symbol x appears in e can be founded with its associated value in alist)
;;; Post-Condition: it will returns the value of e that each symbol of e is evaluated to its aoociated value in alist

;;; CODE:
(define (interpretor e alist)
  (cond ((symbol? e) (lookup-value e alist))
        ((not (not? e))
         (let ((first-op (first-operand e)) (second-op (second-operand e)))
           (cond ((and? e) (and (interpretor first-op alist) (interpretor second-op alist)))
                 ((or? e) (or (interpretor first-op alist) (interpretor second-op alist)))
                 ((imply? e) (imply (interpretor first-op alist) (interpretor second-op alist))))))
        (else
         (let ((first-op (first-operand e)))
           (not (interpretor first-op alist))))))


;;; (interpretor '(x ^ y) '((x #f) (y #t) (z #f)))   ;#f

;;; STRUCTURAL INDUCTION PROOF:

;;; Let P to be the least class that contains all the infix propersition

;;; BASIS:
;;; If the proposition is just a symbol, then we can directly check alist using lookup function. Since we assume our alist to be valid
;;; that is, every symbol x appears in e can be founded with its associated value in alist, so (lookup e alist) will return the boolean
;;; value of e when e is a symbol which is correct.

;;; INDUCTION HYPOTHESIS (IH)
;;; For any porposition p in P, assume the value of all the components of p are true

;;; INDUCTION STEP:
;;; Here we have 2 cases:
;;;  1. p is a infix proposition with 2 operands (in other words, p is either an AND (^), OR (v), or IMPLY (=>)
;;;     proposition). r and s are components of p that p = (r op s). By the IH, we know the value of r and s are all true. 
;;;     Then, since we have all the corrected value of r and s, then the value of p is going to be (operator (value of r) (value of s))
;;;     which is true
;;;  2. p is an infix proposition with only one operand (p is NOT (~)). let r to be a component of p that p = ~r. By the IH,
;;;     we know the value of r is true. then the value of p is going to be (not (value of r)) which is correct!


;;; TEST INPUTS
;;; (interpretor '((x v y) ^ (x => z)) '((x #t) (y #f) (z #t))) ;=> #t
;;;          (T v F) ^ (T => T) = T
(interpretor '(x ^ y) '((x #f) (y #t))) ;=> #f
;;;         (F ^ T) = F


;;; ---------------------------------------------------------------------------------------------------------------------------------
;;; 2.4
;;; ---------------------------------------------------------------------------------------------------------------------------------
(define (translator-interpretor e alist)
  (interpretor (translator e) alist))

(translator-interpretor '(x ^ y) '((x #f) (y #t)))


;;; STRUCTURAL INDUCTION PROOF:
;;; BASIS:
;;; If the proposition is just a symbol, then we can directly check alist using lookup function. If lookup function
;;; doesn't find it in alist, return #f (symbol is not in alist). Otherwise, #t (symbol is in alist)

;;; INDUCTION HYPOTHESIS (IH):
;;; Assume that for the components of the proposition currently under consideration, that we already know if
;;; all the symbols in the components are in the alist or not

;;; INDUCTION STEP (IS):
;;; Consider a proposition P. There are 2 cases:

;;; 1. P is a proposition with 2 operands (in other words, P is either an AND (^), OR (v), or IMPLY (=>)
;;; proposition). R and S are components of P. By the IH, we know whether the symbols in proposition R are all
;;; in alist or not. Similarly, by the IH, we know whether the symbols in proposition S are all in alist or not.
;;; Then, all that's left is to confirm that both components do indeed have all the symbols in the alist (recursive
;;; call returned #t for both components). If the check passes, return #t. Otherwise, return #f.

;;; 2. P is a proposition with 1 operand R (in other words, P is a NOT (~) proposition).
;;; To reiterate, by the IH, we know whether the symbols in proposition R are all in alist or not. Then, all that's
;;; left is to just return that result (#t if all symbols are in alist, #f otherwise). 
