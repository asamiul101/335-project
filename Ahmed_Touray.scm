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
;;; (A v B) is then logically equivalent to (A' v B') so all we need to do is call a constructor (for example: make-or) that will take A' and
;;; B' to re-write (A' v B').

;;; case 2: P = (A ^ B)
;;; (A ^ B) is logically equivalent to (A' ^ B'), where by the IH, A' and B' are re-written propositions that are
;;; logically equivalent to A and B, respectively, using only OR (v) and NOT (~) operators.
;;; Now, all that's needed to be done is to call a funtion (for example: convert-and) that will convert (A' ^ B') to the logically
;;; equivalent proposition ~(~A' v ~B').

;;; case 3: P = ~A
;;; To reiterate, by the IH, A' is the re-written proposition that is logically equivalent to A, where A' is
;;; expressed using only the OR (v) and NOT (~) operators. So, ~A is logically equivalent to ~A'.
;;; Then, all that's left to do is to call a constructor (for example: make-not) that take A'and rewrite to ~A'.

;;; case 4: P = (A => B)
;;; (A => B) is logically equivalent to (A' => B'), where by the IH, A' and B' are re-written propositions that are
;;; logically equivalent to A and B, respectively, using only OR (v) and NOT (~) operators. All that's left now to
;;; do is to call a fucntion (for example: convert-imply)on (A' => B') to convert it to the logically equivalent proposition (~A' v B').

;;; ------------------------------------------------------------------------------------------------------------
;;; PART 2

;;; -------------------------------------------------------------------------------------------------------------
;;; Selectors, Constructors & Classifiers
;;; -------------------------------------------------------------------------------------------------------------
;;; CHECK PREFIX or INFIX Function
;;; ------------------------------------------------------------------------------------------------------------
;;; Pre-condition: inputs an expression representing a proposition
;;; Post-condition: returns boolean true if the input structure is infix and returns false if the input is prefix
(define (check-infix e)
  (cond ((eq? (car e) '=>) #f)  ;;;;; (=> p q) => #f
        ((eq? (car e) '^) #f)
        ((eq? (car e) 'v) #f)
        ((eq? (car e) '~) #f)
        (else #t)))             ;;;; (p => q) => #t

;;; -------------------------------------------------------------------------------------------------------------
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
  (caddr e))                     

;;; --------------------------------------------------------------------------------------------------------------

;;; --------------------------------------------------------------------------------------------------------------
;;; OPERATOR FUNCTION
;;; Pre-condition: inputs an expression representing either a AND, OR, IMPLY, NOT proposition
;;; Post-condition: returns the operator of the inputed expression

;;; CODE
(define (operator e)
  (cond ((eq? (check-infix e) #f) (car e)) ;=> (v p q)
        (else (cadr e))))                  ;=> (p v q)

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
  (or (eq? (operator e) '^) (eq? (operator e) 'and)))

;;; --------------------------------------------------------------------------------------------------------------

;;; --------------------------------------------------------------------------------------------------------------
;;; OR? FUNCTION
;;; Pre-condition: inputs an expression representing a proposition
;;; Post-condition: returns #t if the input expression is an OR expression, #f if not

;;; CODE
(define (or? e)
  (or (eq? (operator e) 'v) (eq? (operator e) 'or)))

;;; --------------------------------------------------------------------------------------------------------------

;;; --------------------------------------------------------------------------------------------------------------
;;; NOT? FUNCTION
;;; Pre-condition: inputs an expression representing a proposition
;;; Post-condition: returns #t if the inputed expression is an NOT expression, #f if not

;;; CODE
(define (not? e)
  (or (eq? (operator e) '~) (eq? (operator e) 'not)))

;;; --------------------------------------------------------------------------------------------------------------

;;; --------------------------------------------------------------------------------------------------------------
;;; IMPLY? FUNCTION
;;; Pre-condition: inputs an expression representing a proposition
;;; Post-condition: returns #t if the inputed expression is an IMPLY expression, #f if not

;;; CODE
(define (imply? e)
  (or (eq? (operator e) '=>) (eq? (operator e) 'imply)))

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
(display "Translator: ") (newline)
(display "Test Case 1: ") (newline)
(display "Translate prepositon (p ^ q) ^ (q ^ p)) into logical equivalent OR(v) and NOT(~): ")
(translator '((p ^ q) ^ (q ^ p)))
(display "Translate prepositon (^ (^ p q) (^ q p))) into logical equivalent OR(v) and NOT(~): ")
(translator '(^ (^ p q) (^ q p))) (newline)

(display "Test Case 2: ") (newline)
(display "Translate prepositon ((p => q) ^ (q => p)) into logical equivalent OR(v) and NOT(~): ")
(translator '((p => q) ^ (q => p)))
(display "Translate prepositon (^ (=> p q) (=> q p)) into logical equivalent OR(v) and NOT(~): ")
(translator '(^ (=> p q) (=> q p)))
(newline)
(display "-------------------------------------------------------------------------------------------------------------------------------------------")
(newline)




;;; STRUCTURAL INDUCTION PROOF:

;;; BASE CASE: If E is a symbol, it returns E. Again, if e has double negation (~ (~ E)), it returns only E by reducing the negations.   

;;; IH: Assume that for each component in the proposition E currently under consideration, we have already
;;;     re-written preposition E with components P, Q and operator AND(^), OR(v), IMPLY(=>), NOT(~) into a logically equivalent proposition using just OR (v) and NOT (~) operators.

;;; IS: We consider that our prepositon E can have four cases. They are given below:
;;; case 1: If E = (P v Q) where P and Q are  the components of preposition E, then, we check if the operator is OR(v) by using or? function. 
;;;         By the IH, P' is the re-written proposition that is logically equivalent to P, where P' is expressed using only
;;;         the OR (v) and NOT (~) operators. Similarly, Q' is the re-written proposition that is logically equivalent to Q,
;;;         where Q' is expressed using only the OR (v) and NOT (~) operators.
;;;         (P v Q) is then logically equivalent to (P' v Q') so all we need to do is call the make-or / make-or-pre constructor
;;;         (based on the infix and prefix structure) on P' and Q' to re-write (P' v Q').

;;; case 2: If E = (P ^ Q) where P and Q are  the components of preposition E, then, we check if the operator is AND(^) by using and? function.
;;;         Now, (P ^ Q) is logically equivalent to (P' ^ Q'), where by the IH, P' and Q' are re-written propositions that are logically equivalent to P and Q, respectively, using only OR (v) and NOT (~) operators.
;;;         Now, all that's needed to be done is to call convert-and function that will convert (P' ^ Q') to the logically equivalent proposition ~(~P' v ~Q').

;;; case 3: If E = ~P, then, we check if the operator is NOT(~) by using not? function.
;;;         To reiterate, by the IH, P' is the re-written proposition that is logically equivalent to P, where P' is
;;;         expressed using only the OR (v) and NOT (~) operators. So, ~P is logically equivalent to ~P'.
;;;         Then, all that's left to do is to call the make-not / make-not-pre constructor (based on the infix or prefix structure) on P'.

;;; case 4: If E = (P => Q) where P and Q are  the components of preposition E, then, we check if the operator is IMPLY(=>) by using imply? function.
;;;         (P => Q) is logically equivalent to (P' => Q'), where by the IH, P' and Q' are re-written propositions that are
;;;         logically equivalent to P and Q, respectively, using only OR (v) and NOT (~) operators. All that's left now to
;;;         do is to call convert-imply function on (P' => Q') to convert it to the logically equivalent proposition (~P' v Q').


;;; -------------------------------------------------------------------------------------------------------------
;;; PART 2.3
;;; -------------------------------------------------------------------------------------------------------------
;;; LOOKUP-VALUE FUNCTION

;;; SPECIFICATION
;;; Pre-condition: it takes input a symbol x, and a association list alist
;;; Post-condition: if x is in the association list, then it will output its value,
;;;   if x is not in the  association list, then it will return a null list '()

;;; DESIGN IDEA
;;; iterative procedure
;;; First, we check every pair of association list, like current pair = (car alist), for the termination. the code will terminate if x is the first 
;;; element of current pair, and so it returns the value of second elements of current pair,
;;; It will also terminate if we check through the list, the x does not appear in any pair of the association list

;;; orignal ALIST
;;; ------------------------------------------
;;; already checked | not checked or processed
;;; ------------------------------------------
;;;                      (alist head ....     )

(define (lookup-value x alist)
  (cond ((eq? (caar alist) x) (cadar alist)) 
        ((eq? (cdr alist) '()) '())          
        (else (lookup-value x (cdr alist)))))

;;; PROOF
;;; let original association list to be ALIST, 
;;; guess invariant: x appears in any pair of original ALIST if and only if x appears in alist

;;; Strong enough?: when the program start, alist = ALIST, then our GI is true.
;;; Weak enough?: As we mentioned in design idea, the program terminates for below two reasons.
;;;   1. x was found in current head pair of alist, x must appears in the same pair of ALIST, then our GI is true
;;;   2. alist contains only an element of a pair and x is not appear in the pair, x is not in the orignal ALIST, our GI is true
;;; Preservable?: We assume that GI is true before each iterative call, while the program does not terminates, thus x is not found in current head pair,
;;;   and alist contains at least 2 pairs, then for the next call new alist becomes (cdr alist), then our GI: x appears in any pair of ALIST if and only if x
;;;   appears in alist still maintained, as previous head pair does not contain x.


;;; TEST DATA
(display "lookup value for x if aList is ((x #f) (y #t) (z #f))): ")
(lookup-value 'x '((x #f) (y #t) (z #f))); => #f
(newline)
(display "-------------------------------------------------------------------------------------------------------------------------------------------")
(newline)


;;; IMPLY FUNCTION

;;; SPECIFICATION
;;; Pre-condition: it takes 2 boolean value p and q as inputs
;;; Post-condition: returns the boolean value of p implies q

;;; USE p=>q <=> ~pvq
(define (imply p q)
  (or (not p) q))

;;; TEST INPUTS
;;; (imply #t #t) => #t
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

;;; Let E be the prefix or infix preposition that contains P and Q components and one of the operators (and, or, not, imply)

;;; BASIS:
;;; Assume that our alist is valid as every component of E has associate truth value in alist. Now, if the proposition E is just a symbol, then we just check if the symbol is inside alist using lookup function.
;;; Then, if the symbol has associate truth value inside alist, it will return the boolean value of that.

;;; INDUCTION HYPOTHESIS (IH)
;;; We assume that each component of prepositon E has a valid associate value inside alist. For this case, we assume that the values of all the components of E are true. 

;;; INDUCTION STEP:
;;; Here we will have two cases. They are:
;;; case 1. E is a infix/prefix proposition that has two operands P and Q and a operator (AND (^), OR (v), or IMPLY (=>).
;;;         By the IH, we know the value of P and Q are all true. As all the corrected value of P and Q are true, the value of E will be true based on (operator (value of p) (value of q)) the logic, which is right.
;;; case 2. E is an infix/prfix proposition has only one operand where E is NOT (~). Assume, P is a component of E that E = ~P.
;;;         By IH, we know the value of P is true. then the value of E is going to be (not (value of P)) which is right


;;; TEST 
(display "Interpretor:")(newline)
(display "Test Case 1: ") (newline)
(display "Interpret the (x ^ y) into truth value: ")
(interpretor '(x ^ y) '((x #f) (y #t))) ;=> #f
(newline)
;;;         (F ^ T) = F
(display "Test Case 2: ") (newline)
(display "Interpret the ((x => y) v (y => z)) into truth value: ")
(interpretor '((x => y) v (y => z)) '((x #f) (y #t) (z #t))) ;=> #t
;;;         (T ^ T) = T
(newline)
(display "-------------------------------------------------------------------------------------------------------------------------------------------")
(newline)


;;; ---------------------------------------------------------------------------------------------------------------------------------
;;; 2.4
;;; ---------------------------------------------------------------------------------------------------------------------------------
;;; SPECIFICATION
;;; Pre-condition: it takes two inputs. One of them is a preposition, and the other one is an association list alist
;;; Post-condition: if the components of the prepositon is in the association list, then it will output its value,
;;; and if it is not in the  association list, then it will return a null list '()

(define (translator-interpretor e alist)
  (interpretor (translator e) alist))


;;; TEST CASES:
(display "Translator-Interpretor: ") (newline)
(display "Test Case 1:") (newline)

(display "Translate (x ^ y) into OR(v) and NOT(~):")
(translator '(x ^ y))

(display "Truth value of (x ^ y): ")
(interpretor '(x ^ y) '((x #f) (y #t)))

(display "Truth value of (~ (~x v ~y)): ")
(interpretor '(~ ((~ x) v (~ y))) '((x #f) (y #t)))

(display "Truth value of (x ^ y) after tranlating into (~ (~x v ~y)): ")
(translator-interpretor '(x ^ y) '((x #f) (y #t))) (newline)

(display "Test Case 2:")(newline)
(display "Translate (x => y) into OR(v) and NOT(~):")
(translator '(x => y))

(display "Truth value of (x => y): ")
(interpretor '(x => y) '((x #f) (y #t)))

(display "Truth value of (~x v y): ")
(interpretor '((~ x) v y) '((x #f) (y #t)))

(display "Truth value of (x ^ y) after tranlating into (~x v y): ")
(translator-interpretor '(x => y) '((x #f) (y #t)))



;;; From the test case, we can see that our translator-inpertretor function works fine.
;;; We can see that translator function converts (x ^ y) into (~ (~x v ~y)).
;;; From, the translator we can see that they both have the same truth value; therefore, translator function works fine
;;; Now, from the translator-interpretor function, we can also see that we get the same truth value for (x ^ y).
;;; So, we can say that our translator-interpretor function is working properly because it is returning the same truth value
;;; after converting the (x ^ y) preposition into (~ (~x v ~y))