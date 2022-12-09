;;; CONSTRUCTORS
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
        (else (list '~ e))))

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


;;; SELECTORS
;;; --------------------------------------------------------------------------------------------------------------
;;; FIRST-OPERAND FUNCTION
;;; Pre-condition: inputs an expression representing a proposition
;;; Post-condition: returns the expression representing the first operand of the inputed expression

;;; CODE
(define (first-operand e)
  (car e))

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
;;; Pre-condition: inputs an expression representing either a AND, OR, IMPLY proposition
;;; Post-condition: returns the operator of the inputed expression

;;; CODE
(define (operator e)
  (cond ((symbol? e) '())
        (else (cadr e))))

;;; --------------------------------------------------------------------------------------------------------------


;;; CLASSIFIERS
;;; --------------------------------------------------------------------------------------------------------------
;;; AND? FUNCTION
;;; Pre-condition: inputs an expression representing a proposition
;;; Post-condition: returns #t if the inputed expression is an AND expression, #f otherwise

;;; CODE
(define (and? e)
  (eq? (operator e) '^))

;;; --------------------------------------------------------------------------------------------------------------

;;; --------------------------------------------------------------------------------------------------------------
;;; OR? FUNCTION
;;; Pre-condition: inputs an expression representing a proposition
;;; Post-condition: returns #t if the inputed expression is an OR expression, #f otherwise

;;; CODE
(define (or? e)
  (eq? (operator e) 'v))

;;; --------------------------------------------------------------------------------------------------------------

;;; --------------------------------------------------------------------------------------------------------------
;;; NOT? FUNCTION
;;; Pre-condition: inputs an expression representing a proposition
;;; Post-condition: returns #t if the inputed expression is an NOT expression, #f otherwise

;;; CODE
(define (not? e)
  (eq? (operator e) '~))

;;; --------------------------------------------------------------------------------------------------------------

;;; --------------------------------------------------------------------------------------------------------------
;;; IMPLY? FUNCTION
;;; Pre-condition: inputs an expression representing a proposition
;;; Post-condition: returns #t if the inputed expression is an IMPLY expression, #f otherwise

;;; CODE
(define (imply? e)
  (eq? (operator e) '=>))

;;; --------------------------------------------------------------------------------------------------------------


;;; PART 1: FRONT-END
;;; --------------------------------------------------------------------------------------------------------------
;;; CONVERT-AND FUNCTION

;;; SPECIFICATION
;;; Pre-Condition: inputs a AND (^) proposition e that was created using the constructors
;;; Post-Condition: returns a proposition that is equivalent to e but using just NOTs (~) and ORs (v)
;;; NOTE: (p ^ q) is logically equivalent to ~(~p v ~q)

(define (convert-and e)
  (let ((first-op (first-operand e)) (second-op (second-operand e)))
    (let ((or-exp (make-or (make-not first-op) (make-not second-op))))
      (make-not or-exp))))

;;; --------------------------------------------------------------------------------------------------------------


;;; --------------------------------------------------------------------------------------------------------------
;;; CONVERT-IMPLY FUNCTION

;;; SPECIFICATION
;;; Pre-Condition: inputs an IMPLY (=>) proposition e that was created using the constructors
;;; Post-Condition: returns a proposition that is equivalent to e but using just NOTs (~) and ORs (v)
;;; NOTE: (p => q) is logically equivalent to (~p v q)

(define (convert-imply e)
  (let ((first-op (first-operand e)) (second-op (second-operand e)))
    (make-or (make-not first-op) second-op)))

;;; --------------------------------------------------------------------------------------------------------------


;;; --------------------------------------------------------------------------------------------------------------
;;; CONVERT-OR-NOT FUNCTION

;;; SPECIFICATION
;;; Pre-Condition: inputs a proposition e that was created using the constructors
;;; Post-Condition: returns a proposition that is equivalent to e but using just NOTs (~) and ORs (v)

(define (convert-or-not e)
  (cond ((symbol? e) e)
        ((not (not? e))
         (let ((first-op (first-operand e)) (second-op (second-operand e)))
           (cond ((and? e) (convert-and (make-and (convert-or-not first-op) (convert-or-not second-op))))
                 ((imply? e) (convert-imply (make-imply (convert-or-not first-op) (convert-or-not second-op))))
                 (else (make-or (convert-or-not first-op) (convert-or-not second-op))))))
        (else (let ((first-op (first-operand e)))
                (make-not (convert-or-not first-op))))))


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