;;; --------------------------------------------------------------------------------------------------------------
;;; PART 1

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
;;; PART 2.1
;;; -------------------------------------------------------------------------------------------------------------
;;; LOAD CONSTRUCTORS/SELECTORS/CLASSIFIERS
(load "infix_doc.scm")
;;(load "prefix_doc.scm")

;(define (choose_operator e)
;  (cond ((eq? (car e) '^) (load "prefix_doc.scm"))
;        ((eq? (car e) 'v)  (load "prefix_doc.scm"))
;        ((eq? (car e) '=>)  (load "prefix_doc.scm"))
;        ((eq? (car e) '~)  (load "prefix_doc.scm"))
;        (else (load "infix_doc.scm"))))


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

;;; -------------------------------------------------------------------------------------------------------------
;;; PART 2.2
;;; -------------------------------------------------------------------------------------------------------------
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


;;; TEST CASES -----------------------
;;; by loading infix_doc.scm
(convert-or-not '((p => q) ^ (q => p)))

;;; by loading prefix_doc.scm
;;; (convert-or-not '(^ (=> p q) (=> q p)))


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
;;; LOOKUP FUNCTION

;;; SPECIFICATION
;;; Pre-condition: input a symbol x, and a association list alist
;;; Post-condition: if x is in the association list, then it will output its value,
;;;   if x is not iin the  association list, then it will return a null list '()

;;; DESIGN IDEA
;;; iterative procedure
;;; Check every pair of of association list, say current pair = (car alist), for the termination, if x is the first element of current pair,
;;; then it will terminates and return the value of second elements of current pair, and if we check through the list, the x does not appear
;;; in any pair of the association list

;;; orignal ALIST
;;; ------------------------------------------
;;; already checked pairs | not checked yet
;;; ------------------------------------------
;;;                     (alist head ....     )

(define (lookup x alist)
  (cond ((eq? (caar alist) x) (cadar alist)) ;; if x is in current pair
        ((eq? (cdr alist) '()) '()) ;; if x is not in the list
        (else (lookup x (cdr alist)))))

;;; PROOF
;;; we let original association list to be ALIST, 
;;; guess invariant: x appears in any pair of original ALIST iff x appears in alist

;;; Strong enough? when the program start, alist = ALIST, then our GI is true.
;;; Weak enough? when the program terminates, as we mentioned in design idea, there are two cases cause the program terminates
;;;   1. x was found in current head pair of alist, x must appears in the same pair of ALIST, then our GI is true
;;;   2. alist contains only a element of a pair and x is not appear in the pair, x is not in the orignal ALIST, our GI is true
;;; Preservable? We assume that GI is true before each iterative call, while the program does not terminates, that means x is not in current head pair,
;;;   and alist does contains at least 2 pairs, then for the next call new alist becomes (cdr alist), then our GI: x appears in any pair of ALIST iff x
;;;   appears in alist still maintained, since previous head pair does not contains x.

;;; Q.E.D

;;; TEST DATA
(lookup 'x '((x #f) (y #t) (z #f))); => #f
(lookup 'y '((x #f) (y #t) (z #f))); => #t
(lookup 'z '((x #f) (y #t) (z #f))); => #f
(lookup 'w '((x #f) (y #f) (z #f))); => ()


;;; -------------------------------------------------------------------------------------------------------------
;;; PART 2.4
;;; -------------------------------------------------------------------------------------------------------------\

;;; IMPLY FUNCTION

;;; SPECIFICATION
;;; Pre-condition: input 2 boolean value p and q
;;; Post-condition: returns the boolean value of p implies q

;;; USE p=>q <=> -pvq
(define (imply p q)
  (or (not p) q))

;;; TEST INPUTS
;;; (imply #t #t) => #t
;;; (imply #t #f) => #f
;;; (imply #f #t) => #t
;;; (imply #f #f) => #t

;;; VALUE FUNCTION

;;; SPECIFICATION:
;;; Pre-Condition: inputs a proposition e that was created using the constructors and a valid association list alist
;;;   (every symbol x appears in e can be founded with its associated value in alist)
;;; Post-Condition: it will returns the value of e that each symbol of e is evaluated to its aoociated value in alist

;;; CODE:
(define (value e alist)
  (cond ((symbol? e) (lookup e alist))
        ((not (not? e))
         (let ((first-op (first-operand e)) (second-op (second-operand e)))
           (cond ((and? e) (and (value first-op alist) (value second-op alist)))
                 ((or? e) (or (value first-op alist) (value second-op alist)))
                 ((imply? e) (imply (value first-op alist) (value second-op alist))))))
        (else
         (let ((first-op (first-operand e)))
           (not (value first-op alist))))))

;;; STRUCTURAL INDUCTION PROOF:

;;; Let P to be the least class that contains all the infix propersition

;;; BASIS:
;;; If the proposition is just a symbol, then we can directly check alist using lookup function. Since we assume our alist to be valid
;;; that is, every symbol x appears in e can be founded with its associated value in alist, so (lookup e alist) will return the boolean
;;; value of e when e is a symbol which is correct.

;;; INDUCTION HYPOTHESIS (IH)
;;; For any porposition p \in P, assume the value of all the components of p are true

;;; INDUCTION STEP:
;;; Here we have 2 cases:
;;;  1. p is a infix proposition with 2 operands (in other words, p is either an AND (^), OR (v), or IMPLY (=>)
;;;     proposition). r and s are components of p that p = (r op s). By the IH, we know the value of r and s are all true. 
;;;     Then, since we have all the corrected value of r and s, then the value of p is going to be (operator (value of r) (value of s))
;;;     which is true
;;;  2. p is an infix proposition with only one operand (p is NOT (-)). let r to be a component of p that p = -r. By the IH,
;;;     we know the value of r is true. then the value of p is going to be (not (value of r)) which is correct!


;;; Q.E.D

;;; TEST INPUTS
(value '((x v y) ^ (x => z)) '((x #t) (y #f) (z #t))) ;=> #t
;;;          (T v F) ^ (T => T) = T
(value '((x v y) ~) '((x #f) (y #t))) ;=> #f
;;;         (- (F v T)) = F