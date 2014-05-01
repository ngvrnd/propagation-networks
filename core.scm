;;; A distinguished nothing.  A kind of proto-zero:
;;; God may have created the integers, but nothing was here when he showed up.
(define nothing #(*the-nothing*))
(define (nothing? thing)
  (eq? thing nothing))
  
;;; cells. 

;;; definition of cells in the message-accepter form.
;;; a cell knows the messages content, add-content, and new-neighbor.
(define (make-cell)
  (let ((neighbors '()) (content nothing))
    (define (add-content increment)
      (cond ((nothing? increment)
	     'ok)
	    ((nothing? content)
	     (set! content increment))
	    (else
	     (if (not (default-equal? content increment))
		 (error "Ack! Inconsistency!")))))
    (define (new-neighbor! new-neighbor)
      (if (not (memq new-neighbor neighbors))
	  (begin
	    (set! neighbors (cons new-neighbor neighbors))
	    ;; welcome to the neighborhood:
	    (alert-propagator new-neighbor))))
    (define (me message)
      (cond ((eq? message 'content)
	     content)
	    ((eq? message 'add-content)
	     add-content)
	    ((eq? message 'new-neighbor)
	     new-neighbor)
	    (else
	     (error "Unknown message " message))))
    me))

;; a cell has contents.  Here is the functional view of the contents of a cell.
(define (content cell)
  (cell 'content))

;; functional form to add content to a cell.
(define (add-content cell increment)
  ((cell 'add-content) increment))

;; functional form to add a neighbor to a cell.
(define (new-neighbor cell neighbor)
  ((cell 'new-neighbor) neighbor))


;;; Propagators
;; the propagator itself: has neighbors and a thing to-do.

(define (propagator neighbors to-do)
 (for-each (lambda (cell)
	     (new-neighbor! cell to-do))
	   (listify neighbors))
 (alert-propagator to-do))

;;; function->propagator-constructor -- create a propagator based on a scheme function.
;;; f->p-c imposes the following convention: list of cells is i1, ..., in, o
;;; where i's are input cells and o is the single output.

(define (function->propagator-constructor f)
  (lambda cells
    (let ((output (car (last-pair cells)))
	  (inputs (except-last-pair cells)))
      (propagator inputs
		  (lambda ()
		    (add-content output
				 (apply f (map content inputs))))))))

;;; a simple wrapper around functions to handle nothings: f(nothing) is nothing.
(define (handling-nothings f)
  (lambda args
    (if (any nothing? args)
	nothing
	(apply f args))))


;; some basic propagators based on scheme functions.

(define adder (function->propagator-constructor (handling-nothings +)))
(define subtractor (function->propagator-constructor (handling-nothings -)))
(define multiplier (function->propagator-constructor (handling-nothings *)))
(define divider (function->propagator-constructor (handling-nothings /)))

(define absolute-value
  (function->propagator-constructor (handling-nothings abs)))
(define squarer
  (function->propagator-constructor (handling-nothings square)))
(define sqrter
  (function->propagator-constructor (handling-nothings sqrt)))


(define =?
  (function->propagator-constructor (handling-nothings =)))
(define <?
  (function->propagator-constructor (handling-nothings <)))
(define >?
  (function->propagator-constructor (handling-nothings >)))
(define <=?
  (function->propagator-constructor (handling-nothings <=)))
(define >=?
  (function->propagator-constructor (handling-nothings >=)))

(define inverter
  (function->propagator-constructor (handling-nothings not)))
(define conjunctor
  (function->propagator-constructor (handling-nothings boolean/and)))
(define conjoiner conjunctor)
(define disjunctor
  (function->propagator-constructor (handling-nothings boolean/or)))
(define disjoiner disjunctor)


