
(define nothing #(*the-nothing*))
(define (nothing? thing)
  (eq? thing nothing))
  
;; definition of cells in the message-accepter form.

(define (content cell)
  (cell 'content))

(define (add-content cell increment)
  ((cell 'add-content) increment))

(define (new-neighbor cell neighbor)
  ((cell 'new-neighbor) neighbor))

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

;;; Propagators

(define (propagator neighbors to-do)
 (for-each (lambda (cell)
	     (new-neighbor! cell to-do))
	   (listify neighbors))
 (alert-propagator to-do))

;;; f->p-c imposes convention: list of cells is i1, ..., in, o
;;; where i's are input cells and o is the single output.

(define (function->propagator-constructor f)
  (lambda cells
    (let ((output (car (last-pair cells)))
	  (inputs (except-last-pair cells)))
      (propagator inputs
		  (lambda ()
		    (add-content output
				 (apply f (map content inputs))))))))
