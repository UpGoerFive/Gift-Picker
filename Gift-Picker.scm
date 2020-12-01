#lang racket

(define (gift-picker people inclusions exclusions)
  (show-gifts (pick-options (add-pairs (shuffle people) inclusions exclusions) '())))

(define (make-pairs people)
  (define (pairs-helper combos result)
    (cond [(null? combos) result]
	  [(null? result) (let ((new (combinations combos 2)))
			    (pairs-helper new new))]
	  [else (pairs-helper (cdr combos) (cons (list (cadar combos) (caar combos)) result))]))
  (pairs-helper people '()))

(define (remove-exclusions shuffled exclusions)
  (filter (lambda (x) (not (member x exclusions))) shuffled))

(define (add-pairs people inclusions exclusions)
    (combos-table people (force-inclusions inclusions (remove-exclusions (make-pairs people)  exclusions)) '()))

(define (force-inclusions inc combos)
  (if (null? inc)
    combos
    (force-inclusions (cdr inc) (filter (lambda (x) (or (equal? (car inc) x) (not (equal? (caar inc) (car x))))) combos))))

(define (combos-table names combos result)
  (if (null? names)
    result
    (let ((options (filter (lambda (x) (equal? (car x) (car names))) combos)))
      (combos-table (cdr names) combos (cons (list (car names) (length options) options) result)))))

(define (pick-options duplicates result)
  (if (null? duplicates)
    result
    (let ((choice (argmin cadr duplicates)))
      (if (= (cadr choice) 0)
	(error "Someone has no gift choice, either rerun or look at inclusions/exclusions")
	(pick-options (trim-list duplicates (caaddr choice) '()) (cons (caaddr choice) result))))))

(define (trim-list ordered choice result)
  (cond [(null? ordered) result]
	[(equal? (caar ordered) (car choice)) (trim-list (cdr ordered) choice result)]
	[else (let ((next (filter (lambda (x) (not (equal? (cdr choice) (cdr x)))) (caddar ordered))))
		(trim-list (cdr ordered) choice (cons (list (caar ordered) (length next) next) result)))]))

(define (show-gifts pairs)
  (if (null? pairs)
    'done
    (begin (display (caar pairs))
	   (display " gives a gift to: ")
	   (display (cadar pairs))
	   (newline)
	   (show-gifts (cdr pairs)))))
