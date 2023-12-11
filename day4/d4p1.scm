(define (read-input filename)
  (let ((handle (open-input-file filename)))
    (let loop ((lines (list)))
      (let ((line (read-line handle)))
	(if (eof-object? line)
	    (begin (close-port handle)
		   (reverse lines))
	    (loop (cons line lines)))))))

(define (parse-numbers str)
  (car (fold-matches
	"[0-9|\|]+" str (cons (list (list) (list)) #:left)
	(lambda (m st)
	  (match-let ((((left right) . side) st)
		      (s (match:substring m)))
	    (cond
	     ((string= "|" s) (cons (list left right) #:right))
	     ((equal? side #:left) (cons (list (cons s left) right) side))
	     ((equal? side #:right) (cons (list left (cons s right)) side))))))))

(define (parse-game str)
  (match-let (((game numbers) (string-split str #\:)))
    (cons game (parse-numbers numbers))))

(define (intersection l1 l2)
  (cond
   ((null? l1) '())
   ((member (car l1) l2)
    (cons (car l1)
	  (intersection (cdr l1) l2)))
   (else (intersection (cdr l1) l2))))

 (define (fold-right f init seq)
   (if (null? seq) 
       init 
       (f (car seq) 
          (fold-right f init (cdr seq)))))

(define (calc-score hand winning)
  (let ((i (intersection hand winning)))
    (if (null? i) 0
      (fold-right (lambda (x acc) (* 2 acc)) 1 (cdr i)))))

(define (part1 file)
  (apply + (map (lambda (line)
		  (match-let (((h w) (cdr (parse-game line))))
		    (calc-score h w)))
		(read-input file))))
