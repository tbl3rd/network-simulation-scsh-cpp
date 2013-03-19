
; This is Scheme Shell (scsh) code for some miscellaneous procedures
; used to build network simulations.


; Define some common procedures on lists ...

; LST is a list.  Value is a thunk that returns a value from LST
; whenever it is called.  A null argument is an error.

(define (list->generator lst)
  (if (null? lst) (error "(list->generator '())"))
  (let ((ring (list->vector lst)))
    (let ((length (vector-length ring))
          (index -1))
      (lambda ()
        (set! index (+ index 1))
        (vector-ref ring (modulo index length))))))


; Value is the list of N calls to GENERATE.

(define (list-of-n n generate)
  (if (zero? n) '()
      (cons (generate) (list-of-n (- n 1) generate))))


; L is a list.  Value is the result of removing A from L, using =? as
; the equality predicate.

(define (remove =? a l)
  (cond ((null? l) '())
        ((=? a (car l)) (cdr l))
        (else (cons (car l) (remove =? a (cdr l))))))





; Make generators of random integers of various sizes.  These use
; Scheme48's MAKE-RANDOM:
;       > (define random (make-random <seed>))
;       > (random)  =>  a pseudo-random number between 0 and 2^28


; Value is a thunk that generates random integers in the range [0,
; #xffff).  This is not terribly useful.

(define (make-random-16-bit-integer seed)
  (let ((generator (make-random seed))
        (bits16 #xffff))
    (lambda () (modulo (generator) bits16))))


; Value is a thunk that generates random integers in the range [0,
; #xffffffff).  This is useful for building random IP addresses.

(define (make-random-32-bit-integer seed)
  (let* ((generator (make-random seed))
         (bits16 #xffff)
         (digit (lambda () (modulo (generator) bits16))))
    (lambda () (+ (* (digit) bits16) (digit)))))


; Value is a thunk that generates random integers in the range [0,
; #xffffffffffff).  This is useful for building random MAC addresses.

(define (make-random-48-bit-integer seed)
  (let* ((generator (make-random seed))
         (bits24 #xffffff)
         (digit (lambda () (modulo (generator) bits24))))
    (lambda () (+ (* (digit) bits24) (digit)))))




; Values are unbuffered input and output ports on a Unix pipe.  This
; is just scsh's PIPE, except that the resulting ports are unbuffered.

(define (unbuffered-pipe)
  (let ((unbuffer
         (lambda (port)
           (set-port-buffering port bufpol/none) port)))
    (call-with-values pipe
                      (lambda (i o)
                        (values (unbuffer i) (unbuffer o))))))



; PATHNAME is a file of strings delimited by DELIM strings: a `cookie'
; file, in other words.  Value is a thunk that supplies a string from
; the file each time it is called, or #f when its source is exhausted.

(define (string-source pathname delim)
  (let ((reader (record-reader delim 'elide-delims 'trim))
        (port (open-input-file pathname)))
    (lambda ()
      (let ((result (reader port)))
        (if (eof-object? result)
            (begin (close port) #f)
            result)))))
