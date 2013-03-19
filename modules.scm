

; Forms for mapping between Scheme objects and their values
; represented as binary images in strings of octets, used
; to help build frames and so on.

(define-structure octets
  (export integer->octets
          octets->integer
          string<->octets
          (define-string<->octets-procedures :syntax))
  (open scheme
        scsh)
  (files octets))



; A random collection of procedures used in the simulator.

(define-structure miscellaneous
  (export list->generator
          list-of-n
          remove
          make-random-16-bit-integer
          make-random-32-bit-integer
          make-random-48-bit-integer
          unbuffered-pipe)
  (open big-scheme
        random
        scheme
        scsh)
  (files miscellaneous))



; A network simulator toolkit.  Currently export only make-network
; which returns a curried simulator procedure that takes the pathname
; of a bridge or router application to test.

(define-structure network
  (export make-network)
  (open scheme
        scsh
        octets
        miscellaneous)
  (files network))
