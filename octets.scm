
; This is Scheme Shell (scsh) code to build binary image strings from
; other Scheme objects.  A binary image string is a string of 8-bit
; values (octets) that represents some value in the native process
; code of some processor.

; Define a bunch of handy procedures for mapping between printable
; external representations of some values as Scheme strings and their
; (potentially UNprintable) binary image representations also as
; string of octets.

; We define these mapping procedures because we need to build network
; packets (frames) corresponding to various standard protocol
; encapsulations, and the Scheme Shell (our platform of choice)
; represents packet data as Scheme strings.  Just to be difficult, the
; Scheme Shell also represents an IP address in process code as a
; native machine word (a C int).

; In other words, we get to do some bitfiddling in Scheme.  *groan*
; Some of the stuff in here has endian dependencies which I haven't
; bothered to abstract around yet.



; Define a procedure that maps an integer into its binary image as a
; string of octets.

; N is a Scheme integer.  Value is the binary image of the integer as
; a string of octets.  MIN-WIDTH is the minimum number of characters
; in the resulting string, defaulting to the minimum number of
; characters necessary to encode the integer.

; Note that (INTEGER->OCTETS 0)   ;=> "^@"
;           (INTEGER->OCTETS 7 3) ;=> "^@^@7"

; I probably could have used FORMAT somehow, but I don't know how.
; Also, these are named to mimic the standard NUMBER->STRING and
; STRING->NUMBER procedures, but note that the strings in this case
; are not necessarily portable (printable) Scheme strings.  Especially
; note that the definitions depend on scsh's nonstandard ASCII->CHAR
; and CHAR->ASCII bindings.

(define (integer->octets n . min-width)
  (let
      ((width (if (null? min-width) 0 (car min-width)))
       (char-zero (ascii->char 0))
       (bits/octet -8)
       (mask #xff)
       (non-negative
        (lambda (left right)
          (let ((difference (- left right)))
            (if (positive? difference) difference 0)))))
    (letrec
        ((pad-list
          (lambda (lst more)
            (if (zero? more) lst
                (cons char-zero (pad-list lst (- more 1))))))
         (build-list
          (lambda (n)
            (let loop ((result '()) (residual n) (len 0))
              (if (zero? residual)
                  (pad-list result (non-negative width len))
                  (loop (cons (ascii->char (bitwise-and mask residual)) result)
                        (arithmetic-shift residual bits/octet)
                        (+ len 1)))))))
      (if (zero? n) (make-string width char-zero)
          (list->string (build-list n))))))



; Define a useful pseudo-inverse of INTEGER->OCTETS.  OCTETS is a
; string of octets.  Value is the Scheme integer whose binary image
; is OCTETS.

(define (octets->integer octets)
  (let* ((bits/octet 8)
         (base (expt 2 bits/octet)))
    (let loop ((result 0)
               (coeffs (string->list octets))
               (exp (- (string-length octets) 1)))
      (if (null? coeffs) result
          (loop (+ result (* (char->ascii (car coeffs)) (expt base exp)))
                (cdr coeffs)
                (- exp 1))))))



; The standard printable external representation of an address in
; networking is often a string of integer-valued fields represented in
; some base radix and delimited by some recurring literal character.
; Each field represents an octet.  Here are some examples of various
; networking printstring formats:

;       A MAC address: "66:6e:6f:72:64:73"
;       An IP address: "99.104.97.111"
;       An SMI OID:    "1.3.6.6.2.1.11.213"

; There is usually also a standard way of representing a value denoted
; by one of these printable strings as a potentially unprintable
; stream of octets -- for transmission over a network for example.  It
; is handy to have a pair of procedures, STRING->OCTETS and
; OCTETS->STRING, to translate between the printable string and binary
; image string representations of these values.

; The goal is a DEFINE form to bind these two complementary procedures
; together in a more-or-less foolproof way.  First, define a procedure
; to build the two procedures, then embed it in a macro that keeps the
; common parameters consistent across the binding forms.



; Values are two procedures, call them STRING->OCTETS and
; OCTETS->STRING, that map addresses between printable external
; representations and their binary network representations.  Both
; representations are Scheme strings, but the network representation
; may not be printable.

(define (string<->octets delim . radix)
  (let ((re (regexp-quote delim))
        (base (if (null? radix) 10 (car radix))))
    (values
     (lambda (str)
       (apply string
              (map (lambda (x) (ascii->char (string->number x base)))
                   ((infix-splitter re) str))))
     (lambda (octets)
       (join-strings
        (map (lambda (x) (number->string (char->ascii x) base))
             (string->list octets)) re 'infix)))))



; (define-string<->octets-procedures delim s->o o->s)

; Define two procedures (s->o s) and (o->s o), such that
;    (equal? s (o->s (s->o s))) and (equal? o (s->o (o->s o)))
; are #t for all s and o.

; If STR is the printable string representation of an address with
; octet values separated by DELIM, then the value of (s->o STR) is
; an octet string with the binary network representation of the
; address.

; If OCTETS is the binary representation of an address in an octet
; string, then the value of (o->s OCTETS) is the printable string
; representation of OCTETS with the integer value of the octets
; separated by DELIM.

(define-syntax define-string<->octets-procedures
  (syntax-rules ()
    ((define-string<->octets-procedures delim radix s->o o->s)
     (begin
       (define s->o #f)
       (define o->s #f)
       (call-with-values
        (lambda () (string<->octets delim radix))
        (lambda (-> <-) (set! s->o ->) (set! o->s <-)))))
    ((define-string<->octets-procedures delim s->o o->s)
     (define-string<->octets-procedures delim 10 s->o o->s))))
