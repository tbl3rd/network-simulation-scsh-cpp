#! /home/tbl/bin/scsh \
-lm modules.scm -dm -o test-network -e top -s
!#

; ARGS is a list of command line argument strings.
; (top '("test-network.scm" "./bridge_test" "2" "2" "2"))

(define-structure test-network
  (export top)
  (open conditions
        display-conditions
        handle
        scsh
        scheme
        network)
  (begin

    (define stderr (error-output-port))

    (define (display-error-lines . lines)
      (for-each (lambda (line) (display line stderr) (newline stderr)) lines))

    (define (display-usage args)
      (newline stderr)
      (display-error-lines
       (string-append
        "Usage: " (car args)
        " test-program #segments #sources #frames"))
      (newline stderr)
      (display-error-lines
       "Where: test-program is passed #segments file descriptors,"
       "       #segments is the number of LAN segments,"
       "       #sources is the number of frame sources,"
       "       #frames is the total number of frames to transmit,"
       "       all # arguments are positive integers,"
       "       and #sources is greater than 1.")
      (newline stderr)
      (display-error-lines
       (string-append
        "Example: " (car args) " ./bridge_test 16 256 2048"))
      (newline stderr)
      (display "Invoked:" stderr)
      (for-each
       (lambda (arg) (display (string-append " " arg) stderr))
       args)
      (newline stderr))

    (define (top args)
     (display-condition
      (call-with-current-continuation
       (lambda (continue)
        (with-handler
         (lambda (condition punt-is-ignored)
           (display-usage args)
           (continue condition)
           (exit 1))
         (lambda ()
	   (exit
	    (wait
	     ((apply make-network (map string->number (cddr args)))
	      (cadr args))))))))
      stderr))))


