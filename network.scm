
; This is Scheme Shell (scsh) code for composing a network and
; simulating traffic sources.  The network represented is a set of
; endstations distributed over one or more LAN segments.  Each
; endstation sends frames to all the other endstations.



; Define a pair of string<->octets procedures for MAC addresses.
; (string->macaddress "66:6e:6f:72:64:73") ;=> "fnords"
; (macaddress->string "fnords") ;=> "66:6e:6f:72:64:73"

(define-string<->octets-procedures ":" 16
  string->macaddress
  macaddress->string)


; Generate a list of N random MAC addresses.

(define (make-macaddresses n)
  (map integer->octets
       (list-of-n n (make-random-48-bit-integer n))
       (list-of-n n (lambda () 6))))

; Generate a list of N random IP addresses.

(define (make-ipaddresses n)
  (map integer->octets
       (list-of-n n (make-random-32-bit-integer n))
       (list-of-n n (lambda () 4))))

; Generate a list of N random IP/MAC address pairs.

(define (make-addresses n)
  (let ((make-mac (list->generator (make-macaddresses n)))
	(make-ip (list->generator (make-ipaddresses n))))
    (let loop ((count n) (addrs '()))
      (if (zero? count) addrs
	  (loop (- count 1)
		(cons (list (make-mac) (make-ip)) addrs))))))


; Value is the binary image (octet) string of an Ethernet/IP frame.
; DST-ADDRS is the binary image string for the destination MAC
; and IP addresses.  SRC-ADDRS is the binary image string for the 
; source MAC and IP addresses.  DATA-STRING is the binary image of 
; the payload data.

(define (ethernet-frame dst-addrs src-addrs data-string)
  (let* ((mac-header-length 14)
	(pid #x0800)
	(version #x40)
	(ip-length #x05)
	(total-length (+ (* ip-length 4) (string-length data-string)))
	(pad 0))
    (string-append (car dst-addrs)
                   (car src-addrs)
                   (integer->octets pid 2)
		   (integer->octets (+ version ip-length) 1)
		   (integer->octets pad 1)
		   (integer->octets total-length 2)
		   (integer->octets pad 8)
		   (car (cdr src-addrs))
		   (car (cdr dst-addrs))
                   data-string)))

; Value is the binary image (octet) string of an 802.3 SNAP/IP frame.
; DST-ADDRS is the binary image string for the destination MAC
; and IP addresses.  SRC-ADDRS is the binary image string for the 
; source MAC and IP addresses.  DATA-STRING is the binary image of 
; the payload data.

(define (snap-frame dst-addrs src-addrs data-string)
  (let* ((snap-header-length 8)
	(pid #x0800)
	(version #x40)
	(ip-header-length #x05)
	(cntl #x03)
	(sap #xaa)
	(oui #x000000)
	(ip-length (string-length data-string))
	(total-length (+ snap-header-length 
			 (+ (* ip-header-length 4) ip-length)))
	(pad 0))
    (string-append (car dst-addrs)
                   (car src-addrs)
                   (integer->octets total-length 2)
		   (integer->octets sap 1)
		   (integer->octets sap 1)
		   (integer->octets cntl 1)
		   (integer->octets oui 3)
		   (integer->octets pid 2)
		   (integer->octets (+ version ip-header-length) 1)
		   (integer->octets pad 1)
		   (integer->octets ip-length 2)
		   (integer->octets pad 8)
		   (car (cdr src-addrs))
		   (car (cdr dst-addrs))
                   data-string)))


; Write the octet string FRAME on the output port SEGMENT.  Use a
; single write to keep the operation atomic.

(define (put-frame segment frame)
  (write-string
   (string-append (integer->octets (string-length frame) 4) frame)
   segment))



; Construct an endstation.  An endstation is a procedure (PROC
; DATA-STRING) that constructs a frame with payload string
; DATA-STRING on output port SEGMENT with source MAC/IP address 
; from ADDR-PAIR and destination addresses chosen from the list 
; of MAC/IP addresses DESTINATIONS.

(define (endstation segment addr-pair destinations)
  (let ((make-destination (list->generator destinations)))
    (lambda (data-string)
      (let ((dst-pair (make-destination)))
        (put-frame segment (ethernet-frame dst-pair addr-pair data-string))))))



; Distribute endstations with addresses from ADDRESSES over points of
; attachment in the list SEGMENTS.  Value is a list of procedures
; (PROC STR) that construct LAN frames with source and destination MAC/IP
; addresses chosen from ADDRESSES with payload STR and write them to a
; port selected from segments.

(define (distribute-endstations segments addresses)
  (let* ((segment (list->generator segments))
         (make-endstation
          (lambda (addr-pair)
            (lambda (content)
              ((endstation (segment) addr-pair 
                           (remove equal? addr-pair addresses)) content)))))
    (map make-endstation addresses)))

; Commence endstation traffic.  ENDSTATION-GENERATOR is a thunk that
; returns an endstation procedure.  DATA-SOURCE is a thunk that
; returns a payload string for the data portion of a frame.

; The resulting traffic is a sequence of events.  Each event consists
; of an endstation from ENDSTATION-GENERATOR sending a frame
; containing data from DATA-SOURCE to one of the other endstations
; that it knows about.

; Value is unspecified, but this returns when DATA-SOURCE returns #f.

(define (generate-traffic endstation-generator data-source)
  (letrec ((send-message
            (lambda ()
              (let ((data (data-source)))
                (if data
                    (begin ((endstation-generator) data)
                           (send-message)))))))
    (send-message)))




; Create COUNT pipes, reveal the input ports for passing to a child
; process.  Value is a pair of lists: the list of input ports for the
; read ends of the pipes, and a list of output ports for the write
; ends of the pipes for use by scsh.  Each pipe port represents a
; point of attachment to a simulated LAN.

(define (make-segments count)
  (let loop ((n count) (ins '()) (outs '()))
    (if (zero? n) (cons ins outs)
        (let ((ports (call-with-values unbuffered-pipe cons)))
          (loop (- n 1) (cons (car ports) ins) (cons (cdr ports) outs))))))



; Make a source of strings that uses the Emacs Zippy the Pinhead quote
; database (cookie file) to generate N strings followed by #F.

(define (make-data-source n)
  (let ((n n) (port #f)
              (reader (record-reader "\000" 'elide-delims 'trim)))
     (let ((read-one (lambda () (reader port)))
          (open-port!
           (lambda ()
             (set! port (open-input-file
                         "/opt/gnu/share/emacs/19.30/etc/yow.lines"))))
          (close-port! (lambda () (close port) (set! port #f)))
          (count-sub1! (lambda () (set! n (- n 1))))
          (count-zero? (lambda () (zero? n))))
      (open-port!)
      (lambda ()
        (cond ((count-zero?) (close-port!) #f)
              (else (count-sub1!)
                    (let ((first-try (read-one)))
                      (cond ((eof-object? first-try)
                             (close-port!) (open-port!) (read-one))
                            (else first-try)))))))))



; Set up a network of STATION-COUNT endstations distributed over
; SEGMENT-COUNT LAN segments.  Value is a procedure (PROC RECEIVER).

; RECEIVER is the pathname of an executable program file that is
; passed a list of SEGMENT-COUNT open file descriptors on its command
; line.  Each file descriptor represents a point of attachment of one
; LAN segment.  PROC delivers the traffic resulting from the network
; to the file descriptors representing the points of attachment.

(define (make-network segment-count station-count frame-count)
  (let* ((segment-pipes (make-segments segment-count))
         (addresses (make-addresses station-count))
         (data-source (make-data-source frame-count))
         (endstation-generator
          (list->generator
           (distribute-endstations (cdr segment-pipes) addresses))))
    (lambda (receiver)
      (let ((bridge
             (fork
              (lambda ()
                (for-each close (cdr segment-pipes))
                (apply exec receiver
                       (map (lambda (port) (number->string (port->fdes port)))
                            (car segment-pipes)))))))
        (for-each close (car segment-pipes))
        (generate-traffic endstation-generator data-source)
        (for-each close (cdr segment-pipes))
        (for-each release-port-handle (car segment-pipes))
        bridge))))
