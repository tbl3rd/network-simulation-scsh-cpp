;;;
;;; Scheme and Scheme Shell (~tbl/bin/scsh) code to run node test.
;;; This is mostly R4RS code with a couple of lines of Shell code at
;;; the end to pipe Scheme output to a test binary compiled from C++
;;; Just ,load this file into the Scheme Shell to run the test.

;;; This is Scheme code to construct tree representations.  We use
;;; lists for everything so the external syntax is simple.  In other
;;; words, (write <node>) will print a tree of nodes in the syntax
;;; expected by the SaNode constructor, SaNode::SaNode(istream &).
;;;
;;; A <node> prints like this:
;;;
;;; (node (type "<name>") (id <n>) (<id> ...) (<node> ...))

;;; where <name> is the name of some node type, <n> is a non-negative
;;; integer, <id> is a '(id <n>), and (<thing> ...) is a, perhaps
;;; null, list of <thing>s.  The <id>s represent the ancestors of the
;;; node.  (This breaks what would be cycles in the graph that would
;;; lead to non-terminating output when printing list representations.)
;;;
;;; The <node>s are representations of descendent nodes.

;;; Make a type designator for the name named NAME.
;;;
(define (make-type name) (list 'type name))

;;; (make-id) creates a new unique identifier object: '(id <n>).
;;;
(define make-id
  (let ((previous-id 0))
    (lambda () (begin (set! previous-id (+ 1 previous-id))
                      (list 'id previous-id)))))

;;; Identifier and type accessors on nodes.
;;;
(define (node->id node) (caddr node))
(define (node->type node) (cadr node))

;;; Get the ancestors and descendents list from a node.
;;; Note that (node->ancestor node) is a list to enable
;;; generalization to DAGs in the future.
;;;
(define (node->ancestors node) (cadddr node))
(define (node->descendents node) (car (cddddr node)))

;;; Make DESCENDENT a descendent of NODE.
;;;
(define (add-descendent! node descendent)
  (set-car! (cddddr node) (cons descendent (node->descendents node))))

;;; Make a new node of type TYPE with the specified ANCESTORS and no
;;; descendents.
;;;
(define (make-node type ancestors)
  (let ((node (list 'node type (make-id) (map node->id ancestors) '())))
    (map (lambda (n) (add-descendent! n node)) ancestors)
    node))

;;; Make a graph from the list of anchor nodes in ANCHORS.
;;;
(define (make-graph anchors) `(graph ,@anchors))

;;; Make N nodes of type TYPE with ancestors in ANCESTORLIST.  The
;;; result is the resulting new nodes in a list of length N.
;;;
(define (make-many-nodes n type ancestorlist)
  (if (zero? n) '()
      (cons (make-node type ancestorlist)
            (make-many-nodes (- n 1) type ancestorlist))))

;;; Branch N nodes of type TYPE from each node in the list
;;; BRANCHPOINTS.  The result is the list of (* N (length
;;; BRANCHPOINTS)) new nodes.
;;;
(define (branch-many-nodes n type branchpoints)
  (apply append (map (lambda (b)
                       (make-many-nodes n type (list b))) branchpoints)))


;;; Build a sample graph: 2 anchors with three branches each, each
;;; resulting node branching to 5 more nodes, with 7 caps on top of
;;; those.  The external representation of anchors is a graph string
;;; that can be parsed by the SaNode constructor.
;;;
(define generic (make-type "generic"))
(define anchors (list (make-node generic '()) (make-node generic '())))
(define d1 (branch-many-nodes 3 generic anchors))
(define d2 (branch-many-nodes 5 generic d1))
(define caps (branch-many-nodes 7 generic d2))
(define graph (make-graph anchors))

;;; A Scheme Shell 'extended process form' to pipe a tree write into a
;;; node_test executable.
;;;
(run (| (begin (write graph)) (./node_test)))
