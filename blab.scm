#!/usr/bin/ol --run

;;;
;;; Blab - a production system
;;;

;; todo: exec isn't actually directly executable as it should be, which costs cpu time during expansion
;; todo: fd-writer should be buffered
;; todo: add different generators and allow selecting them (+ randomize w/ priorities)
;; todo: check env passing direction wrt upreferences (already ok?)
;; maybe: preprocess libraries to fasls for rapid loading

;; up- and backreference planning:
;; (thing \(bound\) thing ...)* makes sense, but now the backreference binding always
;; refers to the last expansion. otoh in upreferences (thing @bound thing ...)* ... \bound 
;; is intended to link to a random expansion within the kleene star. it would be nice 
;; to combine these so that a local repetition + backreference means a random one just
;; like in upreferences. 
;; in practice, carry an env and push an index along with expansion to it? 
;; can it work nested?
;; could the same env be used for upreferences? probably yes
;; fixme: \(42\)* \1 should initialize backreference \1 with ε so that it will always succeed


;;; Data types
;
; node types (come from parser, go to exec construction)
;  - #(union lst)
;  - #(union-dep ((branch . deps) ...))
;  - #(bytes bvec) 
;  - #(sequence lst) 
;  - #(blank) 
;  - #(repeat min max obj) 
;  - #(done a b) 
;  - #(lib name entry (defn ...))  
;  - #(ref names) 
;  - #(defn label node)
;  - #(label <id> node)              -- used for foo = ([a-z]+) \2

; exec type (used by actual data construction)
;  grammar = a vector 
;  E = number, production at vec[E]
;    | #(sequence (E ...))
;    | #(union V), V = vector of E
;    | #(repeat min max E) → #(repeating min chosen-max E)
;    | #(repeating min max E)
;    | byte-vector, raw output data

(import (settings)) 

;;; globals and defaults

(define version-str "Blab 0.2a")

(define default-output "-") ;; default to writing to stdout, can be changed with -o
(define default-nodes "100000") ;; output will usually be <50Kb, can be changed with --nodes 


;;; Requirements

(import 
   (owl parse)
   (owl args)
   (owl iff)
   (only (owl unicode) encode-point))


(define (number->digits num base done)
   (if (< num base)
      (cons (+ 48 num) done)
      (number->digits (div num base) base
         (cons (+ 48 (rem num base)) done))))

(define (number->string num)
   (bytes->string
      (if (< num 0)
         (cons 45 (number->digits (- 0 num) 10 null))
         (number->digits num 10 null))))


;;;
;;; Verbosity-aware output (not in use here yet)
;;;

(define (change-verbosity delta) 
   (mail 'output (tuple 'change-verbosity delta)))

(define-syntax output
   (syntax-rules ()
      ((output thing ...)
         (mail 'output (tuple 'output 1 (list thing ...))))))

(define-syntax debug
   (syntax-rules ()
      ((debug thing ...)
         (mail 'output (tuple 'output 2 (list thing ...))))))

(define-syntax record-time
   (syntax-rules ()
      ((record-time label ... operation)
         (lets
            ((start (time-ms))
             (val operation)
             (elapsed (- (time-ms) start)))
            (debug label ... ": " elapsed "ms")
            val))))

;; stdout | stderr
(define output-message-port stderr)

;; thread receiving output messages and remembering how much we want to hear
(define (output-server)
   (let loop ((limit 0)) ; be quiet by default
      (lets
         ((env (wait-mail))
          (from msg env))
         (tuple-case msg
            ((output verbosity lst)
               (if (<= verbosity limit)
                  (write-bytes stderr (foldr render '(10) lst)))
               (loop limit))
            ((change-verbosity delta)
               (loop (+ limit delta)))
            ((verbosity n)
               (loop n))
            (else
               (write-bytes stderr (foldr render '(10) (list "Bad output: " env)))
               (loop limit))))))


;;;
;;; Nodes
;;;

(define nothing (vector))

(define epsilon (tuple 'bytes nothing))

(define (mk-union lst)
   (cond 
      ((null? lst) epsilon)
      ((null? (cdr lst)) (car lst))
      (else (tuple 'union lst))))
      
;;;
;;; Parsing
;;;

(define (char-ff str)
   (str-fold (λ (seen rune) (put seen rune #true)) empty str))

(define label-leading-chars 
   (char-ff "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(define decimal-chars (char-ff "0123456789"))

(define label-special-chars (char-ff "-_'"))

; #(rep min max thing), marking {rep min,max}]
(define (repetition min max thing) (tuple 'repeat min max thing))
(define (repetition? node) (and (tuple? node) (eq? 'repeat (ref node 1))))

; #(expansion pos min end), repetition whose size has been decided and is being expanded
(define (expansion? node) (and (tuple? node) (eq? 'expansion (ref node 1))))

(define (k a b) a)

(define label-chars
   (ff-union label-leading-chars
      (ff-union decimal-chars label-special-chars k)
      k))

(define blank-chars '(9 10 13 32))

(define get-comment
   (let-parses
      ((skip (get-imm 35))
       (skip (get-greedy* (get-rune-if (lambda (x) (not (eq? x 10)))))))
      'blank))

(define drop-blanks
   (get-greedy*
      (get-either
         (get-rune-if 
            (lambda (rune) (has? blank-chars rune)))
         get-comment)))

; labels must be matched greedily, or foo = bar\nbaz = quux would be parsed as (foo → bar ba) (z → quux)
(define parse-label
   (let-parses
      ((skip drop-blanks)
       (head 
         (get-rune-if 
            (λ (rune) (get label-leading-chars rune #false))))
       (tail 
         (get-greedy*
            (get-rune-if 
               (λ (rune) (get label-chars rune #false))))))
      (runes->string (cons head tail))))

; foo[.bar[.baz ...]]
(define parse-name
   (let-parses
      ((name parse-label)
       (subs
         (get-greedy*
            (let-parses
               ((skip (get-imm 46)) ; . 
                (name parse-label))
               name))))
      (tuple 'ref (cons name subs))))

;; 0b1010xxxx -> mask 0b00001111, want 0x10100000
(define (make-bitfield bits pos mask want)
   (cond
      ((null? bits) ;; for (rand-byte & mask) | want
         ;(print (list 'bitfield mask want))
         (tuple 'bitfield mask want))
      ((eq? (car bits) 'x) ;; can be any
         (make-bitfield (cdr bits) (<< pos 1) (bor pos mask) want))
      ((eq? (car bits) 0) ;; wanted 0
         (make-bitfield (cdr bits) (<< pos 1) mask want))
      (else ;; wanted 1
         (make-bitfield (cdr bits) (<< pos 1) mask (bor pos want)))))

(define get-bitfield-elem 
   (get-any-of
      (let-parses ((skip (get-imm 48))) 0)
      (let-parses ((skip (get-imm 49))) 1)
      (let-parses ((skip (get-imm 95))) 'x))) ;; _

(define parse-bitfield
   (let-parses
      ((skip (get-imm 48)) ;; 0
       (skip (get-imm 98)) ;; b
       (bits (get-greedy* get-bitfield-elem))
       (verify (= (length bits) 8) "didn't get 8 bits"))
      (make-bitfield (reverse bits) 1 0 0)))


;; ff of byte → value, seems silly to open code this, but looks clearer
(define hexes
   (list->ff
      '((48 . 0) (49 . 1) (50 . 2) (51 . 3) (52 . 4) (53 . 5) (54 . 6) (55 . 7) (56 . 8) (57 . 9) ; 0-9
        (97 . 10) (98 . 11) (99 . 12) (100 . 13) (101 . 14) (102 . 15) ; a-f
        (65 . 10) (66 . 11) (67 . 12) (68 . 13) (69 . 14) (70 . 15)))) ; A-F
        
(define get-hex
   (let-parses
      ((byte get-byte)
       (verify (get hexes byte #false) "bad hex"))
      (get hexes byte 'bug)))

(define get-octet
   (let-parses
      ((hi4 get-hex)
       (lo4 get-hex))
      (+ (<< hi4 4) lo4)))

; \x<hex><hex> → 8-bit char 
(define get-quoted-octet
   (let-parses
      ((skip (get-imm 92))   ; \
       (skip (get-imm 120))  ; x
       (val get-octet))
      val))

; \u<hex><hex><hex><hex> → 16-bit char
(define get-quoted-short
   (let-parses
      ((skip (get-imm 92))   ; \
       (skip (get-imm 117))  ; u
       (hi8 get-octet)
       (lo8 get-octet))
      (+ (<< hi8 8) lo8)))

(define get-any-char
   (get-any-of
      get-quoted-octet
      get-quoted-short
      get-rune))

;; \... in string
(define quoted-rune
   (get-any-of
      (let-parses ; \x<hex><hex>
         ((skip (get-imm 120))
          (val get-octet))
         val)
      (let-parses ; \u<hex>{4}
         ((skip (get-imm 117))
          (hi8 get-octet)
          (lo8 get-octet))
         (bor (<< hi8 8) lo8))
      (get-imm 92) ; \\
      (let-parses ((skip (get-imm 110))) 10)   ; \n = 10
      (let-parses ((skip (get-imm 114))) 13)   ; \r = 13
      (let-parses ((skip (get-imm 116)))  9)   ; \t =  9
      (let-parses ((skip (get-imm  #\"))) #\")   ; \" =  "
      (let-parses ((skip (get-imm  #\'))) #\')   ; \" =  "
      ))

;; todo: no special quotations yet (\n, \t, \012, \xff...)
(define (string-rune delim)
   (get-either
      (let-parses
         ((skip (get-imm #\\))
          (rune quoted-rune))
         rune)
      (get-rune-if
         (λ (x) (not (eq? x delim))))))

(define (parse-delimited-string delim)
   (let-parses
      ((skip drop-blanks)
       (skip (get-imm delim)) ; opening "
       (runes (get-greedy* (string-rune delim)))
       (skip (get-imm delim))) ; closing "
      (tuple 'bytes 
         (list->vector 
            (string->bytes 
               (runes->string runes))))))

(define parse-string
   (get-either
      (parse-delimited-string #\")
      (parse-delimited-string #\')))

(define parse-natural-number
   (let-parses
      ((chars (get-greedy+ (get-byte-if (λ (byte) (get decimal-chars byte #false))))))
      (fold (λ (n b) (+ (* n 10) (- b 48))) 0 chars)))

(define parse-integer
   (let-parses
      ((skip drop-blanks)
       (sign 
         (get-any-of
            (get-imm 43)   ; + 
            (get-imm 45)   ; -
            (get-epsilon 43))) ; default +
       (value parse-natural-number))
      (if (eq? sign 45)
         (- 0 value)
         value)))

(define (mock x) (λ () (x (mock x))))

(define (drop-operator op)
   (let-parses
      ((skip drop-blanks)
       (skip (get-imm op)))
      op))

; returns a list of unicode code points
(define parse-char-option
   (let-parses
      ((a get-any-char)
       (verify (not (eq? a 93)) "bad char class end") ;; FIXME: will mach also if quoted
       (b
         (get-either
            (let-parses
               ((skip (get-imm #\-)) ;; [a-]b
                (b get-any-char)
                (verify (not (eq? b 93)) "bad char class end")
                (verify (<= a b) "bad range end"))
               b)
            (get-epsilon a))))
      (iota a 1 (+ b 1))))

(define parse-char-class
   (let-parses
      ((open (get-imm 91))
       (chars (get-greedy* parse-char-option))
       (close (get-imm 93)))
      (let ((chars (foldr append null chars))) ;; keep multiple occurrences intentionally to allow [aab] for changing probability
         (mk-union
            (map (λ (cp) (tuple 'bytes (list->vector (encode-point cp null)))) chars)))))

(define (parse-anon rec)
   (let-parses
      ((skip (drop-operator #\()) ; open sesame
       (val (rec))
       (skip (drop-operator #\)))) ; close sesame
      val))

;; \(exp*\) → create binding for use with \n
;; \( and \) are used as in regexps, where \ is used to differentiate between run-of-the-mill grouping
(define (parse-binding rec)
   (let-parses
      ((skip (drop-operator #\\))
       (skip (drop-operator #\())
       (val (rec))
       (skip (drop-operator #\\))
       (skip (drop-operator #\))))
      (tuple 'bind val)))

(define (parse-upref-binding rec)
   (let-parses
      ((skip (drop-operator #\@))
       (val parse-name))
      (begin
         (tuple 'bind-up val))))

; parse {a,[b]} (a . a|b|#false), or #false if not there
;; todo: also accept + and * as shorthands
(define parse-range
   (get-any-of
      (let-parses
         ((skip (drop-operator #\{))
          (a parse-natural-number)
          (b (get-either
               (let-parses
                  ((skip (drop-operator #\,))
                   (skip drop-blanks)
                   (val (get-either parse-natural-number (get-epsilon #false))))
                  val)
               (get-epsilon a)))
          (verify (or (not b) (<= a b)) "bad range")
          (skip (drop-operator #\})))
         (cons a (if b (+ b 1) b))) ;; b +1 to be able to use directly for ranges
      (let-parses ; * = {0,} (aka Kleene star)
         ((skip (drop-operator #\*)))
         (cons 0 #false))
      (let-parses ; + = {1,}
         ((skip (drop-operator #\+)))
         (cons 1 #false))
      (let-parses ; ? = {0,1}
         ((skip (drop-operator #\?)))
         (cons 0 2)))) ;; note, limit is 1+ to be able to use for rand directly

;; todo: merge mergeable sections in rhses later, like strings and bytes
(define parse-single-byte 
   (let-parses
      ((n parse-natural-number)
       (verify (< n 256) "byte too large"))
      (tuple 'bytes (vector n))))

;; \2 | \? | \foo
(define parse-reference
   (let-parses
      ((skip (get-imm #\\))
       (val
         (get-any-of
            (let-parses
               ((pos parse-natural-number))
               (tuple 'backref pos))
            (let-parses
               ((skip (get-imm #\?)))
               (tuple 'backref #false)) ;; arbirary
            (let-parses
               ((node parse-name)) ;; parse-name returns #(ref <name>) AST node
               (tuple 'upref (ref node 2))))))
      val))

(define (parse-rhs-element self)
   (let-parses
      ((skip drop-blanks)
       (value
         (get-any-of
            parse-name
            parse-bitfield
            parse-single-byte
            parse-string
            parse-char-class
            parse-reference
            (parse-anon self)
            (parse-binding self)
            (parse-upref-binding self)))
       (range (get-either parse-range (get-epsilon #false))))
      (if range
         (tuple 'repeat (car range) (cdr range) value)
         value)))

(define (flatten vals)
   (let ((vals (foldr (λ (val tl) ((if (pair? val) append cons) val tl)) null vals)))
      (if (null? (cdr vals))
         (car vals)
         (tuple 'sequence vals))))

(define (parse-rhs self)
   (let-parses 
      ((vals (get-kleene+ (parse-rhs-element self))))
      (flatten vals)))

(define (parse-options self)
   (let-parses
      ((first (parse-rhs self))
       (opts 
         (get-kleene*
            (let-parses
               ((skip (drop-operator 124)) ; ceci n'est ..
                (val (parse-rhs self)))
               val))))
      (if (null? opts)
         first
         (tuple 'union (cons first opts)))))

(define (parse-library gram)
   (let-parses
      ((name parse-label)
       (skip (drop-operator 123))
       (stuff (gram))
       (skip (drop-operator 125)))
      (tuple 'lib name stuff)))

(define (parse-rule gram)
   (get-either
      (parse-library gram)
      (let-parses
         ((name parse-label)
          (drop (drop-operator 61))
          (opts (parse-options (mock parse-options))))
         (tuple 'defn name opts))))

;  → (library-node ...)
(define (parse-grammar)
   (get-either
      ;; a grammar with definitions
      (let-parses
         ((vals (get-kleene+ (parse-rule parse-grammar)))
          (skip drop-blanks))
         vals)
      ;; a single regexpish definiton
      (let-parses
         ((exp (parse-options (mock parse-options)))
          (skip drop-blanks))
         (list (tuple 'defn "start" exp)))))

(define (parse-toplevel name stdlib)
   (let-parses
      ((body (parse-grammar)))
      (tuple 'lib name
         (if (pair? body)
            (append body stdlib)
            (cons (tuple 'defn "start" body) stdlib)))))


;;;
;;; AST → exec
;;;

;; nodes = #(free X)
(define blank-nodes (tuple 0 null)) ;; #(free x trie-node|#false)

;; save path to current value of counter and increment
(define (insert nodes path) 
   (lets ((id l nodes))
      (tuple (+ id 1)
         (cons (append path (list id)) l))))

;; pat path → id | #false
(define (match pat path)
   (cond 
      ((null? path) 
         (if (and (pair? pat) (number? (car pat)))
            (car pat)
            #false))
      ((null? pat) #false)
      ((equal? (car pat) (car path))
         (match (cdr pat) (cdr path)))
      (else #false)))

;; naive O(n) searches. switch to a tree later.
;; nodes path → id | #false
(define (lookup nodes path)
   (cond
      ((null? nodes) #false)
      ((match (car nodes) path) => (λ (id) id))
      (else (lookup (cdr nodes) path))))

(define (format-ref labels)
   (runes->string
      (foldr 
         (λ (x tl) 
            (let ((tl (if (null? tl) tl (cons 46 tl))))
               (str-foldr cons tl x)))
         null labels)))

;; assign a unique number to all defined labels and store them
(define (collect-labels nodes node path fail) ; → nodes'
   (tuple-case node
      ((lib name lst)
         (let ((path (append path (list name))))
            (for nodes lst
               (λ (ns node) (collect-labels ns node path fail)))))
      ((defn name node)
         ;; libraries are not first class, so the node cannot contain further libs
         (let ((here (append path (list name))))
            (if (lookup (ref nodes 2) here) ;; full equal path already there?
               (begin
                  (output " - dropping redefinition of " (format-ref (cdr here)) " in " (car here) " being " node)
                  ;; replace with a blank to preserve order
                  (insert nodes null)) 
               (begin
                  (debug " - " name " = #" (ref nodes 1))
                  (insert nodes here)))))
      (else
         (fail (list "collect-labels: funny node " node) #false))))

;; collect definition values in the same order that collect-labels labels them
;; these after postprocessing become the rule vector
(define (collect-rhses node)
   (define (walk rout node)
      (tuple-case node
         ((lib name lst) (fold walk rout lst))
         ((defn name node) (cons node rout))
         (else (error "collect-rhses: bad node: " node))))
   (reverse (walk null node)))

;; node → mapping of paths to ids, being numbers 0..n
(define (collect-definitions node fail)
   (ref (collect-labels blank-nodes node null fail) 2))

;; environment search, find the nearest parent frame which has the desired value
(define (tails l)
   (if (null? l)
      (list null)
      (cons l (tails (cdr l)))))

(define (options path names)
   (map (λ (path) (fold (λ (a b) (cons b a)) names path)) ; append to reversed head 
      (tails (reverse path))))

(define (lookup-nearest labels path names)
   (let loop ((opts (options path names)))
      (cond
         ((null? opts) #false)
         ((lookup labels (car opts)) => (λ (id) id))
         (else (loop (cdr opts))))))

;; convert all names to use the labels in labels
;;                        .-----------------> list of library names where we are now
;;                       |     .------------> all labels collected before
;;                       |     |      .-----> cont via which to exit if e.g. an undefined nonterminal is found
(define (make-exec node path labels fail) ; → node'
   (tuple-case node
      ((union lst)
         ;; exec union is a vector (to get roughly O(1) size and ref)
         (tuple 'union
            (list->vector
               (map (λ (node) (make-exec node path labels fail)) lst))))
      ((bytes bvec) 
         ;; raw data is raw
         node)
      ((sequence lst)
         ;; sequence is a list of nodes to be expanded
         (tuple 'sequence 
            (map (λ (node) (make-exec node path labels fail)) lst)))
      ((repeat min max node)
         (tuple 'repeat min max
            (make-exec node path labels fail)))
      ((ref names)
         (let ((id (lookup-nearest labels path names)))
            (if id
               id ; <- number
               (fail null names)))) ; <- we need this root variable (or library) to complete the grammar
      ((upref names)
         (let ((id (lookup-nearest labels path names)))
            (if id
               (tuple 'upref id)
               (fail null names))))
      ((lib name lst)
         (let ((path (append path (list name))))
            (tuple 'lib name
               (map (λ (n) (make-exec n path labels fail)) lst))))
      ((defn name val)
         (tuple 'defn name
            (make-exec val path labels fail)))
      ((bitfield mask set)
         node)
      ((bind exp)
         (tuple 'bind 
            (make-exec exp path labels fail)))
      ((backref num)
         ;; index of the match does not change
         node)
      ((bind-up val)
         (tuple 'bind-up
            (make-exec val path labels fail)))
      (else
         (error "make-exec: funny node " node))))

(define (read-data path)
   (if (equal? path "-")
      ;; read all of stdin
      (list->vector (port->byte-stream stdin))
      (file->vector path)))

(define (try-load-library path)
   (output "Trying to load " path)
   (let ((data (record-time "   + reading " path (file->vector path))))
      (if data
         (let ((ast (record-time "   + parsing " path (try-parse (parse-toplevel path null) (vec-iter data) path "bad grammar: " #false))))
            (if ast
               (tuple-case ast
                  ((lib name defns)
                     ;(print "Loaded.")
                     defns)
                  (else #false))
               #false)) 
         #false))) 

;; node → exec | missing-str | #false + printed error message
(define (ast->exec node libs)
	(define (try node)
		(call/cc
			(λ (ret)
				(define (fail whys unbound)
					(if unbound
                  (begin
                     (ret unbound))
						(begin
							(print* (cons "error: " whys))
							(ret #false))))
				(list->vector
					(collect-rhses 
						(make-exec node null 
							(record-time "   + collecting defns" (collect-definitions node fail))
							fail))))))
	(let loop ((node node) (loaded null))
      (let ((next (try node)))
         (cond
            ((pair? next) ;; (foo bar baz)
               (debug "looking for " next)
               (if (mem string-eq? loaded (car next)) ;; wanted foo[.x] and foo.blab already loaded -> fail
                  (begin
                     (print "The library did not contain " (format-ref next))
                     #false)
                  (lets
                     ((paths (map (λ (dir) (foldr string-append "" (list dir "/" (car next) ".blab"))) libs))
                      (lib (fold (λ (old path) (or old (try-load-library path))) #false paths)))
                     (if lib
                        (loop
                           ;; push the included library to toplevel
                           (set node 3 (append (ref node 3) lib))
                           ;; mark as loaded
                           (cons (car next) loaded))
                        (begin
                           ;; todo - collect all names and suggest a fix if one is similar enough
                           (if (= (length paths) 1)
                              (print* (list "Could not find " (car paths) " needed for '" (format-ref next) "'"))
                              (print* (list "Could not find any of " paths " needed for '" (format-ref next) "'")))
                           #false)))))
            ((not next)
               #false)
            (else next)))))


;;;
;;; Upreference dependency analysis (adds union-dep nodes, may reject grammar)
;;;

;; Some RHSes can contain upreferences. An upreference causes the expansion of 
;; a previous nonterminal to be inserted to current position, where previous 
;; means left hand side of current or any parent node in the (partially constructed)
;; parse tree. The nonterminal expansions which can be referred are at least for
;; now marked, but could be implicit in the future.

;; Upreferences can cause some expansions or the whole grammar to be unsatisfiable, 
;; due to missing parent expansions for upreferences.

;; This pass checks all upreferences and propagages the information upwards so that
;;  - each upreference has at least one possible expansion in which there is a node 
;;    which to refer (no checking finiteness for now)
;;  - each union node in which some branches always cause an upreference to be 
;;    expanded is changed to a dependent-union node, the dependencies of whose 
;;    branches are checked to be satisfiable at expansion time.

; operation
;  - start with entry rule and grammar
;  - have an empty dependency map
;  - compute deps for all expansions of entry rule
;    + if it depends on something, fail and print the missing reference
;  - compute 
;    + a mapping of rules → deps (missing = none)
;    + a new body in which union nodes have been switched 
;      - only when some but not all of the branches have deps
;      - if all have deps, propagate the info upwards to next union or 
;        rule info if it comes out of whole RHS


;;;
;;; Union sorting
;;;

;; the nasty thing about regular languages is that the valid outputs (or inputs) 
;; can be infinitely long. for context free grammar even the branches can be 
;; infinitely deep without even producing any output.

;; summon the pumping lemma (or more precisely the proof of it) to the rescue. 
;; an inifinitely long derivation has an infinitely long path from the derivation 
;; tree root to the leaf, but there is only a finite set of nonterminals, so some 
;; of them must occur multiple times in the path. also note that the shortest 
;; derivation of any rule cannot contain such repetition.

;; we use this property to sort each node where there are multiple choices (the 
;; union nodes) simply by the minimal expansion size of of each of the options. 
;; this way the expander can make smaller outputs by favoring the preceding 
;; options, and is quaranteed to get a finite derivation by choosing the first 
;; one, because we threat rules which do not have finite expansions as errors.

(define (less? a b)
   (cond
      ((not b) #true) 
      ((not a) #false)
      (else (< a b))))

;; compute size of <node> by DP, storing and looking up ids from db
;; this is used to compute the db, and later using the precomputed db 
;; to compute sizes for branches of unions

;; vec db path node → db' n|#false, #false loop detected in all expansions (infinite size)
(define (size-of g db path node)
   (debug "size-of: at path " path)
   (cond
      ((number? node) ;; nonterminal, check if already computed
         (let ((val (iget db node #false)))
            (cond
               (val (values db val))
               ((has? path node) 
                  (values db #false)) ;; loop 
               (else
                  (lets 
                     ((db min (size-of g db (cons node path) (vec-ref g node)))
                      (min (if min (+ 1 min) #false))) ;; using a rule costs 1
                     (values (iput db node min) min))))))
      ((tuple? node)
         (tuple-case node
            ((sequence lst)
               ;; sum of each component, or #false if any are infinite
               (let loop ((db db) (n 0) (lst lst))
                  (if (null? lst)   
                     (values db n)
                     (lets ((db this (size-of g db path (car lst))))
                        (if this
                           (loop db (+ n this) (cdr lst))
                           (values db #false))))))
            ((bytes vec)
               (values db (vec-len vec)))
            ((union vec)
               ;; size of smallest component, or #false if all are infinite
               (let loop ((db db) (min #false) (lst (vector->list vec)))
                  (if (null? lst)
                     (values db min)
                     (lets ((db this (size-of g db path (car lst))))
                        (loop db (if (less? min this) min this) (cdr lst))))))
            ((repeat min max node)
               (lets ((db this (size-of g db path node)))
                  ;; compute the db even though min may be 0 to fill the DP table
                  (values db
                     (cond
                        (this (* this min)) ;; finite → min*it
                        ((= min 0) 0)       ;; infinite but can take 0 → finite
                        (else #false)))))    ;; at least some infinite → infinite
            ((bitfield mask set)
               (values db 1))
            ((bind exp)
               (size-of g db path exp))
            ((backref n)
               (values db 1)) ;; not really, just expansion cost
            ((upref n)
               (values db 1)) ;; ditto
            ((bind-up exp)
               (size-of g db path exp))
            (else
               (error "don't know how to compute size of " node))))
      ((pair? node) ;; (size . node)
         (size-of g db path (cdr node)))
      (else
         (error "size-of: weird node " node))))

;; all nonterminal sizes have been filled to db, 
(define (sort-unions-of db node) ; → node'
   (cond
      ((number? node) node)
      ((tuple? node)
         (tuple-case node
            ((sequence lst)
               (set node 2 (map (λ (n) (sort-unions-of db n)) lst)))
            ((union vec)
               ;; compute sizes, and sort by them
               ;; note, sizes are already computed, so it would be a bug if 
               ;; the grammar was needed in size-of
               ;; leave value (size . node)
               (lets 
                  ((subs (map (λ (node) (sort-unions-of db node)) (vector->list vec)))
                   (psubs 
                     (sort (λ (a b) (less? (car a) (car b)))
                        (map (λ (n) (cons (lets ((db s (size-of 'bug db null n))) s) n))
                           subs))))
                  (set node 2 (list->vector psubs))))
            ((repeat min max body)
               (set node 4 (sort-unions-of db body)))
            ((bitfield mask set) node)
            ((bytes vec) node)
            ((backref n) node)
            ((upref n) node)
            ((bind val)
               (tuple 'bind
                  (sort-unions-of db val)))
            ((bind-up id) node) ;; has just an id
            (else
               (error "don't know how to sort unions of " node))))
      (else
         (error "sort-unions-of: weird node " node))))

;; gram → (#false | iff of id → minimal-derivation-size)
(define (minimal-expansions gram)
   (call/cc
      (λ (ret)
         (fold
            (λ (db i)
               (debug "minimal-expansions: at rule " i)
               ;(debug "minimal-expansions: db " db)
               (lets ((db this (size-of gram db null i)))
                  (if this db
                     (begin
                        (write-bytes stderr (foldr render '(10) (list "No finite expansion for rule #" i ". Use -vv for more info.")))
                        (ret #false)))))
            #empty (iota 0 1 (vec-len gram))))))

;; compute minimal expansions and use them to sort all union nodes
;; gram → gram' | #false, if bad infinite rules
(define (sort-unions gram)
   (debug "   + computing minimal expansions")
   (let ((db (minimal-expansions gram)))
      ;(debug "   + computed " db)
      (if db
         (list->vector
            (vec-foldr 
               (λ (node tl) (cons (sort-unions-of db node) tl))
               null gram))
         #false)))





;;;
;;; Expansion
;;;

; choose how many times to repeat an a+, a* or {n,}
(define (choose-unlimited-size rs)
   (lets
      ((rs b (rand rs 32)) 
       (rs b (rand rs (+ b 1))) ;; 16
       (rs b (rand rs (+ b 1))));; 8
      (rand-nbit rs b)))

'(print
   (let loop ((rs (seed->rands (time-ms))) (n 100))
      (if (= n 0)
         null
         (lets ((rs x (choose-unlimited-size rs)))
            (cons x (loop rs (- n 1)))))))

(define (choose-range rs max node)
   (cond
      ((not max)
         (lets ((rs n (choose-unlimited-size rs)))
            (values rs (tuple 'repeating n node))))
      ((eq? max 0)
         (values rs nothing))
      (else
         (lets ((rs n (rand rs max)))
            (values rs (tuple 'repeating n node))))))


;; pick a node in having size in the range min(smallest-option, limit) from the sorted vector
;; rs #[(size . node) ...] limit → node 
(define (pick-union-node rs uvec lim)
   (lets
      ((len (vec-len uvec))
       (last-size (car (vec-ref uvec (- len 1)))))
      (if (< last-size lim) 
         ;; all options are good
         (lets ((rs n (rand rs len)))
            (values rs (cdr (vec-ref uvec n))))
         ;; pick a value from the range thats is below limit, or is in the smallest sizes 
         ;; (recall that the vector is sorted by size)
         (lets
            ((min (max lim (car (vec-ref uvec 0))))
             (end (bisect (λ (x) (> (car (vec-ref uvec x)) min)) 0 (vec-len uvec)))
             (end (if end end (vec-len uvec))) ;; case when all nodes in the vector are ok
             (rs n (rand rs end)))
            (values rs (cdr (vec-ref uvec n)))))))


;; a data receiver which stores instead of writing the data, and converts it to a vector when 'close:d

(define cacher
   (define (reader buff)
      (λ (block)
         (if (eq? block 'close)
            (list->vector
               (foldr append null
                  (reverse
                     (map vector->list buff))))
            (reader (cons block buff)))))
   (reader null))

(define (env-list-ref env p)
   (cond
      ((null? env)
         (print-to stderr "unbound backreference " p)
         (exit-owl 1))
      ((eq? p 1) 
         (let ((val (car env)))
            (if (vector? val)
               val
               (begin
                  (print-to stderr "Attempted to use backreference inside itself.")
                  (exit-owl 1)))))
      (else 
         (env-list-ref (cdr env) (- p 1)))))

(define (env-ref env p)
  (env-list-ref (cdr env) p))

(define (env-rand rs env)
   (let ((l (length (cdr env))))
      (if (eq? l 0)
         (values rs (vector))
         (lets ((rs p (rand rs l)))
            (values rs (env-ref env (+ p 1)))))))
        
(define (list-change env node value)
   (if (eq? (car env) node)
      (cons value (cdr env))
      (cons (car env)
         (list-change (cdr env) node value))))

(define (env-set env node value)
   (cons (car env)
	   (list-change (cdr env) node value)))

(define (env-push env val)
   (cons (car env)
	   (append (cdr env) (list val))))

(define (env-save-up env rule val)
   (lets
      ((ups flats env)
       (old (get ups rule null)))
      (cons (put ups rule (cons val old)) flats)))

(define (env-ref-up rs env rule)
   (let ((opts (get (car env) rule #false)))
      (if opts
         (rand-elem rs opts)
         (error "No bindings for upref " rule))))

(define empty-env (cons #empty null))


;                                  .--------> list of bindings saved during rhs expansion (\(foo | bar\) and \1)
;                                  |
(define (expand rs w lim gram exp env up)
   (let ((lim (- lim 1)))
      (cond
         ((number? exp) ;; rule reference (aka nonterminal), take the definition for expansion
            ;; bindings do not affect other nonterminals
            (lets ((rs w lim envp upx (expand rs w lim gram (vec-ref gram exp) empty-env up)))
               (values rs w lim env up)))
         ((tuple? exp)
            (tuple-case exp
               ((union vec)
                  (lets
                     ((rs node (pick-union-node rs vec lim))) ;; takes expansion size into account
                     (expand rs w lim gram node env up)))
               ((sequence nodes)
                  (if (null? nodes)
                     (values rs w lim env up)
                     (let loop ((rs rs) (w w) (lim lim) (nodes nodes) (env env) (up up))
                        (lets ((this nodes nodes))
                           (if (null? nodes)
                              (expand rs w lim gram this env up)
                              (lets 
                                 ((rs limp (rand rs (+ (max lim 0) 1))) ;; use 0...lim here
                                  (rs w limpl env up (expand rs w limp gram this env up)))
                                 (loop rs w (- lim (- limp limpl)) nodes env up)))))))
               ((bytes vec)
                  (values rs (w vec) lim env up))
               ((repeating n node) ;; repeating n more, but allowed stop if out of fuel
                  (let loop ((rs rs) (w w) (n n) (lim lim) (last-env env) (up up))
                     (cond
                        ((eq? n 0) (values rs w lim last-env up))
                        ((< lim 0) (values rs w lim last-env up))
                        (else
                           (lets ((rs w lim env up (expand rs w lim gram node env up)))
                              (loop rs w (- n 1) lim env up))))))
               ((repeat min max node) ;; do fixed repeat if min, then an optional one for the rest
                  (if (eq? min 0) ;; no lower bound -> choose n and start repeating (but can stop if low on fuel)
                     (lets ((rs repeater (choose-range rs max node)))
                        (expand rs w lim gram repeater env up))
                     ;; generate the mandatory part first, then optional repeat
                     (let loop ((rs rs) (w w) (lim lim) (n min) (last-env env) (up up))
                        (if (eq? n 0)
                           ;; expand the variable remainder
                           (expand rs w lim gram 
                              (tuple 'repeat 0 (if max (- max min) #false) node) 
                              last-env up)
                           (lets ((rs w lim envp up (expand rs w lim gram node env up)))
                              (loop rs w lim (- n 1) envp up))))))
               ((bitfield mask set)
                  (lets
                     ((val rs (uncons rs #false))
                      (val (bor set (band mask val))))
                     (values rs (w (vector val)) lim env up)))
               ((bind exp)
                  (lets 
                     ((env (env-push env exp)) ;; leave a marker where to put the reference
                      (rs full lim env up (expand rs cacher lim gram exp env up)) ;; internal bindings in wrong order, should leave mark
                      (data (full 'close)))
                     (values rs (w data) lim (env-set env exp data) up)))
               ((backref num)
                  (if num
                     (values rs (w (env-ref env num)) lim env up)
                     (lets ((rs out (env-rand rs env)))
                        (values rs (w out) lim env up))))
               ((upref id)
                  (lets
                     ((opts (get up id null))
                      (l (length opts)))
                     (if (eq? l 0)
                        (begin
                           (print "ERROR: unsatisfied upreference")
                           (exit-owl 1))
                        (lets 
                           ((rs p (rand rs l))
                            (data (list-ref opts p)))
                           (values rs (w data) lim env up)))))
               ((bind-up exp)
                  (lets
                     ((rs full lim env up (expand rs cacher lim gram exp env up))
                      (data (full 'close))
                      (up (put up exp (cons data (get up exp null)))))
                     (values rs (w data) lim env up)))
               (else
                  (print "What are " exp))))
         (else
            (print "What kind of node is " exp)
            (exit-owl 5)))))

(define (vector->exec vec name stdlib)
   (output "Parsing grammar data from " name)
   (let 
      ((ast 
         (record-time "   + parsing " name
            (try-parse (parse-toplevel name null) (vec-iter vec) name "bad grammar: " #false))))
      (if ast
         (begin
            (debug " - parsed to ast " ast)
            (let ((exec (record-time "   + ast->exec" (ast->exec ast stdlib))))
               (debug " - converted to exec")
               (if exec
                  (let ((exec (record-time "   + union sorting" (sort-unions exec))))
                     (debug " - returning exec")
                     exec)
                  #false)))
         (begin
            (debug " - parse failed")
            #false))))



;;;
;;; Command-line arguments
;;;

(define (string->natural x)
   (let ((num (string->integer x)))
      (cond
         ((not num) #false)
         ((< num 0) #false)
         (else num))))

(define (string->count x)
   (or (string->natural x)
      (if (mem equal? '("inf" "infinite" "forever") x)
         'inf
         #false)))

(define (loop rands)
   (append rands 
      (λ () (loop rands))))

;; loop a fixed sequence, do not use this later!
(define (rs-editor-loop rs)
   (lets
      ((rs bits (rand-range rs 2 8))
       (rs n-rands (rand-range rs 1 (<< 1 bits)))
       (rands (ltake rs n-rands)))
      (values rs (loop rands) #false)))

(define (step d s)
   (pair d (lets ((d _ (fx+ d s))) (step d s))))

(define (rs-editor-step rs)
   (lets
      ((d rs (uncons rs #false))
       (s rs (uncons rs #false)))
      (values rs (step d s) #false)))

;; use the default (sensible) random stream as such + reuse it after geenration
(define (rs-editor-passthrough rs)
   (values rs rs #true))

;; x (... x ... x ...) → ((...) (...) (...))
(define (cut-at val lst)
   (let loop ((lst lst) (this null) (done null))
      (cond
         ((null? lst)
            (if (null? this)
               (reverse done)
               (loop (list val) this done)))
         ((eq? (car lst) val)
            (loop (cdr lst) null
               (cons (reverse this) done)))
         (else
            (loop (cdr lst) (cons (car lst) this) done)))))

(define (bytes->num bs)
   (fold
      (λ (n x)
         (let ((x (- x #\0)))
            (if (and n (< -1 x) (< x 10))
               (+ x (* n 10))
               #false)))
      0 bs))

(define rs-editors
   (map 
      (λ (x) (cons (string->list (car x)) (cdr x)))
      (list
         (cons "rand" rs-editor-passthrough)
         (cons "step" rs-editor-step)
         (cons "loop" rs-editor-loop))))

(define (name->random-stream-editor bs)
   (let ((node (assoc bs rs-editors)))
      (if node (cdr node) node)))

(define (random-generator-node lst)
   (let ((l (length lst)))
      (cond
         ((= l 1)
            (let ((sed (name->random-stream-editor (car lst))))
               (if sed
                  (cons sed 1) ;; 1 as implicit weight
                  (begin
                     (print*-to stderr (list "unknown random generator: " (list->string (car lst))))
                     #false))))
         ((= l 2)
            (lets
               ((sed (name->random-stream-editor (car lst)))
                (pri (bytes->num (cadr lst))))
               (cond
                  ((not sed) 
                     (print*-to stderr (list "unknown random generator: " (list->string (car lst)))))
                  ((not pri) 
                     (print*-to stderr (list "bad random generator priority: " (list->string (cadr lst)))))
                  (else
                     (cons sed pri)))))
         (else
            (print*-to stderr (list "zero or one priorities please"))
            #false))))

(define (pick-generator gens pos)
   (let ((pos (- pos (cdar gens))))
      (if (< pos 0)
         (caar gens)
         (pick-generator (cdr gens) pos))))
      
(define (string->random-random-generator-generator str)
   (lets
      ((bytes (string->list str))
       (fields (cut-at #\, bytes))
       (fields (map (λ (x) (cut-at #\= x)) fields))
       (pairs 
         (map random-generator-node fields)))
      (if (all (λ (x) x) pairs)
         (lets
            ((total (fold + 0 (map cdr pairs))))
            (λ (rs)
               (lets 
                  ((rs x (rand rs total))
                   (red (pick-generator pairs x)))
                  (red rs))))
         (begin
            (print-to stderr "request for random generators denied")
            #false))))

(define command-line-rule-exp
 `((output "-o" "--output" has-arg default ,default-output
      comment "path or pattern where to write the data.")
   (count "-n" "--count" cook ,string->count check ,(λ (x) (not (eq? x 0)))
      default "1"
      comment "how many files to generate.")
   (seed "-s" "--seed" cook ,string->natural
      comment "random state (number, defaulo current ms)")
   (nodes "-f" "--nodes" cook ,string->natural default ,default-nodes
      comment "max number of steps before starting to break data generation.")
   (eval  "-e" "--eval" has-arg 
      comment "output the given data")
	(libraries  "-l" "--library" has-arg plural default ,default-library-path
		comment "path to load libraries from if necessary")
   (meta "-M" "--meta" has-arg 
      comment "save metadata about generated files to this file")
   (rand "-r" "--random-generators"
      cook ,string->random-random-generator-generator
      default "rand=10,loop=2,step"
      comment "random generators and their weights to use")
   (parse "-p" "--parse" has-arg 
      comment "try to parse according grammar file given as argument")
   (help "-h" "--help")
   (about "-A" "--about")
   (version "-V" "--version")
   (verbose "-v" "--verbose" plural)))

(define usage-text 
"Usage: blab [-n count] [-s seed] [-f limit] [-e string] [-l path] [-h] [-A] [-v] [-V] [-o pattern] [grammar] ...")

(define what-are-blab

"Blab -- a production system
Copyright (c) 2013 Aki Helin

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the \"Software\"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

Report bugs and other worries to ouspg@ee.oulu.fi, #ouspg on freenode or add 
an issue to http://code.google.com/p/ouspg/issues/list.

More information is available at http://code.google.com/p/ouspg/wiki/Blab.
")



;;;
;;; Startup
;;;

(define (print-about)
   (print what-are-blab))

(define (print-version)
   (print version-str))

(define command-line-rules
   (cl-rules command-line-rule-exp))

(define (print-usage)
   (print usage-text)
   (print (format-rules command-line-rules)))

(define (name-of path)
   (if (equal? path "-")
      "standard input"
      path))

(define (string->grammars str libs)
   (lets
      ((vec (list->vector (string->bytes str)))
       (exec (vector->exec vec "command line arguments" libs)))
      (if exec
         (list exec)
         #false)))

(define (read-grammars paths quiet? libs)
   (foldr
      (λ (path tl)
         (if tl
            (begin
               (output "Reading data grammar data from " (name-of path) ".")
               (let ((data (record-time " - reading " path (read-data path))))
                  (if data
                     (let ((gram (vector->exec data (name-of path) libs)))
                        (if gram
                           (cons gram tl)
                           #false))
                     #false)))
            #false))
      null paths))

(define (verbose-size n units)
   (if (or (< n 1024) (null? (cdr units)))
      (list->string (render n (render (car units) null)))
      (verbose-size (>> n 10) (cdr units))))

(define size-units
   '("b" "Kb" "Mb" "Tb"))

; function which writes to given fd, updates internal state, and prints stats when called with 'close
(define (fd-writer fd name)
   (define (emitter n)
      (λ (block)
         (cond
            ((vector? block) 
               (write-vector block fd)
               (emitter (+ n (vec-len block))))
            ((eq? block 'close)
               (output " - " name ": " (verbose-size n size-units))
               (close-port fd)
               'closed)
            (else
               (print "fd-writer: what is " block)
               (exit-owl 4)))))
   (emitter 0))

;; like fd writer, but don't close or print anything else to stdout
(define (stdout-writer n pos)
   (define (self block)
      (cond
         ((null? block) self)
         ((pair? block) (write-bytes stdout block) self)
         ((eq? block 'close) self)
         ((vector? block) (write-vector block stdout) self)
         (else (print "stdout-writer: what is " block))))
   self)

(define (file-writer patfn)
   (λ (n pos)
      (lets
         ((path (patfn n pos))
          (fd (open-output-file path)))
         (if fd
            (fd-writer fd path)
            (begin
               (output "Cannot write to: '" path "'")
               (exit-owl 3))))))

(define (split-pattern str)
   (define pat
      (str-foldr
         (λ (char tl)
            (if (and (eq? char 37) (pair? tl) (eq? (car tl) 110))
               ;; replace ".. %n .." -> 'n
               (cons 'n (cdr tl))
               (cons char tl)))
         null str))
   (if (has? pat 'n)
      (λ (n pos)
         (bytes->string
            (foldr
               (λ (x tl) (if (eq? x 'n) (render pos tl) (cons x tl)))
               null pat)))
      #false))

;; FIXME: update to ipv6 once we the development machines are out of ipv4 dark age
(define (render-ip ip tl)
   (foldr
      (λ (byte tl)
         (render byte
            (if (null? tl)
               tl
               (cons #\. tl))))
      tl (list (ref ip 0) (ref ip 1) (ref ip 2) (ref ip 3))))

(define (tcp-server port)
   (let ((sock (tcp-socket port)))
      (if sock
         (λ (n pos)
            (lets
               ((ip fd (tcp-client sock)))
               (if ip
                  (fd-writer fd 
                     (bytes->string
                        (render pos 
                           (ilist #\space #\- #\> #\space
                              (render-ip ip null)))))
                  (exit-owl 8))))
         (begin
            (print "Failed to start server to port " port)
            #false))))

(define (tcp-sender ip port)
   (lets ((target-str (ilist #\space #\- #\> #\space (render-ip ip (cons #\: (render port null))))))
      (λ (n pos)
         (let loop ()
            (let ((fd (open-connection ip port)))
               (if fd
                  (fd-writer fd
                     (bytes->string
                        (render pos target-str)))
                  (begin   
                     (interact sleeper-id 2) ;; sleep for a while while target is unreachable
                     (loop))))))))
   

(define (make-writer pat one?)
   (cond
      ((equal? pat "-") stdout-writer)
      ((equal? pat "") 
         (print "Blank output not ok.")
         #false)
      ((m/^:[0-9]+$/ pat) ;; tcp server request
         (let ((port (string->integer (s/:// pat))))
            (if (and (<= 0 port) (<= port #xffff))
               (tcp-server port)
               (begin
                  (print "Not starting a server to bad port " port)
                  #false))))
      ((m/^[0-9]{1,3}(\.[0-9]{1,3}){3}:[0-9]+$/ pat)
         (lets
            ((ip+port (c/:/ pat))
             (port (string->number (cadr ip+port) 10))
             (ss (c/\./ (car ip+port)))
             (bs (map (λ (x) (string->number x 10)) ss)))
            (if (and (all number? bs)
                     (all (λ (x) (< x 256)) bs)
                     (number? port)
                     (< port 65536))
               (tcp-sender (list->vector bs) port)
               (begin
                  (print-to stderr "Not a valid TCP target: " pat)
                  #false))))
      ((split-pattern pat) => file-writer) ; find %n etc
      (one? (file-writer (λ (n pos) pat)))
      (else
         (output "The output pattern must have a %n to avoid overwriting the file when generating more than one of them.")
         (exit-thread 1))))

(define (data-generator n-files gram nodes write seed rs-editor)
   (let 
      ((end (if (number? n-files) (+ n-files 1) -1))
       (rs (seed->rands seed)))
      (let loop ((rs rs) (n 1))
         (if (= n end)
            rs
            (lets
               ((rs nodes (rand rs (* nodes 2))) ;; add variance to sizes
                (writer (write seed n))
                (rs case-rs keep? (rs-editor rs)) ;; per-case random stream, and whether to use it afterwards
                (case-rs writer fuel env up 
                   (expand case-rs writer nodes gram 0 empty-env #empty)))
               (writer 'close)
               (loop (if keep? case-rs rs) (+ n 1)))))))

;; try to read some byte from /dev/urandom, otherwise use current milliseconds
(define (random-random-seed)
   (let ((fd (open-input-file "/dev/urandom"))) ;; #false if not there
      (if fd
         (let ((data (get-block fd 10)))
            (close-port fd)
            (if (vector? data)
               (vec-fold (λ (n d) (+ d (<< n 8))) 0 data)
               (time-ms)))
         (time-ms))))

(define (maybe-write-meta dict seed args)
   (let ((path (getf dict 'meta)))
      (if path
         (let ((meta (foldr render null (list "seed: " seed "\n"))))
            (cond
               ((equal? path "-")
                  (display (list->string meta)))
               ((vector->file (list->vector meta) path)
                  'ok)
               (else
                  (print-to stderr "warning: cannot write metadata to " path)
                  (halt 2)))))))

(define (start-blabbing gs dict args)
   (if gs
      (lets
         ((all (if (null? (cdr gs)) (car gs) (error "no multiple grammars yet: " gs)))
          (n-files (get dict 'count 1))
          (seed 
            (or
               (getf dict 'seed)
               (random-random-seed)))
          (write
            (make-writer (get dict 'output default-output) (eq? n-files 1)))) ; as if given -o -
         (maybe-write-meta dict seed args) 
         ;; todo: add a see-grammar for --verbose
         (data-generator (get dict 'count 1) all (get dict 'nodes default-nodes) write seed (getf dict 'rand))
         0)
      1))

;; (str_a str_b ...) → str_a ++ " " ++ str_b ++ ..
(define (cat-args args)
   (runes->string
      (foldr (λ (s tl) (str-foldr cons (cons 32 tl) s)) null args)))

;; read from stdin unless paths (or --eval) are given
(define (input-paths paths)
   (if (null? paths)
      '("-")
      paths))

;; → rval
(define (start-blab dict args)
   (cond
      ((getf dict 'about) (print-about) 0)
      ((getf dict 'version) (print-version) 0)
      ((getf dict 'help) (print-usage) 0)
      ((getf dict 'parse) =>
         (λ (path)
            (print "PATH " path)
            (let ((gs (read-grammars (list path) (equal? "-" (get dict 'output default-output)) (getf dict 'libraries))))
               (if gs
                  (print gs)
                  (begin
                     (print "Could not load grammar.")
                     1)))))
      ((getf dict 'verbose) =>
         (λ (n)
            (change-verbosity n)
            (start-blab (del dict 'verbose) args)))
      ((getf dict 'eval) =>  
         (λ (string)
            ;; include in case of -e <arg> also the other arguments to allow $ blab -e 42 42 42
            (start-blabbing 
               (string->grammars (cat-args (cons string args)) (get dict 'libraries 'bug))
               dict args)))
      ((read-grammars (input-paths args) (equal? "-" (get dict 'output default-output)) (get dict 'libraries 'bug)) =>
         (λ (gs)
				(start-blabbing gs dict args)))
      (else
         (output "Could not load the grammars.")
         1)))

(define (blab args)
	(process-arguments args command-line-rules usage-text start-blab))

;; still needed occasionally for debug messages. meta thread is used to store 
;; the function->name db used when printing functions.

(define anonimasu (string->symbol "#<function>"))

(define entry
   (λ (args)
      (fork-server 'output output-server)
      (blab (cdr args)))) ; skip binary name in args

entry

