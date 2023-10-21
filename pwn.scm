; get λ
(define-syntax λ
  (syntax-rules ()
    ((_ args body ...)
     (lambda args body ...))))

(define ID (λ (a)
              a))

(define COMPOSE (λ (f)
                   (λ (g)
                      (λ (x)
                         (f (g x))))))

#| Y Combinator
(define Y (λ (f)
             ((λ (x)
                 (f (x x)))
              (λ (x)
                 (f (x x))))))
|#

#| simplest form
(define Y (λ (f)
             ((λ (x)
                 (x x))
              (λ (x)
                 (f (x x))))))
|#

(define Z (λ (f)
             ((λ (x)
                 (f (λ (y)
                       ((x x) y))))
              (λ (x)
                 (f (λ (y)
                       ((x x) y)))))))

#| simplest form
(define Z (λ (f)
             ((λ (x)
                 (x x))
              (λ (x)
                 (f (λ (y)
                       ((x x) y)))))))
|#

(define F (λ (a)
             (λ (b)
                b)))

(define V (λ (a)
             (λ (b)
                a)))

(define CONST V)

(define NOT (λ (p)
               (λ (a)
                  (λ (b)
                     ((p b) a)))))

#|
(define NOT (λ (p)
               ((p F) V)))
|#

(define XOR (λ (p)
               (λ (q)
                  (λ (a)
                     (λ (b)
                        ((p ((q b) a)) ((q a) b)))))))

(define OR (λ (p)
              (λ (q)
                 ((p p) q))))

(define AND (λ (p)
               (λ (q)
                  ((p q) p))))

(define NOR (λ (p)
               (λ (q)
                  (NOT ((OR p) q)))))

; (define NOR ((COMPOSE NOT) OR))

(define NAND (λ (p)
                (λ (q)
                   (NOT ((AND p) q)))))

; (define NAND ((COMPOSE NOT) AND))

(define BEQ (λ (p)
               (λ (q)
                  (NOT ((XOR p) q)))))

; (define BEQ ((COMPOSE NOT) XOR))

(define (_bool p)
  ((p #t) #f))

; conditional

(define IF (λ (p)
             (λ (a)
               (λ (b)
                 ((p a) b)))))

#| IF = ID because p:bool
(define IF ID)
|#

; pair

(define PAIR (λ (a)
               (λ (b)
                 (λ (f)
                   ((f a) b)))))

(define FST (λ (t)
              (t (λ (a)
                   (λ (b)
                     a)))))

(define SND (λ (t)
              (t (λ (a)
                   (λ (b)
                     b)))))

(define (_pair t)
  (cons (FST t) (SND t)))

(define (pair_ t)
  ((PAIR (car t)) (cdr t)))

; option

(define NONE (λ (n)
                (λ (s)
                   n)))

(define SOME (λ (x)
                (λ (n)
                   (λ (s)
                      (s x)))))

(define ISNONE (λ (w)
                    ((w V)(λ (_) F))))

(define FROMSOME (λ (w)
                    ((w NONE) ID))) ; partial

; list

(define CONS (λ (a)
                (λ (t)
                   (λ (f)
                      ((f a) t)))))

#| interesting equivalency
(define CONS (λ (a)
                (λ (t)
                   ((PAIR a) t))))
; so
(define CONS PAIR)
|#

; canonical church encoded lists are boring

; assuming non-empty lists of the form ((PAIR 0) (SOME ((PAIR 1) NONE)))
(define HEAD FST)
(define TAIL SND)

#|
(define SIZE (λ (l)
                (((IF (ISNONE l))
                    ZERO)
                    (SUCC (SIZE (TAIL (FROMSOME l)))))))
|#

(define SIZE (Z (λ (z)
                   (λ (l)
                      (((IF (ISNONE l))
                          ZERO)
                          (λ (x) ((SUCC (z (TAIL (FROMSOME l)))) x)))))))

(define INDEX (λ (l)
                 (λ (n)
                    (FST ((n ((COMPOSE FROMSOME) SND)) (FROMSOME l))))))

#|
(define MAP (λ (f)
               (λ (l)
                  (((IF (ISNONE l))
                      NONE)
                      (SOME ((PAIR (f (FST (FROMSOME l)))) ((MAP f) (SND (FROMSOME l)))))))))
|#

(define MAP (Z  (λ (z)
                   (λ (f)
                      (λ (l)
                         (((IF (ISNONE l))
                           NONE)
                          (λ (x)
                             ((SOME ((PAIR (f (HEAD (FROMSOME l)))) ((z f) (TAIL (FROMSOME l))))) x))))))))

#|
(define FOLDL (λ (f)
                 (λ (k)
                    (λ (l)
                       (((IF (ISNONE l))
                           k)
                           (((FOLDL f) ((f k) (HEAD (FROMSOME l)))) (TAIL (FROMSOME l))))))))
|#

(define FOLDL (Z (λ (z)
                    (λ (f)
                       (λ (k)
                          (λ (l)
                             (((IF (ISNONE l))
                                 k)
                                 (λ (x)
                                    ((((z f) ((f k) (HEAD (FROMSOME l)))) (TAIL (FROMSOME l))) x)))))))))

(define FOLDR (Z (λ (z)
                    (λ (f)
                       (λ (k)
                          (λ (l)
                            (((IF (ISNONE l))
                                k)
                                (λ (x)
                                   (((f (HEAD (FROMSOME l))) (((z f) k) (TAIL (FROMSOME l)))) x)))))))))

; _list and list_ assume possibly empty lists, so (SOME (PAIR...

(define (_list l)
  (if (_bool (ISNONE l))
    '()
    (cons (HEAD (FROMSOME l)) (_list (TAIL (FROMSOME l))))
    )))

(define (list_ l)
  (if (null? l)
    NONE
    (SOME ((PAIR (car l)) (list_ (cdr l))))))

; natural numbers

(define ZERO (λ (f)
                (λ (x)
                   x)))

(define SUCC (λ (n)
                (λ (f)
                   (λ (x)
                      (f ((n f) x))))))

(define PRED (λ (n)
                (λ (f)
                   (λ (x)
                      (((n (λ (g) (λ (h) (h (g f))))) (λ (y) x)) (λ (y) y)))))) ; wtf

(define (_num n)
  ((n (λ (x) (+ 1 x))) 0))

; arithmetic

(define SUM (λ (n)
               (λ (m)
                  ((n SUCC) m))))

#|
(define MUL (λ (n)
               (λ (m)
                  ((n (SUM m)) ZERO))))
|#

(define MUL (λ (n)
               (λ (m)
                  (λ (f)
                     (n (m f))))))

(define SUB (λ (n)
               (λ (m)
                  ((m PRED) n))))

#|
(define POW (λ (n)
               (λ (m)
                  ((m (MUL n)) (SUCC ZERO)))))
|#

(define POW (λ (n)
               (λ (m)
                  (m n))))

(define ISZERO (λ (n)
                  ((n (λ (_) F)) V)))

#|
(define ISZERO (λ (n)
                  ((n (CONST F)) V)))
|#

(define LTE (λ (n)
               (λ (m)
                  (ISZERO ((SUB n) m)))))

(define GT (λ (n)
              (λ (m)
                 (NOT ((LTE n) m)))))

(define EQ (λ (n)
              (λ (m)
                 ((AND ((LTE n) m)) ((LTE m) n)))))

#|
(define MOD (λ (n)
               (λ (m)
                  (((IF ((LTE m) n))
                      ((MOD ((SUB n) m)) n))
                      m))))
|#

(define MOD (Z (λ (z)
                  (λ (n)
                     (λ (m)
                        (((IF ((LTE m) n))
                            (λ (x) (((z ((SUB n) m)) m) x)))
                            n))))))

; util

(define (num_ n) (if (zero? n)
    ZERO
    (SUCC (num_ (- n 1)))))

; test

(define *list* ((PAIR (num_ 9)) (SOME ((PAIR (num_ 8)) (SOME ((PAIR (num_ 7)) NONE))))))
;(define *list* (list_ `(,(num_ 9) ,(num_ 8) ,(num_ 7))))

(for-all ID (list
              (eq? #t (_bool (NOT F)))
              (eq? #f (_bool (NOT V)))

              (eq? #f (_bool ((XOR F) F)))
              (eq? #t (_bool ((XOR V) F)))
              (eq? #t (_bool ((XOR F) V)))
              (eq? #f (_bool ((XOR V) V)))

              (eq? #f (_bool ((AND F) F)))
              (eq? #f (_bool ((AND V) F)))
              (eq? #f (_bool ((AND F) V)))
              (eq? #t (_bool ((AND V) V)))

              (eq? #t (_bool ((NOR F) F)))
              (eq? #f (_bool ((NOR V) F)))
              (eq? #f (_bool ((NOR F) V)))
              (eq? #f (_bool ((NOR V) V)))

              (eq? #t (_bool ((NAND F) F)))
              (eq? #t (_bool ((NAND V) F)))
              (eq? #t (_bool ((NAND F) V)))
              (eq? #f (_bool ((NAND V) V)))

              (eq? #t (_bool ((BEQ F) F)))
              (eq? #f (_bool ((BEQ V) F)))
              (eq? #f (_bool ((BEQ F) V)))
              (eq? #t (_bool ((BEQ V) V)))

              (eq? 0 (_num ZERO))
              (eq? 1 (_num (SUCC ZERO)))
              (eq? 2 (_num (SUCC (SUCC ZERO))))
              (eq? 72 (_num (num_ 72)))
              (eq? (+ 9 7) (_num ((SUM (num_ 9)) (num_ 7))))
              (eq? (* 9 7) (_num ((MUL (num_ 9)) (num_ 7))))
              (eq? 6 (_num (PRED (num_ 7))))
              (eq? (- 9 7) (_num ((SUB (num_ 9)) (num_ 7))))
              (eq? 0       (_num ((SUB (num_ 7)) (num_ 9))))
              (eq? (expt 3 3) (_num ((POW (num_ 3)) (num_ 3))))

              (eq? #t (_bool (ISZERO ZERO)))
              (eq? #f (_bool (ISZERO (SUCC ZERO))))
              (eq? #f (_bool (ISZERO (num_ 7))))
              (eq? #t (_bool ((LTE (num_ 7)) (num_ 9))))
              (eq? #t (_bool ((LTE (num_ 9)) (num_ 9))))
              (eq? #f (_bool ((LTE (num_ 9)) (num_ 7))))
              (eq? #f (_bool ((GT  (num_ 7)) (num_ 9))))
              (eq? #f (_bool ((GT  (num_ 9)) (num_ 9))))
              (eq? #t (_bool ((GT  (num_ 9)) (num_ 7))))
              (eq? #f (_bool ((EQ  (num_ 7)) (num_ 9))))
              (eq? #t (_bool ((EQ  (num_ 9)) (num_ 9))))
              (eq? #f (_bool ((EQ  (num_ 9)) (num_ 7))))
              (eq? (mod 9 7) (_num ((MOD (num_ 9)) (num_ 7))))
              (eq? (mod 7 7) (_num ((MOD (num_ 7)) (num_ 7))))
              (eq? (mod 7 9) (_num ((MOD (num_ 7)) (num_ 9))))

              (equal? '(9 8 7) (_list (list_ '(9 8 7))))
              (equal? '(9 8 7) (_list ((MAP _num) (SOME *list*))))
              (equal? '(9 8 7) (map _num (_list (SOME *list*))))

              (eq? 9 (_num (HEAD *list*)))
              (eq? 8 (_num (HEAD (FROMSOME (TAIL *list*)))))
              (eq? 3 (_num (SIZE (SOME *list*))))
              (eq? 0 (_num (SIZE NONE)))
              (eq? 2 (_num (SIZE (TAIL *list*))))
              (eq? 7 (_num ((INDEX (SOME *list*)) (num_ 2))))

              (eq? (+ 9 9) (_num (HEAD (FROMSOME ((MAP (λ (x) ((SUM x) x))) (SOME *list*))))))
              (eq? 0                                                   (_num (((FOLDR SUM) ZERO) NONE)))
              (eq? (fold-right + 0 (_list ((MAP _num) (SOME *list*)))) (_num (((FOLDR SUM) ZERO) (SOME *list*))))
              (eq? 0                                                   (_num (((FOLDL SUM) ZERO) NONE)))
              (eq? (fold-left  + 0 (_list ((MAP _num) (SOME *list*)))) (_num (((FOLDL SUM) ZERO) (SOME *list*))))

              ))

