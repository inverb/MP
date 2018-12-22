#lang racket

;; pomocnicza funkcja dla list tagowanych o określonej długości
(define (tagged-tuple? tag len p)
  (and (list? p)
       (= (length p) len)
       (eq? (car p) tag)))

(define (tagged-list? tag p)
  (and (pair? p)
       (eq? (car p) tag)
       (list? (cdr p))))

;; reprezentacja danych wejściowych (z ćwiczeń)
(define (var? x)
  (symbol? x))

(define (var x)
  x)

(define (var-name x)
  x)

;; przydatne predykaty na zmiennych
(define (var<? x y)
  (symbol<? x y))

(define (var=? x y)
  (eq? x y))

(define (literal? x)
  (and (tagged-tuple? 'literal 3 x)
       (boolean? (cadr x))
       (var? (caddr x))))

(define (literal pol x)
  (list 'literal pol x))

(define (literal-pol x)
  (cadr x))

(define (literal-var x)
  (caddr x))

(define (clause? x)
  (and (tagged-list? 'clause x)
       (andmap literal? (cdr x))))

(define (clause . lits)
  (cons 'clause lits))

(define (clause-lits c)
  (cdr c))

(define (cnf? x)
  (and (tagged-list? 'cnf x)
       (andmap clause? (cdr x))))

(define (cnf . cs)
    (cons 'cnf cs))

(define (cnf-clauses x)
  (cdr x))

;; oblicza wartość formuły w CNF z częściowym wartościowaniem. jeśli zmienna nie jest
;; zwartościowana, literał jest uznawany za fałszywy.
(define (valuate-partial val form)
  (define (val-lit l)
    (let ((r (assoc (literal-var l) val)))
      (cond
       [(not r)  false]
       [(cadr r) (literal-pol l)]
       [else     (not (literal-pol l))])))
  (define (val-clause c)
    (ormap val-lit (clause-lits c)))
  (andmap val-clause (cnf-clauses form)))

;; reprezentacja dowodów sprzeczności

(define (axiom? p)
  (tagged-tuple? 'axiom 2 p))

(define (proof-axiom c)
  (list 'axiom c))

(define (axiom-clause p)
  (cadr p))

(define (res? p)
  (tagged-tuple? 'resolve 4 p))

(define (proof-res x pp pn)
  (list 'resolve x pp pn))

(define (res-var p)
  (cadr p))

(define (res-proof-pos p)
  (caddr p))

(define (res-proof-neg p)
  (cadddr p))

;; sprawdza strukturę, ale nie poprawność dowodu
(define (proof? p)
  (or (and (axiom? p)
           (clause? (axiom-clause p)))
      (and (res? p)
           (var? (res-var p))
           (proof? (res-proof-pos p))
           (proof? (res-proof-neg p)))))

;; procedura sprawdzająca poprawność dowodu
(define (check-proof pf form)
  (define (run-axiom c)
    (displayln (list 'checking 'axiom c))
    (and (member c (cnf-clauses form))
         (clause-lits c)))
  (define (run-res x cpos cneg)
    (displayln (list 'checking 'resolution 'of x 'for cpos 'and cneg))
    (and (findf (lambda (l) (and (literal-pol l)
                                 (eq? x (literal-var l))))
                cpos)
         (findf (lambda (l) (and (not (literal-pol l))
                                 (eq? x (literal-var l))))
                cneg)
         (append (remove* (list (literal true x))  cpos)
                 (remove* (list (literal false x)) cneg))))
  (define (run-proof pf)
    (cond
     [(axiom? pf) (run-axiom (axiom-clause pf))]
     [(res? pf)   (run-res (res-var pf)
                           (run-proof (res-proof-pos pf))
                           (run-proof (res-proof-neg pf)))]
     [else        false]))
  (null? (run-proof pf)))


;; reprezentacja wewnętrzna

;; sprawdza posortowanie w porządku ściśle rosnącym, bez duplikatów
(define (sorted? vs)
  (or (null? vs)
      (null? (cdr vs))
      (and (var<? (car vs) (cadr vs))
           (sorted? (cdr vs)))))

(define (sorted-varlist? x)
  (and (list? x)
       ;;tu był błąd w kodzie, linijka wyglądała tak
       ;;(andmap (var? x))
       (andmap var? x)
       (sorted? x)))

;; klauzulę reprezentujemy jako parę list — osobno wystąpienia pozytywne i negatywne. Dodatkowo
;; pamiętamy wyprowadzenie tej klauzuli (dowód) i jej rozmiar.

(define (res-clause? x)
  (and (tagged-tuple? 'res-int 5 x)
       (sorted-varlist? (second x))
       (sorted-varlist? (third x))
       (= (fourth x) (+ (length (second x)) (length (third x))))
       (proof? (fifth x))))

(define (res-clause pos neg proof)
  (list 'res-int pos neg (+ (length pos) (length neg)) proof))

(define (res-clause-pos c)
  (second c))

(define (res-clause-neg c)
  (third c))

(define (res-clause-size c)
  (fourth c))

(define (res-clause-proof c)
  (fifth c))

;; przedstawia klauzulę jako parę list zmiennych występujących odpowiednio pozytywnie i negatywnie
(define (print-res-clause c)
  (list (res-clause-pos c) (res-clause-neg c)))

;; sprawdzanie klauzuli sprzecznej
(define (clause-false? c)
  (and (null? (res-clause-pos c))
       (null? (res-clause-neg c))))

;; pomocnicze procedury: scalanie i usuwanie duplikatów z list posortowanych
(define (merge-vars xs ys)
  (cond [(null? xs) ys]
        [(null? ys) xs]
        [(var<? (car xs) (car ys))
         (cons (car xs) (merge-vars (cdr xs) ys))]
        [(var<? (car ys) (car xs))
         (cons (car ys) (merge-vars xs (cdr ys)))]
        [else (cons (car xs) (merge-vars (cdr xs) (cdr ys)))]))

(define (remove-duplicates-vars xs)
  (cond [(null? xs) xs]
        [(null? (cdr xs)) xs]
        [(var=? (car xs) (cadr xs)) (remove-duplicates-vars (cdr xs))]
        [else (cons (car xs) (remove-duplicates-vars (cdr xs)))]))

(define (rev-append xs ys)
  (if (null? xs) ys
      (rev-append (cdr xs) (cons (car xs) ys))))

(define (arg c)
  (car c))

(define (merge-lists list1 list2 x)
  (remove x
          (remove-duplicates-vars (merge-vars list1 list2))
          var=?))

(define (common-var list1 list2)
    (if (or
         (null? list1)
         (null? list2))
        null
        (cond [(var=? (arg list1) (arg list2))
               (arg list1)]
              [(var<? (arg list1) (arg list2))
               (common-var (cdr list1) list2)]
              [else
               (common-var list1 (cdr list2))])))

(define (clause-trivial? c)
  (and (not (null? (common-var (res-clause-pos c) (res-clause-neg c))))
       (res? (res-clause-proof c))))

(define (resolve c1 c2)
  (let ((x (common-var (res-clause-pos c1) (res-clause-neg c2))))
    (if (not (null? x))
        (res-clause (merge-lists (res-clause-pos c1) (res-clause-pos c2) x)
                    (merge-lists (res-clause-neg c1) (res-clause-neg c2) x)
                    (proof-res x (res-clause-proof c1) (res-clause-proof c2)))
        (let ((y (common-var (res-clause-neg c1) (res-clause-pos c2))))
          (if (null? y)
              #f
              (res-clause (merge-lists (res-clause-pos c1) (res-clause-pos c2) y)
                          (merge-lists (res-clause-neg c1) (res-clause-neg c2) y)
                          (proof-res y (res-clause-proof c2) (res-clause-proof c1))))))))

(define (variable s-clause)
  (if (= (length (res-clause-pos s-clause)) 0)
      (arg (res-clause-neg s-clause))
      (arg (res-clause-pos s-clause))))

(define (remove-var var s-clause clause)
  (if (or (member var (res-clause-pos clause))
          (member var (res-clause-neg clause)))
      (resolve s-clause clause)
      clause))

(define (resolve-single-prove s-clause checked pending)
  (let* ((var (variable s-clause))
         (new-checked (map (lambda (x) (remove-var var s-clause x)) checked))
         (new-pending (map (lambda (x) (remove-var var s-clause x)) pending))
         (false-new-checked (filter clause-false? new-checked))
         (false-new-pending (filter clause-false? new-pending)))
    (cond [(not (null? false-new-checked))
           (subsume-add-prove checked pending (list (car false-new-checked)))]
          [(not (null? false-new-pending))
           (subsume-add-prove checked pending (list (car false-new-pending)))]
    [else (subsume-add-prove (cons s-clause new-checked) new-pending '())])))

;; wstawianie klauzuli w posortowaną względem rozmiaru listę klauzul
(define (insert nc ncs)
  (cond
   [(null? ncs)                     (list nc)]
   [(< (res-clause-size nc)
       (res-clause-size (car ncs))) (cons nc ncs)]
   [else                            (cons (car ncs) (insert nc (cdr ncs)))]))

;; sortowanie klauzul względem rozmiaru (funkcja biblioteczna sort)
(define (sort-clauses cs)
  (sort cs < #:key res-clause-size))

;; główna procedura szukająca dowodu sprzeczności
;; zakładamy że w checked i pending nigdy nie ma klauzuli sprzecznej
(define (resolve-prove checked pending)
  (cond
   ;; jeśli lista pending jest pusta, to checked jest zamknięta na rezolucję czyli spełnialna
   [(null? pending) (generate-valuation (sort-clauses checked))]
   ;; jeśli klauzula ma jeden literał, to możemy traktować łatwo i efektywnie ją przetworzyć
   [(= 1 (res-clause-size (car pending)))
    (resolve-single-prove (car pending) checked (cdr pending))]
   ;; w przeciwnym wypadku wykonujemy rezolucję z wszystkimi klauzulami już sprawdzonymi, a
   ;; następnie dodajemy otrzymane klauzule do zbioru i kontynuujemy obliczenia
   [else
    (let* ((next-clause  (car pending))
           (rest-pending (cdr pending))
           (resolvents   (filter-map (lambda (c) (resolve c next-clause))
                                     checked))
           (sorted-rs    (sort-clauses resolvents)))
      (subsume-add-prove (cons next-clause checked) rest-pending sorted-rs))]))

;;predykat pomocniczy sprawdzający, czy klauzula new jest łatwiejsza od clause
(define (easier? new clause)
  (define (checking new-vars clause-vars)
    (cond [(null? clause-vars) #t]
          [(null? new-vars) #f]
          [(var=? (arg new-vars) (arg clause-vars))
           (checking (cdr new-vars) (cdr clause-vars))]
          [(var<? (arg new-vars) (arg clause-vars))
           (checking (cdr new-vars) clause-vars)]
          [else #f]))
  (if (and (checking (res-clause-pos new)
                     (res-clause-pos clause))
           (checking (res-clause-neg new)
                     (res-clause-neg clause)))
      #t
      #f))

;; procedura upraszczająca stan obliczeń biorąc pod uwagę świeżo wygenerowane klauzule i
;; kontynuująca obliczenia.
(define (subsume-add-prove checked pending new)
  (cond
   [(null? new)                 (resolve-prove checked pending)]
   ;; jeśli klauzula do przetworzenia jest sprzeczna to jej wyprowadzenie jest dowodem sprzeczności
   ;; początkowej formuły
   [(clause-false? (car new))   (list 'unsat (res-clause-proof (car new)))]
   ;; jeśli klauzula jest trywialna to nie ma potrzeby jej przetwarzać
   [(clause-trivial? (car new)) (subsume-add-prove checked pending (cdr new))]
   [else
    ;;sprawdzenie, czy klauzula jest łatwiejsza od tych wystepujących wcześniej
    (let ((is-easier (ormap (lambda (x) (easier? (car new) x)) (append checked pending))))
      (if (false? is-easier)
          ;;wyrzucenie klauzul łatwiejszych od obecnie rozpatrywanej
          (subsume-add-prove (filter (lambda (x) (not (easier? x (car new)))) checked)
                             (insert (car new) (filter (lambda (x) (not (easier? x (car new)))) pending))
                             (cdr new))
          (subsume-add-prove checked pending (cdr new))))]))

;;funkcje pomocnicze

(define (next-clause e)
  (car e))

(define (rest-of-clauses e)
  (cdr e))

;;sprawdzanie, czy zmienna ma być zanegowana
(define (var-value e)
  (if (= (length (res-clause-pos e)) 1)
      (list (car (res-clause-pos e)) #t)
      (list (car (res-clause-neg e)) #f)))

;;znajdowanie klauzuli z jedną zmienną
(define (single-var-clause lista)
  (cond [(null? lista) #f]
        [(= (res-clause-size (next-clause lista)) 1)
         (var-value (next-clause lista))]
        [else (single-var-clause (rest-of-clauses lista))]))

(define (var-value-var v)
  (car v))

(define (var-value-value v)
  (cadr v))

;;sprawdzenie, czy zmienna występuje w klauzuli i ewentualna modyfikacja klauzuli
(define (in-clause var clause)
  (let ((var-positive (memq (var-value-var var) (res-clause-pos clause)))
        (var-negative (memq (var-value-var var) (res-clause-neg clause))))
    (cond [(not (false? var-positive))
           (if (not (false? (var-value-value var)))
               #f
               (res-clause (remq (var-value-var var) (res-clause-pos clause))
                           (res-clause-neg clause)
                           (res-clause-proof clause)))]
          [(not (false? var-negative))
           (if (false? (var-value-value var))
               #f
               (res-clause (res-clause-pos clause)
                           (remq (var-value-var var) (res-clause-neg clause))
                           (res-clause-proof clause)))]
          [else clause])))

;;pętla wyrzucająca klauzule, w których zmienna występuje zgodnie z wartościowaniem i zmniejszająca te,
;;w których zmienna występuje przeciwnie
(define (pop-clauses clauses var-value)
  (define (loop unchecked checked)
    (cond [(null? unchecked) checked]
          [else (let ((new-clause (in-clause var-value (car unchecked))))
                  (if (or (false? new-clause)
                          (= 0 (res-clause-size new-clause)))
                      (loop (cdr unchecked) checked)
                      (loop (cdr unchecked)
                            (append (list new-clause) checked))))]))
  (loop clauses '()))

;;znajdowanie dowolnej zmiennej, gdy każda klauzula ma co najmniej dwie
(define (random-variable clauses)
  (cond [(= (length (res-clause-pos (next-clause clauses))) 0)
         (list (car (res-clause-neg (next-clause clauses))) #f)]
        [else (list (car (res-clause-pos (next-clause clauses))) #t)]))

(define (generate-valuation resolved)
  ;;pętla znajdująca wartościowania dla kolejnych zmiennych
  (define (partial-valuation clauses valuation)
    (let ((var-value (single-var-clause clauses)))
      (cond [(null? clauses) valuation]
            ;;przypadek gdy nie ma klauzul zawierających tylko jedną zmienną
            [(false? var-value) (let ((random-var (random-variable clauses)))
                                  (partial-valuation (pop-clauses clauses random-var)
                                                     (append (list random-var) valuation)))]
            ;;przypadek gdy istnieją klauzule z jedną zmienną
            [else (partial-valuation (pop-clauses clauses var-value)
                                     (append (list var-value) valuation))])))
  (list 'sat (partial-valuation resolved '())))

;; procedura przetwarzające wejściowy CNF na wewnętrzną reprezentację klauzul
(define (form->clauses f)
  (define (conv-clause c)
    (define (aux ls pos neg)
      (cond
       [(null? ls)
        (res-clause (remove-duplicates-vars (sort pos var<?))
                    (remove-duplicates-vars (sort neg var<?))
                    (proof-axiom c))]
       [(literal-pol (car ls))
        (aux (cdr ls)
             (cons (literal-var (car ls)) pos)
             neg)]
       [else
        (aux (cdr ls)
             pos
             (cons (literal-var (car ls)) neg))]))
    (aux (clause-lits c) null null))
  (map conv-clause (cnf-clauses f)))

(define (prove form)
  (let* ((clauses (form->clauses form)))
    (subsume-add-prove '() '() clauses)))

;; procedura testująca: próbuje dowieść sprzeczność formuły i sprawdza czy wygenerowany
;; dowód/waluacja są poprawne. Uwaga: żeby działała dla formuł spełnialnych trzeba umieć wygenerować
;; poprawną waluację.
(define (prove-and-check form)
  (let* ((res (prove form))
         (sat (car res))
         (pf-val (cadr res)))
    (if (eq? sat 'sat)
        (valuate-partial pf-val form)
        (check-proof pf-val form))))

;;; Testy:
(define (tests)
  (and
   ;;(x) ^ (~x)
   (prove-and-check (list 'cnf
                          (cons 'cnf-clause (list (list 'literal #t 'x)))
                          (cons 'cnf-clause (list (list 'literal #f 'x)))))
   ;;(x) ^ (x)
   (prove-and-check (list 'cnf
                          (cons 'cnf-clause (list (list 'literal #t 'x)))
                          (cons 'cnf-clause (list (list 'literal #t 'x)))))
   ;;(x v ~x)
   (prove-and-check (list 'cnf
                          (cons 'cnf-clause (list (list 'literal #t 'x) (list 'literal #f 'x)))))
   ;;(x v x)
   (prove-and-check (list 'cnf
                          (cons 'cnf-clause (list (list 'literal #t 'x) (list 'literal #t 'x)))))
   ;;(x v y v z) ^ (y) ^ (z) ^ (x)
   (prove-and-check (list 'cnf
                          (cons 'cnf-clause (list (list 'literal #t 'x) (list 'literal #t 'y) (list 'literal #t 'z)))
                          (cons 'cnf-clause (list (list 'literal #t 'y)))
                          (cons 'cnf-clause (list (list 'literal #t 'z)))
                          (cons 'cnf-clause (list (list 'literal #t 'x)))))
   ;;(x v y) ^ (~x)
   (prove-and-check (list 'cnf
                          (cons 'cnf-clause (list (list 'literal #t 'x) (list 'literal #t 'y)))
                          (cons 'cnf-clause (list (list 'literal #f 'x)))))
   ;;(x v y) ^ (~x v z)
   (prove-and-check (list 'cnf
                          (cons 'cnf-clause (list (list 'literal #t 'x) (list 'literal #t 'y)))
                          (cons 'cnf-clause (list (list 'literal #f 'x) (list 'literal #t 'z)))))
   ;;(x v ~y) ^ (y v ~x)
   (prove-and-check (list 'cnf
                          (cons 'cnf-clause (list (list 'literal #t 'x) (list 'literal #f 'y)))
                          (cons 'cnf-clause (list (list 'literal #t 'y) (list 'literal #f 'x)))))
   ;;(x v y) ^ (~x v ~y)
   (prove-and-check (list 'cnf
                          (cons 'cnf-clause (list (list 'literal #t 'x) (list 'literal #t 'y)))
                          (cons 'cnf-clause (list (list 'literal #f 'x) (list 'literal #f 'y)))))
   ;;(x v y) ^ (x v ~y) ^ (~x v ~z) ^ (~x v z)
   (prove-and-check (list 'cnf
                          (cons 'cnf-clause (list (list 'literal #t 'x) (list 'literal #t 'y)))
                          (cons 'cnf-clause (list (list 'literal #t 'x) (list 'literal #f 'y)))
                          (cons 'cnf-clause (list (list 'literal #f 'x) (list 'literal #f 'z)))
                          (cons 'cnf-clause (list (list 'literal #f 'x) (list 'literal #t 'z)))))
   ;;(x v y v z) ^ (~x) ^ (~y v ~z) ^ (y v ~z) ^ (~y v a) ^ (x v ~a)
   (prove-and-check (list 'cnf
                          (cons 'cnf-clause (list (list 'literal #t 'x) (list 'literal #t 'y) (list 'literal #t 'z)))
                          (cons 'cnf-clause (list (list 'literal #f 'x)))
                          (cons 'cnf-clause (list (list 'literal #f 'y) (list 'literal #f 'z)))
                          (cons 'cnf-clause (list (list 'literal #t 'y) (list 'literal #f 'z)))
                          (cons 'cnf-clause (list (list 'literal #f 'y) (list 'literal #t 'a)))
                          (cons 'cnf-clause (list (list 'literal #t 'x) (list 'literal #f 'a)))))
   ;;(x v ~y) ^ (y v ~z) ^ (z v ~x)
   (prove-and-check (list 'cnf
                          (cons 'cnf-clause (list (list 'literal #t 'x) (list 'literal #f 'y)))
                          (cons 'cnf-clause (list (list 'literal #t 'y) (list 'literal #f 'z)))
                          (cons 'cnf-clause (list (list 'literal #t 'z) (list 'literal #f 'x)))))
   ;;(a v b v c v d) ^ (~a v ~b v ~c) ^ (a v b) ^ (~a)
   (prove-and-check (list 'cnf
                          (cons 'cnf-clause (list (list 'literal #t 'a) (list 'literal #t 'b) (list 'literal #t 'c) (list 'literal #t 'd)))
                          (cons 'cnf-clause (list (list 'literal #f 'a) (list 'literal #f 'b) (list 'literal #f 'c)))
                          (cons 'cnf-clause (list (list 'literal #t 'a) (list 'literal #t 'b)))
                          (cons 'cnf-clause (list (list 'literal #f 'a)))))))