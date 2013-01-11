
; considerati urmatoarele afirmatii:

; Most cars, but not all, are powered by an internal combustion engine (ICE).
; An ICE provides a typical car with about 120 HP.
; A hybrid car is a car that has an ICE and an electric motor.
; There are two categorizations for hybrid cars:
; you can have parallel versus series hybrids;
; and you can have mild versus full hybrids.
; A hybrid would typically provide 100HP from the ICE and 30HP from the electric motor.
; Some cars can be assisted by a low power electric motor (about 20HP).
; A mild hybrid is a hybrid that has an ICE assisted by such a low power electric motor.
; An electric car is considered to be able to be powered by electric power alone.
; Electric cars have about 120HP, and no ICE.
; A full hybrid can be powered only by its electric motor, so it could be considered a kind of electric car.
; Full hybrids feature electric motors of about 100HP.
; The Toyota Prius is a full and parallel hybrid.
; The Honda Insight is a mild parallel hybrid.
; The Chevrolet Volt is a full, series hybrid.
; The Nissan Leaf is an electric car.


; Baza de cunostinte

(define hybrid-net
  (list
   '(Insight Volt Prius Leaf Car Hybrid-Car ICE-Car Assisted-Electric Electri-Car Full-Hybrid Series-Hybrid Parallel-Hybrid Mild-Hybrid
             HP0 HP20 HP30 HP100 HP110 HP120) ; aici constantele (cu litera mare) - concepte, instante, si alte constante (e.g. puteri, scrise ca HP30) - TODO
   '((ISA . 2) (AKO . 2)
               (Attr-ICE-power . 2) (Attr-electric-power . 2)) ; predicate si numar de argumente
   '() ; UNUSED
   '(  ; aici reprezentarea retelei
     ; Car
     (Attr-ICE-power Car HP110)
     ; Hybrid-Car
     (Attr-ICE-power Hybrid-Car HP100)
     (Attr-electric-power Hybrid-Car HP30)
     (AKO Hybrid-Car Car)
     (AKO Hybrid-Car ICE-Car)
     ; ICE-Car
     (Attr-ICE-power ICE-Car HP120)
     (AKO ICE-Car Car)
     ; Assisted-Electric
     (AKO Assisted-Electric Car)
     (Attr-electric-power Assisted-Electric HP20)
     ; Electri-Car
     (AKO Electric-Car Car)
     (Attr-electric-power Electric-Car HP120)
     (Attr-ICE-power Electric-Car HP0)
     ; Full-Hybrid
     (AKO Full-Hybrid Hybrid-Car)
     (AKO Full-Hybrid Electric-Car)
     (Attr-electric-power Full-Hybrid HP100)
     ; Series-Hybrid
     (AKO Series-Hybrid Hybrid-Car)
     ; Parallel-Hybrid
     (AKO Parallel-Hybrid Hybrid-Car)
     ; Mild-Hybrid
     (AKO Mild-Hybrid Hybrid-Car)
     (AKO Mild-Hybrid Assisted-Electric)
     
     (ISA Insight Mild-Hybrid)
     (ISA Insight Parallel-Hybrid)
     (ISA Prius Parallel-Hybrid)
     (ISA Prius Full-Hybrid)
     (ISA Volt Series-Hybrid)
     (ISA Volt Full-Hybrid)
     (ISA Leaf Electric-Car)
     )
   ))


; parametri: nume nod; nume atribut; retea semantica
; returns: valoarea atributului pentru nod, daca a fost definita pentru nodul respectiv; fals altfel
(define (get-attr node-name attr-name net)
  (let* ((statements (cadddr net))
         (value (assoc attr-name
                       (filter (λ (x) (equal? (second x) node-name))
                               statements))))
    (and value (third value))
    )
  )

; parametri: nume nod; retea semantica
; returns: lista de noduri (lista numelor) de care nodul este legat prin relatii ISA (pentru noduri instanta/obiect)
(define (make-isa-ancestor-list node-name net)
  (let ((statements (cadddr net)))
    (map (λ (x) (third x))
         (filter (λ (x) (and (equal? (first x)
                                     'ISA)
                             (equal? (second x)
                                     node-name)))
                 statements)
         )
    )
  )

; parametru: nume nod; retea semantica
; returns: lista de noduri (lista numelor) de care nodul este legat prin relatii AKO (pentru noduri concept/clasa)
(define (make-ako-ancestor-list node-name net)
  (let ((statements (cadddr net)))
    (map (λ (x) (third x))
         (filter (λ (x) (and (equal? (first x)
                                     'AKO)
                             (equal? (second x)
                                     node-name)))
                 statements)
         )
    )
  )

; parametri: nume nod; nume atribut; retea semantica
; returns:
;    valoarea atributului pentru nod, inferata din reteaua semantica;
;    fals daca nu s-a gasit nici o valoare;
;    (pentru bonus) 'CONTRADICTION daca nu se poate determina o singura valoare cea mai apropiata
; Ex.: (infer-attr 'Insight 'Attr-ICE-power hybrid-net) --> (HP100 2)
(define (infer-attr node-name attr-name net)
  (let* ((parents (make-isa-ancestor-list node-name net))
         (parents-with-levels (append (map (λ (x) (list x 1))
                                           parents)
                                      (list (list node-name 0))))
         (candidates (remove-duplicates (get-candidates  parents-with-levels
                                                         attr-name
                                                         net))
                     )
         (sorted-cs (sort candidates
                          (λ (x y) (< (cadr x) (cadr x)))))
         (min-infer-distance (and (not (empty? sorted-cs))
                                  (cadar sorted-cs)))
         )
    (if (empty? sorted-cs)
        ; None found
        'INSUCCES
        ; One or more found
        ;   Check if there exists a second alternative, and if
        ;   there is one, check if it contradicts the first one
        (if (and (< 1 (length sorted-cs))
                 (equal? (cadadr sorted-cs)
                         min-infer-distance)
                 (not (equal? (caadr sorted-cs)
                              (caar sorted-cs)))
                 )
            ; Contradiction: can't decide which one
            'CONTRADICTION
            ;sorted-cs
            (car sorted-cs)
            ;(cadadr sorted-cs)
            )
        )
    )
  )

; Recurses on all the members of @l and their parents (on AKO relation)
; returns: list of values found together with their depth level
; Ex.:
; (get-candidates  '((Mild-Hybrid 0)) 'Attr-ICE-power hybrid-net) --> ((HP100 1) (HP110 2))
(define (get-candidates  l attr net)
  (if (empty? l)
      '()
      ; More elements
      (let* ((first (caar l))
             (level (cadar l))
             (rest (cdr l))
             (statements (cadddr net))
             (v (get-attr first attr net))
             )
        (if v
            ; Value found, return
            (append (list (list v level))
                    (get-candidates rest
                                    attr
                                    net)
                    )
            ; Else, recurse adding parents
            (get-candidates (append rest
                                    (map (λ (x) (list x (+ level 1)))
                                         (make-ako-ancestor-list first net))
                                    )
                            attr
                            net))
        ))
  )

(define (remove-duplicates l)
  (foldr (lambda (x y) (cons x (filter (lambda (z) (not (equal? x z))) y))) empty l))



;; TESTs
(infer-attr 'Insight 'Attr-ICE-power hybrid-net)
(infer-attr 'Volt 'Attr-ICE-power hybrid-net)
(infer-attr 'Prius 'Attr-electric-power hybrid-net)
