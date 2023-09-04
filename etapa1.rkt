#lang racket

(provide (all-defined-out))

; TODO 1
; Implementați o funcție care primește lista preferințelor
; bărbaților și calculează lista bărbaților din problemă.
; Folosiți recursivitate pe stivă.

(define (get-men mpref) ; mpref e o lista de liste
  (if (null? mpref)
      '()
      (cons (car (car mpref)) (get-men (cdr mpref))))) ; din fiecare lista se ia primul element

; TODO 2
; Implementați o funcție care primește lista preferințelor
; femeilor și calculează lista femeilor din problemă.
; Folosiți recursivitate pe coadă.

(define (get-women wpref)
  (get-women-helper wpref '()))

(define (get-women-helper wpref L) ; avem nevoie de un helper pentru recursivitate pe coada
  (if (null? wpref)
      (reverse L) ; elementele vor fi puse in ordine inversa, deci facem reverse pe acumulator
      (get-women-helper (cdr wpref) (cons (car (car wpref)) L))))

; TODO 3
; Implementați o funcție recursivă care primește o listă de liste
; de preferințe (ori lista preferințelor bărbaților, ori a
; femeilor) și o persoană ale cărei preferințe apar în listă,
; și întoarce lista preferințelor acelei persoane.
; Se garantează că persoana apare în listă.
; Exemplu: dacă în lista wpref apare linia (ana bobo adi cos),
; (get-pref-list wpref 'ana) => '(bobo adi cos)
; Observație: de fiecare dată când ne referim la lista
; preferințelor unei persoane p, ne referim la o listă care conține
; doar persoanele de sex opus, nu și pe p pe prima poziție.

(define (get-pref-list pref person)
  (cond
    ((equal? (car (car pref)) person) (cdr (car pref))) ; cand gasim pe person, returnam cdr de lista
    (else (get-pref-list (cdr pref) person))))

; TODO 4
; Implementați o funcție recursivă care primește o listă de tipul
; întors la exercițiul precedent (lista preferințelor unei persoane),
; respectiv două persoane x și y care apar în lista respectivă,
; și întoarce true dacă x este mai sus decât y în topul preferințelor
; și false în caz contrar.
; Nu folosiți operatori condiționali, folosiți în schimb operatori
; logici pentru a obține același efect.

(define (preferable? pref-list x y)
  (and (not (null? pref-list)) ; atata timp cat lista nu e vida se evalueaza restul codului (lista nu va fi niciodata vida)
       (or
        (and (equal? (car pref-list) x)  ; daca elementul curent e x, functia and de la linia 57 va da #t
             (not (equal? (car pref-list) y))) ; deci ne intoarcem din recursivitate; functia or va avea unul din argumente #t si functia preferable? va da #t
        (and (not (equal? (car pref-list) x)) ; daca elementul curent nu e x se evalueaza codul de la linia 59
             (not (equal? (car pref-list) y)) ; daca elementul curent e y se evalueaza codul de la linia 59, functia and de la linia 59 va da #f
             (preferable? (cdr pref-list) x y))))) ; ne vom intoarce din recursivitate, functia or va avea toate argumentele #f, functia preferable? va da #f
             ; daca elementul curent nu e nici x, nici y, reiau pasii anteriori pe cdr de lista
             ; functia preferable? se opreste in true daca da de x prima data sau in false daca da de y prima data in lista

; TODO 5
; Implementați o funcție recursivă care primește o listă de logodne
; (în care fiecare logodnă este o pereche cu punct între parteneri)
; și o persoană, și, în cazul în care această persoană apare pe prima
; poziție într-o logodnă, este întors partenerul său, altfel se
; întoarce false.
; Folosiți cond.

(define (get-partner engagements person)
  (cond
    ((null? engagements) #f)
    ((equal? (car (car engagements)) person) (cdr (car engagements))) ; daca gasim person pe primul loc intr-o pereche, intoarcem partenerul sau
    (else (get-partner (cdr engagements) person))))

; TODO 6
; Implementați o funcție care primește 2 persoane logodite, p1 și p2,
; lista preferințelor lui p1, lista preferințelor tuturor persoanelor
; de același gen cu p2, respectiv lista tuturor logodnelor, și întoarce
; true dacă există un partener mai potrivit pentru p1, și false altfel.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - lista logodnelor este completă, este un posibil rezultat al problemei
; - logodnele din listă au pe prima poziție persoana de același gen cu p2
; - un partener p' este mai potrivit decât p2 dacă îndeplinește 2 condiții:
;   - p1 îl preferă pe p' în raport cu p2
;   - p' îl preferă pe p1 în raport cu persoana cu care este logodit

(define (better-match-exists? p1 p2 p1-list pref2 engagements)
  (cond
    ((null? engagements) #f) ; daca am verificat toate perechile din engagements -> nu exista un better-match
    ((and (not (equal? (car (car engagements)) p2)) ; nu mai iau si perechea (p2 p1) din engagements in considerare
          (preferable? p1-list (car (car engagements)) p2) ; verific daca p1 îl preferă pe p' în raport cu p2
          (preferable? (get-pref-list pref2 (car (car engagements))) p1 (cdr (car engagements)))) ; verific daca p' îl preferă pe p1 în raport cu persoana cu care este logodit
     #t) ; daca se verifica toate conditiile -> exista un better-match
    (else (better-match-exists? p1 p2 p1-list pref2 (cdr engagements))))) ; parcurg restul perechilor din engagements

