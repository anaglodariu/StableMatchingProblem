#lang racket

(provide (all-defined-out))
(require racket/trace)

; ATENȚIE - Veți avea de reimplementat mai multe funcții
; din etapa 1, însă cu un alt mod de rezolvare (folosind
; funcționale sau alte funcții solicitate în enunț).
; Enunțul acestor exerciții va debuta prin "Ca în etapa 1,
; dar este interzisă recursivitatea explicită".


; TODO 1
; Ca în etapa 1, dar este interzisă recursivitatea explicită:
; Implementați o funcție care primește lista preferințelor
; bărbaților și calculează lista bărbaților din problemă.
; Folosiți orice funcțională exceptând foldl/foldr.
(define (get-men mpref)
  (map car mpref)) ; aplicam functia car pe fiecare lista din mpref (mpref este o lista de liste)


; TODO 2
; Ca în etapa 1, dar este interzisă recursivitatea explicită:
; Implementați o funcție care primește lista preferințelor
; femeilor și calculează lista femeilor din problemă.
; Folosiți foldl sau foldr, astfel încât să nu fie necesare
; operații de tip append sau reverse.
(define (get-women wpref)
  (foldr (λ (l acc) ; imi creez o functie anonima care se aplica pe l (elementele din lista de liste wpref) si acc (acumulator)
           (cons (car l) acc)) ; folosim foldr pentru ca ia elementele din wpref de la dreapta la stanga, deci acc nu va fi inversat
         '()
         wpref))


; TODO 3
; Ca în etapa 1, dar este interzisă recursivitatea explicită:
; Implementați o funcție care primește o listă de liste
; de preferințe (ori lista preferințelor bărbaților, ori a
; femeilor) și o persoană ale cărei preferințe apar în listă,
; și întoarce lista preferințelor acelei persoane.
; Se garantează că persoana apare în listă.
; Exemplu: dacă în lista wpref apare linia (ana bobo adi cos),
; (get-pref-list wpref 'ana) => '(bobo adi cos)
; Folosiți minim o funcțională și minim o funcție anonimă.
(define (get-pref-list pref person)
  (apply cdr (filter (λ (x)                         ; fac filter pe pref pentru a obtine lista care are pe primul loc pe person
                       (if (equal? (car x) person)  ; creez functie anonima care filtreaza toata lista de liste si pastreaza doar
                           #t                       ; lista care il contine pe person
                           #f))                     ; pentru a pastra doar lista de preferinte a lui person aplic cdr pe lista
                     pref)))


; TODO 4
; Ca în etapa 1, dar este interzisă recursivitatea explicită
; și sunt permiși operatorii condiționali:
; Implementați o funcție care primește o listă de tipul
; întors la exercițiul precedent (lista preferințelor unei persoane),
; respectiv două persoane x și y care apar în lista respectivă,
; și întoarce true dacă x este mai sus decât y în topul preferințelor
; și false în caz contrar.
; Folosiți funcția member.
(define (preferable? pref-list x y) ; daca y apare dupa x in pref-list, atunci (member ...(member ...)) ne va returna o lista
  (list? (member y (member x pref-list))))  ; daca nu, (member ...(member ...) va returna #f, care nu e o lista
                                  
; TODO 5
; Implementați recursiv funcționala find-first, care primește
; un predicat și o listă și întoarce primul element al listei
; care satisface predicatul, sau false dacă un asemenea element
; nu există.
; Implementarea trebuie să fie eficientă în sensul că nu trebuie
; să continue explorarea listei odată ce s-a găsit elementul.
(define (find-first p L)
  (cond
    ((null? L) #f)        ; se ajunge la lista vida, inseamna ca nu exista niciun element care sa respecte predicatul
    ((p (car L)) (car L)) ; cand se gaseste primul element care respecta predicatul se returneaza si nu se continua recursivitatea
    (else (find-first p (cdr L))))) ; n-am gasit elementul, deci se continua parcurgerea listei L


; TODO 6
; Ca în etapa 1, dar este interzisă recursivitatea explicită:
; Implementați o funcție care primește o listă de logodne
; (în care fiecare logodnă este o pereche cu punct între parteneri)
; și o persoană, și, în cazul în care această persoană apare pe prima
; poziție într-o logodnă, este întors partenerul său, altfel se
; întoarce false.
; Folosiți find-first, fără să îl apelați de 2 ori (hint: define în define).
(define (get-partner engagements person)
  (define (get-partner-helper pereche) ; am definit functia get-partner-helper care verifica 
    (if pereche                ; valoarea returnata de find-first, daca e pereche, avem nevoie de (cdr pereche) (partenerul lui person)
        (cdr pereche)
        #f))                   ; daca nu, returnam #f
  (get-partner-helper (find-first (λ (pereche)
                                    (if (equal? (car pereche) person) ; functia find-first va returna perechea care contine person sau #f 
                                        #t                            ; daca nu gaseste person in lista de perechi 
                                        #f))
                                  engagements))) 
        
  

; TODO 7
; Implementați recursiv funcționala change-first care primește
; un predicat p, o listă L și o valoare val, și întoarce o nouă 
; listă în care primul element din L care satisface predicatul p
; a fost înlocuit cu valoarea val, celelalte rămânând la fel.
; Dacă niciun element din L nu satisface predicatul, lista L
; rămâne neschimbată.
(define (change-first p L val)
  (cond
    ((null? L) '()) ; daca nu-l gasim pe p in L, vom trece prin recursivitate prin toata lista si vom returna tot L neschimbat
    ((p (car L)) (cons val (cdr L))) ; la prima aparitie a lui p in L, ne intoarcem din recursivitate 
    (else (cons (car L) (change-first p (cdr L) val))))) ; pana gasim primul p, inaintam in recursivitate
     
    
 


; TODO 8
; Implementați funcția update-engagements care primește o listă de
; logodne engagements și două persoane p1 și p2, și întoarce lista
; actualizată de logodne, în care vechiul partener al lui p1 este
; înlocuit cu p2.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - p1 era logodită în prealabil
; - fiecare cuplu din lista engagements are pe prima poziție
;   persoanele de același gen cu p1
; Folosiți change-first.
(define (update-engagements engagements p1 p2)
  (change-first (λ (pereche)
                  (if (equal? p1 (car pereche)) ; cream functie anonima pentru predicatul functiei change-first
                      #t
                      #f))
                engagements
                (cons p1 p2))) ; cand se gaseste perechea cu p1 pe prima pozitie, se inlocuieste cu perechea '(p1 . p2)


; TODO
; Copiați implementarea funcției better-match-exists? din etapa 1.
; Funcția nu este repunctată de checker, dar este necesară pentru
; implementarea funcției stable-match? de mai jos.
; Dacă nu ați implementat better-match-exists? în etapa 1, solicitați 
; o rezolvare de la asistent, astfel încât să puteți continua.
(define (better-match-exists? p1 p2 p1-list pref2 engagements)
  (cond
    ((null? engagements) #f) 
    ((and (not (equal? (car (car engagements)) p2))
          (preferable? p1-list (car (car engagements)) p2) 
          (preferable? (get-pref-list pref2 (car (car engagements))) p1 (cdr (car engagements)))) 
     #t)
    (else (better-match-exists? p1 p2 p1-list pref2 (cdr engagements)))))


; TODO 9
; Implementați funcția stable-match? care primește o listă 
; completă de logodne engagements, o listă de preferințe masculine 
; mpref și o listă de preferințe feminine wpref, și întoarce true 
; dacă toate cuplurile din engagements sunt stabile.
; Un cuplu este stabil dacă pentru niciunul din membrii cuplului
; nu există un alt partener mai potrivit (conform definiției de
; la funcția better-match-exists?).
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie



; daca engagements este de forma '((ana . cos) (bia . adi) (cora . bobo))
; daca verificam ca prima pereche e stabila fata de a 2-a (luandu-ul pe p1 = cos si p2 = ana in functia better-match-exists)(adica vedem daca pt cos exista un
; partener mai potrivit)
; cos trebuie sa o prefere pe bia in raport cu ana
; si 
; bia trebuie sa il prefere pe cos in raport cu adi
; daca verificam ca prima pereche e stabila fata de a 2-a (luandu-ul pe p1 = ana si p2 = cos in functia better-match-exists(daca am putea face asta
; fara sa modificam functia)(adica vedem daca pt ana exista un partener mai potrivit)
; ana trebuie sa il prefere pe adi fata de cos
; si
; adi trebuie sa o prefere pe ana fata de bia
; aceasta ultima verificare este echivalenta cu a verifica daca a 2-a pereche e stabila fata de prima
; deci daca folosesc functionala foldl in implementarea lui stable-match? voi lua fiecare pereche din engagements
; si voi aplica better-match-exists? pe ea (deci o voi verifica cu toate celelalte perechi)
(define (stable-match? engagements mpref wpref)
  (foldl (λ (x acc)
           (and
            acc ; cand functia better-match-exists? resturneaza prima oara #t -> acc devine #f si stable-match? va returna #f
            (not (better-match-exists? (cdr x) (car x) (get-pref-list mpref (cdr x)) wpref engagements))))
         #t
         engagements))

