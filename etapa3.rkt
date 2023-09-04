#lang racket

(require "etapa2.rkt")

(provide (all-defined-out))

; TODO 1
; După modelul funcției stable-match?, implementați funcția
; get-unstable-couples care primește o listă de logodne
; engagements, o listă de preferințe masculine mpref și o 
; listă de preferințe feminine wpref, și întoarce lista
; tuturor cuplurilor instabile din engagements.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
; Nu este permisă recursivitatea pe stivă.
; Nu sunt permise alte funcții ajutătoare decât
; better-match-exists? și funcțiile de manipulare a listelor de
; preferințe definite în etapele anterioare.
; Nu este permisă apelarea multiplă a aceleiași funcții pe
; aceleași argumente.
; Folosiți una sau mai multe dintre expresiile let, let*, letrec,
; named let pentru a vă putea conforma acestor restricții.

(define (get-unstable-couples engagements mpref wpref)
  (let loop ((eng engagements) (rev_eng (map (λ (pereche) (cons (cdr pereche) (car pereche))) engagements)) (acc '())) 
    (if (null? eng)             ; imi creez functie de inversare a partenerilor din perechi
        acc                     ; pt a verifica daca exista un partener mai potrivit si pentru tip si pentru tipa
        (let* ((pereche (car eng))
               (rest (cdr eng))
               (man (cdr pereche))
               (woman (car pereche))
               (unstable (better-match-exists? man woman (get-pref-list mpref man) wpref engagements))
               (unstable_rev (better-match-exists? woman man (get-pref-list wpref woman) mpref rev_eng)))
          (if (or unstable unstable_rev)              ; daca exista un partener mai potrivit fie pt tipa, fie pt tip (sau amandoi)
              (loop rest rev_eng (cons pereche acc))  ; atunci adaug perechea instabila in acc
              (loop rest rev_eng acc))))))



; TODO 2
; Implementați funcția engage care primește o listă free-men
; de bărbați încă nelogodiți, o listă de logodne parțiale 
; engagements (unde fiecare cuplu are pe prima poziție o femeie),
; o listă de preferințe masculine mpref și o listă de preferințe 
; feminine wpref, și întoarce o listă completă de logodne stabile,
; obținută conform algoritmului Gale-Shapley:
; - cât timp există un bărbat m încă nelogodit
;   - w = prima femeie din preferințele lui m pe care m nu a cerut-o încă
;   - dacă w este nelogodită, adaugă perechea (w, m) la engagements
;   - dacă w este logodită cu m'
;     - dacă w îl preferă pe m lui m'
;       - m' devine liber
;       - actualizează partenerul lui w la m în engagements
;     - altfel, repetă procesul cu următoarea femeie din lista lui m
; Folosiți named let pentru orice proces recursiv ajutător (deci nu
; veți defini funcții ajutătoare recursive).
; Folosiți let și/sau let* pentru a evita calcule duplicate.

(define (engage free-men engagements mpref wpref)
  (let loop1 ((L free-men) (eng engagements))
    (if (not (null? L))
        (let ((free-man (car L)) (rest (cdr L)))
          (let loop2 ((pref-list-man (get-pref-list mpref free-man)))  ; pref list pt barbat nelogodit
            (let* ((woman (car pref-list-man))                         ; prima femeie din pref list
                   (partner (get-partner eng woman))                   ; partenerul femeii daca are
                   (pref-list-woman (get-pref-list wpref woman)))      ; pref list pt femeie
              (if partner                                              ; daca femeia are deja partener
                  (if (preferable? pref-list-woman free-man partner)   ; daca il prefera pe cel nelogodit in schimbul propriului partener
                      (loop1 (cons partner rest) (update-engagements eng woman free-man))
                      (loop2 (cdr pref-list-man)))
                  (loop1 rest (cons (cons woman free-man) eng))))))  
        eng)))


; TODO 3
; Implementați funcția gale-shapley care este un wrapper pentru
; algoritmul implementat de funcția engage. Funcția gale-shapley
; primește doar o listă de preferințe masculine mpref și o listă
; de preferințe feminine wpref și calculează o listă completă de
; logodne stabile conform acestor preferințe.
(define (gale-shapley mpref wpref)
  (engage (get-men mpref) '() mpref wpref))


; TODO 4
; Implementați funcția get-couple-members care primește o listă
; de perechi cu punct și întoarce o listă simplă cu toate elementele 
; care apar în perechi.
; Folosiți funcționale, fără recursivitate explicită.
(define (get-couple-members pair-list)
  (foldr (λ (x acc)
           (append (list (car x) (cdr x)) acc))
         '()
         pair-list))

