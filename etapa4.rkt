#lang racket

(require "etapa2.rkt")
(require "etapa3.rkt")

(provide (all-defined-out))

;; Preferințele bărbaților și femeilor din problemă se pot schimba
;; în timp, dar de obicei ele nu se schimbă radical de la un moment
;; la altul. De aceea, în loc să rulăm de la zero algoritmul
;; Gale-Shapley de fiecare dată când se schimbă ceva, preferăm să
;; pornim de la lista de logodne stabile obținută în pasul anterior
;; și să o actualizăm, conform algoritmului următor:
;; - eliminăm din engagements cuplurile care au devenit instabile
;;   în urma modificărilor de preferințe
;;   - cuplurile rămase sunt stabile între ele și considerăm că
;;     se găsesc împreună într-o cameră, în timp ce membrii cuplurilor
;;     destrămate stau la coadă la intrarea în cameră
;; - cât timp coada nu este goală
;;   - prima persoană p din coadă intră în cameră și încearcă să se
;;     cupleze cu cineva care este deja acolo, astfel:
;;     - p-list = lista de preferințe a lui p
;;     - determină prima persoană p' din p-list care este în cameră
;;     - dacă p' nu e logodită, logodește p' cu p
;;     - dacă p' e logodită
;;       - dacă p' îl preferă pe p partenerului actual p''
;;         - logodește p' cu p
;;         - încearcă să îl cuplezi pe p'' cu altcineva din cameră
;;           (folosind același algoritm)
;;       - altfel, treci la următoarea persoană din p-list (dacă
;;         aceasta există, altfel p rămâne temporar fără partener)


; TODO 1
; Implementați funcția match care primește o persoană person care
; intră în cameră, lista engagements a cuplurilor din cameră
; (cuplurile având pe prima poziție persoanele de gen opus lui 
; person), o listă pref1 care conține preferințele celor de același 
; gen cu person, o listă pref2 cu preferințele celor de gen diferit, 
; respectiv o coadă queue a persoanelor din afara camerei,
; și întoarce lista de cupluri actualizată astfel încât noile
; cupluri să fie stabile între ele.
; Această listă se obține ca rezultat al încercării de a cupla pe
; person cu cineva din cameră (person va încerca în ordine persoanele 
; din lista sa de preferințe), care poate duce la destrămarea
; unui cuplu și necesitatea de a cupla noua persoană rămasă singură
; cu altcineva din cameră, etc. Procesul continuă până când:
; - ori avem numai cupluri stabile între ele în cameră, nimeni
;   nefiind singur
; - ori toate persoanele rămase singure nu ar fi preferate de nimeni
;   altcineva din cameră, și în acest caz convenim să "logodim"
;   aceste persoane cu valoarea #f, astfel încât funcția să
;   întoarcă în aceeași listă atât informația despre cine din
;   cameră este logodit, cât și despre cine este singur
(define (match person engagements pref1 pref2 queue)
  (if (not person) ; daca person este #f inseamna ca restul perechilor sunt stabile
      engagements
      (let ((pref-list (get-pref-list pref1 person)))
        (let loop ((list pref-list) (eng engagements))
          (if (null? list)
              (cons (cons #f person) eng) ; daca list e null inseamna ca nu am gasit un match pt person
              (let ((possible_partner (car list)))
                (if (find-first (λ (pereche)
                                  (equal? possible_partner (car pereche))) ; verifica daca persoana din list este in camera
                                eng)
                    (let ((partner (get-partner eng possible_partner)))
                      (if (or
                           (preferable? (get-pref-list pref2 possible_partner) person partner) ; verifica preferintele persoanei din pref-list
                           (not partner)) ; verifica daca partner e #f
                          (match partner (update-engagements eng possible_partner person) pref1 pref2 queue) ; face update la eng si cauta match pt partner
                          (loop (cdr list) eng))) ; luam urmatoarea persoana din list
                    (loop (cdr list) eng)))))))) ; luam urmatoarea persoana din list
            


; TODO 2
; Implementați funcția path-to-stability care primește lista
; engagements a cuplurilor din cameră, o listă de preferințe 
; masculine mpref, o listă de preferințe feminine wpref, respectiv
; coada queue a persoanelor din afara camerei, și întoarce lista
; completă de logodne stabile, obținută după ce fiecare persoană
; din queue este introdusă pe rând în cameră și supusă procesului
; descris de funcția match.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
; - persoanele nelogodite din cameră apar în engagements sub forma
;   (#f . nume-bărbat) sau (nume-femeie . #f)
(define (path-to-stability engagements mpref wpref queue)
  (let loop ((eng engagements) (q queue))
    (if (null? q)
        eng
        (let* ((men_list (get-men mpref))
               (women_list (get-women wpref))
               (person (car q))
               (p (λ (x) (equal? x person))))
          (cond
            ((find-first p men_list)
             (let ((new_eng (match person eng mpref wpref queue)))
               (loop new_eng (cdr q))))
            ((find-first p women_list)
             (let* ((rev_eng (map (λ (pereche) (cons (cdr pereche) (car pereche))) eng))
                    (new_eng (match person rev_eng wpref mpref queue))
                    (rev_eng1 (map (λ (pereche) (cons (cdr pereche) (car pereche))) new_eng)))
               (loop rev_eng1 (cdr q)))))))))



; TODO 3
; Implementați funcția update-stable-match care primește o listă 
; completă de logodne engagements (soluția anterioară), o listă de 
; preferințe masculine mpref și o listă de preferințe feminine wpref 
; (adică preferințele modificate față de cele pe baza cărora s-a 
; obținut soluția engagements), și calculează o nouă listă de logodne 
; stabile - conform cu noile preferințe, astfel:
; - unstable = cuplurile instabile din engagements
; - room-engagements = engagements - unstable
; - queue = persoanele din unstable
; - aplică algoritmul path-to-stability
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
(define (update-stable-match engagements mpref wpref)
  (define (stable? pereche unstable)
    (cond
      ((null? unstable) #t)
      ((equal? pereche (car unstable)) #f)
      (else (stable? pereche (cdr unstable)))))
  (let* ((unstable (get-unstable-couples engagements mpref wpref))
         (queue (get-couple-members unstable)))
    (let ((room-engagements (filter (λ (pereche)
                                      (stable? pereche unstable))
                                    engagements)))
      (path-to-stability room-engagements mpref wpref queue))))
         


; TODO 4
; Implementați funcția build-stable-matches-stream care primește
; un flux pref-stream de instanțe SMP și întoarce fluxul de 
; soluții SMP corespunzător acestor instanțe.
; O instanță SMP este o pereche cu punct între o listă de preferințe
; masculine și o listă de preferințe feminine.
; Fluxul rezultat se va obține în felul următor:
; - primul element se calculează prin aplicarea algoritmului
;   Gale-Shapley asupra primei instanțe
; - următoarele elemente se obțin prin actualizarea soluției
;   anterioare conform algoritmului implementat în etapa 4 a temei
; Trebuie să lucrați cu interfața pentru fluxuri. Dacă rezolvați
; problema folosind liste și doar convertiți în/din fluxuri,
; punctajul pe acest exercițiu se anulează în totalitate.
  
(define (build-stable-matches-stream pref-stream)
  (if (stream-empty? pref-stream)
      empty-stream
      (let* ((initial (stream-first pref-stream))
             (mpref_initial (stream-first initial))
             (wpref_initial (stream-rest initial))
             (sol0 (gale-shapley mpref_initial wpref_initial)))
        (stream-cons
         sol0
            (build (stream-rest pref-stream) sol0)))))

(define (build pref-stream sol0)
  (if (stream-empty? pref-stream)
      '()
      (let* ((preferinte (stream-first pref-stream))
             (mpref (stream-first preferinte))
             (wpref (stream-rest preferinte))
             (sol (update-stable-match sol0 mpref wpref)))
        (stream-cons
              sol
              (build (stream-rest pref-stream) sol)))))


