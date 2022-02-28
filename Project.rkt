#lang racket
;Team project

;State table

(define fsm '(("whiteturn1" "move" "e4")
  ("e4" "change" "blackturn1")
  ("blackturn1" "move" "e5")
  ("e5" "change" "whiteturn2")
  ("whiteturn2" "move" "Nf3")
  ("Nf3" "change" "blackturn2")
  ("blackturn2" "move" "Nc6")
  ("Nc6" "change" "whiteturn3")
  ("whiteturn3" "move" "Bc4")
  ))

;=================================================================

;Moving to the next state

(define next-state (λ (state event fsm)
                    (cond
                      ((and (string? state) (string? event))
                       (cond
                         ((empty? fsm) "error404")
                          ((and
                            (equal? state (first (first fsm))))
                            (equal? event (first (reverse (first fsm))))
                            (first (reverse (first fsm))))
                        (#t
                         (next-state state event (rest fsm)))))
                       (#t "error 505"))))

;=================================================================

;Sequence (1st function)

(define run-seq (λ (init-state event-seq fsm)
                       (cond
                         ((null? event-seq) "false")
                         (else
                         (set! init-state (next-state init-state (first event-seq) fsm))
                          (sleep 1)
                          (println init-state)
                          (run-seq init-state (rest event-seq) fsm )
                          (sleep 1)
                          ))))

;Sequence (2nd function)

(define run-seq1 (λ (a b c) ;(init-state event-seq fsm) <--- the arguments for run-seq1.
                       (cond
                         ((null? b) #f ) 
                         (else
                          (let ([current-state (next-state a (car b) c)])  
                          (sleep 1) 
                          (println current-state)  
                          (run-seq1 current-state (cdr b)  c))))))

;Sequnce of "Italian game opening"

(run-seq "whiteturn1" '( "move" "change" "move" "change" "move" "change" "move" "chang" "move") fsm)