#lang racket/gui
; ---------------------------------------- 
; |            ;GROUP PROJECT            |
; ----------------------------------------
;                 Students:
;               Sunny Subash
;              Claudiu Stefan
;              Vivek Sheth
;               Maksim Simonov
; ----------------------------------------


;-----------------------------------------------------------------------
;Maksim Simonov
;------------------------------------------------------------------------
(define brown-line '("4 Privet Drive"  "Hogsmeade"  "Magical Hospital"  "Platform 9(3/4)" "Hogwarts"))
(define orange-line '("Hogsmeade" "Quidditch Store" "Diagon Alley" "Platform 9(3/4)"))
(define blue-line '("Magical Hospital" "Post Office" "Azkaban"))
;-------------------------------------------------------------------------
;everyone
;------------------------------------------------------------------

(define route (λ (from to list)
                
                (drop (take list (+ 1 to )) from)
                (drop (take list (+ 1 to )) from)))

(define l->s (λ(l)
               (cond ((empty? l) "")
                     ((empty? (rest l))  (first l))
                     (else (string-append  (first l)
                                           " -> "
                                           (l->s (rest l)))))))
;-----------------------------------------------------------------------
;Sunny and Claudiu
;------------------------------------------------------------------------
; ------------------------- Fonts & Colors --------------------------
(define title_font (make-font #:size 20 #:family 'script #:smoothing 'smoothed #:weight 'bold))
(define button_font (make-font #:size 15 #:family 'script #:smoothing 'smoothed #:weight 'bold))


(define c1 (make-object color% 127 9 9))
(define c2 (make-object color% 255 196 0))
(define c3 (make-object color% 36 34 30))

; ------------------------- Fonts & Colors --------------------------

;-----------------------------------------------------------------------
;Sunny and Claudiu and Vivek
;------------------------------------------------------------------------

;--------------------------welcome Screen----------------------------
(define welcome (new frame% [label "Welcome to harry potter railway networks"] 
                   [width 420] [height 250]))


(define msgTitle (new message%
                      [parent welcome] 
                      [label "Please Select the train lines:"]
                      [font title_font]))


;-----------------------------------------------------------------------
;Maksim and Vivek
;------------------------------------------------------------------------
; ------------------------- Welcome page --------------------------

(define Brown (new button%
                   [label "Brown line "][parent welcome] [horiz-margin 10][vert-margin 5] [min-width 10]  [font button_font] [callback (λ (o e)
                                                                                (send Brown set-label "Loading...")
                                                                                (sleep 0.1)                                                                     
                                                                                (send Brown set-label "Brown line ")
                                                                                (send welcome show #f)
                                                                                (send BrownLine show #t)
                                                                                )] ))
(define Orange (new button%
                    [label "Orange line"][parent welcome] [horiz-margin 10][vert-margin 5] [min-width 10]  [font button_font] [callback (λ (o e)
                                                                                  (send Orange set-label "Loading...")
                                                                                  (sleep 0.1)                                                                     
                                                                                  (send Orange set-label "Orange line")
                                                                                  (send welcome show #f)
                                                                                  (send OrangeLine show #t)
                                                                                  )] ))

(define Blue (new button%
                  [label "  Blue line   "][parent welcome] [horiz-margin 10][vert-margin 5] [min-width 10]  [font button_font] [callback (λ (o e)
                                                                              (send Blue set-label "Loading...")
                                                                              (sleep 0.1)                                                                     
                                                                              (send Blue set-label "  Blue line   ")
                                                                              (send welcome show #f)
                                                                              (send BlueLine show #t)
                                                                              )] ))


;-----------------------------------------------------------------------
;Sunny and Maksim
;------------------------------------------------------------------------
; ------------------------- Brown Line --------------------------
(define BrownLine (new frame% [label "Harry Potter Search - Brown line"]
                   [width 550] [height 320]))

(define main (new vertical-panel% [parent BrownLine] [style '(border)][border 20]  ))

(new message%
                      [parent main]
                      [label "Harry Potter Search"]
                      [font title_font])
(define vp1
  (new vertical-panel% [parent main] (alignment (list 'center 'top)) [style '(border)] [horiz-margin 10] [vert-margin 5] ))

( define from ( new choice%
[ label "From: " ] [ parent vp1 ] [font button_font] [horiz-margin 5][vert-margin 5] [min-width 10]
[ choices brown-line ]))

( define to ( new choice%
[ label " to     : " ] [ parent vp1 ] [font button_font] [horiz-margin 5][vert-margin 5] [min-width 10]
[ choices brown-line]))

(define vp
  (new vertical-panel% [parent main] (alignment (list 'center 'top)) [style '(border)] [horiz-margin 10] [vert-margin 2] ))

(define new-message0 (new message%
                         [label "Your Journey :"]
                         [parent vp] [stretchable-width #t] ))
(define new-message (new message%
                         [label "Your Journey :"]
                         [parent vp] [stretchable-width #t]))
(define new-message3 (new message%
                         [label "Distance(KM) :"]
                         [parent vp] [stretchable-width #t] ))

(define new-message1 (new message%
                         [label "Your Journey :"]
                         [parent vp] [stretchable-width #t] ))
(define new-message4 (new message%
                         [label "Stops :"]
                         [parent vp] [stretchable-width #t] ))

(define new-message2 (new message%
                         [label ""]
                         [parent vp] [stretchable-width #t]))



(define bord
  (new horizontal-panel% [parent main] (alignment (list 'center 'top)) [style '(border)] [horiz-margin 10] [vert-margin 2] ))

(define button (new button%
[ parent bord ]
[ label " APPARATE " ] [horiz-margin 5][vert-margin 5] [min-width 10] [font button_font]
[ callback (λ (o e)
             (send button set-label "searching...")
                                                                     (sleep 1)
                                                                     
                                                                     (send button set-label  "APPARATE ")
             (send new-message set-label (l->s(route (send from get-selection)
                             (send to get-selection) brown-line)))
             (send new-message1 set-label(real->decimal-string (* (- (length (route (send from get-selection)
                             (send to get-selection) brown-line)) 1) 10)))
             (send new-message2 set-label(real->decimal-string  (- (length (route (send from get-selection)
                             (send to get-selection) brown-line)) 1)))
             (send new-message show #t) (send new-message1 show #t) (send new-message0 show #t) (send new-message3 show #t)
(send new-message4 show #t) )]))
(define button2 (new button%
[ parent bord ]
[ label " Return " ] [horiz-margin 5][vert-margin 5] [min-width 10] [font button_font]
[ callback (λ (o e)
             (send button2 set-label "---")
                                                                     (sleep 1)
                                                                     
                                                                     (send button2 set-label  " Return ")
             (send new-message set-label (l->s(reverse(route (send from get-selection)
                             (send to get-selection) brown-line))))
             (send new-message1 set-label(real->decimal-string (* (- (length (route (send from get-selection)
                             (send to get-selection) brown-line)) 1) 10)))
             (send new-message2 set-label(real->decimal-string  (- (length (route (send from get-selection)
                             (send to get-selection) brown-line)) 1)))
             (send new-message show #t) (send new-message1 show #t) (send new-message0 show #t) (send new-message3 show #t)
(send new-message4 show #t) )]))

(new button%
                   [label "Back"][parent bord][horiz-margin 5][vert-margin 5] [min-width 10] [font button_font]    [callback (λ (o e)
                                                              
                                                             (send welcome show #t)
                                                             (send BrownLine show #f)
                                                             )] )


(send new-message0 show #f)
(send new-message show #f)
(send new-message1 show #f)
(send new-message3 show #f)
(send new-message4 show #f)
;-----------------------------------------------------------------------
;Claudiu and Vivek
;------------------------------------------------------------------------

; ------------------------- Orange Line --------------------------
(define OrangeLine (new frame% [label "Harry Potter Search - Orange line"]
                   [width 550] [height 320]))

(define OL (new vertical-panel% [parent OrangeLine] [style '(border)][border 20]  ))


(new message%
                      [parent OL]
                      [label "Harry Potter Search"]
                      [font title_font])

(define O-bord2
  (new vertical-panel% [parent OL] (alignment (list 'center 'top)) [style '(border)] [horiz-margin 10] [vert-margin 5] ))


( define fromO ( new choice%
[ label "Start From   : " ] [ parent O-bord2 ] [font button_font] [horiz-margin 5][vert-margin 5] [min-width 10]
[ choices orange-line ]))

( define toO ( new choice%
[ label " Destination : " ] [ parent O-bord2 ] [font button_font] [horiz-margin 5][vert-margin 5] [min-width 10]
[ choices orange-line]))




(define O-bord1
  (new vertical-panel% [parent OL] (alignment (list 'center 'top)) [style '(border)] [horiz-margin 10] [vert-margin 2] ))


(define new0 (new message%
                         [label "Your Journey :"]
                         [parent O-bord1] [stretchable-width #t] ))
(define new3 (new message%
                         [label "Your Journey :"]
                         [parent O-bord1] [stretchable-width #t]))
(define new4 (new message%
                         [label "Distance(KM) :"]
                         [parent O-bord1] [stretchable-width #t] ))

(define new1 (new message%
                         [label "Your Journey :"]
                         [parent O-bord1] [stretchable-width #t] ))
(define new5 (new message%
                         [label "Stops :"]
                         [parent O-bord1] [stretchable-width #t] ))

(define new2 (new message%
                         [label ""]
                         [parent O-bord1] [stretchable-width #t]))




(define O-bord
  (new horizontal-panel% [parent OL](alignment (list 'center 'top)) [style '(border)] [horiz-margin 10] [vert-margin 2] ))


(define button1 (new button%
[ parent O-bord ]
[ label " APPARATE " ] [horiz-margin 5][vert-margin 5] [min-width 10] [font button_font]
[ callback (λ (o e)
             (send button1 set-label "searching...")
                                                                     (sleep 1)
                                                                     
                                                                     (send button1 set-label  "APPARATE ")
             (send new3 set-label (l->s(route (send fromO get-selection)
                             (send toO get-selection) orange-line)))
             (send new1 set-label(real->decimal-string (* (- (length (route (send fromO get-selection)
                             (send toO get-selection) orange-line)) 1) 10)))
             (send new2 set-label(real->decimal-string  (- (length (route (send fromO get-selection)
                             (send toO get-selection) orange-line)) 1)))
             (send new3 show #t) (send new1 show #t)(send new2 show #t) (send new0 show #t) (send new4 show #t) (send new5 show #t))]))
(define button4 (new button%
[ parent O-bord ]
[ label "Return" ] [horiz-margin 5][vert-margin 5] [min-width 10] [font button_font]
[ callback (λ (o e)
             (send button4 set-label "---")
                                                                     (sleep 1)
                                                                     
                                                                     (send button4 set-label  "Return")
             (send new3 set-label (l->s(reverse(route (send fromO get-selection)
                             (send toO get-selection) orange-line))))
             (send new1 set-label(real->decimal-string (* (- (length (route (send fromO get-selection)
                             (send toO get-selection) orange-line)) 1) 10)))
             (send new2 set-label(real->decimal-string  (- (length (route (send fromO get-selection)
                             (send toO get-selection) orange-line)) 1)))
             (send new3 show #t) (send new1 show #t)(send new2 show #t) (send new0 show #t) (send new4 show #t) (send new5 show #t))]))



(new button%
                   [label "Back"][parent O-bord]  [horiz-margin 5][vert-margin 5] [min-width 10] [font button_font]   [callback (λ (o e)                                                                                
                                                             (send welcome show #t)
                                                             (send OrangeLine show #f)
                                                             )] )
(send new0 show #f)
(send new3 show #f)
(send new1 show #f)
(send new2 show #f)
(send new4 show #f)
(send new5 show #f)

;-----------------------------------------------------------------------
;Sunny
;------------------------------------------------------------------------

; ------------------------- Blue Line --------------------------
(define BlueLine (new frame% [label "Harry Potter Search - blue line"]
                   [width 550] [height 320]))

(define BL (new vertical-panel% [parent BlueLine] [style '(border)][border 20]  ))

(new message%
                      [parent BL]
                      [label "Harry Potter Search"]
                      [font title_font])
(define vpB
  (new vertical-panel% [parent BL] (alignment (list 'center 'top)) [style '(border)] [horiz-margin 10] [vert-margin 5] ))

( define fromB ( new choice%
[ label "From: " ] [ parent vpB ] [font button_font] [horiz-margin 5][vert-margin 5] [min-width 10]
[ choices blue-line ]))

( define toB ( new choice%
[ label " to     : " ] [ parent vpB ] [font button_font] [horiz-margin 5][vert-margin 5] [min-width 10]
[ choices blue-line]))

(define vpb
  (new vertical-panel% [parent BL] (alignment (list 'center 'top)) [style '(border)] [horiz-margin 10] [vert-margin 2] ))

(define n-message0 (new message%
                         [label "Your Journey :"]
                         [parent vpb] [stretchable-width #t] ))
(define n-message (new message%
                         [label "Your Journey :"]
                         [parent vpb] [stretchable-width #t]))
(define n-message3 (new message%
                         [label "Distance(KM) :"]
                         [parent vpb] [stretchable-width #t] ))

(define n-message1 (new message%
                         [label "Your Journey :"]
                         [parent vpb] [stretchable-width #t] ))
(define n-message4 (new message%
                         [label "Stops :"]
                         [parent vpb] [stretchable-width #t] ))

(define n-message2 (new message%
                         [label ""]
                         [parent vpb] [stretchable-width #t]))



(define bordB
  (new horizontal-panel% [parent BL] (alignment (list 'center 'top)) [style '(border)] [horiz-margin 10] [vert-margin 2] ))

(define button3 (new button%
[ parent bordB ]
[ label " APPARATE " ] [horiz-margin 5][vert-margin 5] [min-width 10] [font button_font]
[ callback (λ (o e)
             (send button3 set-label "searching...")
                                                                     (sleep 1)
                                                                     
                                                                     (send button3 set-label  "APPARATE ")
             (send n-message set-label (l->s(route (send fromB get-selection)
                             (send toB get-selection) blue-line)))
             (send n-message1 set-label(real->decimal-string (* (- (length (route (send fromB get-selection)
                             (send toB get-selection) blue-line)) 1) 10)))
             (send n-message2 set-label(real->decimal-string  (- (length (route (send fromB get-selection)
                             (send toB get-selection) blue-line)) 1)))
             (send n-message show #t) (send n-message1 show #t) (send n-message0 show #t) (send n-message3 show #t)
(send n-message4 show #t) )]))
(define button5 (new button%
[ parent bordB ]
[ label "Return" ] [horiz-margin 5][vert-margin 5] [min-width 10] [font button_font]
[ callback (λ (o e)
             (send button5 set-label "---")
                                                                     (sleep 1)
                                                                     
                                                                     (send button5 set-label  "Return ")
             (send n-message set-label (l->s(reverse(route (send fromB get-selection)
                             (send toB get-selection) blue-line))))
             (send n-message1 set-label(real->decimal-string (* (- (length (route (send fromB get-selection)
                             (send toB get-selection) blue-line)) 1) 10)))
             (send n-message2 set-label(real->decimal-string  (- (length (route (send fromB get-selection)
                             (send toB get-selection) blue-line)) 1)))
             (send n-message show #t) (send n-message1 show #t) (send n-message0 show #t) (send n-message3 show #t)
(send n-message4 show #t) )]))

(new button%
                   [label "Back"][parent bordB][horiz-margin 5][vert-margin 5] [min-width 10] [font button_font]    [callback (λ (o e)
                                                              
                                                             (send welcome show #t)
                                                             (send BlueLine show #f)
                                                             )] )


(send n-message0 show #f)
(send n-message show #f)
(send n-message1 show #f)
(send n-message3 show #f)
(send n-message4 show #f)
; ---------------------------------------------------------------
(send welcome show #t)
