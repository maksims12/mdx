#lang racket/gui
(require compatibility/mlist)
;; ---------------------------------------- 
; |            ;GROUP PROJECT            |
; ----------------------------------------
;                 Students:
;               Sunny Subash
;              Claudiu Stefan
;              Vivek Sheth
;               Maksim Simonov
; ----------------------------------------
;this is done by Claudiu Stefan
; ------------------------- Fonts & Colors --------------------------
(define title_font (make-font #:size 20 #:family 'script #:smoothing 'smoothed #:weight 'bold))
(define button_font (make-font #:size 20 #:family 'script #:smoothing 'smoothed #:weight 'bold))


(define c1 (make-object color% 127 9 9))
(define c2 (make-object color% 255 196 0))
(define c3 (make-object color% 36 34 30))

; ------------------------- Fonts & Colors --------------------------
;this is done by Sunny Subash Vivek Sheth Maksim Simonov
;-------------------------------------------------functionality----------------------------------------
(define brown_line (set "4 Privet Drive" "Hogsmeade" "Magical Hospital" "Platform 9(3/4)" "Hogwarts"))
(define orange_line (set "Hogsmeade" "Quidditch Store" "Diagon Alley"))
(define blue_line (set "Magical Hospital" "Post Office" "Azkaban"))


(define map_edges (list
                   '("4 Privet Drive" "Hogsmeade")
                   '("Hogsmeade" "Magical Hospital")
                   '("Magical Hospital" "Platform 9(3/4)")
                   '("Platform 9(3/4)" "Hogwarts")
                   '("Hogsmeade" "Quidditch Store")
                   '("Quidditch Store" "Diagon Alley")
                   '("Magical Hospital" "Post Office")
                   '("Post Office" "Azkaban")
                   '("Hogsmeade" "4 Privet Drive")
                   '("Magical Hospital" "Hogsmeade")
                   '("Platform 9(3/4)" "Magical Hospital")
                   '("Hogwarts" "Platform 9(3/4)")
                   '("Quidditch Store" "Hogsmeade")
                   '("Diagon Alley" "Quidditch Store")
                   '("Post Office" "Magical Hospital")
                   '("Azkaban" "Post Office")
                   ))

(define map_edges1 (list
                    '("Hogsmeade" "Quidditch Store")
                    '("Quidditch Store" "Diagon Alley")
                    '("Quidditch Store" "Hogsmeade")
                    '("Diagon Alley" "Quidditch Store")
                    ))

(define map_edges2 (list
                    '("Magical Hospital" "Post Office")
                    '("Post Office" "Azkaban")
                    '("Post Office" "Magical Hospital")
                    '("Azkaban" "Post Office")
                    ))

;in order to find out all arrival points, if x is a departure point.
(define edge (lambda (x graph)
               (map (lambda (y)(first (rest y))) (filter (lambda (y)
                                                           (equal? (first y) x)) graph))))


(define pathlist (mlist))
(set! pathlist '())
(define rev (λ (list) (reverse list) pathlist))




;to check if where is a path from a departure point to an arrival point.
(define path (lambda (x y map_edges map_vertex)
               (cond
                 ((and (null? x) (null? y)) "Please enter stops")
                  
                       

                       
                 ((equal? x y)  (set! pathlist (cons x pathlist)))
                 
                 ((not (set-member? map_vertex x)) )
                 ((not (set-member? map_vertex y)) )
                 
                 ((and (set-member? brown_line x)
                       (set-member? brown_line y)
                       (not (equal? x y)))
                  (ormap (lambda (z) (path z y map_edges
                                           (set-remove brown_line x)))
                         (edge x map_edges))

                  (set! pathlist (cons x pathlist)))

                 ((and (set-member? brown_line x)
                       (set-member? brown_line y)
                       (not (equal? x y)))
                  (ormap (lambda (z) (path z x map_edges
                                           (set-remove brown_line y)))
                         (edge y map_edges))
                  (set! pathlist (cons x pathlist)))
                 
                 ((and (set-member? orange_line x)
                       (set-member? orange_line y)
                       (not (equal? x y)))
                  (ormap (lambda (z) (path z y map_edges1
                                           (set-remove orange_line x)))
                         (edge x map_edges1))
                  (set! pathlist (cons x pathlist)))
                 
                 ((and (set-member? blue_line x)
                       (set-member? blue_line y)
                       (not (equal? x y)))
                  (ormap (lambda (z) (path z y map_edges2
                                           (set-remove blue_line x)))
                         (edge x map_edges2))
                  (set! pathlist (cons x pathlist)))

                 ((and (set-member? brown_line x)
                       (set-member? orange_line y))
                  (ormap (lambda (z) (path z y map_edges
                                           (set-remove (set-union brown_line orange_line) x)))
                         (edge x map_edges))
                  (set! pathlist (cons x pathlist)))

                 ((and (set-member? brown_line x)
                       (set-member? blue_line y))
                  (ormap (lambda (z) (path z y map_edges
                                           (set-remove (set-union brown_line blue_line) x)))
                         (edge x map_edges))
                  (set! pathlist (cons x pathlist)))
                 
                 
                 
                 
                 )))



                         



(define route (λ (x y)
                (path x y map_edges (list->set (flatten map_edges)))))

;(route "4 Privet Drive" "Platform 9(3/4)")

;pathlist


;(trace path)

;(edge "4 Privet Drive" map_edges)
;(edge "Hogsmeade" map_edges)



(define l->s (λ(l)
               (cond ((empty? l) "")
                     ((empty? (rest l))  (first l))
                     (else (string-append (first l)" ->" 
                                           
                                           (l->s (rest l)))))))

(define reachability (λ (x)
                       (cond
                         ((empty? x) "no such journey is available")
                         (else (l->s (remove-duplicates x))))))
(define Distance (λ (x)
                   (cond
                     ((empty? x) 0.00)
                     (else (* (- (length (remove-duplicates x)) 1) 10)))))
(define Stop (λ (x)
               (cond
                 ((empty? x) 0.00)
                 (else (- (length (remove-duplicates x)) 1)))))






(define (->string x)
  (call-with-output-string
   (lambda (out)
     (display x out))))
;-------------------------------------------------functionality----------------------------------------
;this is done by Sunny Subash Vivek Sheth Maksim Simonov Claudiu Stefan

; ------------------------- Main  --------------------------
(define Frame (new frame% [label "Harry Potter Railway Station- Home"]
                   [width 550] [height 320]))

(define main (new vertical-panel% [parent Frame] [style '(border)][border 20]  ))

(new message%
     [parent main]
     [label "Harry Potter Search"]
     [font title_font])
(define vp1
  (new vertical-panel% [parent main] (alignment (list 'center 'top)) [style '(border)] [horiz-margin 10] [vert-margin 5] ))

( define from ( new text-field%
                    [ label "From: " ] [ parent vp1 ] [font button_font] [horiz-margin 5][vert-margin 5] [min-width 10]
                    ))

( define to ( new text-field%
                  [ label " to     : " ] [ parent vp1 ] [font button_font] [horiz-margin 5][vert-margin 5] [min-width 10]
                  ))

(define vp
  (new vertical-panel% [parent main] (alignment (list 'center 'top)) [style '(border)] [horiz-margin 10] [vert-margin 2] ))

(define new-message0 (new message%
                          [label "Your Journey :"]
                          [parent vp] [stretchable-width #t] ))
(define new-message5 (new message%
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





(define bor-der
  (new horizontal-panel% [parent main] (alignment (list 'center 'top)) [style '(border)] [horiz-margin 10] [vert-margin 2] ))

(define button (new button%
                    [ parent bor-der ]
                    [ label "SEARCH" ] [horiz-margin 5][vert-margin 5] [min-width 30] [font button_font]
                    [ callback (λ (o e)
                                 (send button set-label "...")
                                 (sleep 1)

                                 (send new-message5 set-label (->string (route (send from get-value)
                                                                               (send to get-value))))
                                                                     
                                 (send button set-label  "SEARCH")
                                 (send new-message5 set-label (reachability  pathlist))
                                 (send new-message1 set-label(real->decimal-string (Distance pathlist)))
                                 (send new-message2 set-label(real->decimal-string  (Stop  pathlist)))
                                 (send new-message5 show #t) (send new-message1 show #t) (send new-message0 show #t) (send new-message3 show #t)
                                 (send new-message4 show #t) (set! pathlist '())                                 

                                 )]))
(define button2 (new button%
                    [ parent bor-der ]
                    [ label "Return" ] [horiz-margin 5][vert-margin 5] [min-width 30] [font button_font]
                    [ callback (λ (o e)
                                 (send button2 set-label "...")
                                 (sleep 1)

                                 (send new-message5 set-label (->string (route (send from get-value)
                                                                               (send to get-value))))
                                                                     
                                 (send button2 set-label  "Return")
                                 (send new-message5 set-label (reachability  (reverse pathlist)))
                                 (send new-message1 set-label(real->decimal-string (Distance pathlist)))
                                 (send new-message2 set-label(real->decimal-string  (Stop  pathlist)))
                                 (send new-message5 show #t) (send new-message1 show #t) (send new-message0 show #t) (send new-message3 show #t)
                                 (send new-message4 show #t) (set! pathlist '())                                 

                                 )]))

;(route "4 Privet Drive" "Platform 9(3/4)")

(define clear (λ ()
                
                (send new-message0 show #f)
                (send new-message1 show #f)
                (send new-message3 show #f)
                (send new-message4 show #f)
                (send new-message2 set-label "")
                (send new-message5 set-label "")
                (send from set-value "")
                (send to set-value "")
                (set! pathlist '())
                
                
                ))
                
                
                

                

                (define btnClear (new button%
                                      [label "CLEAR"][parent bor-der][horiz-margin 5][vert-margin 5] [min-width 10] [font button_font][callback (λ (o e)
                                                                                                                                                  (clear )




                                                                                                                                                  )]))
     


                (send new-message0 show #f)
                (send new-message5 show #f)
                (send new-message1 show #f)
                (send new-message3 show #f)
                (send new-message4 show #f)


                ; ---------------------------------------------------------------
                (send Frame show #t)
                ;(send from set-field-background c3)


                ;(send to set-field-background c3)





                
                
