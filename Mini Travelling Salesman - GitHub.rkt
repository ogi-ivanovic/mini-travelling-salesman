;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Mini Travelling Salesman - GitHub|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; The following program has the main function route, which takes two parameters
;; which as the names of two people who's houses are defined on the map. The
;; function uses a backtracking algorithm to calculate and display the shortest
;; possible distance between the two people's homes, and draws out the path.
;;

(require 2htdp/image)
(require "drawinglib.rkt")

;; Does not contain image here since it causes error once uploading

;; A Node is a (list Posn (listof Posn))

;; A Graph is a (listof Node)


;; Positions of people's homes on the map
(define ME (make-posn 253 280))
(define PHIL (make-posn 333 210))
(define JAHESH (make-posn 325 280))
(define RAYMOND (make-posn 237 241))
(define JORDAN (make-posn 398 278)) 
(define DANIEL (make-posn 251 397))
(define CONNOR (make-posn 207 54))
(define MADI (make-posn 378 76))
(define LBP (make-posn 132 217))
(define DELANEY (make-posn 458 119))
(define VINAY (make-posn 465 300))
(define MAX (make-posn 367 35))
(define TIHO (make-posn 42 20))
(define JOHN (make-posn 145 330))
(define BLUEPARK (make-posn 378 160))
(define MACS (make-posn 68 396))
(define SALIL (make-posn 342 271))


;; List of all the points in the graph (excluding the points which represent
;; people's house)
(define lstaf (list
 (make-posn 33 15)
 (make-posn 70 42)
 (make-posn 113 77)
 (make-posn 93 106)
 (make-posn 74 100)
 (make-posn 55 107)
 (make-posn 45 133)
 (make-posn 42 148)
 (make-posn 43 163)
 (make-posn 74 204)
 (make-posn 94 199)
 (make-posn 84 154)
 (make-posn 89 120)
 (make-posn 111 187)
 (make-posn 132 217)
 (make-posn 200 170)
 (make-posn 161 203)
 (make-posn 135 85)
 (make-posn 162 146)
 (make-posn 157 87)
 (make-posn 155 61)
 (make-posn 169 49)
 (make-posn 207 54)
 (make-posn 220 72)
 (make-posn 185 88)
 (make-posn 193 91)
 (make-posn 208 150)
 (make-posn 209 129)
 (make-posn 199 98)
 (make-posn 243 58)
 (make-posn 300 32)
 (make-posn 367 35)
 (make-posn 379 27)
 (make-posn 378 46)
 (make-posn 390 38)
 (make-posn 455 41)
 (make-posn 378 76)
 (make-posn 305 74)
 (make-posn 291 80)
 (make-posn 313 111)
 (make-posn 378 115)
 (make-posn 375 230)
 (make-posn 369 250)
 (make-posn 357 262)
 (make-posn 285 305)
 (make-posn 275 314)
 (make-posn 241 290)
 (make-posn 218 263)
 (make-posn 248 227)
 (make-posn 288 203)
 (make-posn 306 168)
 (make-posn 315 170)
 (make-posn 340 182)
 (make-posn 377 185)
 (make-posn 458 189)
 (make-posn 458 253)
 (make-posn 415 258)
 (make-posn 421 296)
 (make-posn 374 304)
 (make-posn 357 305)
 (make-posn 306 291)
 (make-posn 453 296)
 (make-posn 321 322)
 (make-posn 342 271)
 (make-posn 398 278)
 (make-posn 456 80)
 (make-posn 458 119)
 (make-posn 303 343)
 (make-posn 288 373)
 (make-posn 241 392)
 (make-posn 258 342)
 (make-posn 247 351)
 (make-posn 232 357)
 (make-posn 155 361)
 (make-posn 135 366)
 (make-posn 118 373)
 (make-posn 103 337)
 (make-posn 113 309)
 (make-posn 113 298)
 (make-posn 84 247)
 (make-posn 110 229)
 (make-posn 145 330)
 (make-posn 119 330)
 (make-posn 107 324)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTIONS WHICH CALCULATE THE ACTUAL ROUTE AND DISPLAY IT BELOW

;; (neighbours n G) produces the list of out-neigbours of the given
;; node, n, and the given graph, G
;; neighbours : Node Graph -> (listof Posn)
;; requires : n is in G

(define (neighbours n G)
  (cond
    [(empty? G) empty]
    [(equal? n (first (first G)))
     (second (first G))]
    [else (neighbours n (rest G))]))

;; Tests
(check-expect (neighbours 'A '((A (B C D)))) '(B C D))
(check-expect (neighbours 'B '((A (C D)) (B (X Y)))) '(X Y))


;; (find-route orig dest G visited) produces a list of all possible routes
;;  from orig to dest in the graph G, while keeping track of which nodes
;;  have been visited, if the graph is not a DAG
;; find-route : Node Node Graph (listof Node) ->(listof (listof Node))

(define (find-route orig dest G visited)
  (cond
    [(equal? orig dest) (list (list orig))]
    [else (local
            [(define nbrs (neighbours orig G))
             (define route (find-route/list nbrs dest G (cons orig visited)))]
            (cond
              [(empty? route) empty]
              [else (map (lambda (x) (cons orig x)) route)]))]))


;; (find-route/list los dest G visited) produces a list of all possible
;;  routes from any Node in los to dest in the graph G, whil keeping track
;;  of which nodes ohave been visited, if the graph is not a DAG
;; find-route/list : (listof Node) Node Graph (listof Node) -> (listof Node)

(define (find-route/list los dest G visited)
  (cond
    [(empty? los) empty]
    [(member? (first los) visited)
     (find-route/list (rest los) dest G visited)]
    [else (local
            [(define route (find-route (first los) dest G visited))]
            (cond
              [(false? route) (find-route/list (rest los) dest G visited)]
              [else (append route
                            (find-route/list (rest los) dest G visited))]))]))


;; (length-route route) determines the length fo the given route, by calculating
;;  the distance between all of the points in the route
;; length-route : (listof Node) -> Num
;; requires : route contains at least 2 points

(define (length-route route)
  (local
    [(define (distance-of-points p1 p2)
       (local [(define delta-x (- (posn-x p2) (posn-x p1)))
               (define delta-y (- (posn-y p2) (posn-y p1)))]
         (sqrt (+ (sqr delta-x) (sqr delta-y)))))]

    (cond
      [(empty? (rest (rest route))) 0 ]
      [else (+ (distance-of-points (first route) (second route))
                 (length-route (rest route)))])))


;; (shortest-route routes shortest-so-far) determines the shortes route from
;;  routes, while keeping track of the shortest-so-far
;; shortest-route : (listof (listof Node)) (listof Node) -> (listof Node)
;; requires : routes contains at least on route

(define (shortest-route routes shortest-so-far)
  (cond
    [(empty? routes) shortest-so-far]
    [(< (length-route (first routes)) (length-route shortest-so-far))
     (shortest-route (rest routes) (first routes))]
    [else (shortest-route (rest routes) shortest-so-far)]))


;; (route orig dest) draws the shortest route from orig to dest onto the
;;  eastbridge map, and outlines the line to make it thicker
;; route : Node Node -> Image
;; requires : orig and dest are in graph

(define (route orig dest)
  (local
    [;; defines route to be the list of all routes from orig to dest in graph
     (define route (find-route orig dest gra empty))
     
     ;; defines shortest to be the shortest route from ori to dest from route
     (define shortest (shortest-route (rest route) (first route)))

     ;; (draw-route route pic) draws the given route onto the given pic
     ;; draw-route : (listof Node) Image -> Image
     ;; requires : route contains at least two routes
     (define (draw-route route pic)
       (cond
         [(empty? (rest (rest route)))
          (add-line pic (posn-x (first route)) (posn-y (first route))
                    (posn-x (second route)) (posn-y (second route)) "red")]
         [else (add-line (draw-route (rest route) pic) (posn-x (first route))
                         (posn-y (first route)) (posn-x (second route))
                         (posn-y (second route)) "red")]))]

    ;; draws the shortest possible route from route onto eb, and draws over
    ;; the line mltiple times to thicken it
    (place-image (circle 5 "solid" "blue") (posn-x orig) (posn-y orig)
                 (place-image (circle 5 "solid" "blue") (posn-x dest)
                              (posn-y dest)
    (draw-route shortest
     (draw-route shortest
      (draw-route shortest
       (draw-route shortest
        (draw-route shortest
         (draw-route shortest
          (draw-route shortest
           (draw-route shortest
            (draw-route shortest
             (draw-route shortest
              (draw-route shortest
               (draw-route shortest
                (draw-route shortest
                 (draw-route shortest
                  (draw-route shortest
                   (draw-route shortest
                    (draw-route shortest
                     (draw-route shortest
                      (draw-route shortest
                       (draw-route shortest
                        (draw-route shortest
                         (draw-route shortest
                          (draw-route shortest
                           (draw-route shortest
                            (draw-route shortest eb)))))))))))))))))))))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The graph defining all the points on the image eb, such that all points
;; follow the lines of the road, and once lines are drawn to connect all the
;; points, all the lines will coverall of the roads needed to connect
;; everyone's houses which are defined on the map, and the graph contains
;; Posns which represent people's houses, and just regular points on the
;; graph which follow the road

(define gra
  (list (list (make-posn 33 15) (list (make-posn 70 42)))
        (list (make-posn 70 42) (list (make-posn 33 15) (make-posn 113 77)))
        (list (make-posn 113 77) (list (make-posn 70 42) (make-posn 93 106)
                                      (make-posn 135 85)))
        (list (make-posn 93 106) (list (make-posn 113 77) (make-posn 74 100)
                                       (make-posn 89 120)))
        (list (make-posn 74 100) (list (make-posn 93 106) (make-posn 55 107)))
        (list (make-posn 55 107) (list (make-posn 74 100) (make-posn 45 133)))
        (list (make-posn 45 133) (list (make-posn 55 107) (make-posn 42 148)))
        (list (make-posn 42 148) (list (make-posn 45 133) (make-posn 43 163)))
        (list (make-posn 43 163) (list (make-posn 42 148) (make-posn 74 204)))
        (list (make-posn 74 204) (list (make-posn 43 163) (make-posn 94 199)))
        (list (make-posn 94 199) (list (make-posn 74 204) (make-posn 111 187)))
        (list (make-posn 111 187) (list (make-posn 94 199) (make-posn 84 154)
                                       (make-posn 162 146) (make-posn 132 217)))
        (list (make-posn 84 154) (list (make-posn 111 187) (make-posn 89 120)))
        (list (make-posn 89 120) (list (make-posn 84 154) (make-posn 93 106)))
        (list (make-posn 135 85) (list (make-posn 157 87) (make-posn 113 77)))
        (list (make-posn 157 87) (list (make-posn 135 85) (make-posn 162 146)
                                       (make-posn 155 61)))
        (list (make-posn 155 61) (list (make-posn 157 87) (make-posn 169 49)))
        (list (make-posn 162 146) (list (make-posn 157 87) (make-posn 111 187)))
        (list (make-posn 132 217) (list (make-posn 161 203) (make-posn 111 187)
                                        (make-posn 110 229)))
        (list (make-posn 161 203) (list (make-posn 132 217)
                                        (make-posn 200 170)))
        (list (make-posn 200 170) (list (make-posn 161 203)
                                        (make-posn 208 150)))
        (list (make-posn 208 150) (list (make-posn 200 170)
                                        (make-posn 209 129)))
        (list (make-posn 209 129) (list (make-posn 208 150) (make-posn 199 98)))
        (list (make-posn 199 98) (list (make-posn 209 129) (make-posn 193 91)))
        (list (make-posn 193 91) (list (make-posn 199 98) (make-posn 220 72)
                                       (make-posn 185 88)))
        (list (make-posn 220 72) (list (make-posn 193 91) (make-posn 207 54)
                                       (make-posn 243 58)))
        (list (make-posn 185 88) (list (make-posn 193 91) (make-posn 157 87)))
        (list (make-posn 207 54) (list (make-posn 220 72) (make-posn 169 49)))  
        (list (make-posn 169 49) (list (make-posn 207 54) (make-posn 155 61)))
        (list (make-posn 367 35) (list (make-posn 300 32) (make-posn 378 46)))
        (list (make-posn 381 26) (list (make-posn 367 35)))
        (list (make-posn 390 38) (list (make-posn 381 26)
                                        (make-posn 455 41)))
        (list (make-posn 455 41) (list (make-posn 390 38) (make-posn 456 80)))
        (list (make-posn 456 80) (list (make-posn 455 41) (make-posn 458 119)
                                       (make-posn 378 76)))
        (list (make-posn 458 119) (list (make-posn 456 80) (make-posn 378 115)))
        (list (make-posn 378 76) (list (make-posn 456 80) (make-posn 378 46)
                                       (make-posn 378 115) (make-posn 305 74)))
        (list (make-posn 378 46) (list (make-posn 378 76)
                                       (make-posn 390 38)))
        (list (make-posn 300 32) (list (make-posn 367 35) (make-posn 243 58)))
        (list (make-posn 243 58) (list (make-posn 300 32) (make-posn 220 72)))
        (list (make-posn 453 296) (list (make-posn 421 296) VINAY))
        (list VINAY (list (make-posn 453 296)))
        (list (make-posn 377 185) (list (make-posn 375 230) (make-posn 458 189)
                                        (make-posn 340 182) BLUEPARK))
        (list (make-posn 305 74) (list (make-posn 291 80) (make-posn 378 76)))
        (list (make-posn 291 80) (list (make-posn 313 111) (make-posn 305 74)))
        (list (make-posn 313 111) (list (make-posn 378 115) (make-posn 291 80)))
        (list (make-posn 378 115) (list (make-posn 313 111) (make-posn 378 76)
                                        (make-posn 458 119) BLUEPARK))
        (list BLUEPARK (list (make-posn 377 185) (make-posn 378 115)))
        (list (make-posn 375 230) (list (make-posn 369 250)
                                        (make-posn 369 250)))
        (list (make-posn 369 250) (list (make-posn 357 262)
                                        (make-posn 375 230)))
        (list (make-posn 357 262) (list (make-posn 369 250)
                                        (make-posn 342 271)))
        (list (make-posn 285 305) (list (make-posn 275 314)
                                        (make-posn 306 291)))
        (list (make-posn 275 314) (list (make-posn 241 290) (make-posn 285 305)
                                        (make-posn 258 342)))
        (list (make-posn 241 290) (list (make-posn 218 263) (make-posn 275 314)
                                        ME))
        (list ME (list (make-posn 241 290)))
        (list (make-posn 218 263) (list  (make-posn 241 290) RAYMOND))
        (list (make-posn 248 227) (list (make-posn 288 203) RAYMOND))
        (list RAYMOND (list (make-posn 218 263) (make-posn 248 227)))
        (list (make-posn 288 203) (list (make-posn 306 168)
                                        (make-posn 248 227)))
        (list (make-posn 306 168) (list (make-posn 315 170)
                                        (make-posn 288 203)))
        (list (make-posn 315 170) (list (make-posn 340 182)
                                        (make-posn 306 168)))
        (list (make-posn 340 182) (list (make-posn 377 185) (make-posn 315 170)
                                        PHIL))
        (list PHIL (list (make-posn 340 182)))
        (list (make-posn 458 189) (list (make-posn 458 253)
                                        (make-posn 377 185)))
        (list (make-posn 458 253) (list (make-posn 415 258)
                                        (make-posn 458 189)))
        (list (make-posn 415 258) (list (make-posn 398 278)
                                        (make-posn 458 253)))
        (list (make-posn 421 296) (list (make-posn 398 278)
                                        (make-posn 453 296)))
        (list (make-posn 398 278) (list (make-posn 374 304) (make-posn 421 296)
                                        (make-posn 415 258)))
        (list (make-posn 374 304) (list (make-posn 357 305)
                                        (make-posn 398 278)))
        (list (make-posn 357 305) (list (make-posn 342 271)
                                        (make-posn 374 304)))
        (list (make-posn 342 271) (list (make-posn 357 305) (make-posn 357 262)
                                        JAHESH))
        (list (make-posn 306 291) (list (make-posn 321 322) (make-posn 285 305)
                                        JAHESH))
        (list JAHESH (list (make-posn 306 291) (make-posn 342 271)))
        (list (make-posn 321 322) (list (make-posn 303 343)
                                        (make-posn 306 291)))
        (list (make-posn 303 343) (list (make-posn 288 373)
                                        (make-posn 321 322)))
        (list (make-posn 288 373) (list  (make-posn 303 343) DANIEL))
        (list (make-posn 241 392) (list (make-posn 232 357) DANIEL))
        (list DANIEL (list (make-posn 241 392) (make-posn 288 373)))
        (list (make-posn 258 342) (list (make-posn 247 351)
                                        (make-posn 275 314)))
        (list (make-posn 247 351) (list (make-posn 232 357)
                                        (make-posn 258 342)))
        (list (make-posn 232 357) (list (make-posn 155 361) (make-posn 247 351)
                                        (make-posn 241 392)))
        (list (make-posn 155 361) (list (make-posn 135 366) (make-posn 232 357)
                                          (make-posn 145 330)))
        (list (make-posn 135 366) (list (make-posn 118 373)
                                        (make-posn 155 361)))
        (list (make-posn 110 229) (list (make-posn 84 247) (make-posn 132 217)))
        (list (make-posn 84 247) (list (make-posn 113 298) (make-posn 110 229)))
        (list (make-posn 113 298) (list (make-posn 113 309) (make-posn 84 247)))
        (list (make-posn 113 309) (list (make-posn 107 324)
                                        (make-posn 113 298)))
        (list (make-posn 103 337) (list (make-posn 107 324)
                                        (make-posn 118 373)))
        (list (make-posn 118 373) (list (make-posn 103 337) (make-posn 135 366)
                                        MACS))
        (list MACS (list (make-posn 118 373)))
        (list (make-posn 145 330) (list (make-posn 119 330)
                                        (make-posn 155 361)))
        (list (make-posn 107 324) (list (make-posn 119 330) (make-posn 113 309)
                                        (make-posn 103 337)))
        (list (make-posn 119 330) (list (make-posn 107 324)
                                        (make-posn 145 330))))) 
        

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPER FUNCTIONS USED TO MAKE THE GRAPH BELOW

;; Template for the function call which plots the given point onto eb
;; (place-image (circle 5 "solid" "red") x y eb)


;; (plot lst pic) plots every point from lst onto the given pic
;; plot : (listof Posn) Image -> Image
;; requires : lst has at least 1 point

(define (plot lst pic)
  (cond
    [(empty? (rest lst))
     (place-image (circle 5 "solid" "red") (posn-x (first lst))
                  (posn-y (first lst)) pic)]
    [else
     (plot (rest lst) (place-image (circle 5 "solid" "red") (posn-x (first lst))
                  (posn-y (first lst)) pic))]))


;; Points produces the image eb with every point in the graph (excluding the
;; points which represent people's houses) plotted onto it
(define points (plot lstaf eb))


;; Draws all the possible paths which are in gra
(define paths (better-paths gra))


;; (close x y) produces a list of points which are close to the given
;;  coordinates, x & y, from the list of all points in the graph (excluding
;;  points which represent people's houses)
;; close : Nat Nat -> (listof Posn)
;; requires : x <= 500
;;            y <= 403

(define (close x y)
  (local
    [;; (close-h x y lst) produces a list of points which are close to the
     ;; given coordinates, x & y, from lst
     ;; close-h : Nat Nat (listof Posn) -> (listof Posn)
     (define (close-h x y lst)
       (cond
         [(empty? lst) empty]
         [(and (> 10 (abs (- x (posn-x (first lst)))))
               (> 10 (abs (- y (posn-y (first lst))))))
          (cons (first lst) (close-h x y (rest lst)))]
         [else (close-h x y (rest lst))]))]
    (close-h x y lstaf)))


;; (close-name name) calls the close function with the x and y components of
;;  the given name, which represents one of people's house coordinates
;; close-name : Posn -> Image
(define (close-name name)
  (close (posn-x name) (posn-y name)))


;; (plot-point point) plots the given point onto eb
;; plot-point : Posn -> Image
;; requires : (posn-x point) <= 500
;;            (posn-y point) <= 403

(define (plot-point point)
  (place-image (circle 5 "solid" "red") (posn-x point)
               (posn-y point) eb))

;; (tester lst graph) determines if every node in the lst is in the graph
;; tester : (list of Posn) Graph -> (listof Posn)
(define (tester lst graph)
  (local
    [(define (nodes graph)
       (foldr (lambda (frst rst)
                (cons (first frst) rst)) empty graph))
     (define nodes-in-graph (nodes graph))]
    (filter (lambda (x) (not (member x nodes-in-graph))) lst)))

;; (better-paths graph) draws lines which connect every node to all of it's
;;  out-neighbours in the graph
;; better-paths : Graph -> Image

(define (better-paths graph)
  (local
    [;; (nodes-path-help node children pic) draws lines from node to every
     ;;  point in children onto the given pic
     ;; nodes-path-help : Node (listof Node) Image -> Image
     (define (nodes-path-help node children pic)
       (cond
         [(empty? (rest children))
          (add-line pic (posn-x node) (posn-y node)
                    (posn-x (first children)) (posn-y (first children))
                    "red")]
         [else (add-line (nodes-path-help node (rest children) pic)
                         (posn-x node) (posn-y node)
                         (posn-x (first children)) (posn-y (first children))
                         "red")]))

     ;; (nodes-path node pic) draws lines from the node to all of it's
     ;;  out-neighbours onto the given pic
     ;; nodes-path : Node Image -> Image
     (define (nodes-path node pic)
       (nodes-path-help (first node) (second node) pic))

     ;; (all-paths graph) draws lines from every node to all of it's
     ;;  out-neighbours onto eb, which reslts in drawing all the possible
     ;;  paths which can be found in graph
     ;; all-paths : Graph
     (define (all-paths graph)
       (cond
         [(empty? (rest graph))
          (nodes-path (first graph)
             (nodes-path (first graph)
                (nodes-path (first graph)
                            (nodes-path (first graph) eb))))]
         [else (nodes-path (first graph)
                  (nodes-path (first graph)
                     (nodes-path (first graph)
                                 (all-paths (rest graph)))))]))]

    ;; Calls the all-paths function and draws all the possible paths from
    ;; the graph which was originally passed to better-paths
    (all-paths graph)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;