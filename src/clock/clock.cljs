(ns clock.clock
  (:require
   [reagent.core :as reagent :refer [atom]]
   ;;[goog.Timer :as Timer]
   ;;[clojure.math.numeric-tower :as math]
   ;;[clojure.math.combinatorics :as combo]
   ))



(enable-console-print!)

(defn abs [x]  (.abs js/Math x))
(defn sqrt [x]  (.sqrt js/Math x))
(defn square [x] (* x x))
(defn hypt [x y]  (sqrt (+ (square x) (square y))))
(defn sin [x] (.sin js/Math x))
(defn cos [x] (.cos js/Math x))
(def pi (.-PI js/Math))
(defn floor [x] (.floor js/Math x))
;;(defn mod (.mod js/Math x))

(def the-time-stamp (atom ""))


(def mins (atom 0))
(def secs (atom 0))
(def hrs (atom 0))


(def sx1 (atom 0))
(def sx2 (atom 0))
(def sx3 (atom 100))
(def sx4 (atom 0))


(def sy1 (atom 0))
(def sy2 (atom 0))
(def sy3 (atom 100))
(def sy4 (atom 0))

(def win-width (atom 0))
(def win-height (atom 0))
(def win-centre-x (atom 0))
(def win-centre-y (atom 0))
(def clock-radius (atom 0))

(def clock-edge (atom []))

(def dx (atom 0))
(def tick (atom 0))

;; interval clock javascript
(def one-second 1000)



(js/setInterval (fn [] (swap! tick inc)) one-second)


;; calc
(defn seconds-hand [r t]
  [
   (+ (floor (* r (sin (/ (* pi t) 30)))))
   (- (floor (* r (cos (/ (* pi t) 30)))))
   ])

(defn second-hand [t]
  (let [cx @win-centre-x
        cy @win-centre-y]
  (let [[x1 y1] (seconds-hand (* 0.1 @clock-radius) t)]
    (reset! sx1 (+ cx x1))
    (reset! sy1 (+ cy y1)))
  (let [[x1 y1] (seconds-hand (* 0.2 @clock-radius) t)]
    (reset! sx2 (+ cx x1))
    (reset! sy2 (+ cy y1)))
  (let [[x1 y1] (seconds-hand (* 0.4 @clock-radius) t)]
    (reset! sx3 (+ cx x1))
    (reset! sy3 (+ cy y1)))
  (let [[x1 y1] (seconds-hand @clock-radius t)]
    (reset! sx4 (+ cx x1))
    (reset! sy4 (+ cy y1)))))

    
  ;; (let [cx 250
  ;;         cy 250
  ;;         cx2 250
  ;;         cy2 350]
  ;;     (let [tock (mod time 60)
  ;;           [x2 y2] (seconds-hand 100 tock)]
  ;;       ;;(println "x2 = " x2)
  ;;       [:circle {:cx (+ 250 x2) :cy (+ 250 y2) :r 10 :stroke "black" :fill "blue"}]
  ;;       )
  ;;     )
  ;; )


;; clock radius about 80 percent of available square space in window
(defn grab-jswin-dimensions []
  (let [h (.-innerHeight js/window)
        w (.-innerWidth js/window)]
    ;; take account of fact big menu at top of window
    (if (not (= (- h 100) @win-height))
      (reset! win-height (- h 100)))
    (if (not (= w @win-width))
      (reset! win-width w)))
  (reset! win-centre-x (floor (/ @win-width 2)))
  (reset! win-centre-y (floor (/ @win-height 2)))
  (reset! clock-radius (* 0.80 (/ (min @win-height @win-width) 2)))
  )



;; given win-centre-x win-centre-y clock-radius
;; draw out 12 hour hands
;; ;; 2 pi radians all way around clock once
;; (defn ^:export hour-marks []
;;   (reset! clock-edge 
;;           (map
;;            (fn [y] (seconds-hand @clock-radius y))
;;            (map (fn [x] (* x (/ pi 6)))
;;                 (rest (range 13)))))
;;   @clock-edge)



;; (defn ^:export test-marks []
;;   (let [cx 250
;;         cy 250
;;         radius 100
;;         key 0]    
;;     (for [point
;;           (map
;;            (fn [y] (seconds-hand radius y))
;;            (map (fn [x] (* x (/ pi 6)))
;;                 (rest (range 13))))]
;;       (let [[px py] point]
;;         (println "point = "  px py)))))

(defn test-marks []
  (map (fn [xd]
         (let [px (first xd)
               py (second xd)]
           [(+ px @win-centre-x) (+ py @win-centre-y)]))
       (map (fn [y] (seconds-hand 100 (* y 30)))
            (map (fn [x] (* x (/ pi 6)))
                 (rest (range 14))))))

;;let* [angle (/ pi 6)
(defn test-marks2-helper [angle]
  (let [x (+ @win-centre-x (* @clock-radius (sin angle)))
        y (+ @win-centre-y (* @clock-radius (cos angle)))]
    [x y]))


(defn test-marks2 []
  (map test-marks2-helper
       (map (fn [x] (* x (/ pi 6))) (range 12))))

  
  ;; (map (fn [xd]
  ;;        (let [px (first xd)
  ;;              py (second xd)]
  ;;          [(+ px @win-centre-x) (+ py @win-centre-y)]))
  ;;      (map (fn [y] (seconds-hand @clock-radius y))
  ;;           (map (fn [x] (* x (/ pi 6)))
  ;;                (rest (range 14))))))




;; seconds is given as integer 0 to 60 say
;; 2pi / 60 gives each second as pi / 30 radians
(defn clock-seconds-hand [ obj ]
  (let [cx (get obj :cx)
        cy (get obj :cy)
        radius (get obj :radius)
        seconds (get obj :seconds)] 
    [:line  {:x1 cx :y1 cy
             :x2 (str (+ cx (* radius
                               (.sin js/Math
                                     (/
                                      (* seconds js/Math.PI)
                                      30)))))
             :y2 (str (- cy  (* radius
                                (.cos js/Math
                                      (/
                                       (* seconds js/Math.PI)
                                       30)))))
             :style {:stroke-width "2" :stroke "black" :fill "none"}}]))



;; minutes is given as integer 0 to 60 say
;; 2pi / 60 gives each second as pi / 30 radians
;; minutes-hand code and second-hands code could be identical
;; subtract 15 mins to orientate zero point upwards
(defn clock-minutes-hand [obj]
  (let [cx (get obj :cx)
        cy (get obj :cy)
        radius (get obj :radius)
        minutes (get obj :minutes)]
    [:line  {:x1 cx :y1 cy
             :x2 (str (+ cx (* radius
                               (.sin js/Math
                                     (/
                                      (* minutes js/Math.PI)
                                      30)))))
             :y2 (str (- cy  (* radius
                                 (.cos js/Math
                                         (/
                                          (* minutes js/Math.PI)
                                          30)))))
             :style {:stroke-width "6" :stroke "black" :fill "none"}}]))




;; hours is given as integer 0 to 24 say
;; 2pi / 24 gives each hour as pi/12 radians
(defn clock-hours-hand [obj]
  (let [cx (get obj :cx)
        cy (get obj :cy)
        radius (get obj :radius)
        hours (get obj :hours)] 
       [:line  {:x1 cx :y1 cy
                :x2 (str (+ cx (* radius
                                   (.sin js/Math
                                         (/
                                          (* hours js/Math.PI)
                                          6)))))
                :y2 (str (- cy  (* radius
                                    (.cos js/Math
                                          (/
                                           (* hours js/Math.PI)
                                           6)))))
                :style {:stroke-width "10" :stroke "black" :fill "none"}}]))





   
   ;; [:rect {:x "450" :y "450" :width "50" :height "50" :style {:stroke-width "1" :stroke "red" :fill "none"}}]
   ;; [:rect {:x "400" :y "400" :width "50" :height "50" :style {:stroke-width "1" :stroke "blue" :fill "none"}}]

   ;; [:rect {:x "450" :y "0" :width "50" :height "50" :style {:stroke-width "1" :stroke "red" :fill "none"}}]
   ;; [:rect {:x "400" :y "50" :width "50" :height "50" :style {:stroke-width "1" :stroke "blue" :fill "none"}}]

   ;; [:rect {:x "0" :y "450" :width "50" :height "50" :style {:stroke-width "1" :stroke "red" :fill "none"}}]
   ;; [:rect {:x "50" :y "400" :width "50" :height "50" :style {:stroke-width "1" :stroke "blue" :fill "none"}}]

   ;; [:line  {:x1 "250" :y1 "250" :x2 "500" :y2 "250" :style {:stroke-width "1" :stroke "black" :fill "none"}}]
   ;; [:line  {:x1 "250" :y1 "250" :x2 "250" :y2 "0"  :style {:stroke-width "1" :stroke "black" :fill "none"}}]

   ;; draw seconds hand on clock face using @tick
   ;; centre 250,250 @tick mod 60
   
   ;; [:rect {:x "0" :y "0" :width "50" :height "50" :class "corner-box" }]
   ;; [:rect {:x "50" :y "50" :width "50" :height "50" :style {:stroke-width "1" :stroke "blue" :fill "none"}}]
   ;; [:rect {:x "0" :y "0" :width "500" :height "500" :style {:stroke-width "1" :stroke "green" :fill "none"}}]



(defn clock-face2 [ time-stamp ]
  [:svg {:width 500 :height 500 :class "clock-face"  :viewBox "0 0 500 500"}
   [:circle {:cx "250" :cy "250" :r 250 :style {:stroke-width "1" :stroke "black" :fill "none"}}]
   ;;[:circle {:cx "250" :cy "250" :r 200 :style {:stroke-width "1" :stroke "black" :fill "none"}}]
   [:circle {:cx "250" :cy "250" :r 150 :style {:stroke-width "1" :stroke "black" :fill "none"}}]
   
   [clock-hours-hand {:cx  250 :cy  250 :radius 150 :hours  (get time-stamp :hours )}]
   [clock-minutes-hand {:cx 250 :cy 250 :radius 200 :minutes(get time-stamp :minutes )}]
   [clock-seconds-hand {:cx 250 :cy 250 :radius 250 :seconds (get time-stamp :seconds )}]

   ;;(js/console.log "hours = " (get time-stamp :hours))
   ;;(js/console.log "minutes = " (get time-stamp :minutes))
   
   ]
  )


  ;;
  ;; (if (> @dx 450)
  ;;   (reset! dx 50))
  ;; (swap! dx (fn [x] (+ @dx 1)))
  
  ;;(second-hand @tick)
  ;; (let [[x1 y1] (seconds-hand 60 @tick)]
  ;;   (reset! sx3 (+ 250 x1))
  ;;   (reset! sy3 (+ 250 y1)))
  ;;(grab-jswin-dimensions)
  
  ;;[:polygon {:points (str "200,10 " @dx ",190 160,210") :stroke "red" :fill "lime"}]
  ;;(sleep (fn [] [splodge]) 500)
  


(defn clock []
  (let [cd (js/Date.)]
    (let [hours (.getHours cd)
          minutes (.getMinutes cd)
          seconds (.getSeconds cd)
          time-stamp (.toLocaleTimeString cd)]
      (reset! the-time-stamp time-stamp)
      [:div
       {:style {:text-align "center"}}
       [:h1 "Clock App" ]
       [:div {:class "clock"}
        [clock-face2 {:hours hours :minutes minutes :seconds seconds}]
        ;;[clock-face2 {:hours @hrs :minutes @mins :seconds @secs}]
        ]
              
       ;; [:button {:on-click #(reset! hrs (mod (+ @hrs 1) 12))} (str "hours " @hrs)]
       ;; [:button {:on-click #(reset! mins (mod (+ @mins 1) 60))} (str "minutes " @mins)]
       ;; [:button {:on-click #(reset! secs (mod (+ @secs 1) 60))} (str "seconds " @secs)]
       
       [:p {:style {:visible false}} (str "update ticks : " @tick )]
       [:p  (str "local time : "@the-time-stamp)]
       ])))





(reagent/render-component [clock] (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)



