;This function plots minimum jerk for vowels
(ns CVsyn.plot_MJ
    (:use [incanter.charts]
          [incanter.core :only (view)]
          [overtone.live])
    ;(:use [clojure.contrib.math])
    )
;(def server (osc-server 44100 "KangWoo"));TouchOSC line
;(zero-conf-on);TouchOSC line
;(definst f1 [freq 750.0 Amp 6.0 BW 50.0 ] (* Amp (bpf (saw 100) freq (/ BW freq))));TouchOSC line
;(definst f2 [freq 1150.0 Amp 5.6 BW 70.0 ] (* Amp (bpf (saw 100) freq (/ BW freq))));TouchOSC line
;(definst f3 [freq 2600.0 Amp 5.2 BW 110.0 ] (* Amp (bpf (saw 100) freq (/ BW freq))));TouchOSC line

(defn zip [& colls]
  (into [] (apply map vector colls))
  (apply map vector colls)
  )
(def F1 [750 530 280 370 290]) ;F1 five
;(def F1 [750 280 290]) ;F1 three
(def F2 [1000 1840 2250 730 680]) ;F2 five
;(def F2 [1000 2250 680]) ;F2 three
(def x (zip (take (dec (count F1)) F1) (rest F1))) ;([750 530] [530 280] [280 370] [370 290])
(def x2 (zip (take (dec (count F2)) F2) (rest F2))) ;([1000 1840] [1840 2250] [2250 730] [730 680])
(def p (range 0.0 1.0 0.01))

(defn make-f [f-list]
  (let [l (count f-list)
        p-nodes (into [] (map #(/ % (dec l)) (range l)))
        pf-map (zipmap p-nodes f-list)
        slist (zip (drop-last p-nodes) (rest p-nodes))]
    (fn [p] 
      (first 
        (for [[lo-p hi-p] slist
              :when (<= lo-p p hi-p)]
          (let [xi (pf-map lo-p)
                xf (pf-map hi-p)
                p (* (- p lo-p) (dec l))] 
            (+ xi (* (- xi xf) (- (* 15.0 (* p p p p)) (* 6.0 (* p p p p p)) (* 10.0 (* p p p)) )))))))))
        
(def y-f1 (make-f [750 530 280 370 290]))
(def y-f2 (make-f [1000 1840 2250 730 680]))
  
(defn y1 [p] (y-f1 p)) 
(defn y2 [p] (y-f2 p)) 
;(y 0.1)


;(vector (y 0.1) (y 0.55) (y 0.95))
;(def l 3)

;(defn make-f []
;  (fn [p] 
;    (cond 
;      (< p 0.25)
;      (let [P (* p 4) xi (first (first (drop 0 x))) xf (second (first (drop 0 x)))] 
 ;       (+ xi (* (- xi xf) (- (* 15.0 (* P P P P)) (* 6.0 (* P P P P P)) (* 10.0 (* P P P)) ))))   
 ;     (< p 0.5)
 ;;     (let [P (- (* p 4) 1) xi (first (first (drop 1 x))) xf (second (first (drop 1 x)))]
 ;       (+ xi (* (- xi xf) (- (* 15.0 (* P P P P)) (* 6.0 (* P P P P P)) (* 10.0 (* P P P)) ))))   
 ;     (< p 0.75)
 ;     (let [P (- (* p 4) 2) xi (first (first (drop 2 x))) xf (second (first (drop 2 x)))]
 ;       (+ xi (* (- xi xf) (- (* 15.0 (* P P P P)) (* 6.0 (* P P P P P)) (* 10.0 (* P P P)) ))))     
 ;     (<= p 1)
 ;     (let [P (- (* p 4) 3) xi (first (first (drop 3 x))) xf (second (first (drop 3 x)))]
 ;       (+ xi (* (- xi xf) (- (* 15.0 (* P P P P)) (* 6.0 (* P P P P P)) (* 10.0 (* P P P)) )))) 
 ;        )))
;(defn make-f [] ;three values
;  (fn [p] 
;    (cond 
;      (< p 0.5) (let [P (- (* p 2) 0) xi (first (first (drop 0 x))) xf (second (first (drop 0 x)))]
;        (+ xi (* (- xi xf) (- (* 15.0 (* P P P P)) (* 6.0 (* P P P P P)) (* 10.0 (* P P P)) ))))      
;      (< p 1) (let [P (- (* p 2) 1) xi (first (first (drop 1 x))) xf (second (first (drop 1 x)))]
;        (+ xi (* (- xi xf) (- (* 15.0 (* P P P P)) (* 6.0 (* P P P P P)) (* 10.0 (* P P P)) )))) 
;         )))
;(def y (make-f)) 
;(stop);TouchOSC line
(view (xy-plot p (map #(y1 %) p)))
(view (xy-plot p (map #(y2 %) p)))

;(osc-handle server "/1/fader1" (fn [msg] (println msg)(y (first (:args msg)))));TouchOSC line

;(doseq [] (f1)(f2)(f3))
;(stop)
;(osc-handle server "/1/fader1" (fn [msg] (println msg)
;                                 (let [[freq-f1 freq-f2] (y (first (:args msg)))]
;                                   (doseq [](ctl f1 :freq freq-f1) (ctl f2 :freq freq-f2)))))
;(osc-handle server "/1/push12" (fn [msg](println msg) (stop)));stop
