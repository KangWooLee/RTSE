;1-control version, vowel only
(ns CVsyn.vowel_1_control
    (:use [incanter.charts]
          [incanter.core :only (view)]
          [overtone.live])
    ;(:use [clojure.contrib.math])
    )

;(use 'overtone.live);TouchOSC line
;(use '[incanter.core :only (view)])
(def server (osc-server 44100 "KangWoo"));TouchOSC line
(zero-conf-on);TouchOSC line
(definst f1 [freq 750.0 Amp 6.0 BW 50.0 ] (* Amp (bpf (saw 100) freq (/ BW freq))));TouchOSC line
(definst f2 [freq 1150.0 Amp 5.6 BW 70.0 ] (* Amp (bpf (saw 100) freq (/ BW freq))));TouchOSC line
(definst f3 [freq 2890.0 Amp 5.2 BW 110.0 ] (* Amp (bpf (saw 100) freq (/ BW freq))));TouchOSC line

(defn zip [& colls]
  (into [] (apply map vector colls))
  (apply map vector colls)
  )
;(def F1 [750 530 280 370 290]) ;F1 five
;(def F2 [1000 1840 2250 730 680]) ;F2 five
;(def x (zip (take (dec (count F1)) F1) (rest F1))) ;([750 530] [530 280] [280 370] [370 290])
;(def x2 (zip (take (dec (count F2)) F2) (rest F2))) ;([1000 1840] [1840 2250] [2250 730] [730 680])
;(def p (range 0.0 1.0 0.01))

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
        
(def y-f1 (make-f [750.0 280.0 290.0]))
(def y-f2 (make-f [1000.0 2250.0 680.0]))
  
(defn y [p] (vector (y-f1 p) (y-f2 p))) 

;(view (xy-plot p (map #(y %) p)))

(osc-handle server "/1/fader1" (fn [msg] (println msg)
                                 (let [[freq-f1 freq-f2] (y (first (:args msg)))]
                                   (doseq [](ctl f1 :freq freq-f1) (ctl f2 :freq freq-f2)))))
(osc-handle server "/1/push11" (fn [msg](println msg) (doseq [] (f1)(f2)(f3))));stop
(osc-handle server "/1/push12" (fn [msg](println msg) (stop)));stop

(stop)
