 ;3/20 version, 1-control vowel, m,w,y, b,d,l
(ns CVsyn.remote-one-vowel
    (:use [incanter.charts]
          [incanter.core :only (view)]
          [overtone.live]))
(def server (osc-server 44100 "KangWoo"));TouchOSC line
(zero-conf-on);TouchOSC line
(definst f1 [freq 750.0 Amp 6.0 BW 50.0 ] (* Amp (bpf (saw 100) freq (/ BW freq))));TouchOSC line
(definst f2 [freq 1150.0 Amp 5.6 BW 70.0 ] (* Amp (bpf (saw 100) freq (/ BW freq))));TouchOSC line
(definst f3 [freq 2890.0 Amp 5.2 BW 110.0 ] (* Amp (bpf (saw 100) freq (/ BW freq))));TouchOSC line
;u:290, 680 or 290, 700 
;o: 370 730 2890
;eo (but) 590 880 2540
;a: 750,1000,2890 or 710, 1100 2540 or 750,1000,2300
;e: 530, 1840 or 550 1770 2490 or 690f 1660 2490
;i: 280, 2250 2890
;w: 300 1320 2480

(definst f1-dyn [start-freq 100.0 end-freq 500.0 dur 0.02 Amp 1.0 BW 50.0] (* 6.0 Amp (bpf (saw 100) (line start-freq end-freq dur) (/ BW (line start-freq end-freq dur)))))
(definst f2-dyn [start-freq 1000.0 end-freq 1500.0 dur 0.02 Amp 1.0 BW 70.0] (* 5.6 Amp (bpf (saw 100) (line start-freq end-freq dur) (/ BW (line start-freq end-freq dur)))))
(definst f3-dyn [start-freq 2000.0 end-freq 2500.0 dur 0.02 Amp 1.0 BW 110.0] (* 5.2 Amp (bpf (saw 100) (line start-freq end-freq dur) (/ BW (line start-freq end-freq dur)))))
(definst f4-dyn [start-freq 3000.0 end-freq 3500.0 dur 0.02 Amp 1.0 BW 250.0] (* 4.8 Amp (bpf (saw 100) (line start-freq end-freq dur) (/ BW (line start-freq end-freq dur)))))
(definst f5-dyn [start-freq 3500.0 end-freq 4000.0 dur 0.02 Amp 1.0 BW 200.0] (* 4.4 Amp (bpf (saw 100) (line start-freq end-freq dur) (/ BW (line start-freq end-freq dur)))))
(definst f6-dyn [start-freq 4000.0 end-freq 4500.0 dur 0.02 Amp 1.0 BW 1000.0] (* 4.0 Amp (bpf (saw 100) (line start-freq end-freq dur) (/ BW (line start-freq end-freq dur)))))
;main functions
(def vowel (ref {:t 100 :fa 750.0 :fb 1000.0 :fc 2600.0 :vol 1.0}))
(defn zip [& colls]
  (into [] (apply map vector colls))
  (apply map vector colls))
(defn make-vowel [vowel]
  (doseq[] (f1 (vowel :fa))(f2 (vowel :fb))(f3 (vowel :fc))))
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
;(def y-f1 (make-f [750.0 280.0 290.0]))
;(def y-f2 (make-f [1000.0 2250.0 680.0]))
(def y-f1 (make-f [750.0 280.0 590 290.0]))
(def y-f2 (make-f [1000.0 2250.0 880 680.0]))
(defn y [p] (vector (y-f1 p) (y-f2 p))) 

 (defn alter-vowel 
    [val]  
    (let [[freq-f1 freq-f2] (y val)]
                                   (doseq [](ctl f1 :freq freq-f1) (ctl f2 :freq freq-f2)
                                   (dosync (ref-set vowel (merge @vowel {:fa freq-f1 :fb freq-f2}))))))
(defn make-sound [t_ref [{t_init :t, fa1 :fa, fb1 :fb, fc1 :fc vol1 :vol} {t_end :t, fa2 :fa, fb2 :fb, fc2 :fc vol2 :vol}]]
(let [t_dur (- t_end t_init); ms->s change
      t_dur_fcn (* 0.001 (- t_end t_init))] ;ms->s change
(if (zero? t_init)
  (at (+ t_init t_ref) (doseq[]  (f1-dyn fa1 fa2 t_dur_fcn vol1) (f2-dyn fb1 fb2 t_dur_fcn vol1) (f3-dyn fc1 fc2 t_dur_fcn vol1) ))
  (at (+ t_init t_ref) (doseq[] (kill f1-dyn f2-dyn f3-dyn) (f1-dyn fa1 fa2 t_dur_fcn vol2)
                              (f2-dyn fb1 fb2 t_dur_fcn vol2) (f3-dyn fc1 fc2 t_dur_fcn vol2))))))
(defn speak [syllable]
  (let [time (now)
        slist (zip (take (dec (count syllable)) syllable) (rest syllable))]
    (dorun (map #(make-sound time %) slist))
    (at (+ 100 time)  
     (doseq[]  
       (make-vowel @vowel)
       (osc-handle server "/1/fader1" (fn [msg] (println msg)(alter-vowel (first (:args msg)))))
       (kill f1-dyn f2-dyn f3-dyn)))))
;sequences
(defn m1 [vowel] 
  [{:t 0 :fa 240.0 :fb 1000.0 :fc 2600.0 :vol 0.5}
   {:t 100 :fa 240.0 :fb 1000.0 :fc 2600.0 :vol 0.5}
   {:t 120  :fa (vowel :fa) :fb (vowel :fb)  :fc (vowel :fc) :vol (vowel :vol)}])
(defn b1 [vowel] 
  [{:t 0 :fa 200 :fb 720 :fc 1500 :vol 3} ;f2 = 950 or 720
   {:t 15  :fa (vowel :fa) :fb (vowel :fb)  :fc (vowel :fc) :vol 3}; t=25
   {:t 60  :fa (vowel :fa) :fb (vowel :fb)  :fc (vowel :fc) :vol (vowel :vol)}])
(defn l1 [vowel] 
  [{:t 0 :fa 590 :fb 880 :fc 2500 :vol 0.5};eo sound
   {:t 40 :fa 590 :fb 880 :fc 2500 :vol 2};eo sound
   {:t 60  :fa (vowel :fa) :fb (vowel :fb)  :fc (vowel :fc) :vol 2}
   {:t 80  :fa (vowel :fa) :fb (vowel :fb)  :fc (vowel :fc) :vol (vowel :vol)}])
(defn w1 [vowel] 
  [{:t 0 :fa 300 :fb 680 :fc 2480 :vol 1};start of u
   {:t 40 :fa 300 :fb 680 :fc 2480 :vol 1};keep u
   {:t 140  :fa (vowel :fa) :fb (vowel :fb)  :fc (vowel :fc) :vol (vowel :vol)}])
(defn y1 [vowel] 
  [{:t 0 :fa 280 :fb 2250 :fc 2890 :vol 1};start of i
   {:t 40 :fa 280 :fb 2250 :fc 2890 :vol 1};keep i
   {:t 100  :fa (vowel :fa) :fb (vowel :fb)  :fc (vowel :fc) :vol (vowel :vol)}])
(defn v1 [vowel] 
  [{:t 0 :fc 180 :fb 22000 :fa 22000 :vol 0.7};v: f2:880 or 300 1000 2500
   {:t 80 :fa 180 :fb 22000 :fc 22000 :vol 0};
   {:t 85 :fa 180 :fb 680 :fc 2600 :vol 0}
   {:t 90 :fa 280 :fb 680 :fc 2600 :vol 0};may kill fb and fc or add more formants or BW
   ;{:t 110 :fa 290 :fb 680 :fc 2890 :vol 1};
   {:t 150  :fa (vowel :fa) :fb (vowel :fb)  :fc (vowel :fc) :vol (vowel :vol)}
   ])
(defn d1 [vowel] 
  [{:t 0 :fa 180 :fb 2000 :fc 3500 :vol 2} ;f2 = 1800, or 500,1400,1900
   ;{:t 60 :fa 290 :fb 680 :fc 2700 :vol 2} ;   
   {:t 25  :fa (vowel :fa) :fb (vowel :fb)  :fc (vowel :fc) :vol 2}
   {:t 60  :fa (vowel :fa) :fb (vowel :fb)  :fc (vowel :fc) :vol (vowel :vol)}])
;(defn d1 [vowel] 
;  [{:t 0 :fa 260 :fb 1800 :fc 2800 :vol 2} ;f2 = 1800, or 500,1400,1900
;   {:t 25 :fa 720 :fb (/ (+ 2000 (vowel :fb)) 2) :fc 2800 :vol 2} ;   
;   {:t 45  :fa (vowel :fa) :fb (vowel :fb)  :fc (vowel :fc) :vol 2}
;   {:t 90  :fa (vowel :fa) :fb (vowel :fb)  :fc (vowel :fc) :vol (vowel :vol)}])
(defn g1 [vowel] 
  [{:t 0 :fa 300 :fb 3500 :fc 1500 :vol 2} ; 0,3500
   ;{:t 60 :fa 290 :fb 680 :fc 2700 :vol 2} ;   
   {:t 30  :fa (vowel :fa) :fb (vowel :fb)  :fc (vowel :fc) :vol 2}
   {:t 45  :fa (vowel :fa) :fb (vowel :fb)  :fc (vowel :fc) :vol (vowel :vol)}])

 (defn enable-m
  [val]
  (if (= val 1.0) (doseq [] (speak (m1 @vowel))) 
    (doseq[]
 (osc-handle server "/1/fader1" (fn [msg] (println msg)(alter-vowel (first (:args msg)))))(stop))))
  (defn enable-b
  [val]
  (if (= val 1.0) (doseq [] (speak (b1 @vowel))) 
    (doseq[]
 (osc-handle server "/1/fader1" (fn [msg] (println msg)(alter-vowel (first (:args msg)))))(stop))))
   (defn enable-l
  [val]
  (if (= val 1.0) (doseq [] (speak (l1 @vowel))) 
    (doseq[]
 (osc-handle server "/1/fader1" (fn [msg] (println msg)(alter-vowel (first (:args msg)))))(stop))))
(defn enable-w
  [val]
  (if (= val 1.0) (doseq [] (speak (w1 @vowel))) 
    (doseq[]
 (osc-handle server "/1/fader1" (fn [msg] (println msg)(alter-vowel (first (:args msg)))))(stop))))
(defn enable-y
  [val]
  (if (= val 1.0) (doseq [] (speak (y1 @vowel))) 
    (doseq[]
 (osc-handle server "/1/fader1" (fn [msg] (println msg)(alter-vowel (first (:args msg)))))(stop))))
(defn enable-v
  [val]
  (if (= val 1.0) (doseq [] (speak (v1 @vowel))) 
    (doseq[]
 (osc-handle server "/1/fader1" (fn [msg] (println msg)(alter-vowel (first (:args msg)))))(stop))))
 (defn enable-d
  [val]
  (if (= val 1.0) (doseq [] (speak (d1 @vowel))) 
    (doseq[]
 (osc-handle server "/1/fader1" (fn [msg] (println msg)(alter-vowel (first (:args msg)))))(stop)))) 
 (defn enable-g
  [val]
  (if (= val 1.0) (doseq [] (speak (g1 @vowel))) 
    (doseq[]
 (osc-handle server "/1/fader1" (fn [msg] (println msg)(alter-vowel (first (:args msg)))))(stop))))
  (defn enable-vowel
   [val]
   (if (= val 1.0) (doseq [] (f1 (vowel :fa))(f2 (vowel :fb))(f3 (vowel :fc)))(stop)))
 (defn enable-play
   [val]
   (if (= val 1.0) (doseq [] (f1 (vowel :fa))(f2 (vowel :fb))(f3 (vowel :fc)))))
;(view (xy-plot p (map #(y %) p)))

;(osc-handle server "/1/fader1" (fn [msg] (println msg)
;                                 (let [[freq-f1 freq-f2] (y (first (:args msg)))]
;                                   (doseq [](ctl f1 :freq freq-f1) (ctl f2 :freq freq-f2)
;                                   (dosync (ref-set vowel (merge @vowel {:fa freq-f1 :fb freq-f2})))
;                                     ))))
(osc-handle server "/1/fader1" (fn [msg] (println msg)(alter-vowel (first (:args msg)))))
(osc-handle server "/1/push7" (fn [msg] (println msg)(enable-m (first (:args msg)))));currently m+vowel
(osc-handle server "/1/push8" (fn [msg] (println msg)(enable-b (first (:args msg)))));currently b+vowel
(osc-handle server "/1/push9" (fn [msg] (println msg)(enable-l (first (:args msg)))));currently l+vowel
(osc-handle server "/1/push4" (fn [msg] (println msg)(enable-w (first (:args msg)))));currently w+vowel
(osc-handle server "/1/push5" (fn [msg] (println msg)(enable-y (first (:args msg)))));currently y+vowel
(osc-handle server "/1/push6" (fn [msg] (println msg)(enable-v (first (:args msg)))));currently v+vowel
(osc-handle server "/1/push1" (fn [msg] (println msg)(enable-d (first (:args msg)))));currently d+vowel
(osc-handle server "/1/push2" (fn [msg] (println msg)(enable-g (first (:args msg)))));currently g+vowel
(osc-handle server "/1/push10" (fn [msg](println msg)(enable-vowel (first (:args msg)))));vowel only
(osc-handle server "/1/push11" (fn [msg](println msg)(enable-play (first (:args msg)))));play
(osc-handle server "/1/push12" (fn [msg](println msg) (stop)));stop
;(osc-rm-handler server "/1/fader1")
;(osc-close client)
;(osc-close server) 
(stop)
