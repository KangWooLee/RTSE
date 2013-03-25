(ns CVsyn.remote.core
  (:use overtone.live)
  (:use [overtone.at-at :only (mk-pool)])
  (:use [overtone.at-at :only (after)]))

;server definition
(def server (osc-server 44100 "KangWoo"))
(zero-conf-on)
;(osc-listen server (fn [msg] (println msg)) :debug);listener connection
;(osc-rm-listener server :debug)
 
;define static formants 
(definst f0 [freq 120.0 Amp 6.4 BW 50.0 ] (* Amp (bpf (saw 100) freq (/ BW freq))))
(definst f1 [freq 750.0 Amp 6.0 BW 50.0 ] (* Amp (bpf (saw 100) freq (/ BW freq))))
(definst f2 [freq 1150.0 Amp 5.6 BW 70.0 ] (* Amp (bpf (saw 100) freq (/ BW freq))))
(definst f3 [freq 2600.0 Amp 5.2 BW 110.0 ] (* Amp (bpf (saw 100) freq (/ BW freq))))
(definst f4 [freq 3300.0 Amp 4.8 BW 250.0 ] (* Amp (bpf (saw 100) freq (/ BW  freq))))
(definst f5 [freq 3850.0 Amp 4.4 BW 200.0 ] (* Amp (bpf (saw 100) freq (/ BW freq))))
(definst f6 [freq 4900.0 Amp 4.0 BW 1000.0 ] (* Amp (bpf (saw 100) freq (/ BW freq))))
;u:290, 680 or 290, 700 
;o: 370 730 2890
;eo (but) 590 880 2540
;a: 750,1000,2890 or 710, 1100 2540 or 750,1000,2300
;e: 530, 1840 or 550 1770 2490 or 690f 1660 2490
;i: 280, 2250 2890
;w: 300 1320 2480

;define dynamic formants
(definst f1-dyn [start-freq 100.0 end-freq 500.0 dur 0.02 Amp 1.0 BW 50.0] (* 6.0 Amp (bpf (saw 100) (line start-freq end-freq dur) (/ BW (line start-freq end-freq dur)))))
(definst f2-dyn [start-freq 1000.0 end-freq 1500.0 dur 0.02 Amp 1.0 BW 70.0] (* 5.6 Amp (bpf (saw 100) (line start-freq end-freq dur) (/ BW (line start-freq end-freq dur)))))
(definst f3-dyn [start-freq 2000.0 end-freq 2500.0 dur 0.02 Amp 1.0 BW 110.0] (* 5.2 Amp (bpf (saw 100) (line start-freq end-freq dur) (/ BW (line start-freq end-freq dur)))))
(definst f4-dyn [start-freq 3000.0 end-freq 3500.0 dur 0.02 Amp 1.0 BW 250.0] (* 4.8 Amp (bpf (saw 100) (line start-freq end-freq dur) (/ BW (line start-freq end-freq dur)))))
(definst f5-dyn [start-freq 3500.0 end-freq 4000.0 dur 0.02 Amp 1.0 BW 200.0] (* 4.4 Amp (bpf (saw 100) (line start-freq end-freq dur) (/ BW (line start-freq end-freq dur)))))
(definst f6-dyn [start-freq 4000.0 end-freq 4500.0 dur 0.02 Amp 1.0 BW 1000.0] (* 4.0 Amp (bpf (saw 100) (line start-freq end-freq dur) (/ BW (line start-freq end-freq dur)))))
;main functions
(defn make-sound [t_ref [{t_init :t, fa1 :fa, fb1 :fb, fc1 :fc vol1 :vol} {t_end :t, fa2 :fa, fb2 :fb, fc2 :fc vol2 :vol}]]
(let [t_dur (- t_end t_init); ms->s change
      t_dur_fcn (* 0.001 (- t_end t_init))] ;ms->s change
(if (zero? t_init)
  (at (+ t_init t_ref) (doseq[]  (f1-dyn fa1 fa2 t_dur_fcn vol1) (f2-dyn fb1 fb2 t_dur_fcn vol1) (f3-dyn fc1 fc2 t_dur_fcn vol1) ))
  (at (+ t_init t_ref) (doseq[] (kill f1-dyn f2-dyn f3-dyn) (f1-dyn fa1 fa2 t_dur_fcn vol2)
                              (f2-dyn fb1 fb2 t_dur_fcn vol2) (f3-dyn fc1 fc2 t_dur_fcn vol2))))))
(defn zip [& colls]
  (into [] (apply map vector colls)))
(defn make-vowel [vowel]
  (doseq[] (f1 (vowel :fa))(f2 (vowel :fb))(f3 (vowel :fc))))
 (def vowel (ref {:t 100 :fa 750.0 :fb 1000.0 :fc 2600.0 :vol 1.0}))
 
 (defn loud-alter-vowel-f1
 [f val] 
 (let [newval (scale-range val 0 1 100 1000)]
      (ctl f :freq newval)
      (dosync (ref-set vowel (merge @vowel {:fa newval}))) ))
 (defn loud-alter-vowel-f2
 [f val] 
 (let [newval (scale-range val 0 1 500 3000)]
      (ctl f :freq newval)
      (dosync (ref-set vowel (merge @vowel {:fb newval})))))
(defn speak [syllable]
  (let [time (now)
        slist (zip (take (dec (count syllable)) syllable) (rest syllable))]
    (dorun (map #(make-sound time %) slist))
    (at (+ 100 time)  
     (doseq[]  
       (make-vowel @vowel)
       (osc-handle server "/1/fader1" (fn [msg] (println msg)(loud-alter-vowel-f1 f1 (first (:args msg)))))
       (osc-handle server "/1/fader2" (fn [msg] (println msg)(loud-alter-vowel-f2 f2 (first (:args msg)))))
       (kill f1-dyn f2-dyn f3-dyn)))
     ))
(defn m1 [vowel] 
  [{:t 0 :fa 240.0 :fb 1000.0 :fc 2600.0 :vol 0.5}
   {:t 100 :fa 240.0 :fb 1000.0 :fc 2600.0 :vol 0.5}
   {:t 120  :fa (vowel :fa) :fb (vowel :fb)  :fc (vowel :fc) :vol (vowel :vol)}])
;last version
(speak (m1 @vowel))
(stop)

(defn silent-alter-vowel-f1 ;gen-f1f2
    [vowel val]  
  (dosync (ref-set vowel (merge @vowel {:fa (scale-range val 0 1 100 1000)}))))
 (defn silent-alter-vowel-f2 ;gen-f1f2
    [vowel val]   
  (dosync (ref-set vowel (merge @vowel {:fb (scale-range val 0 1 500 3000)}))))
 (defn enable-m
  [val]
  (if (= val 1.0) (doseq [] (speak (m1 @vowel))) 
    (doseq[]
 (osc-handle server "/1/fader1" (fn [msg] (println msg)(silent-alter-vowel-f1 vowel (first (:args msg)))))
 (osc-handle server "/1/fader2" (fn [msg] (println msg)(silent-alter-vowel-f2 vowel (first (:args msg)))))(stop))
    ))

(dosync
 (osc-handle server "/1/fader1" (fn [msg] (println msg)(silent-alter-vowel-f1 vowel (first (:args msg)))))
 (osc-handle server "/1/fader2" (fn [msg] (println msg)(silent-alter-vowel-f2 vowel (first (:args msg))))))

(osc-handle server "/1/push7" (fn [msg] (println msg)(enable-m (first (:args msg)))));currently m+vowel
(osc-handle server "/1/push12" (fn [msg](println msg) (stop)));stop
;(osc-rm-handler server "/1/fader1")
;(osc-close client)
;(osc-close server) 
(stop)
