(ns CVSyn.mabala.core
  (:use overtone.live)
  (:use [overtone.at-at :only (mk-pool)])
  (:use [overtone.at-at :only (after)]))
(load-file "src/mabala/data.clj")
(def my-pool (mk-pool))
(definst f0 [freq 120.0 BW 50.0 Amp 6.4] (* Amp (bpf (saw 100) freq (/ BW freq))))
(definst f1 [freq 450.0 BW 50.0 Amp 6.0] (* Amp (bpf (saw 100) freq (/ BW freq))))
(definst f2 [freq 1450.0 BW 70.0 Amp 5.6] (* Amp (bpf (saw 100) freq (/ BW freq))))
(definst f3 [freq 2450.0 BW 110.0 Amp 5.2] (* Amp (bpf (saw 100) freq (/ BW freq))))
(definst f4 [freq 3300.0 BW 250.0 Amp 4.8] (* Amp (bpf (saw 100) freq (/ BW  freq))))
(definst f5 [freq 3850.0 BW 200.0 Amp 4.4] (* Amp (bpf (saw 100) freq (/ BW freq))))
(definst f6 [freq 4900.0 BW 1000.0 Amp 4.0] (* Amp (bpf (saw 100) freq (/ BW freq))))
; u:320, 800 o: 500,1000, a:1000, 1400(bpf 60,200,100?)  e:500,2300 i:320,2500 or 280, 2250 



(definst f1-dyn [start-freq 100.0 end-freq 500.0 dur 0.02 BW 50.0 Amp 1.0] (* 6.0 Amp (bpf (saw 100) (line start-freq end-freq dur) (/ BW (line start-freq end-freq dur)))))
(definst f2-dyn [start-freq 1000.0 end-freq 1500.0 dur 0.02 BW 70.0 Amp 1.0] (* 5.6 Amp (bpf (saw 100) (line start-freq end-freq dur) (/ BW (line start-freq end-freq dur)))))
(definst f3-dyn [start-freq 2000.0 end-freq 2500.0 dur 0.02 BW 110.0 Amp 1.0] (* 5.2 Amp (bpf (saw 100) (line start-freq end-freq dur) (/ BW (line start-freq end-freq dur)))))
(definst f4-dyn [start-freq 3000.0 end-freq 3500.0 dur 0.02 BW 250.0 Amp 1.0] (* 4.8 Amp (bpf (saw 100) (line start-freq end-freq dur) (/ BW (line start-freq end-freq dur)))))
(definst f5-dyn [start-freq 3500.0 end-freq 4000.0 dur 0.02 BW 200.0 Amp 1.0] (* 4.4 Amp (bpf (saw 100) (line start-freq end-freq dur) (/ BW (line start-freq end-freq dur)))))
(definst f6-dyn [start-freq 4000.0 end-freq 4500.0 dur 0.02 BW 1000.0 Amp 1.0] (* 4.0 Amp (bpf (saw 100) (line start-freq end-freq dur) (/ BW (line start-freq end-freq dur)))))

(def freq1_temp (atom 1000))
(def freq2_temp (atom 1400))
(def freq3_temp (atom 2890))
(defn change_freq [fa fb fc]
  (doseq[] (f1 fa) (f2 fb) (f3 fc))
  )

(defn test_nasal []
  (let [time (now)
        dur_nasal 0.02 ] ;dur=0.08
    (at time (doseq[] (f1 240.0 70.0) (f2 1000.0 430.0) (f3 2600 75.0) )) ; consonant 240 1000 2600
    (at (+ 100 time) (doseq[] (kill [f1 f2 f3]) (f1-dyn 240 750.0 dur_nasal 70.0) 
                             (f2-dyn 1000 1000.0 dur_nasal 430.0) (f3-dyn 2600 2300.0 dur_nasal 75.0))) ; freq change
    (at (+ 180 time) (doseq[] (kill [f1-dyn f2-dyn f3-dyn]) (f1 750.0 80.0) (f2 1000.0 120.0) (f3 2300 75.0))) ;a
    (at (+ 500 time) (stop) ) ; nasal end, 820,1180, 2630 or 1000 1400 2890
    ))
(defn test_labial []
  (let [time (now)
        dur_nasal 0.02 ] ;dur=0.08
    (at time (doseq[]  (f1 400.0 50.0) (f2 1000.0 70.0) (f3 2000 110.0) )) ; consonant 240 1000 2600
    (at (+ 20 time) (doseq[] (kill [f1 f2 f3]) (f1-dyn 400 650.0 dur_nasal 50.0) 
                             (f2-dyn 1000 1200.0 dur_nasal 70.0) (f3-dyn 2000 2500.0 dur_nasal 110.0))) ; freq change
    (at (+ 40 time) (doseq[] (kill [f1-dyn f2-dyn f3-dyn]) (f1-dyn 650 750.0 dur_nasal 50.0) 
                             (f2-dyn 1200 1150.0 dur_nasal 70.0) (f3-dyn 2500 2500.0 dur_nasal 110.0)))
    (at (+ 400 time) (doseq[] (kill [f1-dyn f2-dyn f3-dyn]) (f1-dyn 750 750.0 dur_nasal 50.0) 
                             (f2-dyn 1150 1000.0 dur_nasal 70.0) (f3-dyn 2500 2300.0 dur_nasal 110.0)))
    (at (+ 430 time) (stop) ) ; nasal end, 820,1180, 2630 or 1000 1400 2890
    ))
(defn test1 []
  (let [time (now)
        dur_nasal 0.02 ] ;dur=0.08
    (at time (doseq[]  (f3 180.0 :Amp 0.7) )) ; consonant 240 1000 2600
    ;(at (+ 80 time) (doseq[] (kill [f1 f2 f3]) (f1-dyn 400 650.0 dur_nasal 50.0) 
    ;                         (f2-dyn 1000 1200.0 dur_nasal 70.0) (f3-dyn 2000 2500.0 dur_nasal 110.0))) ; freq change
    (at (+ 80 time) (doseq[] (kill f3) (kill [f1-dyn f2-dyn f3-dyn]) (f1-dyn 320 320.0 dur_nasal 50.0) 
                             (f2-dyn 800 800.0 dur_nasal 70.0) (f3-dyn 2500 2500.0 dur_nasal 110.0)))
    (at (+ 110 time) (doseq[] (kill [f1-dyn f2-dyn f3-dyn]) (f1-dyn 320 750.0 dur_nasal 50.0) 
                             (f2-dyn 800 1000.0 dur_nasal 70.0) (f3-dyn 2500 2300.0 dur_nasal 110.0)))
    (at (+ 430 time) (stop) ) ; nasal end, 820,1180, 2630 or 1000 1400 2890
    ))
(test1)
(stop)

(defn make_sound [start_t current_t fa fb fc amp]
  (if (zero? current_t)
    (at start_t (doseq[]  (f1-dyn fa fa) (f2-dyn fb fb) (f3-dyn fc fc) ))
    (at (+ current_t start_t) (doseq[] (kill f1-dyn f2-dyn f3-dyn) (f1-dyn @freq1_temp fa)
                                (f2-dyn @freq2_temp fb) (f3-dyn @freq3_temp fc))))
  (doseq[] (reset! freq1_temp fa)(reset! freq2_temp fb)(reset! freq3_temp fc)))

(defn trial_ba []
  (let [time (now)]
  (make_sound time 0 400 1000 2000 1) ;start_time,current_time, f1, f2, f3, amplitude
  (make_sound time 20 650 1200 2500 1) 
  (make_sound time 50 750 1150 2500 1)
  (make_sound time 400 750 1000 2300 1);a
  (at (+ 430 time) (stop))
  ))
(trial_ba)
(stop)
(defn trial_ma []
  (let [time (now)]
  (make_sound time 0 240 1000 2600 1); m sound start
  ;(make_sound ti te freq1 freq2 freq3); ti - init time of segmeng; te =
  (make_sound time 100 750 1000 2300 1) 
  (make_sound time 180 750 1000 2300 1);a
  (at (+ 400 time) (stop))
  ))
(trial_ma)
(stop)
(defn trial_v []
  (let [time (now)]
  (make_sound time 0 300 1000 2300 1)
  (make_sound time 20 300 800 2300 1) 
  ;(make_sound time 40 600 1000 2300 1)
  ;(make_sound time 300 300 800 2300 1)
  (at (+ 100 time) (stop))
  ))
(trial_v) ;not good yet
(stop)
(defn trial_lu []
  (let [time (now)]
  (make_sound time 0 500 800 2500 1); l start
  (make_sound time 10 320 800 2300 1) ; 'u' component
  (make_sound time 30 750 1000 2300 1);la
  ;(make_sound time 30 500 1000 2300 1) ;lu, actually use 'o' sound
  (at (+ 250 time) (stop))
  ))
(trial_lu)
(stop)

(defn trial_ai []
  (let [time (now)]
  (make_sound time 0 750 1000 2300 1)
  (make_sound time 100 320 2500 2300 1);la
  ;(make_sound time 30 500 1000 2300 1) ;lu
  (at (+ 400 time) (stop))
  ))
(trial_ai)
(stop)
;TIME = 000; F1=400; F2=1000; F3=2000; F0=120; AV=72
;TIME +  20; F1=650; F2=1200; F3=2500; AV=72
;TIME +  20; F1=750; F2=1150; F3=2500; AV=72
;TIME = 400; F1=750; F2=1000; F3=2300; F0=90; AV=72
;TIME + 30; AV=0
(defn maba []
  (let [time (now)]
    ;(at time (trial_ba)) ;ma
    (at (+ 10000 time) (trial_ma));ba
    )) 
(test_nasal)
(test_labial)
(maba)
(stop)

 
