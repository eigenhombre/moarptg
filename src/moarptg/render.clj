(ns moarptg.render
  (:require [lanterna.screen :as s]))

(comment
  (s/stop scr)
  (def scr (s/get-screen))
  (s/start scr))

(defn blob [width height]
  (->> #(rand-nth '[█ " "])
       (repeatedly width)
       (apply str)
       (fn [])
       (repeatedly height)))

(defn blank [width height]
  (->> (repeat width " ")
       (apply str)
       (fn [])
       (repeatedly height)))

(def max-width 90)
(def max-height 49)

(def firmament
  (->> #(into [] (repeat max-width \space))
       (repeatedly max-height)
       (into [])
       atom))

(defn clear-firmament []
  (dotimes [y max-height]
    (dotimes [x max-width]
      (swap! firmament assoc-in [x y] " "))))

(defn paint [x y stuff]
  (doseq [[i s] (map-indexed vector stuff)]
    (doseq [[j c] (map-indexed vector s)]
      (swap! firmament assoc-in [(+ i y) (+ x j)] c))
    (s/put-string scr x (+ y i) s)))

(defn make-sprites []
  (let [spacing-y 2
        spacing-x 5]
    (loop [x 1
           y 1
           blobsizex 2
           blobsizey 4]
      (paint x y (blob blobsizex blobsizey))
      (let [next-x (+ spacing-x x blobsizex)]
        (if (< (+ next-x spacing-x blobsizex) max-width)
          (recur next-x y blobsizex blobsizey)
          (let [next-blobsizex (+ 1 blobsizex)
                next-blobsizey (+ 2 blobsizey)
                next-y (+ spacing-y y blobsizey)]
            (when (< (+ next-y next-blobsizey) max-height)
              (recur 1 next-y next-blobsizex next-blobsizey))))))))

(defn print-firmament []
  (doseq [s (->> firmament
                 deref
                 ;; Drop blank lines at end
                 reverse
                 (drop-while #(every? (partial = \space) %))
                 reverse)]
    (println (apply str s))))

(defn sprightly []
  (make-sprites)
  (print-firmament))

(def real-space-coords
  {:origin [0 0 0]
   :extent [100 100 100]})

(def block-space-coords
  {:origin [0 0]
   :extent [max-width max-height]})

(defn rand-real-point []
  (let [{[rex rey rez] :extent} real-space-coords]
    (map #(- (* (rand) %) (/ % 2)) [rex rey rez])))

(defn random-line-real-space []
  [(rand-real-point) (rand-real-point)])

(defn realpoint->blockpoint [[rx ry _]]
  (let [{[bex bey] :extent} block-space-coords
        {[rex rey rez] :extent} real-space-coords]
    [(int (+ (* (/ rx rex) bex) (/ bex 2)))
     (int (- (/ bey 2) (* (/ ry rey) bey)))]))

(defn draw-line-in-block-coords [[[x0 y0] [x1 y1]]]
  (let [delx (- x1 x0)
        dely (- y1 y0)
        num-div (max (Math/abs delx)
                     (Math/abs dely))
        incx (/ delx num-div)
        incy (/ dely num-div)
        xrange (map int (range x0 (inc x1) incx))
        yrange (map int (range y0 (inc y1) incy))
        blockpoints (partition 2 (interleave xrange yrange))]
    (comment (println delx
                      dely
                      '***
                      num-div
                      incx
                      incy
                      blockpoints))
    (doseq [[x y] blockpoints]
      (s/put-string scr x y "█"))))

(paint 0 0 (blank max-width max-height))
#_(make-sprites)

#_(dotimes [_ 100]
    (->> (random-line-real-space)
         (map realpoint->blockpoint)
         draw-line-in-block-coords))

(s/redraw scr)
