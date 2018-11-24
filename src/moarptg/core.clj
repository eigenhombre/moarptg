(ns moarptg.core
  (:gen-class))

(defn d
  ([] (inc (rand-int 6)))
  ([n] (apply + (repeatedly n d))))

(defn num-figures []
  ({1 1
    2 2
    3 2
    4 3
    5 (d 2)
    6 (d (inc (rand-int (d))))}
   (d)))

(comment (->> num-figures
              (repeatedly 1000)
              frequencies
              (sort-by (comp - second))))

(defn ar-code []
  (->> '[ϕh ϕh ϕh ϕh ϕh ϕh
         ϕh ϕh ϕh ϕv ϕv ϕv
         v2 v2 v2 ϕv ϕv ϕv
         sq sq sq sq sq sq
         h2 h2 h2 h2 h2 h2
         h2 h3 h3 h3 v3 v3]
       (partition 6)
       rand-nth
       rand-nth))

(defn ar-string [ar-c]
  (->> ar-c
       keyword
       {:ϕv "vertical golden section"
        :ϕh "horizontal golden section"
        :sq "square"
        :h2 "two squares wide"
        :h3 "three squares wide"
        :v2 "two squares high"
        :v3 "three squares high"}))

(defn horizon-line []
  ({12 :above-picture-plane
    11 0.9
    10 0.8
    9 0.7
    8 0.6
    7 0.5
    6 0.4
    5 0.3
    4 0.2
    3 0.1
    2 :bottom-of-picture-plane} (d 2)))

(defn figure-height-fraction-of-painting-height []
  (rand-nth [2 1 1 0.6 0.6 0.5 0.5 0.3 0.3 0.3 0.1 0.05]))

(defn offset-from-horizon-in-fig-fractions [rh]
  (->> (repeatedly #({12 2
                      11 1
                      10 0.5
                      9 0.25
                      8 0
                      7 0
                      6 0
                      5 0
                      4 -0.25
                      3 -0.5
                      2 -1} (d 2)))
       (drop-while (partial <= rh))
       first))

(defn painting-info []
  (let [ar (ar-code)]
    {:horizon (horizon-line)
     :ar-code ar
     :aspect (ar-string ar)
     :figures (sort-by (comp - :relative-height)
                       (for [_ (range (num-figures))]
                         (let [rh (figure-height-fraction-of-painting-height)]
                           {:relative-height rh
                            :gender (rand-nth '[m m m f f f f f u])
                            :species (rand-nth '[h h h h h a a m])
                            :offset-from-horizontal-plane (offset-from-horizon-in-fig-fractions rh)
                            })))}))

(defn upc [{:keys [ar-code horizon figures]}]
  (->> figures
       (map #(format " (%s%s %s %s)"
                     (:gender %)
                     (:species %)
                     (:relative-height %)
                     (:offset-from-horizontal-plane %)))
       clojure.string/join
       (str ar-code " " horizon)))

(defn show-painting [{:keys [horizon figures aspect]}]
  (str
   (format "Aspect ratio: %s.\n" aspect)
   (cond
     (float? horizon)
     (format "Horizon line is %.0f%% of the picture height from the bottom."
             (* 100 horizon))
     (= horizon :bottom-of-picture-plane)
     "Horizon line is the bottom of the picture plane."
     (= horizon :above-picture-plane)
     "Horizon line is above the picture plane.")
   "\nFigures:\n"
   (clojure.string/join "\n"
                        (for [{:keys [relative-height
                                      offset-from-horizontal-plane]} figures]
                          (format "\t%.0f%% of picture height; offset from eye level %.0f%% of figure."
                                  (double (* relative-height 100))
                                  (double (* offset-from-horizontal-plane 100)))))))

(defn -main []
  (dotimes [_ 40]
    (println (upc (painting-info)))))
