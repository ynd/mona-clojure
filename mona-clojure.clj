(import
  '(java.awt Graphics Graphics2D Color Polygon)
  '(java.awt.image BufferedImage PixelGrabber)
  '(java.io File)
  '(javax.imageio ImageIO)
  '(javax.swing JFrame JPanel JFileChooser))

; ---------------------------------------------------------------------
; This section defines the building blocks of the genetic programs.

; color :: Integer -> Integer -> Integer -> Integer -> Color
(defn color [red blue green alpha] {:type :Color :red red :blue blue :green green :alpha alpha})

; point :: Integer -> Integer -> Point
(defn point [x y] {:type :Point :x x :y y})

; polygon :: Color -> [Point] -> Polygon
(defn polygon [color points] {:type :Polygon :color color :points points})

; draw-polygon :: Graphics -> Polygon -> Nothing
(defn draw-polygon [graphics polygon]
  (doto graphics
    (.setColor (new Color (:red (:color polygon)) 
                          (:blue (:color polygon))
                          (:green (:color polygon))
                          (:alpha (:color polygon))))
    (.fillPolygon (let [jpolygon (new Polygon)]
                    (doseq [p (:points polygon)] (. jpolygon (addPoint (:x p) (:y p))))
                    jpolygon)))
  nil)

; ----------------------------------------------------------------------
; This sections defines helper functions.

; random-double :: Double
(defn random-double
  "Returns a double between -1.0 and 1.0."
  []
  (- (* 2 (rand)) 1))

; remove-item :: Sequence -> Integer -> Sequence
(defn remove-item
  "Returns a sequence without the n-th item of s."
  [s n]
  (cond
    (vector? s) (into (subvec s 0 n)
                (subvec s (min (+ n 1) (count s)) (count s)))
    (list? s) (concat (take n s)
                      (drop (inc n) s))))

; replace-item :: [a] -> Integer -> a -> [a]
(defn replace-item
  "Returns a list with the n-th item of l replaced by v."
  [l n v]
  (concat (take n l) (list v) (drop (inc n) l)))

; grab-pixels :: BufferedImage -> [Integer]
(defn grab-pixels
  "Returns an array containing the pixel values of image."
  [image]
  (let [w (. image (getWidth))
        h (. image (getHeight))
        pixels (make-array (. Integer TYPE) (* w h))]
    (doto (new PixelGrabber image 0 0 w h pixels 0 w)
      (.grabPixels))
    pixels))

; ----------------------------------------------------------------------
; This sections define the primitives of the genetic algorithm.

; program :: S-Expression -> Maybe Integer -> Maybe BufferedImage -> Program
(defn program [code fitness image] {:type :Program :code code :fitness fitness :image image})

; initial-program :: Program
(def initial-program (program '(fn [graphics]) nil nil))

; program-header :: Program -> S-Expression
(defn program-header [p] (take 2 (:code p)))

; program-expressions :: Program -> S-Expression
(defn program-expressions [p] (drop (count (program-header p)) (:code p)))

; mutate :: a -> Map -> a
(defmulti mutate :type)

; mutate :: Color -> Map -> Color
(defmethod mutate :Color [c settings]
  (let [dr (int (* (:red c) (random-double)))
        dg (int (* (:green c) (random-double)))
        db (int (* (:blue c) (random-double)))
        da (int (* (:alpha c) (random-double)))]
    (assoc c :red (max (min (- (:red c) dr) 255) 0)
             :green (max (min (- (:green c) dg) 255) 0)
             :blue (max (min (- (:blue c) db) 255))
             :alpha (max (min (- (:alpha c) da) 255)))))

; mutate :: Point -> Map -> Point
(defmethod mutate :Point [p settings]
  (let [dx (int (* (:x p) (random-double)))
        dy (int (* (:y p) (random-double)))]
    (assoc p :x (max (min (- (:x p) dx) (:image-width settings)) 0)
             :y (max (min (- (:y p) dy) (:image-height settings)) 0))))

; mutate :: Polygon -> Map -> Polygon
(defmethod mutate :Polygon [p settings] 
  ; mutate-point :: Polygon -> Map -> Polygon
  (defn mutate-point [p settings]
    (let [n (rand-int (count (:points p)))]
      (assoc p :points (assoc (:points p) n (mutate (get (:points p) n) settings)))))

  ; mutate-color :: Polygon -> Map -> Polygon
  (defn mutate-color [p settings] (assoc p :color (mutate (:color p) settings)))
  
  (let [roulette (rand-int 2)]
    (cond
      (= 0 roulette) (mutate-point p settings)
      (= 1  roulette) (mutate-color p settings))))

; mutate :: Program -> Map -> Program
(defmethod mutate :Program [p settings]
  ; add-polygon :: Program -> Map -> Program
  (defn add-polygon [p settings]
    (assoc p :code 
             (concat (:code p)
                     [(list 'draw-polygon
                            (first (nth (:code initial-program) 1))
                            (polygon
                              (color (rand-int 255) (rand-int 255) (rand-int 255) (rand-int 255))
                              (vec (map 
                                 (fn [n]
                                     (point 
                                       (rand-int (:image-width settings))
                                       (rand-int (:image-height settings))))
                                 (range 5)))))])
             :fitness nil :image nil))

  ; remove-polygon :: Program -> Map -> Program
  (defn remove-polygon [p settings]
    (let [n (rand-int (count (program-expressions p)))]
      (assoc p :code (concat (program-header p)
                             (remove-item (program-expressions p) n))
               :fitness nil :image nil)))

  ; mutate-polygon :: Program -> Map -> Program
  (defn mutate-polygon [p settings]
    (let [expressions (program-expressions p)
          n (rand-int (count expressions))
          target (nth expressions n)]
      (assoc p :code
               (concat (program-header p)
                       (replace-item expressions
                                     n
                                     (list (nth target 0)
                                           (nth target 1)
                                           (mutate (nth target 2) settings))))
               :fitness nil :image nil)))
  
  (let [polygon-count (count (program-expressions p))
        roulette (cond
                   (empty? (program-expressions p)) 4
                   (>= polygon-count (:max-polygons settings)) (rand-int 4)
                   :else (rand-int 5))]
    (cond
      (> 3 roulette) (mutate-polygon p settings)
      (= 3 roulette) (remove-polygon p settings)
      (= 4 roulette) (add-polygon p settings))))

; fitness :: Program -> Map -> Program
(defn fitness [individual settings]
  (if (:fitness individual)
    individual
    (let [gen-image (new BufferedImage (:image-width settings)
                                       (:image-height settings)
                                       BufferedImage/TYPE_INT_ARGB)
          src-pixels (:source-pixels settings)]
      (apply (eval (:code individual)) [(. gen-image (createGraphics))])
      (def gen-pixels (grab-pixels gen-image))
      (loop [i (int 0)
             lms (int 0)]
        (if (< i (alength gen-pixels))
          (let [src-color (new Color (aget src-pixels i))
                gen-color (new Color (aget gen-pixels i))
                dr (- (. src-color (getRed)) (. gen-color (getRed)))
                dg (- (. src-color (getGreen)) (. gen-color (getGreen)))
                db (- (. src-color (getBlue)) (. gen-color (getBlue)))]
            (recur (unchecked-inc i) (int (+ lms (* dr dr) (* dg dg) (* db db )))))
          (assoc individual :fitness lms :image gen-image))))))

; select :: [Program] -> Map -> [Program]
(defn select [individuals settings]
  (take (:select-rate settings)
        (sort-by :fitness
                 (pmap (fn [i] (fitness i settings))
                       individuals))))

; evolve :: Map -> Nothing
(defn evolve [settings]
  (loop [i 0
         population (list initial-program)]
    (let [fittest (select population settings)
          newborns (map (fn [i] (mutate i settings)) fittest)]
      ((:new-generation-callback settings (fn [a b])) i fittest)
      (when-not (= (first population) (first fittest))
        ((:new-fittest-callback settings (fn [a b])) i fittest))
      (recur (inc i) (concat fittest newborns)))))

; ----------------------------------------------------------------------
; This sections defines the graphical interface.

; main :: Nothing
(defn main []
  (def file-chooser (new JFileChooser))
  (doto file-chooser
    (.setCurrentDirectory (new File "."))
    (.showOpenDialog nil))
  
  (let [jframe (new JFrame "Fittest Program")
        fittest (atom (list initial-program))
        image (ImageIO/read (. file-chooser (getSelectedFile)))
        settings {:image-width (. image (getWidth))
                  :image-height (. image (getHeight))
                  :source-pixels (grab-pixels image)
                  :select-rate 1 :max-polygons 50
                  :new-fittest-callback (fn [i f]
                                            (swap! fittest (fn [o n] n) f)
                                            (. jframe (repaint)))}]
    (doto jframe
      (.setSize (. image (getWidth)) (. image (getHeight)))
      (.add (proxy [JPanel] []
        (paint [g]
          (doto g 
            (.setColor Color/white)
            (.fillRect 0 0 (. image (getWidth)) (. image (getHeight)))
            (.drawImage (:image (first @fittest)) nil 0 0)))))
      (.setVisible true))
    (evolve settings)))

(main)