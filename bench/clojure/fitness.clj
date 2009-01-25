(import
  '(java.awt Color)
  '(java.awt.image BufferedImage PixelGrabber)
  '(java.io File)
  '(javax.imageio ImageIO))

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

; fitness :: Program -> Map -> Program
(defn fitness [settings]
  (let [gen-image (new BufferedImage (:image-width settings)
                                     (:image-height settings)
                                     BufferedImage/TYPE_INT_ARGB)
        src-pixels (:source-pixels settings)]
    (def gen-pixels (grab-pixels gen-image))
    (loop [i (int 0)
           lms (int 0)]
      (if (< i (alength gen-pixels))
        (let [src-color (new Color (aget src-pixels i))
              gen-color (new Color (aget gen-pixels i))
              dr (- (. src-color (getRed)) (. gen-color (getRed)))
              dg (- (. src-color (getGreen)) (. gen-color (getGreen)))
              db (- (. src-color (getBlue)) (. gen-color (getBlue)))]
          (recur (unchecked-inc i) (int (+ lms (* dr dr) (* dg dg) (* db db )))))))))

(def image (ImageIO/read (new File "../../mona_lisa_crop.jpg")))

(doseq [p (range 100)]
  (fitness {:image-width (. image (getWidth)) :image-height (. image (getHeight)) :source-pixels (grab-pixels image)}))

(System/exit 0)