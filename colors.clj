(ns nodebox-colors
  (:import [nodebox.graphics Color])
  (:import [nodebox.util MathUtils]))

(defrecord RGBA [red green blue alpha])
(defrecord HSBA [hue saturation brightness alpha])

(defn to-rgba
  "Convert a NodeBox color to rgba components."
  [c]
  (cond 
    (instance? Color c) (RGBA. (.getRed c) (.getGreen c) (.getBlue c) (.getAlpha c))))

(defn to-hsba
  "Convert a NodeBox color to hsba components."
  [c]
  (cond 
    (instance? Color c) (HSBA. (.getHue c) (.getSaturation c) (.getBrightness c) (.getAlpha c))))

(defn to-nodebox-rgba
  "Convert rgba components to a NodeBox color."
  [c]
  (Color. (:red c) (:green c) (:blue c) (:alpha c)))

(defn to-nodebox-hsba
  "Convert hsba components to a NodeBox color."
  [c]
  (Color/fromHSB (:hue c) (:saturation c) (:brightness c) (:alpha c)))

(defn to-nodebox-color
  "Convert color components to a NodeBox color."
  [c]
  (cond 
    (instance? RGBA c) (to-nodebox-rgba c)
    (instance? HSBA c) (to-nodebox-hsba c)))

(defn adjust_hsb_param
  "Manipulate a HSBA parameter"
  ([color param step]
    (let [c (to-hsba color)]
    (to-nodebox-color (apply assoc c [param (+ (param c) step)])))))

(defn brightness
  "Darken or lighten a color"
  ([color step]
   (adjust_hsb_param color :brightness (/ step 100.0))))

(defn saturation
  "(De)saturate a color"
  ([color step]
    (adjust_hsb_param color :saturation (/ step 100.0))))

(defn rotate-rgb
  ([color, angle]
  (let [c (to-hsba color) h (mod (+ (:hue c) (/ angle 360.0)) 1)]
   (to-nodebox-color (apply assoc c [:hue h])))))

; Approximation of Itten's RYB color wheel.
; In HSB, colors hues range from 0-360.
; However, on the artistic color wheel these are not evenly distributed. 
; The second tuple value contains the actual distribution.
(def wheel [
    [  0   0] [ 15   8]
    [ 30  17] [ 45  26]
    [ 60  34] [ 75  41]
    [ 90  48] [105  54]
    [120  60] [135  81]
    [150 103] [165 123]
    [180 138] [195 155]
    [210 171] [225 187]
    [240 204] [255 219]
    [270 234] [285 251]
    [300 267] [315 282]
    [330 298] [345 329]
    [360 0  ]])

(defn find-angle-artistic
 [wheel-list h]
 (let [[x0 y0] (wheel-list 0)
       [x1 Y1] (wheel-list 1)
       y1 (cond (< Y1 y0)
            (+ Y1 360)
            :else Y1)]
   (cond (and (<= y0 h) (<= h y1))
     (+  (/ (* (- x1 x0) (- h y0)) (- y1 y0)) (* 1.0 x0))
     :else (recur (vec (rest wheel-list)) h))))

(defn find-hue-artistic
 [wheel-list a]
 (let [[x0 y0] (wheel-list 0)
       [x1 Y1] (wheel-list 1)
       y1 (cond (< Y1 y0)
            (+ Y1 360)
            :else Y1)]
   (cond (and (<= x0 a) (<= a x1))
     (+  (/ (* (- y1 y0) (- a x0)) (- x1 x0)) (* 1.0 y0))
     :else (recur (vec (rest wheel-list)) a))))

(defn rotate-ryb
  ([color ang]
  (let [c (to-hsba color)
        h (* (:hue c) 360)
        angle (mod ang 360)
        a (mod (+ (find-angle-artistic wheel h) angle) 360)
        new-h (mod (find-hue-artistic wheel a) 360)]
    (to-nodebox-color (apply assoc c [:hue (/ new-h 360.0)]) ))))

(defn invert-color
    [color]
    (let [c (to-rgba color)]
    (to-nodebox-color (RGBA. (- 1.0 (:red c)) (- 1.0 (:green c)) (- 1.0 (:blue c)) (:alpha c)))))

(defn complement
  [color]
  (rotate-ryb color 180))

(defn analog
  [color angle d seed]
  (let [rnd (MathUtils/randomFromSeed seed)
        confine-range (fn [number r] (- (* number (* 2 r)) r))
        rotated-color (rotate-ryb color (confine-range (.nextDouble rnd) angle))
        resulting-color (to-hsba rotated-color) 
        s (* (:saturation resulting-color) (- 1 (confine-range (.nextDouble rnd) d)))
        b (* (:brightness resulting-color) (- 1 (confine-range (.nextDouble rnd) d)))]
    (to-nodebox-color (apply assoc resulting-color [:saturation s :brightness b]))))
