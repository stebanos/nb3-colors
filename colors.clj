(ns nodebox-colors
  (:import [nodebox.graphics Color]))

(defrecord RGBA [red green blue alpha])
(defrecord HSBA [hue saturation brightness alpha])

(defn to-rgba
  "Convert a NodeBox color to rgba components."
  [c]
  (cond 
    (instance? Color c) (HSBA. (.getRed c) (.getGreen c) (.getBlue c) (.getAlpha c))))

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

;(defn adjust_hsb_param
;  "Manipulate a HSBA parameter"
;  ([color param step]
;    (let [c (to-hsba color)]
;    (to-nodebox-color (assoc c param (+ (param c) step))))))

(defn darken
  "Darken a color"
  ([color step]
    (let [c (to-hsba color)]
    (to-nodebox-color (assoc c :brightness (- (:brightness c) step))))))

(defn lighten
  "Lighten a color"
  ([color step]
    (let [c (to-hsba color)]
    (to-nodebox-color (assoc c :brightness (+ (:brightness c) step))))))

(defn desaturate
  "Desaturate a color"
  ([color step]
    (let [c (to-hsba color)]
    (to-nodebox-color (assoc c :saturation (- (:saturation c) step))))))

(defn saturate
  "Saturate a color"
  ([color step]
    (let [c (to-hsba color)]
    (to-nodebox-color (assoc c :saturation (+ (:saturation c) step))))))
