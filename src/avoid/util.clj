(ns avoid.util (:gen-class))

(defn square [x] (* x x))

(defn dot-product [v1 v2]
  (apply + (apply (partial map *) [v1 v2])))

(defn scalar-vector-multiplication [s v]
  (map (partial * s) v))

(defn vector-plus [& vectors]
  (apply (partial map +) vectors))

(defn vector-minus [& vectors]
  (apply (partial map -) vectors))

(def magnitude
  (comp (fn [x] (Math/sqrt x)) (partial apply +) (partial map square)))

(defn position-distance [p1 p2]
  (Math/sqrt (apply + (map (comp square -) p1 p2))))

(defn normalize [v]
  (let [m (magnitude v)]
    (if (not= m 0.0)
      (scalar-vector-multiplication (/ 1.0 m) v)
      v)))

(def normalized-direction (comp normalize vector-minus))

(defn collide [p1 v1 p2 v2]
  (let
   [n (normalized-direction p1 p2)
    negative-n (scalar-vector-multiplication -1.0 n)
    vn1 (scalar-vector-multiplication
         (dot-product v1 negative-n) negative-n)
    vn2 (scalar-vector-multiplication
         (dot-product v2 n) n)
    vt1 (vector-minus vn1 v1)
    vt2 (vector-minus vn2 v2)]
    {:v1-next (vector-plus vt1 vn2)
     :v2-next (vector-plus vt2 vn1)}))

(defn collision-time [[px1 py1] [vx1 vy1] r1 [px2 py2] [vx2 vy2] r2]
  (let [cx (- px2 px1)
        cy (- py2 py1)
        vx (- vx2 vx1)
        vy (- vy2 vy1)
        r (+ r1 r2)
        r-square (square r)
        a (* -1.0 (+ (* cx vx) (* cy vy)))
        b (Math/sqrt (- (* (square r) (+ (square vx) (square vy))) (square (- (* cx vy) (* cy vx)))))
        c (/ 1.0 (+ (square vx) (square vy)))
        t1 (* c (- a b))
        t2 (* c (+ a b))
        valid-times (filter (partial <= 0.0) [t1 t2])]
        ; (println t1 t2)
    (if (and false (<= (+ (square cx) (square cy)) r-square))
      0.0
      (if (not (empty? valid-times)) (apply min valid-times)))))

(defn collision-times [[px1 py1] [vx1 vy1] r1 [px2 py2] [vx2 vy2] r2]
  (let [cx (- px2 px1)
        cy (- py2 py1)
        vx (- vx2 vx1)
        vy (- vy2 vy1)
        r (+ r1 r2)
        r-square (square r)
        v-squares-sum (+ (square vx) (square vy))
        b-sqrt-input (- (* r-square v-squares-sum) (square (- (* cx vy) (* cy vx))))]
    (if (and (> v-squares-sum 0.0) (> b-sqrt-input 0.0))
      (let [a (* -1.0 (+ (* cx vx) (* cy vy)))
            b (Math/sqrt b-sqrt-input)
            c (/ 1.0 v-squares-sum)
            t1 (* c (- a b))
            t2 (* c (+ a b))]
        [t1 t2]))))