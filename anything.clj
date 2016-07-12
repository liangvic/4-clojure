
(print-str "Hello, World" " " "from Kilimanjaro")
(- 5 6 7)
(-(* 3 4) 13)
"hello"
(/ 4 3)
(/ 4.0 3.0)
; floating points taint the type outcome
(/ 4 3.0)

(+ 4/3 2/6)
; these 'global variables' are namespaced (user is default)
(def mangoes 2)
(def oranges 5)
(+ mangoes oranges)

(def height-ft 5)
(def height-in 4)

(def height-total-in (+ (* height-ft 12) height-in))
(def height-total-cm (* 2.54 height-total-in))
height-total-in
height-total-cm

(def steph-height-cm 152.3)
(def christina-height-cm 172.72)
(def average-height-cm (/ (+ steph-height-cm christina-height-cm height-total-cm) 3))
average-height-cm

(def average-height-total-in (/ average-height-cm 2.54) )
(def average-height-ft (quot average-height-total-in 12))
(def average-height-in (rem average-height-total-in 12))
average-height-ft
average-height-in

;vectors - sequential collection
;simple and complex data structures are IMMUTABLE unless you redefine it
(def days [1 2 3 4 5 6 7 8 9 0])
(conj days 10)
(conj days [11, 12, 13])
; value equality, no reference equality
(= days [1 2 3 4 5 6 7 8 9 0])
(= days [0 9 8 7 6 5 4 3 2 1])

(pop days)

(count days)
(nth days 1)
(first days)
; returns a sequence (linked-list)
(rest days)

;anything can be a key: boolean, data structure, etc.
(def address (hash-map :state "California" :city "Fremont" :street "Belmont Terrace"))
(def me (hash-map :name "Vic" :age 22 :address address))
me
(assoc address :zip 94539)
(dissoc address :state)
(count address)
(get address :street)
(get address :housenumber)
(get address :housenumber "DEFAULT")
;update/update-in get, [apply function to value], assoc
(update-in me [:age] - 3)


(update-in me [:address] dissoc address :state)

(def stephanie (hash-map :name "Stephanie" :age 22 :address "Roanoke"))
(def christina (hash-map :name "Christina" :age 22 :address "Seattle"))
(def users [me, stephanie, christina])
(get-in users christina)

;mapping - apply to each value
(map inc days)

;reduce, apply and accumulate
(reduce + days)

(defn withinRange
  "Return true if number is specified number"
  [number numMin numMax]
  (and (> number numMin) (< number numMax)))

; conditionals
(def y 120)
(cond
  (> (+ y 40) 150) "cat"  ;if
  (< (+ y 40) 150) "dog" ;else-if
  :else "rat") ;else

(if (> (+ y 40) 150)
  "cat"
  "dog")

;;Standard recursion
(defn add-all-method1 [numbers]
  (cond
    (empty? numbers) 0
    :else (+ (first numbers) (add-all-method1 (rest numbers)))))

(add-all-method1 [1 2 3 4 5 6 7 8 9])

;;Recursion - without expansion
(defn add-all-helper [numbers starting-number]
  (cond
    (empty? numbers) starting-number
    :else (add-all-helper (rest numbers) (+ starting-number (first numbers)))))

(defn add-all-method2 [numbers]
  (add-all-helper numbers 0))

(add-all-method2 [1 2 3 4 5 6 7 8 9])


;;Recusion with recur
(defn add-all-method3 [numbers]
  (loop [nums numbers ;first loop numbers, afterwards use recurse nums
         starter-number 0]
    (cond
      (empty? nums) starter-number
      :else (recur (rest nums) (+ starter-number (first nums))))))

(add-all-method3 [1 2 3 4 5 6 7 8 9])

;;implementations of count
(defn length-method3 [numbers]
  (loop [nums numbers
         base 0]
    (cond
      (empty? nums) base
      :else (recur (rest nums) (+ base 1)))))

(length [1 2 3 4 5  6 7 8 9])


;;implementation of mutltiply all
(defn multiply-all-method1 [numbers]
  (cond
    (empty? numbers) 1
    :else (* (multiply-all-method1 (rest numbers)) (first numbers))))

(multiply-all-method1 [1 2 3 4 5 6])


(defn multiply-all-method3 [numbers]
  (loop [nums numbers
         base 1]
    (cond
      (empty? nums) base
      :else (recur (rest nums) (* base (first nums))))))


(multiply-all-method3 [1 2 3 4 5 6])

;;Atom (for things that change over time))
(def counter (atom 0))
(deref counter)
;; mutates
(swap! counter inc)
(deref counter)
counter

(def me (atom {:name "Vic" :food "chili"}))
(def me-right-now (deref me))
me-right-now
;;apply anonymous function that changes food
(swap! me (fn [m] (assoc m :food "cheese")))
;;snapshot me-right-now is still the same
me-right-now
(def me-after (deref me))
me-after
;;Standard closure way is not to create multiple atoms, but have 1 atom that captures state of everything




