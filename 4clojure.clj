;;Problem 19
(defn getLast [inputSeq]
  (first (reverse inputSeq))
)
(getLast '(1 2 3 4 5))
(getLast [1 2 3 4 5])

;;Problem 23
(defn reverseit [coll]
    (if (empty? coll)
      nil
      (cons (last coll) (reverseit (drop-last coll)))
      )
)

(reduce conj () [1 2 3 4 5 6])

(cons 2 (cons 1 nil))


;;Problem 26 - Tail recursive
(defn fib [nums]
  (if  (= nums 0)
    ()
    (rest (reverse
            (loop [nums nums fibseq '(1 0)]
              (if (= nums 1)
                fibseq
                (recur (dec nums) (conj fibseq (+ (first fibseq) (second fibseq))))
                )
              )
    )))
  )

(fib 0)

;;Problem 26 - lazy seq!
(defn fib2 [nums]
  (take nums ((fn fibhelp [a b] (lazy-seq (cons a (fibhelp b (+ a b))))) 1 1)))

(fib2 3)

;;Problem 27 - Palindrome
(defn palindrome? [coll]
  (= (reverse (seq coll)) (seq coll)))

(false? (palindrome? '(:a :b :c)))

;;Problem 28 - Flatten
(defn flattenit [coll]
    (if (coll? coll)
      (if (empty? coll)
        []
        (concat (flattenit (first coll)) (flattenit (rest coll))))
      [coll]
      ))

(def coll '((1 2) 3 [4 (5 6)]))
(flattenit coll)

;;Problem 29 - getCaps
;;(defn getCaps [

(concat [] "asb")
(filter #(= \a %) (concat [] "asb"))

(def a (into [] "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(defn contains [searchColl target] (some #(= target %) searchColl))
(contains "asd" \a)
(filter #(contains a %) (concat [] "AFDsfF"))

(defn getCaps [string]
  (apply str (filter
               ;;check if each character is CAPS or not
               (fn isCaps [targetChar] (some #(= targetChar %) (into [] "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
               string)))
  ;; cleaner answer using regex
(#(apply str (re-seq #"[A-Z]" %)) "ASDfdsF")


;;Problem 30 Compress sequence
(defn dedup [coll]
  (if (empty? (rest coll))
    coll
	  (if (= (first coll) (second coll))
		  (dedup (rest coll))
		  (concat [(first coll)] (dedup (rest coll)))
    )
    )
)

(def a [ 1 2 3 3 4 5 ])
(concat [1] [ 1 2 3 ])
(dedup a)

  ;; modification of Vitaly's answer
  ;; reduce with a blank vector as starting value
(reduce
    #(if (= %2 (last %1))
      %1
      (conj %1 %2))
  '[] a)

(conj '[2] [1])
(concat [2] [1])


;;Problem 31 Pack a Sequence
(defn packseq [coll] (partition-by list coll))
(def a [1 1 2 1 1 1 3 3 4])
(def tst [ 1 2 3 4 ])
(packseq a)

;;Problem 32 Duplicate
(defn duplicate [coll] (interleave coll coll))
(duplicate tst)
  ;;Vitaly's
  (mapcat #(list % %) tst)
  ;;I like this one too
  (reduce #(conj %1 %2 %2) [] tst)

;;Problem 33 Duplicate XTREME
(defn replicateSeq [coll xtimes] (mapcat #(repeat xtimes %) coll))
(replicateSeq tst 3)

;;Problem 34 Range
(defn createRange [a b]
  (take (- b a) ((fn helper [start] (lazy-seq (cons start (helper (inc start))))) a)))
(createRange 2 5)
(defn lazy
  ([] (lazy 1))
  ([n] (lazy-seq (cons n (lazy (inc n))))))
(lazy 2)
  ;;the awesome solution - iterate return lazy-seq of x, f(x), f(f(x)), etc.
  (fn [x y] (take (- y x) (iterate inc x)))

;;Problem 38 Max
(defn getMax [& args] (reduce #(if (> %1 %2) %1 %2) args))
(getMax 1 2 3)
  ;;what are this even (compose function of last (sort (list args)))
  ((comp last sort list) 1 2 3 4)

;;Problem 39 Interleave
(def a [:a :b :c]) (def b [1 2 3])
(defn interleaveIt [colla collb]
  (loop [coll1 colla coll2 collb together []]
    (if (or (empty? coll1) (empty? coll2))
      together
      (recur (rest coll1) (rest coll2) (conj together (first coll1) (first coll2)))
    )
  ))
(interleaveIt a b)
  ;;fancy answer
  (mapcat list a b)

;;Problem 40 Interpose
(defn interposeIt [separator coll]
  (apply conj [(first coll)] (mapcat #(conj [separator] %) (rest coll))))

(mapcat #(conj [1] %) b)
(interposeIt 1 a)
  ;;and the sweet version
  (#(butlast (interleave %2 (repeat %1))) 1 a)


;;Problem 41 Drop every nth
(defn dropItLikeItsHot [coll Nth]
  (mapcat #(if (= (count %) Nth) (butlast %) %) (partition Nth Nth [] coll)))

(def a [1 2 3 4 5 6 7 8 9 10])
(dropItLikeItsHot a 2)
(partition-all 4 5 a)
  ;;herp derp
  (#(apply concat (partition-all (dec %2) %2 %1)) a 4)

;;Problem 42 Factorials
(defn factorial [x]
  (loop [x x acc 1]
    (if (= 1 x)
      acc
      (recur (dec x) (* x acc))
      )
    )
  )
(factorial 3)
  ;;darn, nice
  (#(reduce * (range 1 (inc %))) 3)

;;Problem 43 Reverse Interleave
(defn reverseInterleave [coll nsub]
  (loop [coll coll index nsub output []]
    (if (= index 0)
      output
    (recur (rest coll) (dec index) (conj output (map first (partition-all nsub nsub coll)))))))

(list (partition 2 a))
(first (first (partition-all 4 4 a)))
(map first (partition-all 4 4 a))

(reverseInterleave a 2)
  ;;so I'm learning that I have a thing for tail recursion now that I've learned it...
  ;;Hey, listen! 'map list' applies list to each value in the partition subsequences in parallel
  ;;Takeaway - map can be applied to multiple collections
  (#(apply map list (partition %2 %1)) a 2)
  (map list '(1 2 3) '(4 5 6))
  (map list '(1 4) '(2 5) '(3 6))

;;Problem 44 Rotate sequence
(defn shift [xsteps coll]
  (let [xrem (rem xsteps (count coll))]
    (if (neg? xrem)
      (concat (drop (+ xrem (count coll)) coll) (take (+ xrem (count coll)) coll))
      (concat (drop xrem coll) (take xrem coll)))))

(shift -2 a)
(concat (drop 3 a) (take 3 a))
(let [posx (rem 4 (count a))] posx)
  ;;darn mod return positive remainder
  (#(let [steps (mod %1 (count %2))]
      (concat (drop steps %2) (take steps %2))) 2 a)


;;Problem 46 Higher order functions
(= 4 (((fn reverseapply [func] #(func %2 %1) ) quot) 2 8))
  ;;more wholesome answer
  (= 4 (((fn flip [f] (fn [& args] (apply f (reverse args)))) quot) 2 8))


;;Problem 50 Split by type
(defn splitbytype [coll] (map val (group-by type coll)))
(def tst ["a" 1 "aa" :ab 2 :ad 4])
(splitbytype tst)


;;destructuring
(let [[:as d] [1 2 3 4 5]] d)
(let [[a b c] [1 2 3 4 5]] [c b a])
(let [[a b & c] [1 2 3 4 5]] (into [] c))

;;Problem 53 Longest Increasing Subsequence
(defn longsub [inputcoll]
  (loop [coll (rest inputcoll) longest [] current [(first inputcoll)]]
      (cond
        (empty? coll) longest
        (= (inc (last current)) (first coll))
          (if (and (> (inc (count current)) (count longest)) (> (inc (count current)) 1))
               (recur (rest coll) (conj current (first coll)) (conj current (first coll)))
               (recur (rest coll) longest (conj current (first coll)))
            )
        :else
        (recur (rest coll) longest [(first coll)]))
      ))

(def a [1 2 3 0 1 2 3 0 4 5])
a
(first a)
(conj [(first a)] 0)
(last [0])

(empty? ())
[nil]
(count [])
(longsub a)

;;Problem 54 Partition
(defn partitionIt [nsteps coll]
  (loop [inputcoll coll nsteps nsteps outputcoll []]
      (if (< (count (take nsteps inputcoll)) nsteps)
        outputcoll
        (recur (drop nsteps inputcoll) nsteps (conj outputcoll (take nsteps inputcoll)))
        )
      ))

(partition 4 a)
(partition 4 [1 2])
(partition 4 [1 2 3 4 5])
(take 3 [1 2])
(drop 3 a)
;;(partitionIt 4 a)
;;(partitionIt 12 a)
  ;;vlisch strikes again!
  ((fn p [acc n col]
  (if (< (count col) n)
    acc
    (p (conj acc (take n col)) n (drop n col)))) [] 4 a)

;;Problem 55 Frequencies
(defn frequencyIt [coll]
  (let [distinctKeys (group-by identity coll)]
  (zipmap (keys distinctKeys) (map #(count %) (vals distinctKeys)))))

(group-by identity a)
(frequencyIt a)
  ;;magical! creates maps out of each entry and then merges by adding
  (reduce #(merge-with + % {%2 1}) {} a)


;;Problem 56 Distinct
;; 1) throw it into set O(n)
;; 2) iterate through coll for order O(n)

(defn distinctIt [coll]
  (loop [remaining coll output []]
    (if (empty? remaining) output
      (recur (filter #(not= % (first remaining)) remaining) (conj output (first remaining))))))

(distinctIt a)
  ;;notable: lischenk passes in a predicate function to remove - essentially filter
  ((fn f [acc col] (
        if (empty? col)
        acc
        (let [elem (first col), r (fn [x] (= elem x))]
          (f (conj acc elem) (remove r col))
        )
    )) [] a)

;;Problem 58 Function Composition
(defn compIt2 [& funcs]
  (fn [& args]
    (loop [funcsRemaining funcs input args]
    (if (empty? funcsRemaining)
      (first input)
        (if (= (count input) 1)
          (recur (drop-last funcsRemaining) [(apply (last funcsRemaining) input)])
          (recur (drop-last funcsRemaining) [(reduce (last funcsRemaining) input)])
          )))))
    ;;CompIt 2 passes, but erg, I'm not sure how robust this is lol

((compIt2 rest reverse) [1 2 3 4])
((compIt2 +) 1 2 3)
((compIt2 zero? #(mod % 8) +) 3 5 7 9)

(identity [123 2432])
((fn [args] (identity args)) [1 2 34])

    ;;This is the answer I was trying to make originally
    (defn aPlus [& funcs] (fn [& args] (reduce #(%2 %1) (apply (last funcs) args) (rest (reverse funcs)))))
    ((aPlus zero? #(mod % 8) +) 3 5 7 9)
    ((aPlus rest reverse) [1 2 3 4])

;;Problem 59 Juxtaposition
(defn juxta [& funcs] (fn [& args] (reduce #(conj %1 (apply %2 args))  [(apply (first funcs) args)] (rest funcs))))

(def funcs [+ max min])
(def args [ 1 2 3 4 5 6])
[(apply (last funcs) args)]

((juxta + max min) 2 3 5 1 6 4)
(= [21 6 1] ((juxta + max min) 2 3 5 1 6 4))

  ;;good solution 1
  (((fn myJuxt [& fs]
    (fn [& args]
      (for [f fs]
        (apply f args)))) + max min) 2 3 5 1 6 4)
  ;;good solution 2 - maps do make more sense here
  (((fn [& fs] (fn [& a] (map #(apply % a) fs))) + max min) 2 3 5 1 6 4)

;;Problem 60! Sequence reductions
;;concept: use variadic args to act as case?
(defn reducelazy
  ([func acc restargs]
   (reducelazy func (cons acc restargs)))
  ([func args]
    (lazy-seq
      (cons (first args)
        ((fn step [accumulator remaining]
          (when (not (empty? remaining))
            (lazy-seq
              (cons (func accumulator (first remaining))
                    (step (func accumulator (first remaining)) (rest remaining))))))
        (first args) (rest args))))))


(take 5 (reducelazy + (range)))
(reducelazy conj [1] [2 3 4])
(= (last (reducelazy * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120)

(reducelazy / (range 10 20))
(reductions / (range 10 20))


(concat [[1]] [2] [3])
(lazy-cat [1] [2] [2])

(range)
;;(= (take 5 (__ + (range))) [0 1 3 6 10])

  ;;lischenk, majestic as always
  ((fn my-reductions
    ([f col] (my-reductions f (first col) (rest col)))
    ([f init col]
         (cons
          init
          (if (empty? col)
            nil
            (lazy-seq (my-reductions f (f init (first col)) (rest col))))))
  )conj [1] [2 3 4])

;;WOOOHOO Problem 60 finished almost 2 weeks later haha



;;Problem 61 Map Construction
(defn zipmapIt
  [vkeys vvals]
  (reduce merge (map #(hash-map %1 %2) vkeys vvals)))

(def a1 [1 2 3 4 5 6])
(def b1 [\a \b \c \d \e \f])
(def mapa {:3 4 :5 6})
(def mapb {\y \z})

(merge mapa mapb)
(zipmapIt a1 b1)

  ;;sweet diggity forgot about interleave
  (#(apply hash-map (interleave %1 %2)) a1 b1)

;;Problem 62 Iterate
(defn iterateIt
  [func initval]
  (lazy-seq (cons initval (iterateIt func (func initval)))))

(take 5 (iterateIt inc 0))
(take 5 (iterateIt #(* 2 %) 1))

;;Problem 63 Group Sequence
(defn groupBy
  [filterBy args]
  (apply merge-with concat (map #(hash-map %1 [%2]) (map filterBy args) args)))

(def a [1 2 3 4 5 6 7 8 9 10])
(defn func [x] (> x 5))
(func 1)
(apply merge-with concat (map #(hash-map %1 [%2]) (map func a) a))
(groupBy #(> % 5) [1 3 6 8])

;;Problem 65 Black Box Testing
;;what makes each sequence type unique/ what constraints do they have?

(defn type?
  [sequ]
  (cond
    (reversible? sequ) :vector
    (associative? sequ) :map
    (= (count (conj sequ 1 1)) (+ 2 (count sequ))) :list
    :else :set
        ))


(def seta #{10 2})
(def mapa {:a 1, :b 2})
(def lista '(1 2 3 4 ))
(def veca [\a \b \c \d])

(count (flatten (seq mapa)))
(flatten (seq seta))
(flatten (seq lista))
(conj mapa {2 1})
(conj seta {2 1})
(conj seta 1 1)

(count (conj '() 1 1))
(+ 2 (count '()))

(map type? [{} #{} [] ()])

  ;;hurdur
  (fn [a]
  (let [base (empty a)]
    (cond
     (= base {}) :map
     (= base #{}) :set
     (= base '()) (if (reversible? a) :vector :list))))


;;Problem 66 GCD
;; 1) my dumb solution O(n) is to divide by the smaller number and iterate down from there
;; 2) aint nobody got time for prime trees
;; 3) then I find there's the Euclidean Algorithm, which I'm pretty sure I learned in CS277 or s/t

;; To find GCD(A, B)
;; If A = 0 then GCD = B
;; If B = 0 then GCD = A
;; Else Let A in quotient-remainder form be A = B*Q + R
;; Find GCD(B, R) given GCD(A, B) = GCD(B, R)

(defn GCD [A B]
  (loop [A A B B]
    (cond
      (= 0 A) B
      (= 0 B) A
      :else (let [R (rem A B)]
              (recur B R)))))

(GCD 270 192)
(rem 858 1023)

  ;;lischenk's great answer
  ((fn gcd [x y]
  (let [a (min x y) b (max x y) z (mod b a)]
    (if (= 0 z) a (gcd z a)))) 270 192)

;;Problem 67 First N Primes
(defn nthPrime [n]
  (loop [primes [2 3] current 5]
    (cond
      (= n (count primes)) primes
      (some #(= 0 (mod current %)) (rest primes)) (recur primes (+ 2 current))
      :else (recur (conj primes current) (+ 2 current))
      )))

(nthPrime 5)

;;Problem 69 Merge with a Function
;; reduce on f(x) for map args
;; f(x) is a reduce on the entries with g(x)
;; g(x) returns (inputfunc vala valb) if vala/valb exist, else just returns exists? vala/valb

(defn mergewith [func & maps]
  (letfn [ (getVal [vala valb]
                  (cond
                    (nil? vala) valb
                    (nil? valb) vala
                    :else (func vala valb)))
           (mergeMaps [mapa mapb]
                      (let [ mapkeys (keys (merge mapa mapb))
                             mapvals (map #(getVal (mapa %) (mapb %)) mapkeys)]
                     (zipmap mapkeys mapvals)))]
    (reduce mergeMaps maps)))

(def maptesta {:a 2 :b 3 :c 4})
(def maptestb {:a 3 :d 5 :e 6})
(def maptestc {:a 4 :f 7 :g 8})
(def maptestd {1 4, 2 7, 3 8})
(:z maptesta)
(maptestd 1)
(map #(+ (val %1) (val %2) (val %3)) maptesta maptestb maptestc)

(mergewith + maptesta maptestb maptestc)

  ;;this one is pretty sweet
  ((fn [f & a]
  (into {}
    (map
      (fn [k] [k (reduce f (keep #(% k) a))]) ;;create entries - only reduce on vals that exist in all maps for each key
      (keys (apply merge a))))) + maptesta maptestb maptestc)

(into {} '([:a 1] [:b 2]))

;;Problem 70 Word Sorting
(defn sortSplit [string] (into [] (sort-by #(.toLowerCase %) (.split #"[\s\W]+" string))))

(require '[clojure.string :as str])

(def teststr "Have a nice day.")
(sort-by #(str/lower-case %) (str/split teststr #"\s"))
  ;; k...so apparently str/ lib is not allowed
  ;; new approach: create vector pairs of the lowercase word and it's real form. Then apply sort-by to that
  ;; to split the words, partition by spaces and then sort-by #(mod 32 (compare %1 %2)
(apply str (interpose "." (.split #"," "Hello,How,Are,You,Today")))
(compare "Ab" "Ba")
(compare "Aa" "Ab")
(compare "A" "a")
(mod 32 (compare "A" "b"))

(sortSplit teststr)
(seq (.split #"[\s\W]+" teststr))
  ;;I like think one though - replaces all non alphanumber/space (eliminate punctuation)
  ((fn [x] (sort-by #(.toLowerCase %) (.split (.replaceAll x "[^a-zA-Z ]" "") " "))) teststr)

;;Problem 73 - Tic Tac Toe
(defn whoWon? [board]
  (let [winningCombinations [[0 1 2] [3 4 5] [6 7 8]
                             [0 3 6] [1 4 7] [2 5 8]
                             [0 4 8] [2 4 6]]
        board (into [] (flatten board))]
    (letfn [(winningCombo? [coll]
                           (if (and (= (count (distinct coll)) 1 ) (not= (first coll) :e))
                             (first coll)
                             nil))]
        (let [winner (keep winningCombo? (map (fn [combo] (map #(get board %) combo)) winningCombinations))]
          (if (empty? winner)
            nil
            (first winner)
          )))))

(def testgame [[:x :e :e]
               [:o :x :e]
               [:o :e :x]])

(def testgame2 [[:x :o :x]
            [:x :o :x]
            [:o :x :o]])

(whoWon? testgame)
(whoWon? testgame2)

(defn winningCombo? [coll]
                           (if (and (= (count (distinct coll)) 1 ) (not= (first coll) :e))
                             (first coll)
                             nil))
(winningCombo? '(:c :c :c))

;;Problem 74 Filter Perfect Squares
;; my dumb way is to create a list of squares up until the last square is greater than the greatest in the list
;; then
(defn filterPerfectSquares [inputStr]
  (let [nums (map #(Integer. %) (seq (.split #"\D" inputStr)))
        maxNum (apply max nums)
        squares (loop [base 2 sqrs [1]]
                  (if (> (* base base) maxNum)
                    sqrs
                  (recur (inc base) (conj sqrs (* base base)))))]
    (clojure.string/join "," (map str (filter (fn square? [x] (some #(= x %) squares)) nums)))
    ))


(def teststr "4,5,6,7,8,9,12,16")
(filterPerfectSquares teststr)

;; Ooo java interop http://clojure.org/reference/java_interop
(. Integer parseInt "12")

  ;; Yea, this is pretty nice
  ((fn [strs]
  (apply str
    (interpose ","
      (filter
        #(= 0.0 (mod (Math/sqrt %) 1))
        (map #(Integer/parseInt %) (.split strs ",")))))) teststr)


;;Problem 75 Euler's Totient Function(?)
;; reusing the nthPrime solution from above
(defn totient [x]
  (let [potentialPrimes ((fn primesUpToHalf [n]
            (loop [primes [2 3] current 5]
              (cond
                (> current (/ n 2)) primes
                (some #(= 0 (mod current %)) (rest primes)) (recur primes (+ 2 current))
                :else (recur (conj primes current) (+ 2 current))
                ))) x)]
    (if
      (= 1 x) 1
      (count (filter #(not-any? (fn [prime] (and (zero? (mod x prime)) (zero? (mod % prime)))) potentialPrimes) (range 1 x))))
    ))


(totient 1)
(totient 10)
(totient 2)
;;   (filter #(= (mod x %) 1)
(def testx 10)
(def prime 2)
(and (= 0 (mod testx prime)) (= 0 (mod 3 prime)))

  ;;aaand bootiful
  ((letfn [(gcd [a b] (if (= b 0) a (gcd b (mod a b)) ))]
     (fn [x] (count (filter #(= 1 (gcd x %)) (range x))))) 10)

;;Problem 77 Anagram Finder
;; Map<Word, Map a <Char, Count>> -> Create Sets of words based on map equality
;; a -> group letter in word by identity

(defn anagramFinder [words]
  (letfn [(chargroup [word] (group-by identity word))
          (charmap [word] (zipmap (keys (chargroup word)) (map #(count (val %)) (chargroup word))))
          (wordcharmap [coll] (zipmap coll (map charmap coll)))
          (wordcharmapgroup [coll] (group-by #(get (wordcharmap coll) %) coll))
          (wordsets [wordcmg] (map #(into #{} (val %)) (wordcharmapgroup wordcmg)))]
    (into #{} (filter #(< 1 (count %)) (wordsets words)))))


(def testvec ["meat" "mat" "team" "mate" "eat"])
(defn charvec [word] (group-by identity word))
(charvec "meeeet")
  ;;hmmm, zipmap is actually shorter here
(defn charmap1 [word] (zipmap (keys (charvec word)) (map #(count (val %)) (charvec word))))
(charmap1 "meeet")
(defn charmap2 [word] (reduce-kv (fn getcount [m k v] (assoc m k (count v))) {} (charvec word)))
(charmap2 "meeet")

(defn wordcharmap [coll] (zipmap coll (map charmap1 coll)))
(wordcharmap testvec)
(def wordcharmapgroups (group-by #(get (wordcharmap testvec) %) testvec))
wordcharmapgroups

(def wordsets (map #(into #{} (val %)) wordcharmapgroups))
wordsets
(into #{} (filter #(< 1 (count %)) wordsets))

(anagramFinder testvec)

  ;;ermahger
  ((fn [strings];anagrams have the same histogram
  (reduce merge #{};prepare the result
          (map set (remove #(= 1 (count %));remove the words without any anagrams
                           (vals (group-by frequencies strings))))));frequencies to build the histogram
   testvec)

  ;;even better
  (#(->> %
       (group-by sort)
       vals
       (filter (fn [s] (< 1 (count s))))
       (map set)
       set)
    testvec)
(sort (first testvec)) ;;the genius is here
(group-by sort testvec)

;;Problem 78 Reimplement Trampoline
(defn trampolineIt [func & args]
    (loop [result (apply func args)]
      (if (not (fn? result))
        result
        (recur (result)))))

(defn stop? [x] (if (> x 50) x #(triple x)))
(defn sub-two [x] #(stop?(- x 2)))
(defn triple [x] #(sub-two (* 3 x)))

(#(+ 4 3))
(trampolineIt triple 2)

;;Problem 79 Triangle Minimal Path
;; Ooo topics: graph-theory. Sounds like a graph traversal problem
;; My gut tells me to enumerate all possible paths like a binary tree and find it that way lol

(defn minPath [triangle]
  (letfn [(allPaths [acc index remainder]
    (if (empty? remainder)
      acc
      (+ acc (min (allPaths (get (first remainder) index) index (rest remainder)) (allPaths (get (first remainder) (inc index)) (inc index) (rest remainder))))))]
  (allPaths (first (first triangle)) 0 (rest triangle))))

 (def test '([3]
           [2 4]
          [1 9 3]
         [9 9 2 4]
        [4 6 6 7 8]
       [5 7 3 5 1 4])) ; 3->4->3->2->7->1
(minPath test)
;;   a node can only access it's same index or index + 1
;;   collapse each layer into the lower one like a binary tree -end with 2^level possibilities
;;  0           [0]                    0            2^0
;;  1          [0 1]              0         1       2^1
;;  2         [0 1 2]            0 1       1 2      2^2
;;  3        [0 1 2 3]         0 1 1 1   2 2 2 3    2^3
;;  4       [0 1 2 3 4]      01 12 23 34            2^4
;;  5      [0 1 2 3 4 5]

  ;;lischenko does it again!
  ((fn m [[[r] & t]]
  (if (nil? r) 0
      (+ r (min
            (m (map rest t))
            (m (map butlast t)))))) test)

;;Problem 80 Perfect Numbers
(defn perfect? [x]
  (= x (apply + (filter #(integer? (/ x %)) (range 1 (inc (/ x 2)))))))

(perfect? 6)
(perfect? 7)
(perfect? 496)
(perfect? 500)



;; Problem 81 Intersection
(defn intersectionIt [s1 s2]
  (into #{} (filter #(contains? s1 %) s2)))

(intersectionIt #{0 1 2 3} #{2 3 4 5})

;;Problem 82 Word Chains
;; returns true if they can be arranged into one continous word chain, and false if they cannot
;; one way to think of this might be making a map of dependencies

;; I admit my solution isn't wholly correct :P It would be better if I ran a comparison on all neighbors in
;; every permutation of words.
(defn chain? [coll]
  (letfn [(neighbors? [a b]
    (loop [a a b b diffs 0]
      (cond (> diffs 1) false
            (= a b) true
            (= (first a) (first b)) (recur (rest a) (rest b) diffs)
            (= (count a) (count b)) (recur (rest a) (rest b) (inc diffs))
            (> (count a) (count b)) (recur (rest a) b (inc diffs))
            :else (recur a (rest b) (inc diffs)))))]
    (let [neighborlists (zipmap coll (map (fn [x] (filter #(neighbors? % x) (remove #(= x %) coll))) coll))]
      (>= 2 (count (filter #(= 1 (count (val %))) neighborlists))))))

(def tst ["spout" "do" "pot" "pout" "spot" "dot"])
(def tst2 #{"share" "hares" "shares" "hare" "are"})
(group-by sort tst)
(sort (first tst2))

(defn neighbors? [a b]
  (loop [a a b b diffs 0]
    (cond (> diffs 1) false
          (= a b) true
          (= (first a) (first b)) (recur (rest a) (rest b) diffs)
          (= (count a) (count b)) (recur (rest a) (rest b) (inc diffs))
          (> (count a) (count b)) (recur (rest a) b (inc diffs))
          :else (recur a (rest b) (inc diffs)))))

(neighbors? "spout" "shout")
(neighbors? "spout" "pout")
(neighbors? "spout" "spouv")
(neighbors? "spout" "spot")
(neighbors? "spout" "shot")

;;it is a set!
(def neighborlists (zipmap tst (map (fn [x] (filter #(neighbors? % x) (remove #(= x %) tst))) tst)))
neighborlists
;;starting with the shortest list, start removing dependencies
(sort-by #(count (val %)) neighborlists)

(<= 1 (count (filter #(= 1 (count (val %))) neighborlists)))

(def tst3 #{"cot" "hot" "bat" "fat"})
(chain? tst3)
(chain? tst2)

;; [spout [pout spot], do [dot], pot [pout dot], pout [spout, pot], spot [spout pot], dot [do pot]

;;   Perhaps the success criterion should be if max two words have one dependency - the "HEAD" and "TAIL" so to say
;;   esp. given we have sets as inputs, and in a chain, all but one or two words should have neigbors

  ;;vlisch - this is more right than mine, anyway :P
  ;; there's also a good explanation/example by meerwolf
  #(letfn [(is-mutation?
             ([f xy] (is-mutation? (map (fn [w] (apply str (f w))) xy)))
             ([[x y :as xy]]
                (if (= (first x) (first y))
                  (is-mutation? rest xy)  ; trim left
                  (if (= (last x) (last y))
                    (is-mutation? butlast xy) ; trim right
                    (= 1 (count (last (sort-by count xy)))) ; longest chunk is 1 letter long
                    ))))

           (valid-chain? [c]
              (every? is-mutation? (zipmap (butlast c) (rest c))))

           (permut [[h & t]]
              (if (nil? t) [[h]]
                 (for [p (permut t)
                       i (range (inc (count p)))
                       :let [s (split-at i p)]]
                   (lazy-cat (first s) [h] (second s)))))

           (find-chains [s]
              (->> s vec permut (filter valid-chain?)))]

     (->> % find-chains empty? not))


;;Problem 83 Some, but not the whole, truth
(defn SBNTWT [& args]
  (and (not (every? true? args)) (boolean (some true? args)))
  )

(def tst '(false false))
(and true (some true? tst))

;;Problem 84 Transitive Closure
(defn transitiveClosure [coll]
  (letfn [(getNext [[a b] pairset]
                   (let [transitiveRel (filter #(= b (first %)) pairset)]
                     (if
                        (empty? transitiveRel) [a b]
                        [a (second (first transitiveRel))])))]

  (loop [result coll remaining coll]
    (if
      (empty? remaining) result
      (let [current (first remaining)
            findattempt (getNext current result)] ;;try to find next within results
        (if
          (= current findattempt) (recur result (rest remaining)) ;;end of the line
          (recur (into #{findattempt} result) (into #{findattempt} (rest remaining))))
          ))
      ))
    )

(def tst #{[8 4] [9 3] [4 2] [27 9]})

(into #{} (cons [2 3] #{[2 3]}))

(first tst)
(transitiveClosure tst)
(into #{[2 1]} #{[2 3]})

  ;;amazingly simple
  (#(loop [s %]
   (let [n (into s
                 (for [[a b] s [c d] s
                       :when (= b c)]
                   [a d]))]
      (if (= n s) n (recur n)))) tst)

;;Problem 85 Power Sets [TODO]

;;so this works, but it takes too long (times out) on 4clojure =/
;; need to reimplement in 2^n instead of n!
(defn powerSetblah [coll]
   (let [subset (mapcat (fn addSubSet [x] (powerSet (into #{} (remove #(= x %) coll)))) coll)]
  (set (concat #{coll} subset))))

;;bitmap - this should be 2^n, but it's not very pretty
(defn powerSetBM [c]
  (if (zero? (count c)) #{#{}}
  (let [ coll (vec c)
         size (count coll)
         card (reduce #(* %2 %) (repeat size 2)) ;;cardinality
         bitmap (vec (repeat size 0))]
    (letfn [(addone [bits] (loop [i 0 bitvec bits ]
                                 (if (zero? (bitvec i)) (assoc bitvec i 1)
                                   (recur (inc i) (assoc bitvec i 0)))))
            (getCombo [bm] (set (reduce #(if (zero? (bm %2)) %
                                           (cons (coll %2) %)) #{} (range 0 size))))]
     (loop [i 0 bm bitmap agg #{}]
       (if (= i (dec card)) (conj agg (getCombo bm))
         (recur (inc i) (addone bm) (conj agg (getCombo bm))))))
      )))

;;LEGIT idiomatic solution (so bootiful, I am wowed)
(defn powerSet [s] (reduce (fn [agg cur] (into agg (map #(conj % cur) agg))) #{#{}} s))

(def tst #{1 2 3 4})
(powerSet tst)

(powerSet #{:a})
(powerSet #{1 :a})
(= (count (powerSet (into #{} (range 10)))) 1024)


;;(map #(conj % (first tst)))

;;Problem 86 Happy Numbers
(defn happynumber? [n]
  (loop [seen #{n} current n]
    (let [digitarray (map #(Character/digit % 10) (str current))
          sum (reduce #(+ % (* %2 %2)) 0 digitarray)]
      (cond
        (= 1 sum) true
        (contains? seen sum) false
        :else (recur (into seen #{sum}) sum)))))

(happynumber? 2)
(happynumber? 7)
(happynumber? 986543210)

;;Problem 87 Symmetric Difference
(defn symmetricDiff [seta setb]
  (clojure.set/union (clojure.set/difference seta setb) (clojure.set/difference seta setb)))
(symmetricDiff #{1 2 3 4 5 6} #{1 3 5 7})

;;Problem 90 Cartesian Theory
(defn cartesianproduct [seta setb]
  (set (for [x seta y setb] [x y])))

(cartesianproduct #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})


;;Problem 93 Partially Flatten
(defn parflat [coll]
  (mapcat #(if (coll? (first %)) (parflat %) [%]) coll))

(parflat [[[[:a :b]]] [[:c :d]] [:e :f]])

;;Problem 94 Game of Life
;; The 'board' consists of both live (#) and dead ( ) cells. Each cell interacts with its eight neighbours (horizontal, vertical, diagonal), and its next state is dependent on the following rules:

;; 1) Any live cell with fewer than two live neighbours dies, as if caused by under-population.
;; 2) Any live cell with two or three live neighbours lives on to the next generation.
;; 3) Any live cell with more than three live neighbours dies, as if by overcrowding.
;; 4) Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

(defn gameoflife [b]
  (let [size (count b)
        board (vec (map vec b))]
    (letfn [ (withinbounds? [x] (not-any? #(or (< (dec size) %) (neg? %)) x))
             (neighborindices [col row] (filter withinbounds? (conj [] [(dec col) (dec row)] [col (dec row)] [(inc col) (dec row)]
                                     [(dec col) row] [(inc col) row]
                                     [(dec col) (inc row)] [col (inc row)] [(inc col) (inc row)])))
             (getcoord [[col row]] ((board row) col))
             (numliveneighbors [col row] (count (filter #(= \# %) (map getcoord (neighborindices col row)))))
             (nextform [col row] (let [n (numliveneighbors col row)
                                       this (getcoord [col row])]
                                   (if (= this \#)
                                     (if (or (= 2 n) (= 3 n)) \# \space)
                                     (if (= 3 n) \# \space))))
             (nextgen [] (map-indexed (fn [y row] (apply str (map-indexed (fn [x col] (nextform x y)) row))) board))]
          (vec (nextgen))
      )))

(def gol ["     "
          "     "
          " ### "
          "     "
          "     "])
(gameoflife gol)


;;Problem 98 Equivalence Classes
(defn equivClasses [f D]
  (set (map #(set (val %)) (group-by f D))))

(equivClasses #(rem % 3) #{0 1 2 3 4 5})
(equivClasses #(* % %) #{-2 -1 0 1 2})

;;Problem 99 Product Digits
(defn prodDigs [a b]
  (let [c (* a b)]
    (loop [cur c agg '()]
      (if (< cur 10) (vec (cons (rem cur 10) agg))
      (recur (quot cur 10) (cons (rem cur 10) agg))))))

(prodDigs 1 1)
(prodDigs 99 9)

(quot (quot 1232131 10) 10)

;;Problem 100 LCM
(defn lcm [& a]
  (loop [agg (zipmap a a)]
    (let [sums (vals agg)
          maximum (apply max sums)]
    (if (every? #(= maximum %) sums) maximum
      (recur (zipmap (keys agg) (map
               (fn incr [x] (if (< (val x) maximum) (+ (key x) (val x)) (val x)))
               agg)))))))

(lcm 2 3 4)

  ;;adereth's Euclidean answer
  ((fn [& x]
     (let
       [gcd (fn gcd [a b] (if (= 0 b) a (gcd b (mod a b))))
        lcm (fn lcm [a b] (/ (* a b) (gcd a b)))]
     (reduce lcm x))) 2 3 4)

;;Problem 107 Simple Closure
(defn powerUp [n]
  (fn [x] (apply * (repeat n x))))

((powerUp 2) 6)

;;Problem 108 Lazy Searching
(defn findMin [& seqs]
  (loop [colls seqs]
    (if (some empty? colls) nil
      (let [firsts (map first colls)
            maximum (apply max firsts)]
        (if (every? #(= maximum %) firsts) maximum
          (recur (map (fn [coll] (drop-while #(< % maximum) coll)) colls))))
      )))
(findMin [1 2 3 4 5] [3 4 5 6] [8])

(def testseqs '([2 2 3 4 5] [2 3 4 5 6] [2 3 4 5]))
(def testa [2 2 2])
(every? #(= (apply max testa) % ) testa)
(apply min [3 4 5 1 3])

;;Problem 132 Insert between two items
(defn insertBetween [pred sep coll]
  (if (empty? coll) coll
    ((fn step [frst more]
       (if (empty? more) [frst]
         (let [[nxt & rst] more]
           (cons
             frst
             (if (pred frst nxt) (cons sep (lazy-seq (step nxt rst)))
             (lazy-seq (step nxt rst)))
            )))) (first coll) (rest coll))))


(insertBetween < :less [1 6 7 4 3])
(insertBetween > :more [1])

  ;;_pcl does it again, combining lazy-cat and mapcatting against coll and (rest coll)
  ((fn [p v xs]
    (mapcat
      #(if (p %1 %2) [%1 v] [%1])
      xs
      (lazy-cat (rest xs) [1]))) < :less [1 6 7 4 3])

;;Problem 134 Nil key
(defn nilkey? [k m]
  (and (contains? m k) (nil? (m k))))

;;Problem 135 Infix Calculator
(defn infixcalc [a f b & things]
  (loop [acc (f a b) rst things]
    (if (empty? rst) acc
    (recur ((first rst) acc (second rst)) (drop 2 rst))
      )))

(infixcalc 38 + 48 - 2 / 2)

  ;;the pcl solution
    ((fn [& xs]
    (reduce #((first %2) %1 (last %2)) (first xs) (partition 2 (rest xs)))) 38 + 48 - 2 / 2)

;;Problem 143 dot product
(#(reduce + (map (fn [a b ] (* a b)) % %2)) [0 1 0] [1 0 0])














