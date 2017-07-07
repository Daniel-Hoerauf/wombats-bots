(fn [state time-left]
  (def turn-directions [:right :left :about-face])
  (def smoke-directions [:forward :backward :left :right :drop])
  (def game-parameters
  {;; HP Modifiers
   :collision-hp-damage 10
   :food-hp-bonus 5
   :poison-hp-damage 10
   ;; Score Modifiers
   :food-score-bonus 10
   :wombat-hit-bonus 10
   :zakano-hit-bonus 8
   :steel-barrier-hit-bonus 2
   :wood-barrier-hit-bonus 2
   :wombat-destroyed-bonus 25
   :zakano-destroyed-bonus 15
   :wood-barrier-destroyed-bonus 3
   :steel-barrier-destroyed-bonus 25
   ;; In game parameters
   :shot-distance 5})
  (def arena-size (first (:global-dimensions state)))
  (def arena-half (/ arena-size 2))
  (def shot-range (:shot-distance game-parameters))
  
  (defn add-locs
    "Add local :x and :y coordinates to state matrix"
    [arena]
    (reduce 
      #(conj %1 
        (reduce 
          (fn [acc node] (conj acc (assoc node :x (count acc) :y (count %1))))
          [] %2))
      [] arena))
  
  (defn in?
      "Return true if coll contains elem"
      [elem coll]
      (some #(= elem %) coll))
  
  (defn filter-arena
      "Filter the arena to return only nodes that contain one of the given type"
      ([arena] (flatten arena))
      ([arena & filters]
      (let [node-list (flatten arena)]
        (filter #(in? (get-in % [:contents :type]) filters) node-list))))
  
  (defn get-direction
      "Get the current direction of your wombat from the 2d arena array"
      [arena]
      (get-in (nth (nth arena 3) 3) [:contents :orientation]))
  
  (defn facing?
      "Returns true if a move forward will bring you closer to desired location
      If no self coordinates are provided, use distance from {:x 3 :y 3}"
      ([dir node self-node]
        (case dir 
              "s" (if (>= arena-half (:y self-node))
                      (and (< (:y self-node) (:y node)) (<= (:y node) (+ (:y self-node) arena-half)))
                      (or (> (:y node) (:y self-node)) (>= (- (:y self-node) arena-half) (:y node))))
              "w" (if (<= arena-half (:x self-node))
                      (and (> (:x self-node) (:x node)) (>= (:x node) (- (:xa self-node) arena-half)))
                      (or (< (:x node) (:x self-node)) (>= (- (:x node) arena-half) (:x self-node))))
              "n" (if (<= arena-half (:y self-node))
                      (and (> (:y self-node) (:y node)) (>= (:y node) (- (:y self-node) arena-half)))
                      (or (< (:y node) (:y self-node)) (>= (- (:y node) arena-half) (:y self-node))))
              "e" (if (>= arena-half (:x self-node))
                      (and (< (:x self-node) (:x node)) (<= (:x node) (+ (:x self-node) arena-half)))
                      (or (> (:x node) (:x self-node)) (>= (- (:x self-node) arena-half) (:x node))))
              nil))
      ([dir node]
        (facing? dir node {:x 3 :y 3})))
  
  (defn distance-to-tile
      "Get the number of moves it would take to move from current location.
      If no self coordinates are provided, use distance from {:x 3 :y 3}"
      ([dir node self-node]
        (+ (Math/abs (- (:y node) (:y self-node)))
           (Math/abs (- (:x node) (:x self-node)))
           (if (facing dir node self-node) 0 1)))
      ([dir node]
        (distance-to-tile dir node {:x 3 :y 3})))

  (defn turn-to-dir
      "Returns one of [:right :left :about-face]"
      [curr-dir next-dir]
      (def ^:private orientations ["n" "e" "s" "w"])
      (let [curr-idx (.indexOf orientations curr-dir)
            next-idx (.indexOf orientations next-dir)]
            (case (mod (- curr-idx next-idx) 4)
                0  nil
                1  :left
                2  :about-face
                3  :right)))
  
  (defn can-shoot?
      "Returns true if there is a barrier, Zakano, or Wombat within shooting range"
      ([dir arena self]
        (def shootable (case dir
            "n" #(and (= (:x self) (:x %)) (>= shot-range (mod (- (:y self) (:y %)) arena-size)))
            "e" #(and (= (:y self) (:y %)) (>= shot-range (mod (- (:x %) (:x self)) arena-size)))
            "s" #(and (= (:x self) (:x %)) (>= shot-range (mod (- (:y %) (:y self)) arena-size)))
            "w" #(and (= (:y self) (:y %)) (>= shot-range (mod (- (:x self) (:x %)) arena-size)))
            #(false)))
        (let [shootable
              (filter shootable (filter-arena arena "wood-barrier" "steel-barrier" "zakano" "wombat"))]
            (not (empty? (filter #(not (and (= (:x %) (:x self)) (= (:y self) (:y %)))) shootable)))))
      ([dir arena] (can-shoot? dir arena {:x 3 :y 3})))
  
  (defn build-resp
      "Helper method to construct the return command"
      ([action direction]
        {:action (keyword action)
         :metadata {:direction (keyword direction)}})
      ([action] {:action (keyword action)
                 :metadata {}}))
  
  (defn possible-points
      "Get all locations with possible points"
      ([arena self]
        (filter #(not (and (= (:x %) (:x self)) (= (:y %) (:y self))))
                (filter-arena (add-locs arena) 
                               "food" "wood-barrier" "steel-barrier" "zakano" "wombat")))
      ([arena]
        (possible-points arena {:x 3 :y 3})))
    
  (defn front-tile
      "Returns a map containing {:x x, :y y}, where x and y are the coordinated directly in front"
      ([dir self]
        (case dir
          "n" {:x (:x self) :y (mod (dec (:y self)) 20)}
          "e" {:x (mod (inc (:x self)) 20) :y (:y self)}
          "s" {:x (:x self) :y (mod (inc (:y self)) 20)}
          "w" {:x (mod (dec (:x self)) 20) :y (:y self)}))
      ([dir] front-tile dir {:x 3 :y 3}))
  
  (defn is-clear?
      "Return true if you can move forward without a collision or poison"
      [arena {x :x y :y}]
      (not (in? (get-in (nth (nth arena y) x) [:contents :type])
                ["zakano" "wombat" "wood-barrier" "steel-barrier" "poison"])))
      
  (defn new-direction
      "Pick new direction to turn"
      [dir loc self]
      (def ^:private orientations ["n" "e" "s" "w"])
      (let [available (remove #(= % dir) orientations)
            positions (filter #(facing? % loc self) available)]
            (if (not (empty? positions))
                (turn-to-dir dir (first positions))
                ; TODO improve this logic
                :left)))

  (defn move-to
      "Take the best action to get to given space"
      ([arena dir loc self]
        (def ^:private orientations ["n" "e" "s" "w"])
        (if (and (facing? dir loc self) (is-clear? arena (front-tile dir self)))
            (build-resp :move)
            (build-resp :turn (new-direction dir loc self))))
      ([arena dir loc]
        (move-to dir arena loc {:x 3 :y 3})))
    
  (defn select-target
      "Pulls the coordinates of the closest point source to the player"
      ([arena self]
        (let [possible (possible-points arena self)
              direction (get-direction arena)]
             (first (sort-by :dist (map #(assoc % :dist (distance-to-tile direction % self)) possible)))))
      ([arena] (select-target arena {:x 3 :y 3})))
    
  (defn pick-move
      "Select the move to give the highest amount of points"
      [arena self]
      (if (can-shoot? (get-direction arena) (add-locs arena))
          (build-resp :shoot)
          (move-to arena (get-direction arena) (select-target arena self) self)))
  
  (let [command-options [(repeat 0 {:action :move
                                     :metadata {}})
                         (repeat 1 {:action :turn
                                    :metadata {:direction (rand-nth turn-directions)}})
                         (repeat 0 {:action :shoot
                                      :metadata {}})
                         (repeat 0 {:action :smoke
                                    :metadata {:direction (rand-nth smoke-directions)}})]]

    {:command (pick-move (:arena state) {:x 3 :y 3})
     :state {:move (pick-move (:arena state) {:x 3 :y 3})
             :direction (get-direction (:arena state))
             :shootable (can-shoot? (get-direction (:arena state)) (add-locs (:arena state)))
             :distance (distance-to-tile (get-direction (:arena state)) {:x 4 :y 3})}}))