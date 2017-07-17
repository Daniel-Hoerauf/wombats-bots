(fn [state time-left]
  (def arena-size (get-arena-size state))
  (def arena-half (/ arena-size 2))
  (def shot-range (:shot-distance game-parameters))
  
  (defn pick-move
      "Select the move to give the highest amount of points"
      [arena self]
      (if (can-shoot-enemy? (get-direction arena) (add-locs arena) arena-size shot-range)
          (build-resp :shoot)
          (if (empty? (filter-arena (focus-sight arena) "food"))
              (if (can-shoot-barrier? (get-direction arena) (add-locs arena) arena-size shot-range)
                  (build-resp :shoot)
                  (move-to arena arena-half (get-direction arena) (select-target arena arena-half self) self))
              (move-to arena arena-half (get-direction arena) (select-target-nowall arena arena-half self) self))))

    {:command (pick-move (:arena state) {:x 3 :y 3})
     :state {:move (pick-move (:arena state) {:x 3 :y 3})
             :saved (merge-global-state (get-global-state state :saved-state :saved) state arena-size)
             :direction (get-direction (:arena state))
             :shootable (can-shoot-enemy? (get-direction (:arena state)) (add-locs (:arena state)) arena-size shot-range)
             :distance (distance-to-tile (get-direction (:arena state)) {:x 4 :y 3} arena-half)}})
             
