(fn [state time-left]
  (def arena-size (get-arena-size state))
  
  (defn pick-move
      "Select the move to give the highest amount of points"
      [arena self]
      (if (can-shoot (get-direction arena) arena arena-size :wall true)
          (build-resp :shoot)
          (if (empty? (filter-arena (focus-sight arena) "food"))
              (if (can-shoot (get-direction arena) arena arena-size)
                  (build-resp :shoot)
                  (move-to arena arena-size (get-direction arena) (select-target arena arena-size :wombat self) self))
              (move-to arena arena-size (get-direction arena) (select-target arena arena-size :wombat self :wall false) self))))

    {:command (pick-move (:arena state) {:x 3 :y 3})
     :state {}})