(ns backgammon.core
  (:require [reagent.core :refer [atom render]]
            [backgammon.hotsync :as hotsync]))

(enable-console-print!)

(hotsync/connect! (str "https://" "backgammon" "." "fire" "base" "io.com"))

(def game-state* (atom {}))

(def state* (atom {:game-id nil
                   :player-id nil}))

;; -------------------------
;; Components
(defn tri-down
  [w h x y fill]
  [:polygon {:points (str "0,0 "
                          w ",0 "
                          (/ w 2) "," h)
             :transform (str "translate(" x " " y ")")
             :fill fill}])

(defn tri-up
  [w h x y fill]
  [:polygon {:points (str "0,0 "
                          (/ w 2) "," (- h) " "
                          w ",0")
             :transform (str "translate(" x " " y ")")
             :fill fill}])

(defn checker
  [r x y color]
  [:circle {:r (- r 0.5) :cx x :cy y
            :fill (if (= :black color) "#222" "#eee")
            :stroke "#888"}])

(defn get-xy
  [w h pad checker-d i j]
  (if (< i 12)
    {:x (+ (if (> i 5) (- checker-d) 0)
           (- w (* 2 pad) checker-d (/ checker-d 2) (* i checker-d)))
     :y (- h pad (/ checker-d 2) (* j checker-d))}
    {:x (+ (if (> i 17) checker-d 0)
           (+ (* 2 pad) checker-d (/ checker-d 2) (* (- i 12) checker-d)))
     :y (+ pad (/ checker-d 2) (* j checker-d))}))

(defn- get-click-xy
  [e]
  (let [rect (-> e .-currentTarget .getBoundingClientRect)]
    [(- (.-clientX e) (.-left rect))
     (- (.-clientY e) (.-top rect))]))

(defn- xy->n
  [x y pad checker-d]
  (cond
    (< (+ checker-d (* 2 pad)) x (+ (* 7 checker-d) (* 2 pad)))
    (if (> y (+ pad (* 6.5 checker-d)))
      (Math/ceil (- 12 (/ (- x (+ checker-d (* 2 pad))) checker-d)))
      (Math/ceil (+ 12 (/ (- x (+ checker-d (* 2 pad))) checker-d))))
    (< (+ (* 8 checker-d) (* 2 pad)) x (+ (* 14 checker-d) (* 2 pad)))
    (if (> y (+ pad (* 6.5 checker-d)))
      (Math/ceil (- 6  (/ (- x (+ (* 8 checker-d) (* 2 pad))) checker-d)))
      (Math/ceil (+ 18 (/ (- x (+ (* 8 checker-d) (* 2 pad))) checker-d))))
    :else nil))


(defn board
  [checker-r game-state & {:keys [on-click]}]
  (fn [checker-r game-state & {:keys [on-click]}]
    (let [checker-d (* 2 checker-r)
          pad (/ checker-d 4)
          w (+ (* 4 pad) (* 15 checker-d))
          h (+ (* 2 pad) (* 13 checker-d))]
      [:svg
       (merge
        {:width w :height h}
        (if on-click
          {:on-click (fn [e]
                       (let [[x y] (get-click-xy e)]
                         (on-click {:n (xy->n x y pad checker-d)})))}))
       [:rect {:width w :height h :fill "#222"}]
       [:rect {:width checker-d
               :height (* 13 checker-d)
               :fill "#444"
               :x pad
               :y pad}]
       [:rect {:width checker-d
               :height (* 13 checker-d)
               :fill "#444"
               :x (- w pad checker-d)
               :y pad}]
       [:rect {:width (* 6 checker-d)
               :height (* 13 checker-d)
               :fill "#666"
               :x (+ (* 2 pad) checker-d)
               :y pad}]
       [:rect {:width (* 6 checker-d)
               :height (* 13 checker-d)
               :fill "#666"
               :x (- w (* 2 pad) (* 7 checker-d))
               :y pad}]
       [tri-down checker-d (* 6 checker-d) (+ (* 2 pad) (* 1 checker-d)) pad "#333"]
       [tri-down checker-d (* 6 checker-d) (+ (* 2 pad) (* 2 checker-d)) pad "#eee"]
       [tri-down checker-d (* 6 checker-d) (+ (* 2 pad) (* 3 checker-d)) pad "#333"]
       [tri-down checker-d (* 6 checker-d) (+ (* 2 pad) (* 4 checker-d)) pad "#eee"]
       [tri-down checker-d (* 6 checker-d) (+ (* 2 pad) (* 5 checker-d)) pad "#333"]
       [tri-down checker-d (* 6 checker-d) (+ (* 2 pad) (* 6 checker-d)) pad "#eee"]

       [tri-down checker-d (* 6 checker-d) (+ (* 2 pad) (* 8 checker-d)) pad "#333"]
       [tri-down checker-d (* 6 checker-d) (+ (* 2 pad) (* 9 checker-d)) pad "#eee"]
       [tri-down checker-d (* 6 checker-d) (+ (* 2 pad) (* 10 checker-d)) pad "#333"]
       [tri-down checker-d (* 6 checker-d) (+ (* 2 pad) (* 11 checker-d)) pad "#eee"]
       [tri-down checker-d (* 6 checker-d) (+ (* 2 pad) (* 12 checker-d)) pad "#333"]
       [tri-down checker-d (* 6 checker-d) (+ (* 2 pad) (* 13 checker-d)) pad "#eee"]

       [tri-up checker-d (* 6 checker-d) (+ (* 2 pad) (* 1 checker-d)) (- h pad) "#eee"]
       [tri-up checker-d (* 6 checker-d) (+ (* 2 pad) (* 2 checker-d)) (- h pad) "#333"]
       [tri-up checker-d (* 6 checker-d) (+ (* 2 pad) (* 3 checker-d)) (- h pad) "#eee"]
       [tri-up checker-d (* 6 checker-d) (+ (* 2 pad) (* 4 checker-d)) (- h pad) "#333"]
       [tri-up checker-d (* 6 checker-d) (+ (* 2 pad) (* 5 checker-d)) (- h pad) "#eee"]
       [tri-up checker-d (* 6 checker-d) (+ (* 2 pad) (* 6 checker-d)) (- h pad) "#333"]

       [tri-up checker-d (* 6 checker-d) (+ (* 2 pad) (* 8 checker-d)) (- h pad) "#eee"]
       [tri-up checker-d (* 6 checker-d) (+ (* 2 pad) (* 9 checker-d)) (- h pad) "#333"]
       [tri-up checker-d (* 6 checker-d) (+ (* 2 pad) (* 10 checker-d)) (- h pad) "#eee"]
       [tri-up checker-d (* 6 checker-d) (+ (* 2 pad) (* 11 checker-d)) (- h pad) "#333"]
       [tri-up checker-d (* 6 checker-d) (+ (* 2 pad) (* 12 checker-d)) (- h pad) "#eee"]
       [tri-up checker-d (* 6 checker-d) (+ (* 2 pad) (* 13 checker-d)) (- h pad) "#333"]

       ;; checkers
       (doall
        (map-indexed (fn [i space]
                       (doall
                        (map (fn [j]
                               (let [{:keys [x y]} (get-xy w h pad checker-d i j)]
                                 ^{:key (str i "-" j)}
                                 [checker checker-r x y (:color space)]))
                             (range (:count space)))))
                     (:board game-state)))])))

(defn area
  []
  (fn []
    (let [link (aget js/window.location "href")
          player-id (* 1 (first (.match link #"\d$")))
          new-link (.replace link #"\d$" (if (= 1 player-id) 2 1))]
      [:div "Give this link to your opponent: " new-link])))


(defn- get-parsed-hash
  []
  (let [hash js/window.location.hash]
    (if-not (empty? hash)
      (-> hash (.substr 1) (.split "-")))))


(defn game
  []
  (fn []
    [:div
     [board 20 @game-state*
      :on-click (fn [{:keys [n]}]
                  (let [space (nth (:board @game-state*) (dec n))]
                    (swap! game-state* update-in [:board (dec n)] assoc
                           :color (if (= 1 (:player-id @state*)) :white :black)
                           :count (inc (:count space)))))]
     [area]]))


(defn- set-game-and-player-id
  [game-id player-id]
  (swap! state* assoc
         :game-id game-id
         :player-id (* 1  player-id)))


(defn- reset-board!
  []
  (let [game-id (:game-id @state*)
        new-value [{:color :black :count 2}
                   {} {} {} {}
                   {:color :white :count 5}
                   {}
                   {:color :white :count 3}
                   {} {} {}
                   {:color :black :count 5}
                   {:color :white :count 5}
                   {} {} {}
                   {:color :black :count 3}
                   {}
                   {:color :black :count 5}
                   {} {} {} {}
                   {:color :white :count 2}]]
    (reset! game-state* {:board new-value})))


(defn- new-game
  []
  (fn []
    [:button
     {:on-click (fn [e]
                  (let [game-id (apply str (for [_ (range 8)]
                                             (.toString (Math/floor (rand 16)) 16)))]
                    (hotsync/sync-atom game-id game-state*)
                    (set! js/window.location.hash (str "#" game-id "-1"))
                    (set-game-and-player-id game-id 1)
                    (reset-board!)))}
     "NEW GAME"]))


(defn page []
  (when-let [hash (get-parsed-hash)]
    (set-game-and-player-id (first hash) (second hash))
    (hotsync/sync-atom (first hash) game-state*))
  (fn []
    (if (:game-id @state*)
      [game]
      [new-game])))

(defn ^:export main []
  (render [page] (.getElementById js/document "app")))
