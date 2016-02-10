(ns backgammon.hotsync
  (:require [matchbox.core :as m]))

(enable-console-print!)

(def state* (atom {:firebase-root nil}))

(defn connect!
  [path]
  (swap! state* assoc :firebase-root (m/connect path)))

(defn sync-atom
  [str-key a*]
  (if-let [root (:firebase-root @state*)]
    (let [local-state* (atom {:last-value nil})]
      (add-watch a* :hotsync-watch
                 (fn [k a* old-v new-v]
                   (let [str-v (pr-str new-v)]
                     (swap! local-state* assoc :last-value str-v)
                     (m/reset-in! root [str-key] str-v))))
      (m/listen-to root [str-key] :value
                   (fn [[_ v]]
                     (let [last-value (:last-value @local-state*)]
                       (when (or (nil? last-value)
                                 (not= last-value v))
                         (swap! local-state* assoc :last-value v)
                         (reset! a* (cljs.reader/read-string v))))))
      :ok)
    (println "Firebase root not set... :|")))
