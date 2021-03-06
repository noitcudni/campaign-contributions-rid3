(ns viz.core
  (:require [reagent.core :as reagent]
            [re-frame.core :as re-frame]
            [viz.events :as events]
            [viz.views :as views]
            [viz.config :as config]))

(defn dev-setup []
  (when config/debug?
    (enable-console-print!)
    (println "dev mode")))

(defn mount-root []
  (re-frame/clear-subscription-cache!)
  (reagent/render [views/main-panel]
                  (.getElementById js/document "app")))


(defn ^:export init []
  (re-frame/dispatch-sync [::events/initialize-db])
  (re-frame/dispatch-sync [:window-width js/window.innerWidth])
  (re-frame/dispatch-sync [:window-height js/window.innerHeight])

  (dev-setup)
  (set! js/window.onresize (fn []
                             (re-frame/dispatch [:window-width js/window.innerWidth])
                             (re-frame/dispatch [:window-height js/window.innerHeight])))
  (mount-root))
