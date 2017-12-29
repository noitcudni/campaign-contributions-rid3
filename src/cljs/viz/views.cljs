(ns viz.views
  (:require [re-frame.core :as re-frame]
            [viz.subs :as subs]
            [rid3.core :as rid3]
            [reagent.core :as reagent]
            ))

(def transition-duration 800)

(defn sim-did-update [ratom]
  (let [sim (-> (js/d3.forceSimulation)
                ;; TODO: customize link's id https://github.com/d3/d3-force#links
                (.force "link" (.id (-> js/d3 .forceLink) (fn [d] (.-id d))))
                (.force "charge" (js/d3.forceManyBody))
                (.force "center" (js/d3.forceCenter (/ (:width @ratom) 2)
                                                    (/ (:height @ratom) 2)))
                )
        _ (re-frame/dispatch-sync [:set-var :sim sim])
        node-dataset (clj->js (-> @ratom
                                  (get :dataset)
                                  (get :nodes)))
        link-dataset (clj->js (-> @ratom
                                  (get :dataset)
                                  (get :links)))
        node-elems @(re-frame/subscribe [:get-var :node-elems])
        link-elems @(re-frame/subscribe [:get-var :link-elems])

        tick-handler (fn []
                       (-> node-elems
                           (.attr "cx" (fn [_ idx]
                                         (.-x (get node-dataset idx))))
                           (.attr "cy" (fn [_ idx]
                                         (.-y (get node-dataset idx)))))

                       (-> link-elems
                           (.attr "x1" (fn [_ idx]
                                         (-> (get link-dataset idx) .-source .-x)))
                           (.attr "y1" (fn [_ idx]
                                         (-> (get link-dataset idx) .-source .-y)))
                           (.attr "x2" (fn [_ idx]
                                         (-> (get link-dataset idx) .-target .-x)))
                           (.attr "y2" (fn [_ idx]
                                         (-> (get link-dataset idx) .-target .-y)))
                           ))
        ]
    (-> sim
        (.nodes node-dataset)
        (.on "tick" tick-handler))
    (-> sim
        (.force "link")
        (.links link-dataset))
    ))

(defn force-viz [ratom]
  (let [drag-started (fn [d idx]
                       (let [sim @(re-frame/subscribe [:get-var :sim])
                             d (-> sim .nodes (get idx))
                             _ (.log js/console "drag-started: " ) ;;xxx
                             ]
                         (when (= 0 (-> js/d3 .-event .-active))
                           (-> sim (.alphaTarget 0.3) (.restart)))
                         (set! (.-fx d) (.-x d))
                         (set! (.-fy d) (.-y d))))
        dragged (fn [_ idx]
                  (let [sim @(re-frame/subscribe [:get-var :sim])
                        d (-> sim .nodes (get idx))]
                    (set! (.-fx d) (.-x js/d3.event))
                    (set! (.-fy d) (.-y js/d3.event))))
        drag-ended (fn [_ idx]
                     (let [sim @(re-frame/subscribe [:get-var :sim])
                           d (-> sim .nodes (get idx))]
                       (when (= 0 (-> js/d3 .-event .-active))
                         (-> sim (.alphaTarget 0)))
                       (set! (.-fx d) nil)
                       (set! (.-fy d) nil)))
        ;; TODO customize scaling type and range
        ;; TODO automatiically figures out the max and min in domain
        radius-scale (-> js/d3
                         .scaleLinear
                         (.domain (clj->js [0 60]))
                         (.range (clj->js [2 30])))]
    [rid3/viz
     {:id "force"
      :ratom ratom
      :svg {:did-mount (fn [node ratom]
                         (-> node
                             (.attr "width" (:width @ratom))
                             (.attr "height" (:height @ratom))
                             (.style "background-color" "grey"))
                         )}
      :pieces
      [
       {:kind :elem-with-data
        :tag "line"
        :class "link" ;;TODO customize link class
        :did-mount (fn [node ratom]
                     (.log js/console ">> line")
                     (.log js/console node)
                     (let [r (-> node
                                 (.attr "stroke-width" (fn [d]
                                                         ;; TODO stroke scale
                                                         (radius-scale (.-value d))))
                                 (.attr "stroke" "#E5E5E5")
                                 )]
                       (re-frame/dispatch-sync [:set-var :link-elems r])))
        :prepare-dataset (fn [ratom]
                           (-> @ratom
                               (get :dataset)
                               (get :links)
                               clj->js))}

       {:kind :elem-with-data
        :tag "circle"
        :class "node" ;;TODO customize circle class. WARNING: class can't be an empty string
        :did-mount (fn [node ratom]
                     (.log js/console ">> circle")
                     (.log js/console node) ;;xxx
                     (let [r (-> node
                                 (.attr "r" (fn [d]
                                              ;; (.log js/console ">> d : " (pr-str d)) ;;xxx
                                              (radius-scale (.-total d))
                                              ))
                                 (.attr "fill" (fn [n]
                                                 "red"))
                                 (.call (-> js/d3
                                            (.drag)
                                            (.on "start" drag-started)
                                            (.on "drag" dragged)
                                            (.on "end" drag-ended))))]
                       (re-frame/dispatch-sync [:set-var :node-elems r])))
        :prepare-dataset (fn [ratom]
                           (-> @ratom
                               (get :dataset)
                               (get :nodes)
                               clj->js))}

       {:kind :raw
        :did-mount sim-did-update
        :did-update sim-did-update}
       ]}]))

(defn main-panel []
  (fn []
    (let [data (re-frame/subscribe [::subs/data])
          ;; data (swap! data :window-width (fn []
          ;;                                  @window-width-ratom))
          ]
      [force-viz data]
      )))
