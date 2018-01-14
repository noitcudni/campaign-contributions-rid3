(ns viz.views
  (:require [re-frame.core :as re-frame]
            [viz.subs :as subs]
            [rid3.core :as rid3]
            [re-com.core :as re-com]

            [reagent.core :as reagent]))

(defn get-neighbors [links node]
  (let [id (aget node "id")]
    (->> links
         (filter #(or (= (:target %) id)
                      (= (:source %) id)))
         (mapcat (fn [x] [(:target x) (:source x)]))
         (into #{})
         )))

(defn force-viz [ratom]
  (let [drag-started (fn [d idx]
                       (let [sim @(re-frame/subscribe [:get-var :sim])
                             d (-> sim .nodes (get idx))]
                         (when (= 0 (-> js/d3 .-event .-active))
                           (-> sim (.alphaTarget 0.1) (.restart)))
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
                         (.domain (clj->js [0 800000]))
                         (.range (clj->js [0.1 10])))
        link-scale (-> js/d3
                       .scaleLinear
                       (.domain (clj->js [0 100000]))
                       (.range (clj->js [0.1 5])))
        ]
    [rid3/viz
     {:id "force"
      :ratom ratom :svg {:did-mount (fn [node ratom]
                         (-> node
                             (.attr "width" 2000)
                             (.attr "height" 2000)
                             (.style "background-color" "grey")))}

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
                                                         ;; 0.5
                                                         (link-scale (.-total d))
                                                         ;; (radius-scale (.-value d))
                                                         ))
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
                                              ;; 2
                                              ))
                                 (.attr "fill" (fn [n]
                                                 ;; TODO: customize color
                                                 (.log js/console (.-type n)) ;;xxx
                                                 (cond (= "org" (.-type n)) "yellow"
                                                       (= "R" (.-party n)) "red"
                                                       (= "D" (.-party n)) "blue"
                                                       )

                                                 ))
                                 (.call (-> js/d3
                                            (.drag)
                                            (.on "start" drag-started)
                                            (.on "drag" dragged)
                                            (.on "end" drag-ended))))]
                       (re-frame/dispatch-sync [:set-var :node-elems r])))
        :prepare-dataset (fn [ratom]
                           (-> @ratom (get :dataset) (get :nodes) clj->js))}

       {:kind :elem-with-data
        :tag "text"
        :class "text"
        :did-mount (fn [node ratom]
                     (let [r (-> node
                                 (.text (fn [d]
                                          (when (not= "org" (.-type d))
                                            (.-label d))))
                                 (.attr "font-size" (fn [d] 12))
                                 (.attr "dx" 15)
                                 (.attr "dx" 4))]
                       (re-frame/dispatch-sync [:set-var :text-elems r])
                       ))
        :prepare-dataset (fn [ratom]
                           (-> @ratom (get :dataset) (get :nodes) clj->js))
        }

       {:kind :raw
        :did-mount (fn [ratom]
                     (.log js/console ">> raw")
                     (let [sim (-> (js/d3.forceSimulation)
                                   ;; TODO: customize link's id https://github.com/d3/d3-force#links
                                   (.force "link" (.id (-> js/d3 .forceLink) (fn [d] (.-id d))))
                                   (.force "charge" (js/d3.forceManyBody))
                                   (.force "center" (js/d3.forceCenter 1000 1000)))
                           _ (re-frame/dispatch-sync [:set-var :sim sim])

                           node-dataset (clj->js (-> @ratom
                                                     (get :dataset)
                                                     (get :nodes)))
                           link-dataset (clj->js (-> @ratom
                                                     (get :dataset)
                                                     (get :links)))
                           node-elems @(re-frame/subscribe [:get-var :node-elems])
                           text-elems @(re-frame/subscribe [:get-var :text-elems])
                           link-elems @(re-frame/subscribe [:get-var :link-elems])

                           tick-handler (fn []
                                          (-> node-elems
                                              (.attr "cx" (fn [_ idx]
                                                            (.-x (get node-dataset idx))))
                                              (.attr "cy" (fn [_ idx]
                                                            (.-y (get node-dataset idx))))
                                              (.on "click" (fn [n idx]
                                                             (let [neighbors (get-neighbors (-> @ratom (get :dataset) (get :links)) n)]
                                                               (.log js/console "neighbors: " neighbors) ;;xxx
                                                               (-> text-elems
                                                                   (.text (fn [curr]
                                                                            (cond (or (= (.-id curr) (.-id n))
                                                                                      (not= "org" (.-type curr))) (.-label curr)
                                                                                  )))
                                                                   (.attr "font-size" (fn [curr] 12))
                                                                   (.attr "dx" 15)
                                                                   (.attr "dx" 4))

                                                               (-> node-elems
                                                                   (.attr "fill" (fn [curr]
                                                                                   (if (contains? neighbors (aget curr "id"))
                                                                                     ;; customize color
                                                                                     "#009a9a"
                                                                                     ;; "blue"
                                                                                     "#5d5d5d")))
                                                                   ))

                                                             )))
                                          ;; textElements
                                          ;; .attr(“x”, node => node.x)
                                          ;; .attr(“y”, node => node.y)
                                          (-> text-elems
                                              (.attr "x" (fn [_ idx]
                                                           (.-x (get node-dataset idx))
                                                           ))
                                              (.attr "y" (fn [_ idx]
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
        }

       ]}]))

(defn state-checkbox [iso sel-states-ratom]
  (fn [iso sel-states-ratom]
    (let [state-ratom (reagent/atom false)]
      [re-com/checkbox :model state-ratom :label iso :on-change (fn [x]
                                                                  (reset! state-ratom x)
                                                                  (if x
                                                                    (swap! sel-states-ratom conj iso)
                                                                    (swap! sel-states-ratom disj iso))
                                                                  (.log js/console @sel-states-ratom)
                                                                  )])))

(defn control-panel []
  (fn []
    (let [sel-states-ratom (reagent/atom #{})]
     [re-com/v-box
      :children
      [[state-checkbox "AL" sel-states-ratom]
       [state-checkbox "AK" sel-states-ratom]
       [state-checkbox "AZ" sel-states-ratom]
       [state-checkbox "AR" sel-states-ratom]
       [state-checkbox "CA" sel-states-ratom]
       [state-checkbox "CO" sel-states-ratom]
       [state-checkbox "CT" sel-states-ratom]
       [state-checkbox "DE" sel-states-ratom]
       [state-checkbox "FL" sel-states-ratom]
       [state-checkbox "GA" sel-states-ratom]
       [state-checkbox "HI" sel-states-ratom]
       [state-checkbox "ID" sel-states-ratom]
       [state-checkbox "IL" sel-states-ratom]
       [state-checkbox "IN" sel-states-ratom]
       [state-checkbox "IA" sel-states-ratom]
       [state-checkbox "KS" sel-states-ratom]
       [state-checkbox "KY" sel-states-ratom]
       [state-checkbox "LA" sel-states-ratom]
       [state-checkbox "ME" sel-states-ratom]
       [state-checkbox "MD" sel-states-ratom]
       [state-checkbox "MA" sel-states-ratom]
       [state-checkbox "MI" sel-states-ratom]
       [state-checkbox "MN" sel-states-ratom]
       [state-checkbox "MS" sel-states-ratom]
       [state-checkbox "MO" sel-states-ratom]
       [state-checkbox "MT" sel-states-ratom]
       [state-checkbox "NE" sel-states-ratom]
       [state-checkbox "NV" sel-states-ratom]
       [state-checkbox "NH" sel-states-ratom]
       [state-checkbox "NJ" sel-states-ratom]
       [state-checkbox "NM" sel-states-ratom]
       [state-checkbox "NY" sel-states-ratom]
       [state-checkbox "NC" sel-states-ratom]
       [state-checkbox "ND" sel-states-ratom]
       [state-checkbox "OH" sel-states-ratom]
       [state-checkbox "OK" sel-states-ratom]
       [state-checkbox "OR" sel-states-ratom]
       [state-checkbox "PA" sel-states-ratom]
       [state-checkbox "RI" sel-states-ratom]
       [state-checkbox "SC" sel-states-ratom]
       [state-checkbox "SD" sel-states-ratom]
       [state-checkbox "TN" sel-states-ratom]
       [state-checkbox "TX" sel-states-ratom]
       [state-checkbox "UT" sel-states-ratom]
       [state-checkbox "VT" sel-states-ratom]
       [state-checkbox "VA" sel-states-ratom]
       [state-checkbox "WA" sel-states-ratom]
       [state-checkbox "WV" sel-states-ratom]
       [state-checkbox "WI" sel-states-ratom]
       [state-checkbox "WY" sel-states-ratom]]
      ])))



(defn main-panel []
  (let [data (re-frame/subscribe [::subs/test-data])]
    [control-panel]

    #_[force-viz data]
    ))
