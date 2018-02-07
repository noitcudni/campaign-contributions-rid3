(ns viz.views
  (:require [re-frame.core :as re-frame]
            [viz.subs :as subs]
            [rid3.core :as rid3]
            [re-com.core :as re-com]
            [reagent.core :as reagent]))

(def transition-duration 800)

(defn get-neighboring-links [links node]
  (let [id (aget node "id")]
    (->> links
         (filter #(or (= (:target %) id)
                      (= (:source %) id)))
         ;; (into #{})
         )))

(defn get-neighbor-ids [neighboring-links]
  (->> neighboring-links
       (mapcat (fn [x] [(:target x) (:source x)]))
       (into #{})))



(defn sim-did-update [ratom]
  (let [sim (-> (js/d3.forceSimulation)
                ;; TODO: customize link's id https://github.com/d3/d3-force#links
                (.force "link" (.id (-> js/d3 .forceLink) (fn [d] (.-id d))))
                (.force "charge" (js/d3.forceManyBody))
                (.force "center" (js/d3.forceCenter (/ (:width @ratom) 2)
                                                    (/ (:height @ratom) 2))))
        _ (re-frame/dispatch-sync [:set-var :sim sim])
        node-dataset (clj->js
                      (let [curr-dataset (:curr-dataset @ratom)]
                        (if (empty? curr-dataset) [] (:nodes curr-dataset))))
        link-dataset (clj->js
                      (let [curr-dataset (:curr-dataset @ratom)]
                        (if (empty? curr-dataset) [] (:links curr-dataset))))
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

                                          (let [curr-dataset (:curr-dataset @ratom)]
                                            (if (empty? curr-dataset) [] (:nodes curr-dataset)))

                                          (let [n-links (get-neighboring-links (->> @ratom :curr-dataset :links) n)
                                                neighbors (get-neighbor-ids n-links)

                                                _ (re-frame/dispatch [:hl-neighbors neighbors n-links])]
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
                                                                (.log js/console "curr: " curr) ;xxx
                                                                (if (contains? neighbors (aget curr "id"))
                                                                  (cond (= (aget curr "type") "org") "#009a9a"
                                                                        (= (aget curr "party") "D")  "#0000ff"
                                                                        (= (aget curr "party") "R")  "#ff0000"
                                                                        :else "#000000")
                                                                  "#5d5d5d" ;;grey out
                                                                  )))
                                                ))

                                          )))
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
                         (.domain (clj->js [0 800000]))
                         (.range (clj->js [0.1 10])))
        link-scale (-> js/d3
                       .scaleLinear
                       (.domain (clj->js [0 100000]))
                       (.range (clj->js [0.1 5])))
        ]
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
                     (let [r (-> node
                                 (.attr "stroke-width" (fn [d]
                                                         ;; TODO stroke scale
                                                         ;; 0.5
                                                         (link-scale (.-total d))
                                                         ;; (radius-scale (.-value d))
                                                         ))
                                 (.attr "stroke-opacity" 0.3)
                                 (.attr "stroke" "#E5E5E5")
                                 )]
                       (re-frame/dispatch-sync [:set-var :link-elems r])))
        :prepare-dataset (fn [ratom]
                           (clj->js
                            (let [curr-dataset (:curr-dataset @ratom)]
                              (if (empty? curr-dataset) [] (:links curr-dataset))))
                           )}

       {:kind :elem-with-data
        :tag "circle"
        :class "node" ;;TODO customize circle class. WARNING: class can't be an empty string
        :did-mount (fn [node ratom]
                     (let [r (-> node
                                 (.attr "r" (fn [d]
                                              (radius-scale (.-total d))
                                              ;; 2
                                              ))
                                 (.attr "fill" (fn [n]
                                                 ;; TODO: customize color
                                                 (cond (= "org" (aget n "type")) "#e5e500" ;;yellow
                                                       (= "R" (aget n "party")) "#ff0000" ;;red
                                                       (= "D" (aget n "party")) "#0000ff" ;;blue
                                                       )
                                                 ))
                                 (.call (-> js/d3
                                            (.drag)
                                            (.on "start" drag-started)
                                            (.on "drag" dragged)
                                            (.on "end" drag-ended))))]
                       (re-frame/dispatch-sync [:set-var :node-elems r])))
        :prepare-dataset (fn [ratom]
                           (clj->js
                            (let [curr-dataset (:curr-dataset @ratom)]
                              (if (empty? curr-dataset) [] (:nodes curr-dataset)))))
        }

       {:kind :elem-with-data
        :tag "text"
        :class "text"
        :did-mount (fn [node ratom]
                     (let [r (-> node
                                 (.text (fn [d]
                                          (when (not= "org" (.-type d))
                                            (.-label d))
                                          ))
                                 (.attr "font-size" (fn [d] 12))
                                 (.attr "dx" 15)
                                 (.attr "dx" 4))]
                       (re-frame/dispatch-sync [:set-var :text-elems r])
                       ))
        :prepare-dataset (fn [ratom]
                           (clj->js
                            (let [curr-dataset (:curr-dataset @ratom)]
                              (if (empty? curr-dataset) [] (:nodes curr-dataset)))))
        }

       {:kind :raw
        :did-mount sim-did-update
        :did-update sim-did-update}
       ]}]))

(defn state-checkbox [iso sel-states-ratom]
  (fn [iso sel-states-ratom]
    (let [state-ratom (reagent/atom false)]
      [re-com/checkbox :model state-ratom :label iso :on-change (fn [x]
                                                                  (reset! state-ratom x)
                                                                  (if x
                                                                    (swap! sel-states-ratom conj iso)
                                                                    (swap! sel-states-ratom disj iso))
                                                                  ;; (.log js/console @sel-states-ratom)
                                                                  (re-frame/dispatch [:sel-states @sel-states-ratom])
                                                                  )])))

(defn control-panel []
  (fn []
    (let [sel-states-ratom (reagent/atom #{})]
      [re-com/scroller
       :v-scroll :auto
       :child [re-com/v-box
               :height "100px"
               :width "150px"
               :style {:padding "13px"}
               :gap "15px"
               :children
               [
                [re-com/v-box
                 :children
                 [[re-com/label :label "New England"]
                  [state-checkbox "CT" sel-states-ratom]
                  [state-checkbox "ME" sel-states-ratom]
                  [state-checkbox "MA" sel-states-ratom]
                  [state-checkbox "NH" sel-states-ratom]
                  [state-checkbox "RI" sel-states-ratom]
                  [state-checkbox "VT" sel-states-ratom]
                  ]]

                [re-com/v-box
                 :children
                 [[re-com/label :label "Mideast"]
                  [state-checkbox "DE" sel-states-ratom]
                  [state-checkbox "MD" sel-states-ratom]
                  [state-checkbox "NJ" sel-states-ratom]
                  [state-checkbox "NY" sel-states-ratom]
                  [state-checkbox "PA" sel-states-ratom]

                  ]]

                [re-com/v-box
                 :children
                 [[re-com/label :label "Great Lakes"]
                  [state-checkbox "IL" sel-states-ratom]
                  [state-checkbox "IN" sel-states-ratom]
                  [state-checkbox "MI" sel-states-ratom]
                  [state-checkbox "OH" sel-states-ratom]
                  [state-checkbox "WI" sel-states-ratom]

                  ]]

                [re-com/v-box
                 :children
                 [[re-com/label :label "Southeast"]
                  [state-checkbox "AL" sel-states-ratom]
                  [state-checkbox "AR" sel-states-ratom]
                  [state-checkbox "FL" sel-states-ratom]
                  [state-checkbox "GA" sel-states-ratom]
                  [state-checkbox "KY" sel-states-ratom]
                  [state-checkbox "LA" sel-states-ratom]
                  [state-checkbox "MS" sel-states-ratom]
                  [state-checkbox "NC" sel-states-ratom]
                  [state-checkbox "SC" sel-states-ratom]
                  [state-checkbox "TN" sel-states-ratom]
                  [state-checkbox "VA" sel-states-ratom]
                  [state-checkbox "WV" sel-states-ratom]
                  ]]

                [re-com/v-box
                 :children
                 [[re-com/label :label "Plains"]
                  [state-checkbox "IA" sel-states-ratom]
                  [state-checkbox "KS" sel-states-ratom]
                  [state-checkbox "MN" sel-states-ratom]
                  [state-checkbox "MO" sel-states-ratom]
                  [state-checkbox "NE" sel-states-ratom]
                  [state-checkbox "ND" sel-states-ratom]
                  [state-checkbox "SD" sel-states-ratom]

                  ]]

                [re-com/v-box
                 :children
                 [[re-com/label :label "Rocky Mountain"]
                  [state-checkbox "CO" sel-states-ratom]
                  [state-checkbox "ID" sel-states-ratom]
                  [state-checkbox "MT" sel-states-ratom]
                  [state-checkbox "UT" sel-states-ratom]
                  [state-checkbox "WY" sel-states-ratom]
                  ]]

                [re-com/v-box
                 :children
                 [[re-com/label :label "Southwest"]
                  [state-checkbox "AZ" sel-states-ratom]
                  [state-checkbox "NM" sel-states-ratom]
                  [state-checkbox "OK" sel-states-ratom]
                  [state-checkbox "TX" sel-states-ratom]
                  ]]

                [re-com/v-box
                 :children
                 [[re-com/label :label "West"]
                  [state-checkbox "AK" sel-states-ratom]
                  [state-checkbox "CA" sel-states-ratom]
                  [state-checkbox "HI" sel-states-ratom]
                  [state-checkbox "NV" sel-states-ratom]
                  [state-checkbox "OR" sel-states-ratom]
                  [state-checkbox "WA" sel-states-ratom]
                  ]]
                ]]])
    ))

(defn money-detail-panel []
  [re-com/scroller
   ;; :height "100px"
   ;; :width "200px"
   :child
   [re-com/v-box
    :style {:padding "13px"}
    :height "100px"
    :width "100px"
    :children
    [[:div
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]
      [:div "hello"]

      ]]
    #_[re-com/alert-box
     :alert-type :info
     :heading "Contribution Details"
     :body [:div
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]
            [:div "hello"]

            ]
     ]]]
  )

(defn main-panel []
  (let [selected-ratom (reagent/atom #{})
        data (re-frame/subscribe [::subs/data])
        hl-neighbors (re-frame/subscribe [::subs/get-hl-neighbors])
        _ (.log js/console "hl-neighbors: " @hl-neighbors)
        ]
    [re-com/h-box
     :children
     [[control-panel]
      (when-not (empty? @hl-neighbors)
       [money-detail-panel])
      [force-viz data]]]
    ))


(let [d [{:id "D000055937", :type "org", :label "Readco LLC", :total 5200, :indivs 5200, :pacs 0}
         {:id "Law Offices of Ellen B Lubell", :type "org", :label "Law Offices of Ellen B Lubell", :total 3700, :indivs 3700, :pacs 0}]]
  (->> d
       (map (fn [x]
              [(:id x) x]
              ))
       (into {})
       )

  )

;; (let []
;;   (get-in @re-frame.db/app-db [:test-data :curr-dataset :nodes]))
