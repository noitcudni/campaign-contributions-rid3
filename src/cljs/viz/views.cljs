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
                                                _ (re-frame/dispatch [:hl-neighbors (aget n "id") neighbors n-links])]
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

(defn norm-fill-helper [n]
  ;; TODO: customize color
  (cond (= "org" (aget n "type")) "#e5e500" ;;yellow
        (= "R" (aget n "party")) "#ff0000" ;;red
        (= "D" (aget n "party")) "#0000ff" ;;blue
        ))

(defn force-viz [ratom]
  (let [drag-started (fn [d idx]
                       (let [sim @(re-frame/subscribe [:get-var :sim])
                             d (-> sim .nodes (get idx))
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
                                                 (norm-fill-helper n)))
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
                                                                  (re-frame/dispatch [:clear-neighbors])
                                                                  (re-frame/dispatch [:sel-states @sel-states-ratom])
                                                                  )])))

(defn control-panel [show-about-ratom]
  (fn []
    (let [sel-states-ratom (reagent/atom #{})]
      [re-com/scroller
       :v-scroll :auto
       :child [re-com/v-box
               :height "100px"
               :min-width "180px"
               :max-width "180px"
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


(defn detail-close-btn []
  [re-com/button
   :label "Close"
   :style {:margin-bottom "3px"}
   :class "btn-danger btn-block"
   :on-click (fn []
               (let [node-elems @(re-frame/subscribe [:get-var :node-elems])]
                 (-> node-elems
                     (.attr "fill" (fn [n]
                                     (norm-fill-helper n)))))
               (re-frame/dispatch [:clear-neighbors]))])

(defn money-detail-panel [hl-neighbor-ratom]
  (fn []
    (let [w (str (* 7 (+ (->> @hl-neighbor-ratom
                               (map :label)
                               (map count)
                               (apply max))
                          (->> @hl-neighbor-ratom
                               (map :target-contrib-total)
                               (map str)
                               (map count)
                               (apply max)))
                    ) "px")]
      [re-com/scroller
       :v-scroll :auto
       :h-scroll :off
       :min-width w
       :max-width w
       :child
       [re-com/v-box
        :style {:padding "13px"}
        :height "100px"
        :min-width w
        :max-width w
        :children
        [
         [detail-close-btn]
         [re-com/gap :size "5px"]
         [:table #_{:style {:border "1px solid black"}}
          (->> (-> [:tbody]
                   (concat (->> (let [indices (range (count @hl-neighbor-ratom))
                                      v (map vector indices @hl-neighbor-ratom)]
                                  (for [[i d] v
                                        :let [bk-color (if (even? i) "#DCDCDC" "#FFFAFA")]]
                                    [:tr
                                     [:td {:style {:background-color bk-color :padding "2px"}} (:label d)]
                                     [:td {:style {:background-color bk-color :padding "2px" :text-align "right"}} (:target-contrib-total d)]]))
                                (into []))))
              (into []))]
         [re-com/gap :size "10px"]
         [detail-close-btn]
         ]]])))

(defn main-panel []
  (let [show-about-ratom (reagent/atom true)
        data (re-frame/subscribe [::subs/data])
        hl-neighbors (re-frame/subscribe [::subs/get-hl-neighbors])]
    (fn []
      [re-com/h-box
       :children
       [
        [re-com/button
         :label "About this project"
         ;; :class "btn-info"
         :style {:position "absolute"
                 :z-index 999
                 :top "10px"
                 :left (str (- (:width @data) 150) "px")
                 }
         :on-click (fn []
                     (reset! show-about-ratom true))]

        ;; fb like button
        [:iframe
         {:src "https://www.facebook.com/plugins/like.php?href=https%3A%2F%2Fnoitcudni.github.io%2Fcampaign-contributions-rid3%2F&width=74&layout=button_count&action=like&size=large&show_faces=true&share=false&height=21&appId=111208182271324"
          :width 74
          :height 30
          :style {:border "none"
                  :overflow "hidden"
                  :position "absolute"
                  :z-index "99"
                  :left (str (- (:width @data) 230) "px")
                  :top "10px"
                  }
          :scrolling "no"
          :frameborder "0"
          :allowTransparency "true"
          }]
        ;; twitter button
        [:iframe
         {:src "https://platform.twitter.com/widgets/tweet_button.html?size=l&url=https%3A%2F%2Fnoitcudni.github.io%2Fcampaign-contributions-rid3%2F&text=2016%20campaign%20contribution%20data%20viz%0A"
          :width 80
          :height 28
          :style {:border 0
                  :overflow "hidden"
                  :position "absolute"
                  :z-index "99"
                  :left (str (- (:width @data) 312) "px")
                  :top "10px"
                  }
          :scrolling "no"
          :frameborder "0"
          }]

        (when @show-about-ratom
          [re-com/modal-panel
           :child [re-com/v-box
                   :max-width "500px"
                   :children [
                              [:h2 "About This Project"]
                              [:p "The 2016 campaign contribution data is from " [:a {:href "https://www.opensecrets.org/"} "opensecrets.org"] "."]
                              [:p "In the left pane, you can select the state(s) of interest to be graphed. Currently, I’m graphing both Senators and House of Representatives. I may provide a way to segment the two in the future."]
                              [:p "A blue circle denotes a Democrat legislator while a red circle denotes a Republican. Yellow circles are donors. The size of each circle represents the total dollar amount, either outgoing from a donor or incoming to a legistator.  The width of the link between two nodes represents the amount given from that donor to that legislator.  Clicking on a circle will display the dollar amount in detail. The results will be sorted from highest to lowest."]
                              [:p "One thing worth noting is that I didn’t group the legislators in any way. The connectivity between the nodes is the only determining factor when it comes to grouping."]
                              [:p "This data visualization is created by " [:a {:href "https://github.com/noitcudni"} "Lih Chen"] "."]
                              [:p "Feel free to check out the source code on " [:a {:href "https://github.com/noitcudni/campaign-contributions-rid3"} "Github"] "."]
                              [re-com/button
                               :label "Close"
                               :class "btn-info btn-block"
                               :on-click (fn []
                                           (.log js/console "here")
                                           (.log js/console @show-about-ratom)
                                           (reset! show-about-ratom false))]
                              ]
                   ]
           ])

        [control-panel show-about-ratom]
        (when-not (empty? @hl-neighbors)
          [money-detail-panel hl-neighbors])
        [force-viz data]]]
      )))


;; (let []
;;   (get-in @re-frame.db/app-db [:test-data :curr-dataset :nodes]))
