(ns viz.subs
  (:require [re-frame.core :as re-frame]))

(re-frame/reg-sub
 ::name
 (fn [db]
   (:name db)))

(re-frame/reg-sub
 ::data
 (fn [db]
   (:test-data db)))

(re-frame/reg-sub
 :get-var
 (fn [db [_ var-key]]
   (get db var-key)))

(re-frame/reg-sub
 ::get-hl-neighbors
 (fn [db]
   (.log js/console "get-hl-neighbors")
   (let [lookup-table (->> (get-in db [:test-data :curr-dataset :nodes])
                           (map (fn [x] [(:id x) x]))
                           (into {})
                           )
         neighbor-ids (get db :curr-neighbors)
         contrib-table (->> (:curr-n-links db)
                            (map (fn [x] [(:source x) (:total x)]))
                            (into {})
                            )
         ;; _ (.log js/console "neighbors: " neighbors)
         ;; _ (.log js/console "contrib-table : " contrib-table)
         ]
     (->> neighbor-ids
          (map (fn [id]
                 (let [total (get contrib-table id)]
                   (assoc (get lookup-table id) :target-contrib-total
                          (if-not (nil? total)
                            (js/parseInt total)
                            nil)))
                 ))
          (remove #(nil? (:target-contrib-total %)))
          (sort-by :target-contrib-total)
          reverse
          )
     )
   ))
