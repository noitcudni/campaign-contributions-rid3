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
   (let [lookup-table (->> (get-in db [:test-data :curr-dataset :nodes])
                           (map (fn [x] [(:id x) x]))
                           (into {})
                           )
         neighbor-ids (:curr-neighbors db)
         ]
     (->> neighbor-ids
          (map (fn [id]
                 (get lookup-table id)
                 ))
          (sort-by :total)
          reverse
          ))
   ))
