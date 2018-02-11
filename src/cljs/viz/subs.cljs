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
                           (into {}))

         curr-node (get lookup-table (:curr-sel-id db))
         neighbor-ids (get db :curr-neighbors)
         contrib-table (if (= (:type curr-node) "org")
                         (->> (:curr-n-links db)
                              (map (fn [x] [(:target x) (:total x)]))
                              (into {}))

                         (->> (:curr-n-links db)
                             (map (fn [x] [(:source x) (:total x)]))
                             (into {})
                             ))]
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
