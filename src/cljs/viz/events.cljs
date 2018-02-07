(ns viz.events
  (:require [re-frame.core :as re-frame]
            [viz.db :as db]))

(re-frame/reg-event-db
 ::initialize-db
 (fn  [_ _]
   db/default-db))

(re-frame/reg-event-fx
 :set-var
 (fn [{:keys [db]} [_ var-key d3-node]]
   {:db (-> db
            (assoc var-key d3-node))}))

(re-frame/reg-event-fx
 :window-width
 (fn [{:keys [db]} [_ width]]
   {:db (-> db
            (assoc-in [:test-data :width] width))}))

(re-frame/reg-event-fx
 :window-height
 (fn [{:keys [db]} [_ height]]
   {:db (-> db
            (assoc-in [:test-data :height] height))}))

(re-frame/reg-event-fx
 :sel-states
 (fn [{:keys [db]} [_ sel-states]]
   (let [dataset (-> db
                     (get-in [:test-data :dataset]))
         ;; concat links
         links (->> sel-states
                    (mapcat (fn [s] (-> dataset (get-in [s :links])))))
         ;; aggregated nodes
         nodes (->> sel-states
                    (mapcat (fn [s] (-> dataset (get-in [s :nodes]))))
                    (group-by :id)
                    (map (fn [[id node-data]]
                           (-> (first node-data)
                               (assoc :total (->> node-data (map :total) (reduce +))))))
                    )]

     {:db (-> db
              (assoc-in [:test-data :curr-dataset]
                        {:nodes nodes :links links}))}
     )))

(re-frame/reg-event-fx
 :hl-neighbors
 (fn [{:keys [db]} [_ neighbors n-links]]
   {:db (-> db
            (assoc :curr-neighbors neighbors)
            (assoc :curr-n-links n-links))}
   ))
