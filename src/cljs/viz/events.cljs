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
