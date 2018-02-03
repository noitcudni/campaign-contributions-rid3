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
   {:db (-> db
            (assoc-in [:test-data :sel-states] sel-states))}))
