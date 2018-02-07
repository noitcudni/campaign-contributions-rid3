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
   (:curr-neighbors db)))
