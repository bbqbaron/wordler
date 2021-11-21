(ns core
  (:require [reagent.core :as ra]
            [reagent.dom :as rd]
            [words5 :refer [words]]
            [clojure.string :as str]))

(declare get-word-list)

(defn entry->list [[k [sub ct]]]
  (concat
   (map (partial apply str k)
        (get-word-list sub))
   (repeat ct k)))

(defn get-word-list [data]
  (mapcat
   entry->list
   data))

(defonce word-list (get-word-list words))

(defn pick-word [] (rand-nth word-list))

(defn init-state []
  {:answer (pick-word) :word "     "})

(defonce state
  (ra/atom (init-state)))

(defn pick-word! []
  (swap! state assoc :answer (pick-word)))

(defn endgame [{:keys [answer guesses word] :as st}]
  (cond
    (= word answer)
    (assoc st :state :victory)
    (> (count guesses) 5)
    (assoc st :state :loss)
    :else st))

(defn reset-game! []
  (reset! state (init-state)))

(defn fill-key! [c]
  (case c
    "Enter" (reset-game!)
    (when (re-find #"^[a-z]$" c)
      (swap! state
             (comp
              endgame
              (fn [{:keys [answer word] :as st}]
                (let [full? (>= (count (remove str/blank? word)) 5)]
                  (->
                   st
                   (cond->
                    full?
                     (->
                      (update :guesses (fnil conj []) word)
                      (assoc :word
                             (apply str
                                    (for [[ac c] (map vector answer word)]
                                      (if (= ac c)
                                        c " "))))))
                   (update
                    :word
                    (fn update-word [word]
                      (str/replace-first word #" " c)))))))))))

(defn block [n c]
  (let [answer (:answer @state)
        correct? (= c (nth (:answer @state) n))
        exists? (and (not correct?) (some #{c} (vec answer)))]
    [:div
     {:style
      (merge
       {:outline "2px solid black"
        :height "96px"
        :width "96px"
        :text-align :center
        :margin "10px"}
       (when correct? {:background-color "green"})
       (when exists? {:background-color "yellow"}))}
     [:p
      {:style {:font-size "xxx-large"}}
      c]]))

(defn word-row [word]
  (into [:div.row]
        (vec
         (map-indexed block
                      (take 5
                            (concat
                             word
                             "     "))))))

(defn entry []
  (let [{:keys [answer] status :state :as st-now} @state]
    (into
     [:div.container
      [:div.row
       [:button.primary {:on-click (fn [_] (reset-game!))}
        "Reset"] 
       [:small "(Press enter to reset)"]]
      [:medium.row "Type letters to guess. Green = right letter, right place. Yellow = right letter, wrong place."]]
     (case status
       (:loss :victory)
       [[:form.row
         {:on-submit (fn [_] (.preventDefault _) (reset-game!))}
         [:p status "! The word was '" answer "'"]]]
       (into [#_{:style {:display :grid
                         :grid-template-rows (str/join " " (repeat 5 "100px"))
                         :grid-auto-columns "100px"
                         :grid-gap "25px"}}
              (word-row (:word st-now))]
             (mapv word-row (reverse (:guesses st-now))))))))

(defn doit [e]
  (fill-key! (.. e -key)))

(defn ^:after-load start []
  (pick-word!)
  (.addEventListener (.-body js/document) "keypress"
                     doit)
  (rd/render
   [entry]
   (.getElementById js/document "app")))

(comment
  (defn ->trie [wds]
    (into {}
          (map
           (fn [[k wds2]]
             (let [[wds3 found] (map
                                 (group-by empty?
                                           (map rest wds2))
                                 [false true])]
               [k [(->trie wds3) (count found)]]))
           (group-by first wds)))))
