(ns yuan.game.tic-tac-toe
  (:use [clojure.set])
  (:require [yuan.game.game-tree :as t]
            [yuan.game.alpha-beta-pruning :as ab]))

(defonce ^:dynamic *board-size* 3)
(def k-in-a-row 3)

(defn within? [yx]
  (every? #(< -1 % *board-size*) yx))

(defn yx->cell [[y x]]
  (+ x (* y *board-size*)))

(def boards-spec
  (map (fn [n]
         {:initial        (vec (repeat (* n n) :-))
          :all-cells      (apply sorted-set (range (* n n)))
          :neighbor-cells (vec
                           (for [x    (range n)
                                 y    (range n)
                                 :let [yxs (map (partial map + [y x])
                                                [[0 1] [1 0] [-1 0] [0 -1]])]]
                             (vec (map yx->cell (filter within? yxs)))))
          :win-cells      (filter #(= k-in-a-row (count %))
                                  (for [x0       (range n)
                                        y0       (range n)
                                        delta-yx [[1 0] [1 1] [0 1] [-1 1]]]
                                    (set
                                     (for [yx     (take k-in-a-row (iterate #(map + delta-yx %) [y0 x0]))
                                           :while (within? yx)]
                                       (yx->cell yx)))))})
       (range 10)))

(def board-spec (nth boards-spec *board-size*))

(defn empty-cells
  [board]
  (difference (:all-cells board-spec)
              (apply union (vals board))))

(defn neighbors [player board]
  (let [n (:neighbor-cells board-spec)]
    (intersection (set (mapcat n (player board)))
                  (empty-cells board))))

(defn turn [board]
  (apply min-key (cons #(count (% board))
                       (keys board))))

(defn mark
  [board cell]
  {:pre [(contains? (empty-cells board) cell)]}
  (update-in board [(turn board)] #(conj % cell)))

(defn winner
  "Return a keyword (:x or :o or :draw) representing the winner or nil if nobody wins in the board"
  [board]
  (if (empty? (empty-cells board))
    :draw
    (letfn [(won? [player]
              (let [cells (player board)]
                (and (seq cells)
                     (some #(every? cells %) (:win-cells board-spec))
                     player)))]
      (some won? (keys board)))))

(defn legal-moves
  [board]
  (if (winner board)
    []
    (map (partial mark board) (empty-cells board))))

(defn score-ttt [player board]
  "The more empty cells in the neighbors the better"
  (if-let [w (winner board)]
    (t/game-scores
     (if (= :draw w)
       w
       (if (= w player) :win :lose)))
    (* 0.001 (count (neighbors player board)))))

(defn interpose2 [sep1 sep2 coll]
  "interpose in 2D"
  (->> coll
       (map (partial interpose sep2))
       (interpose sep1)))

(defn apply-assoc [map kvs]
  (if (seq kvs)
    (apply assoc (cons map kvs))
    map))

(defn read-board [board]
  "#ttt reader literal"
  (let [cells (vec (map (comp keyword name)
                         board))]
    (merge-with into
                {:x #{}, :o #{}}
                (dissoc (group-by #(get-in cells [%])
                                  (:all-cells board-spec))
                        :- nil))))

(defn format-board [board]
  (let [marks  (mapcat #(interleave (% board) (repeat %))
                       (keys board))
        marked (apply-assoc (:initial board-spec) marks)
        rows   (partition *board-size* (map name marked))]
    (str "#ttt ["
         (apply str (flatten (interpose2 "\n      " " " rows)))
         "]\n")))

(defn ab-agent []
  (ab/make-agent turn score-ttt))

(defn play-by-agents [[game-agent & more] board]
  (println (format-board board))
  (let [{:keys [board moves] :as tree} (t/game-tree legal-moves board)]
    (if (seq moves)
      (recur more (:board (game-agent tree)))
      (winner board))))

(defn -main
  [& args]
  (binding [t/*ai-level* 5]
    (play-by-agents (cycle [t/random-agent (ab-agent)]) #ttt [])))
