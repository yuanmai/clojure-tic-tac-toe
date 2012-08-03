(ns yuan.game.tic-tac-toe-test
  (:use expectations
        yuan.game.tic-tac-toe
        yuan.game.game-tree))

(binding [*ai-level*   3
          *board-size* 3]

  (given #ttt [- - -
               - - -
               - - -]
         (expect #(mark % 6) {:x #{}, :o #{6}}
                 turn :o))

  (given [b x] (expect b
                       (within? [x x]))
         false *board-size*
         false -1
         true  0)

  (given [x] (expect x
                     (read-string (format-board x)))
         #ttt [x - -
               - o -
               - - -])

  (given #ttt [x - -
               - o -
               - - -]
         (expect
          #(score-ttt :o %)        0.004
          #(score-ttt :x %)        0.002
          #(count (empty-cells %)) 7
          winner                   nil?
          #(count (legal-moves %)) 7))

  (given #ttt [x o -
               o x -
               - o x]
         (expect winner :x
                 legal-moves []
                 #(score-ttt :o %) (game-scores :lose)
                 #(score-ttt :x %) (game-scores :win)))

  (expect :draw
          (winner #ttt [x o x
                        o x o
                        o x o]))

  (given [winner board] (expect winner
                                (play-by-agents (repeat (ab-agent)) board))
         :draw #ttt []
         :o    #ttt [o - o
                     - o x
                     x x -])

  (run-tests ['yuan.game.tic-tac-toe-test]))
