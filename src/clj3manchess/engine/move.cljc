(ns clj3manchess.engine.move
  (:require [schema.core :as s]
            [clj3manchess.engine.state :as st]
            [clj3manchess.engine.vectors :as v :refer [abs]]
            [clj3manchess.engine.pos :as p :refer [rank file]]
            [clj3manchess.engine.fig :as f]
            [clj3manchess.engine.board :as b]
            [clj3manchess.engine.color :as c]
            [clojure.set :as se]
            [clj3manchess.engine.castling :as ca]
            [clojure.set :as set]
            [clojure.string :as str]))

(def VecMove {(s/required-key :vec) v/Vec
              (s/required-key :from) p/Pos
              (s/required-key :before) st/State})

(def PawnCapVecMove {(s/required-key :vec) v/PawnCapVec
                     (s/required-key :from) p/Pos
                     (s/required-key :before) st/State})

(def FromTo {(s/required-key :from) p/Pos
             (s/required-key :to) p/Pos})

(def Desc {(s/required-key :from) p/Pos
           (s/required-key :to) p/Pos
           (s/optional-key :prom) f/PromFigType})

(def DescMove {(s/required-key :from) p/Pos
               (s/required-key :to) p/Pos
               (s/optional-key :prom) f/PromFigType
               (s/required-key :before) st/State})

(def Move (s/either DescMove VecMove))

(s/defn to :- p/Pos [move :- Move]
  (if (contains? move :to) (:to move) (v/bv-to (dissoc move :before))))

(s/defn get-bef-sq :- b/Square [move :- Move, where :- p/Pos]
  (b/getb (:board (:before move)) where))

(s/defn is-the-fig-we-cap-not-ours :- s/Bool [m :- Move]
  (not= (:moves-next (:before m))
        (:color (get-bef-sq m (to m)))))

(s/defn can-we-en-passant :- s/Bool [m :- PawnCapVecMove]
  (let [to-sq (get-bef-sq m (to m))
        from-sq (get-bef-sq m (:from m))
        enp-sq (get-bef-sq m (assoc (to m) 0 3))]
    (and ;;(nil? to-sq) not really, imagine a gray queen teleported there
         ;;             next to jumped white pawn and black pawn ready to cap
     (= :pawn (:type enp-sq) (:type from-sq))
     (st/matching-ep (:en-passant (:before m)) (to m)
                     (:color from-sq);(:moves-next (:before m)) this is handled by :not-your-move
                     (:color enp-sq))))) ;(st/match-ep (:en-passant (:before m)) (to m))]
                       ;(case match
                       ;  :last (= (c/prev-col (:moves-next (:before m)))
                       ;           (:color (get-bef-sq m (assoc (to m) 0 3))))
                       ;  :prev (= (c/next-col (:moves-next (:before m)))
                       ;           (:color (get-bef-sq m (assoc (to m) 0 3))))
                       ;  false)


(def initially-checked-impossibilities #{:nothing-to-move-here
                                         :not-your-move
                                         :cannot-en-passant
                                         :capturing-own-piece
                                         :not-all-empties
                                         :no-castling-possibility
                                         :capturing-thru-moats
                                         :passing-unbridged-moats
                                         :no-promotion
                                         :wrong-pawn-direction})
(def those-not-disqualifying-threat #{:no-promotion
                                      :not-your-move
                                      nil})
(def later-checked-impossibilities #{:we-in-check
                                     :castling-over-check
                                     :initiating-check-thru-moats})
(def impossibilities (se/union initially-checked-impossibilities
                               later-checked-impossibilities))

(def InitiallyCheckedImpossibility (apply s/enum initially-checked-impossibilities))
(def LaterCheckedImpossibility (apply s/enum later-checked-impossibilities))
(def Impossibility (apply s/enum impossibilities))

(s/defn initial-impossibilities-check :- (s/maybe InitiallyCheckedImpossibility)
  "checks all initially-checked, ending with :no-promotion and :not-your-move"
  [m :- VecMove]
  (cond
    (nil? (get-bef-sq m (:from m))) :nothing-to-move-here
    (and (= (:type (get-bef-sq m (:from m))) :pawn)
         (contains? (:vec m) :plusfile)
         (not (can-we-en-passant m))) :cannot-en-passant
    (= (:color (get-bef-sq m (:from m)))
       (:color (get-bef-sq m (to m)))) :capturing-own-piece
    (and (contains? (:vec m) :abs)
         (not (b/check-empties (:board (:before m))
                               (v/empties-cont-vec (:vec m) (:from m))))) :not-all-empties
    (and (contains? (:vec m) :castling)
         ((:castling (:before m))
          {:type (:castling (:vec m)) :color (:moves-next (:before m))}))
    :no-castling-possibility
    (and (contains? (:vec m) :castling)
         (not (b/check-empties
               (:board (:before m))
               (->> v/castling-empties
                    (map (partial + (* 8 (c/segm (:color (:moves-next (:before m)))))))
                    (map (fn [x] [0 x])))))) :not-all-empties
    (and (not (nil? (get-bef-sq m (to m))))
         (not (cond (v/is-filevec? (:vec m)) (empty? (v/moats-file-vec (:from m) (abs (:vec m)) (:plusfile (:vec m))))
                    (v/is-diagvec? (:vec m)) (nil? (v/moat-diag-vec (:from m) (to m) (:plusfile (:vec m))))
                    (v/is-knights? (:vec m)) (nil? (v/moat-knight-vec (:from m) (to m)))
                    :else true))) :capturing-thru-moats
    (when-let [unbridged (not-empty (:moats (:before m)))]
      (cond (v/is-filevec? (:vec m)) (some unbridged
                                           (v/moats-file-vec (:from m) (abs (:vec m)) (:plusfile (:vec m))))
            (v/is-diagvec? (:vec m)) (unbridged (v/moat-diag-vec (:from m) (to m) (:plusfile (:vec m))))
            (v/is-knights? (:vec m)) (unbridged (v/moat-knight-vec (:from m) (to m))))) :passing-unbridged-moats
    ;(= (:type (get-bef-sq m (:from m))) :pawn) (if-not (:inward (:vec m))
    ;                                             (if-not (:crossedCenter (get-bef-sq m (:from m)))
    ;                                               :wrong-pawn-direction
    ;                                               (if )))
    (and (= (:type (get-bef-sq m (:from m))) :pawn)
         (= (:inward (:vec m)) (:crossedCenter (get-bef-sq m (:from m))))) :wrong-pawn-direction
    (and (= (:type (get-bef-sq m (:from m))) :pawn)
         (= (p/rank (to m)) 5)
         (not (f/promfigtypes (:prom (:vec m))))) :no-promotion
    (not= (:moves-next (:before m))
          (:color (get-bef-sq m (:from m)))) :not-your-move))

(s/defn board-after-pawn-cap :- b/Board [bef :- b/Board,
                                         from :- p/Pos, to :- p/Pos,
                                         ep :- st/EnPassant]
  (let [we (b/getb bef from)]
    (if (and (st/matching-ep ep to (:color we) (:color (b/getb bef [3 (p/file to)])))
             (= (:type (b/getb bef [3 (p/file to)])) :pawn))
      (-> bef
          (b/mov from to)
          (b/clr [3 (p/file to)]))
      (b/mov bef from to))))

(s/defn board-after-pawn-prom :- b/Board [bef :- b/Board,
                                          from :- p/Pos, to :- p/Pos,
                                          prom :- f/PromFigType]
  (let [color (:color (b/getb bef from))]
    (-> bef
        (b/clr from)
        (b/put to {:type prom :color color}))))

(s/defn board-after-castling :- b/Board [bef :- b/Board,
                                         color :- c/Color, castling :- ca/CastlingType]
  (let [color-segm*8 (* 8 (c/segm color))
        kfm-on-segm (+ p/kfm color-segm*8)
        castling-sgnf (castling v/castling-file-diff-sgnf)
        old-rook-pos (+ color-segm*8 (castling v/castling-bef-rook-pos))
        to-empty (castling v/castling-empties)
        to-empty (map (partial + color-segm*8) to-empty)
        to-empty (conj to-empty kfm-on-segm old-rook-pos)
        new-king-pos (+ kfm-on-segm (castling-sgnf 2))
        new-rook-pos (+ kfm-on-segm (castling-sgnf 1))
        emptied (loop [cur-res bef, left-to to-empty]
                  (if (empty? left-to) cur-res
                      (recur (b/clr cur-res [0 (first left-to)])
                             (rest left-to))))]
    (-> emptied
        (b/put [0 new-rook-pos] {:type :rook :color color})
        (b/put [0 new-king-pos] {:type :king :color color}))))

(s/defn both-sides-of-a-color-unbridged :- s/Bool [m :- st/MoatsState, col :- c/Color]
  (and (m col)
       (m (c/next-col col))))

(s/defn any-side-of-a-color-unbridged :- s/Bool [m :- st/MoatsState, col :- c/Color]
  (or (m col)
      (m (c/next-col col))))

(s/defn both-sides-of-a-color-bridged :- s/Bool [m :- st/MoatsState, col :- c/Color]
  (not (any-side-of-a-color-unbridged m col)))

(s/defn both-moats-of-a-color :- [(s/one c/Color "left") (s/one c/Color "right")] [col :- c/Color]
  [col (c/next-col col)])

(s/defn after-moats-state :- st/MoatsState [vecmove :- VecMove, after-board :- b/Board]
  (let [{:keys [vec before from]}   vecmove
        {:keys [moats board alive]} before
        condit                      (and (not (v/is-castvec? vec))
                                         (or (not= (:type (b/getb board from)) :pawn)
                                             (contains? vec :prom)))]
    (if-not condit moats
            (loop [res moats, left c/colors]
              (if (empty? left) res
                  (recur (let [now           (first left)
                               any-unbridged (any-side-of-a-color-unbridged moats now)]
                           (if-not any-unbridged res
                                   (if-not (alive now) (remove (set (both-moats-of-a-color now)) res)
                                           (if (->> (range 8)
                                                    (apply (partial + (c/segm now)))
                                                    (apply (partial (vec 0)))
                                                    (apply (partial b/getb after-board))
                                                    (apply :color)
                                                    (apply #{now})
                                                    (every? false?))
                                             (remove (set (both-moats-of-a-color now)) res)
                                             res))))
                         (rest left)))))))

(s/defn is-there-a-threat-with-these-vecs :- s/Bool
  [this :- b/Board, from :- p/Pos, alive :- st/Alive, ep :- st/EnPassant, vecs :- [v/Vec]]
  (let [our-before {:board         this
                    :moats         #{:white :gray :black}
                    :moves-next    nil
                    :castling      #{}
                    :en-passant    ep
                    :halfmoveclock 0 :fullmovenumber 0
                    :alive         alive}]
    (->> vecs
         (some #(let [mov   {:from from :vec % :before our-before}
                      impos (initial-impossibilities-check mov)]
                  (boolean (those-not-disqualifying-threat impos)))))))
(s/defn is-there-a-threat :- s/Bool
  ([this :- b/Board, to :- p/Pos, from :- p/Pos, alive :- st/Alive, ep :- st/EnPassant]
   (is-there-a-threat this to from alive ep (:type (b/getb this from))))
   ([this :- b/Board, to :- p/Pos, from :- p/Pos, alive :- st/Alive, ep :- st/EnPassant, ft :- f/FigType]
   (let [vecs-seq ((v/vecft (v/tvec ft)) from to)]
     (is-there-a-threat-with-these-vecs this from alive ep vecs-seq))))

(s/defn are-we-initiating-a-check-thru-moat :- s/Bool [vec from to who alive ep b]
  (and (not (cond (v/is-filevec? vec) (empty? (v/moats-file-vec from (abs vec) (:plusfile vec)))
              (v/is-diagvec? vec) (nil? (v/moat-diag-vec from to (:plusfile vec)))
              (v/is-knights? vec) (nil? (v/moat-knight-vec from to))
              :else true))
       (some (->> [(c/prev-col who) (c/next-col who)]
                  (map #(is-there-a-threat b (b/where-is-king b %) to alive ep))))))

(defonce queenside-rook-pos (v/castling-bef-rook-pos :queenside))
(defonce kingside-rook-pos (v/castling-bef-rook-pos :kingside))

(s/defn after-castling :- ca/CastlingPossibilities [bef-cas :- ca/CastlingPossibilities
                                                    who :- c/Color
                                                    ft :- f/FigType
                                                    from :- p/Pos, to :- p/Pos]
  (->> bef-cas
       (remove (set/join #{{:color who}} (case ft
                                           :king #{{:type :queenside} {:type :kingside}}
                                           :rook #{{:type (when (zero? (rank from))
                                                            (case (- (file from) (* (c/segm who) 8))
                                                              queenside-rook-pos :queenside
                                                              kingside-rook-pos :kingside))}}
                                           nil)))
       (remove (set/join #{{:color (p/color-segm to)}} (when (zero? (rank to))
                                                         (case (mod (file to) 8)
                                                           queenside-rook-pos #{{:type :queenside}}
                                                           kingside-rook-pos #{{:type :kingside}}
                                                           p/kfm #{{:type :kingside} {:type :queenside}}
                                                           nil))))))

(s/defn after-sans-eval-death :- (s/either st/State Impossibility) [vecmove :- VecMove]
  (if-let [impos (initial-impossibilities-check vecmove)]
    impos
    (let [{:keys [before vec from]} vecmove
          m vecmove ;;just an alias
          {:keys [board moats moves-next castling en-passant halfmoveclock fullmovenumber alive]} before
          to (v/addvec vec from)
          new-board (cond (v/is-castvec? vec) (board-after-castling board moves-next (:castling vec))
                          (and (v/is-diagvec? vec)
                               (= (:type (get-bef-sq m (:from m))) :pawn)) (board-after-pawn-cap board from to en-passant)
                          (f/promfigtypes (:prom vec)) (board-after-pawn-prom board from to (:prom vec))
                          :else (b/mov board from to))
          new-en-passant (if (nil? (:last en-passant)) {} {:prev (:last en-passant)})
          new-en-passant (if (= :pawnlongjump vec) (assoc :last (p/file from) new-en-passant) new-en-passant)]
      (if (are-we-initiating-a-check-thru-moat vec from to moves-next alive new-en-passant new-board)
        :initiating-check-thru-moats
        {:board new-board
         :moats (after-moats-state vecmove new-board)
         :moves-next (if (alive (c/next-col moves-next)) (c/next-col moves-next) (c/prev-col moves-next))
         :castling (after-castling castling moves-next (:type (get-bef-sq m (:from m))) from to)
         :en-passant new-en-passant
         :halfmoveclock (if (or (= (:type (b/getb board from)) :pawn)
                                (not (nil? (b/getb board to)))) 0 (inc halfmoveclock))
         :fullmovenumber (inc fullmovenumber)
         :alive alive}))))

(defonce AMFT (->> p/all-pos
                   (map (fn [from] [from (->> p/all-pos
                                              (filter (complement #(or (= from %)
                                                            (and (empty? ((v/vecft ::v/contvec) from %))
                                                                 (nil? ((v/vecft ::v/knightvec) from %))))))
                                        (set))]))
                   (into {})))

(defn testing-tostring-amft-or-sth [vfile predicate]
    (str/join "\n" (->> (range 6)
                                       (map (partial - 5))
                                       (map #(->> (range vfile (+ vfile 24))
                                                  (map (fn [fx] (mod fx 24)))
                                                  (map (fn [fx] [% fx]))
                                                  (map predicate)
                                                  (map (fn [fx] (if fx "X" "_")))
                                                  (apply str))))))
(defn testing-tostring-amft [pos vfile]
  (testing-tostring-amft-or-sth vfile (AMFT pos)))

(s/defn can-i-move-wo-check :- s/Bool [sta :- st/State, who :- c/Color]
  (some (fn [from] ) (b/where-are-figs-of-color (:board sta) who)))

(s/defn eval-death :- st/State [sta :- st/State]
  (let [noking (set (filter (complement (partial contains? (b/where-are-kings (:board sta)))) c/colors))
        checkmate (set (filter (complement (:alive sta)) c/colors)) ;replace alive-bef with canIMoveWOCheck
        died (set/union noking checkmate)]
    sta))
