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
           (s/optional-key :prom) (s/maybe f/PromFigType)})

(def DescMove {(s/required-key :from) p/Pos
               (s/required-key :to) p/Pos
               (s/optional-key :prom) (s/maybe f/PromFigType)
               (s/required-key :before) st/State})

(def Move (s/conditional #(contains? % :to) DescMove
                         #(contains? % :vec) VecMove))

(s/defn to :- p/Pos [move :- Move]
  (if (and (not (contains? move :vec))
           (contains? move :to)) (:to move) (v/bv-to (dissoc move :before))))

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
                                     :castling-passing-thru-check
                                     :castling-starting-with-check
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
    (and (map? (:vec m)) (= (:type (get-bef-sq m (:from m))) :pawn)
         (contains? (:vec m) :plusfile)
         (not (can-we-en-passant m))) :cannot-en-passant
    (= (:color (get-bef-sq m (:from m)))
       (:color (get-bef-sq m (to m)))) :capturing-own-piece
    (and (map? (:vec m)) (contains? (:vec m) :abs)
         (not (b/check-empties (:board (:before m))
                               (v/empties-cont-vec (:vec m) (:from m))))) :not-all-empties
    (and (map? (:vec m)) (contains? (:vec m) :castling)
         ((complement (:castling (:before m)))
          {:type (:castling (:vec m)) :color (:moves-next (:before m))})) :no-castling-possibility
    (and (map? (:vec m)) (contains? (:vec m) :castling)
         (not (b/check-empties
               (:board (:before m))
               (->> (v/castling-empties (:castling (:vec m)))
                    (map (partial + (* 8 (c/segm (:moves-next (:before m))))))
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
    ;                                             (if-not (:crossed-center (get-bef-sq m (:from m)))
    ;                                               :wrong-pawn-direction
    ;                                               (if )))
    (and (map? (:vec m))
         (= (:type (get-bef-sq m (:from m))) :pawn)
         (= (:inward (:vec m)) (:crossed-center (get-bef-sq m (:from m))))) :wrong-pawn-direction
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
                                             (not (nil? (:prom vec)))))]
    (if-not condit moats
            (loop [res moats, left c/colors]
              (if (empty? left) res
                  (recur (let [now           (first left)
                               any-unbridged (any-side-of-a-color-unbridged moats now)]
                           (if-not any-unbridged res
                                   (if-not (alive now) (remove (set (both-moats-of-a-color now)) res)
                                           (if (->> (range 8)
                                                    (map (partial + (c/segm now)))
                                                    (map (fn [xx] [0 xx]))
                                                    (map (partial b/getb after-board))
                                                    (map :color)
                                                    (map #{now})
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
   (if-let [fromsq (b/getb this from)]
     (is-there-a-threat this to from alive ep (:type fromsq))))
  ([this :- b/Board, to :- p/Pos, from :- p/Pos, alive :- st/Alive, ep :- st/EnPassant, ft :- f/FigType]
   (let [vecs-seq ((v/vecft (v/tvec ft)) from to)]
     (is-there-a-threat-with-these-vecs this from alive ep vecs-seq))))

(s/defn are-we-initiating-a-check-thru-moat :- s/Bool [vec from to who alive ep b]
  (and (not (cond (v/is-filevec? vec) (empty? (v/moats-file-vec from (abs vec) (:plusfile vec)))
                  (v/is-diagvec? vec) (nil? (v/moat-diag-vec from to (:plusfile vec)))
                  (v/is-knights? vec) (nil? (v/moat-knight-vec from to))
                  :else true))
       (some #(is-there-a-threat b (b/where-is-king b %) to alive ep)
             [(c/prev-col who) (c/next-col who)])))

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

(defn asedac-new-board [vec board moves-next whatype from to en-passant]
  (cond (v/is-castvec? vec) (board-after-castling board moves-next (:castling vec))
        (and (v/is-diagvec? vec)
             (= whatype :pawn)) (board-after-pawn-cap board from to en-passant)
        (f/promfigtypes (:prom vec)) (board-after-pawn-prom board from to (:prom vec))
        :else (b/mov board from to)))
(defn after-en-passant-nothing [ep] (if (nil? (:last ep)) {} (:prev (:last ep))))
(defn after-en-passant-something [ep where] (assoc (after-en-passant-nothing ep) :last where))
(defn after-en-passant [ep vec from]
  (if (v/is-pawnlongjumpvec? vec)
    (after-en-passant-something ep (p/file from))
    (after-en-passant-nothing ep)))
(def StateOrImpossibility (s/conditional keyword? Impossibility :else st/State))
(s/defn after-sans-eval-death-and-check :- StateOrImpossibility
  [{vec :vec
    [from-rank from-file :as from] :from
    {:keys [board moats moves-next castling en-passant halfmoveclock fullmovenumber alive] :as before} :before
    :as m} :- VecMove]
  (if-let [impos (initial-impossibilities-check m)]
    impos
    (if-let [to (v/addvec vec from)]
      (let [what (b/getb board from)
            tosq (b/getb board to)
            whatype (:type what)
            new-board (asedac-new-board vec board moves-next whatype from to en-passant)
            new-en-passant (after-en-passant en-passant vec from)]
        (if (are-we-initiating-a-check-thru-moat vec from to moves-next alive new-en-passant new-board)
          :initiating-check-thru-moats
          (let [nxtcolmvs (c/next-col moves-next)
                prvcolmvs (c/prev-col moves-next)]
            {:board new-board
             :moats (after-moats-state m new-board)
             :moves-next (if (alive nxtcolmvs) nxtcolmvs prvcolmvs)
             :castling (after-castling castling moves-next whatype from to)
             :en-passant new-en-passant
             :halfmoveclock (if (or (= whatype :pawn)
                                    (not (nil? tosq))) 0 (inc halfmoveclock))
             :fullmovenumber (inc fullmovenumber)
             :alive alive}))) (println vec from))))

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

(s/defn can-i-move-wo-check :- s/Bool
  ([sta :- st/State, who :- c/Color]
   (some #(can-i-move-wo-check sta who %) (b/where-are-figs-of-color (:board sta) who)))
  ([sta who from] (let [type-of-fig-there (:type (b/getb (:board sta) from))
                        tvec              (v/tvec type-of-fig-there)
                        vecft             (v/vecft tvec)] (-> #(can-i-move-wo-check sta who from vecft %)
                                                              (some (AMFT from)))))
  ([sta who from vecft to] (when-not (nil? to)
                             (let [vecft (vecft from to)
                                   vecft (if (and (coll? vecft) (not (map? vecft))) vecft #{vecft})]
                               (->> vecft
                                    (some #(can-i-move-wo-check sta who from vecft to %))
                                    (when-not (->> vecft
                                                   (filter (complement nil?))
                                                   empty?))))))
  ([sta who from vecft to vect] (let [afterr        (after-sans-eval-death-and-check
                                                     {:vec vect :before sta :from from})
                                      not-nil       (not (nil? afterr))
                                      not-impos     (not (impossibilities afterr))
                                      just-ok-impos (#{:not-your-move :no-promotion} afterr)]
                                  (and not-nil (or not-impos just-ok-impos)))))

(s/defn eval-death :- st/State [sta :- st/State]
  (let [noking (-> contains?
                   (partial (b/where-are-kings (:board sta)))
                   complement
                   (filter c/colors)
                   set)
        checkmate (-> can-i-move-wo-check
                      (partial sta)
                      complement
                      (filter c/colors)
                      set)
        died (set/union noking checkmate)]
    (->> sta
         :alive
         (remove died)
         set
         (assoc sta :alive))))
(s/defn threat-checking :- [p/Pos] [board :- b/Board, where :- p/Pos, alive :- st/Alive, ep :- st/EnPassant]
  (let [who (:color (b/getb board where))]
    (->> p/all-pos
         (map #(if-let [tjf (b/getb board %)]
                 (when (and (not= (:color tjf) who)
                            (alive (:color tjf))
                            (is-there-a-threat board % alive ep (:type tjf))) %)))
         (filter identity))))
(s/defn check-checking :- [p/Pos] [board :- b/Board, who :- c/Color, alive :- st/Alive]
  (when (alive who) (if-let [king-pos (b/where-is-king board who)]
                      (threat-checking board king-pos alive {}))))
(s/defn after-sans-eval-death :- StateOrImpossibility
  [{vec :vec
    [from-rank from-file :as from] :from
    {:keys [board moats moves-next castling en-passant halfmoveclock fullmovenumber alive] :as before} :before
    :as m} :- VecMove]
  (let [nxtcolmvs (c/next-col moves-next)
        prvcolmvs (c/prev-col moves-next)
        to (v/addvec vec from)
        what (b/getb board from)
        tosq (b/getb board to)
        whatype (:type what)
        {:as new-state-after
         new-board :board
         new-en-passant :en-passant} (after-sans-eval-death-and-check m)]
    (if-not (v/is-castvec? vec) new-state-after
            (cond (not (empty? (check-checking board moves-next alive))) :castling-over-check
                  (not (empty? (check-checking new-board moves-next alive))) :we-in-check
                  (not (empty? (check-checking (apply (partial b/mov new-board)
                                                      (let [color moves-next
                                                            color-segm*8 (* 8 (c/segm color))
                                                            kfm-on-segm (+ p/kfm color-segm*8)
                                                            castling (:castling vec)
                                                            castling-sgnf (castling v/castling-file-diff-sgnf)
                                                            new-king-pos (-> kfm-on-segm
                                                                             (+ 2)
                                                                             (mod 24))
                                                            new-rook-pos (-> kfm-on-segm
                                                                             (+ 1)
                                                                             (mod 24))
                                                            new-king-pos [0 new-king-pos]
                                                            new-rook-pos [0 new-rook-pos]]
                                                        [new-king-pos new-rook-pos]))
                                               moves-next alive))) :castling-over-check
                  :else new-state-after))))
(s/defn after [{vec :vec :as m} :- VecMove]
  (let [sans-check (after-sans-eval-death m)]
    (if-not (map? sans-check) sans-check
            (let [{:keys [board moves-next alive]} sans-check
                  we (c/prev-col moves-next)]
              (if-not (empty? (check-checking board we alive)) :we-in-check
                      (eval-death sans-check))))))
(s/defn generate-vecs ; :- #{BoundVec}
  ([figtype :- f/FigType
    {:keys [from prom] [rank-to :as to] :to :as ftp}] ;;:- (s/either Desc DescMove)]
   (->> ((v/vecft (v/tvec figtype)) from to)
        (#(cond (nil? %) #{} (or (= :pawnlongjump %) (map? %)) #{%} :else %))
        (map (if (and (= figtype :pawn)
                      (= rank-to 5)
                      (not (nil? prom)))
               #(assoc % :prom prom) identity))
        (map #(assoc ftp :vec %))
        set))
  ([{:keys [from before] :as ftp}] ;;:- DescMove]
   (if-let [ftb (get-bef-sq ftp from)]
     (-> ftb
         :type
         (generate-vecs ftp)
         set)
     :nothing-to-move-here)))

(defn generate-afters-mapentries ; :- {BoundVec st/State} te BoundVecs to takie z generate-vecs
  [{before :before :as ftp}] (let [vcs (generate-vecs ftp)]
                               (if (keyword? vcs) vcs
                                   (->> vcs
                                        (filter #(not (impossibilities %)))
                                        (map (fn [x] [x (after x)]))))))
(defn generate-afters-seq
  [{before :before :as ftp}] (let [vcs (generate-vecs ftp)]
                               (if (keyword? vcs) vcs
                                   (->> vcs
                                        (filter #(not (impossibilities %)))
                                        (map after)))))
(defn generate-afters-set [ftp] (let [afcs (generate-afters-seq ftp)]
                                  (if (keyword? afcs) afcs (set afcs))))
(defn generate-afters-map [ftp] (let [afcs (generate-afters-mapentries ftp)]
                                  (if (keyword? afcs) afcs (into {} afcs))))
(defn after-of-afters [ftp] (let [settt (generate-afters-set ftp)]
                              (if (keyword? settt) settt
                                  (let [sett (filter #(not (contains? impossibilities %)) settt)
                                        _ (if (not= (count sett) 1) (println sett settt))] (first sett)))))
