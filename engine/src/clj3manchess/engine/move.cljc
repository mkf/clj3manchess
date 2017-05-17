(ns clj3manchess.engine.move
  (:require [schema.core :as sh]
            #?(:clj [clojure.spec :as s]
               :cljs [cljs.spec :as s])
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

(s/def ::from ::p/pos)
(s/def ::to ::p/pos)
(s/def ::before ::st/state)
(s/def ::vecmove (s/and ::v/bound (s/keys :req-un [::before])))
(def FromTo {(sh/required-key :from) p/Pos
             (sh/required-key :to) p/Pos})
(s/def ::fromto (s/keys :req-un [::from ::to]))
(def Desc {(sh/required-key :from) p/Pos
           (sh/required-key :to) p/Pos
           (sh/optional-key :prom) (sh/maybe f/PromFigType)})
(s/def ::desc (s/keys :req-un [::from ::to] :opt-un [::v/prom]))
(def DescMove {(sh/required-key :from) p/Pos
               (sh/required-key :to) p/Pos
               (sh/optional-key :prom) (sh/maybe f/PromFigType)
               (sh/required-key :before) st/State})
(s/def ::descmove (s/and ::desc (s/keys :req-un [::before])))
(s/def ::move (s/or :desc ::descmove :vec ::vecmove))

(defn to [move] ;; :- Move]
  (cond (and (not (s/valid? ::v/any move))
             (contains? move :to)) (:to move)
        (map? move) (v/bv-to move)
        :else (println "Is not even a map -- to fn:" move)))
(def m-to to) ;;alias
(s/fdef to :args (s/cat :move ::move) :ret ::v/addvec-ret)

(defn get-bef-sq [move where]
  "Alias for getb with move:before:board as board"
  (-> move :before :board (b/getb where)))
(s/fdef get-bef-sq :args (s/cat :move ::move :where ::p/pos) :ret ::b/sq)

(defn tosq [m] (get-bef-sq m (to m)))
(defn tosq-color [m] (when-let [tosq (tosq m)] (:color tosq)))

(defn is-the-fig-we-cap-not-ours [m]
  "is before:movesnext not equal to color of fig on to square"
  (not= (:moves-next (:before m))
        (:color (get-bef-sq m (to m)))))
(s/fdef is-the-fig-we-cap-not-ours :args (s/cat :m ::move) :ret boolean?)

(defn what [m] (get-bef-sq m (:from m)))
(defn whatype [m] (-> m what :type))
(defn whacolor [m] (-> m what :color))
(defn enp-sq [m] (get-bef-sq m (assoc (to m) 0 3)))
(defn enp-sq-type [m] (-> m enp-sq :type))
(defn enp-sq-color [m] (-> m enp-sq :color))
(defn can-we-en-passant [m]
    (and ;;(nil? to-sq) not really, imagine a gray queen teleported there
         ;;             next to jumped white pawn and black pawn ready to cap
     (= :pawn (enp-sq-type m) (whatype m))
     (st/matching-ep (:en-passant (:before m)) (to m)
                     (whacolor m);(:moves-next (:before m)) this is handled by :not-your-move
                     (enp-sq-color m)))) ;(st/match-ep (:en-passant (:before m)) (to m))]
(s/fdef can-we-en-passant :args (s/cat :m (s/and ::vecmove ::v/pawncap)) :ret boolean?)

(def initially-checked-impossibilities #{:figtype-incapable
                                         :nothing-to-move-here
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
                                      :not-your-move})
(def later-checked-impossibilities #{:we-in-check
                                     :castling-passing-thru-check
                                     :castling-starting-with-check
                                     :initiating-check-thru-moats})
(def impossibilities (se/union initially-checked-impossibilities
                               later-checked-impossibilities))

(def InitiallyCheckedImpossibility (apply sh/enum initially-checked-impossibilities))
(s/def ::initially-checked-impossibility initially-checked-impossibilities)
(def LaterCheckedImpossibility (apply sh/enum later-checked-impossibilities))
(s/def ::later-checked-impossibility later-checked-impossibilities)
(def Impossibility (apply sh/enum impossibilities))
(s/def ::impossibility impossibilities)

(defn figtype-capable?
  ([type m]
   (case type
     :pawn (s/valid? ::v/pawn m)
     :rook (s/valid? (s/and ::v/axis (v/bno :prom)) m)
     :knight (s/valid? ::v/knight m)
     :bishop (s/valid? (s/and ::v/diag (v/bno :prom)) m)
     :queen (s/valid? (s/and ::v/cont (v/bno :prom)) m)
     :king (s/valid? (s/or :castling ::v/castling
                           :cont (s/and ::v/cont v/one-just-abs (v/bno :prom))) m)))
  ([m] (figtype-capable? (whatype m) m)))

(defn mult-cont? [type m] (and (#{:rook :bishop :queen} type)
                               (s/valid? v/more-than-one-abs m)))
(defn nothing-to-move-here? [m] (-> m what nil?))
(defn is-move-figtype-incapable? [m] (not (figtype-capable? m)))
(defn is-move-a-pawncap? [m] (and (= (whatype m) :pawn) (s/valid? ::v/pawncap m)))
(defn cannot-we-en-passant? [m] (and (is-move-a-pawncap? m)
                                     (not (can-we-en-passant m))))
(defn is-there-an-addition-error? [m] (= ::v/addition-error (to m)))
(defn is-move-capturing-own-piece? [m] (= (whacolor m) (tosq-color m)))
(defn more-than-one-abs? [m] (mult-cont? (whatype m) m))
(defn not-all-empties? [m] (not (b/check-empties (-> m :before :board)
                                                 (v/empties-cont-vec m (:from m)))))
(defn is-valid-castling? [m] (s/valid? ::v/castling m))
(defn is-there-no-castling-possibility? [m] (and (is-valid-castling? m)
                                               (not ((-> m :before :castling)
                                                     {:type (:castling m)
                                                      :color (-> m :before :moves-next)}))))
(defn are-not-all-empties? [m] (and (is-valid-castling? m)
                                    (not (b/check-empties (-> m :before :board)
                                                          (->> (-> m :before :castling)
                                                               v/castling-empties
                                                               (map (partial + (* 8 (c/segm (-> m :before :moves-next)))))
                                                               (map (fn [x] [0 x])))))))
(defn moats-vec [m]
  (cond (v/is-filevec? m) (set (v/moats-file-vec (:from m) (abs m) (:plusfile m)))
        (v/is-diagvec? m) (set (filter identity [(v/moat-diag-vec (:from m) (to m) (:plusfile m))]))
        (v/is-knights? m) (set (filter identity [(v/moat-knight-vec (:from m) (to m))]))
        :else #{}))
(defn is-move-capturing-thru-moats? [m] (and (not (nil? (tosq m))) (not (empty? (moats-vec m)))))
(defn is-move-passing-unbridged-moats? [m] (when-let [unbridged (not-empty (-> m :before :moats))]
                                             (some unbridged (moats-vec m))))
(defn is-move-a-pawncont? [m] (or (is-move-a-pawncap? m) ;;not really needed, supposed to speed up things
                                  (and (= (whatype m) :pawn)
                                       (s/valid? ::v/pawncont m))))
(defn has-move-a-wrong-pawn-direction? [m] (and (is-move-a-pawncont? m)
                                                (= (:inward m) (:crossed-center (what m)))))
(defn has-move-a-lack-of-needed-promotion? [m] (and (is-move-a-pawncont? m) (= (p/rank (to m)))
                                                    (not (f/promfigtypes (:prom m)))))
(defn has-move-the-wrong-color-moving? [m] (not= (-> m :before :movesnext) (whatype m)))
(def initial-impossibilities-chain
  [[:nothing-to-move-here nothing-to-move-here?]
   [:figtype-incapable is-move-figtype-incapable?]
   [:cannot-en-passant cannot-we-en-passant?]
   [::v/addition-error is-there-an-addition-error?]
   [:capturing-own-piece is-move-capturing-own-piece?]
   [:not-all-empties not-all-empties?]
   [:no-castling-possibility is-there-no-castling-possibility?]
   [:not-all-empties not-all-empties?]
   [:capturing-thru-moats is-move-capturing-thru-moats?]
   [:passing-unbridged-moats is-move-passing-unbridged-moats?]
   [:wrong-pawn-direction has-move-a-wrong-pawn-direction?]
   [:no-promotion has-move-a-lack-of-needed-promotion?]
   [:not-your-move has-move-the-wrong-color-moving?]])
(defn impos-chain [the-chain]
  #(loop [cha the-chain]
     (if (empty? cha) nil
         (let [[key fun] (first the-chain)]
           (if (fun %) key
               (recur (rest cha)))))))
(s/fdef impos-chain
        :args (s/coll-of (s/tuple keyword? (s/fspec :args ::vecmove
                                                    :ret boolean?))
                         :kind vector? :distinct true :into []))
(def initial-impossibilities-check
  "checks all initially-checked, ending with :no-promotion and :not-your-move"
  (impos-chain initial-impossibilities-chain))

(defn board-after-pawn-cap ;;:- b/Board
  [bef ;;:- b/Board,
                                         from to
                                         ep]
  (let [we (b/getb bef from)]
    (if (and (st/matching-ep ep to (:color we) (:color (b/getb bef [3 (p/file to)])))
             (= (:type (b/getb bef [3 (p/file to)])) :pawn))
      (-> bef
          (b/mov from to)
          (b/clr [3 (p/file to)]))
      (b/mov bef from to))))
(s/fdef board-after-pawn-cap :args (s/cat :bef ::b/any :from ::from :to ::to :ep ::st/en-passant)
        :ret ::b/any)

(defn board-after-pawn-center [bef from to]
  (let [color (:color (b/getb bef from))]
    (-> bef (b/clr from)
        (b/put to {:type :pawn :color color :crossed-center true}))))

(sh/defn board-after-pawn-prom :- b/Board [bef :- b/Board,
                                          from :- p/Pos, to :- p/Pos,
                                          prom :- f/PromFigType]
  (let [color (:color (b/getb bef from))]
    (-> bef
        (b/clr from)
        (b/put to {:type prom :color color}))))

(sh/defn board-after-castling :- b/Board [bef :- b/Board,
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

(sh/defn both-sides-of-a-color-unbridged :- sh/Bool [m :- st/MoatsState, col :- c/Color]
  (and (m col)
       (m (c/next-col col))))

(sh/defn any-side-of-a-color-unbridged :- sh/Bool [m :- st/MoatsState, col :- c/Color]
  (or (m col)
      (m (c/next-col col))))

(sh/defn both-sides-of-a-color-bridged :- sh/Bool [m :- st/MoatsState, col :- c/Color]
  (not (any-side-of-a-color-unbridged m col)))

(sh/defn both-moats-of-a-color :- [(sh/one c/Color "left") (sh/one c/Color "right")] [col :- c/Color]
  [col (c/next-col col)])

(sh/defn after-moats-state :- st/MoatsState [vecmove ;; :- VecMove
                                            , after-board :- b/Board]
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

(sh/defn is-there-a-threat-with-these-vecs :- sh/Bool
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
                  (and (boolean (those-not-disqualifying-threat impos))
                       (s/valid? ::v/bound mov)))))))
(sh/defn is-there-a-threat :- sh/Bool
  ([this :- b/Board, to :- p/Pos, from :- p/Pos, alive :- st/Alive, ep :- st/EnPassant]
   (if-let [fromsq (b/getb this from)]
     (is-there-a-threat this to from alive ep (:type fromsq))))
  ([this :- b/Board, to :- p/Pos, from :- p/Pos, alive :- st/Alive, ep :- st/EnPassant, ft :- f/FigType]
   (let [vecs-seq ((v/vecft (v/tvec ft)) from to)
         vecs-seq (if-not (s/valid? ::v/any vecs-seq) vecs-seq #{vecs-seq})]
     (is-there-a-threat-with-these-vecs this from alive ep vecs-seq))))

(sh/defn are-we-initiating-a-check-thru-moat :- sh/Bool [vec from to who alive ep b]
  (and (not (cond (v/is-filevec? vec) (empty? (v/moats-file-vec from (abs vec) (:plusfile vec)))
                  (v/is-diagvec? vec) (nil? (v/moat-diag-vec from to (:plusfile vec)))
                  (v/is-knights? vec) (nil? (v/moat-knight-vec from to))
                  :else true))
       (some #(is-there-a-threat b (b/where-is-king b %) to alive ep)
             [(c/prev-col who) (c/next-col who)])))

(defonce queenside-rook-pos (v/castling-bef-rook-pos :queenside))
(defonce kingside-rook-pos (v/castling-bef-rook-pos :kingside))

(sh/defn after-castling :- ca/CastlingPossibilities [bef-cas :- ca/CastlingPossibilities
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
(def StateOrImpossibility (sh/conditional keyword? Impossibility :else st/State))
(defn after-sans-eval-death-and-check ;;:- StateOrImpossibility
  [{
    [from-rank from-file :as from] :from
    {:keys [board moats moves-next castling en-passant halfmoveclock fullmovenumber alive] :as before} :before
    :as m} ;; :- VecMove
]
  (if-let [impos (initial-impossibilities-check m)]
    (if-let [impos (:impossibility impos)] impos
     (if-let [to (v/addvec m from)]
       (if (= to ::v/addition-error) to
           (let [what (b/getb board from)
                 tosq (b/getb board to)
                 whatype (:type what)
                 new-board (asedac-new-board m board moves-next whatype from to en-passant)
                 new-en-passant (after-en-passant en-passant m from)]
             (if (are-we-initiating-a-check-thru-moat m from to moves-next alive new-en-passant new-board)
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
                  :alive alive})))) (println m from)))))
(s/fdef after-sans-eval-death-and-check ::args (sh/cat :m ::vecmove))
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

(defn can-i-move-wo-check ;;:- sh/Bool
  ([sta who]
   (some #(can-i-move-wo-check sta who %) (b/where-are-figs-of-color (:board sta) who)))
  ([sta who from] (let [type-of-fig-there (:type (b/getb (:board sta) from))
                        tvec              (v/tvec type-of-fig-there)
                        vecft             (v/vecftset tvec)] (-> #(can-i-move-wo-check sta who from vecft %)
                                                              (some (AMFT from)))))
  ([sta who from vecft to] (when-not (nil? to)
                             (let [vecft (vecft from to)
                                   vecft (if (and (coll? vecft) (not (s/valid? ::v/any vecft))) vecft #{vecft})]
                               (when-not (->> vecft
                                              (filter (complement nil?))
                                              empty?) (->> vecft
                                                           (some #(can-i-move-wo-check sta who from vecft to %))
                                                           )))))
  ([sta who from vecft to vect] (let [afterr        (after-sans-eval-death-and-check
                                                     (assoc vect :before sta :from from))
                                      not-nil       (not (nil? afterr))
                                      not-impos     (not (impossibilities afterr))
                                      just-ok-impos (#{:not-your-move :no-promotion} afterr)]
                                  (and not-nil (or not-impos just-ok-impos)))))
(s/fdef can-i-move-wo-check :args (s/or :exported (s/cat :sta ::st/state :who ::c/color)
                                        :internal1 (s/cat :sta ::st/state :who ::c/color :from ::from)
                                        :internal2 (s/cat :sta ::st/state :who ::c/color :from ::from
                                                          :vecft (s/fspec :args (s/cat :from ::from :to ::to)
                                                                          :ret (s/coll-of
                                                                                (s/and ::v/any (v/bno :prom))
                                                                                :kind set? :max-count 8
                                                                                :distinct true :into #{})) :to ::to)
                                        :internal3 (s/cat :sta ::st/state :who ::c/color :from ::from
                                                          :vecft (s/fspec :args (s/cat :from ::from :to ::to)
                                                                          :ret (s/coll-of
                                                                                (s/and ::v/any (v/bno :prom))
                                                                                :kind set? :max-count 8
                                                                                :distinct true :into #{})) :to ::to
                                                          :vect (s/and ::v/any (v/bno :prom)))))

(sh/defn eval-death :- st/State [sta :- st/State]
  (let [;; noking (-> (partial contains? (b/where-are-kings (:board sta)))
        ;;            complement
        ;;            (filter c/colors)
        ;;            set)
        noking (-> (filter #((complement contains?) (b/where-are-kings (:board sta)) %) c/colors) set)
        ;; checkmate (-> (partial can-i-move-wo-check sta)
        ;;               complement
        ;;               (filter c/colors)
        ;;               set)
        checkmate (-> (filter #(not (can-i-move-wo-check sta %)) c/colors) set)
        died (set/union noking checkmate)
        _ (println (prn-str [noking checkmate]))]
    (assoc sta :alive
           (-> sta
                :alive
                set
                (set/difference died)
                set))))
(sh/defn threat-checking :- [p/Pos] [board :- b/Board, where :- p/Pos, alive :- st/Alive, ep :- st/EnPassant]
  (let [who (:color (b/getb board where))]
    (->> p/all-pos
         (map #(if-let [tjf (b/getb board %)]
                 (when (and (not= (:color tjf) who)
                            (alive (:color tjf))
                            (is-there-a-threat board % alive ep (:type tjf))) %)))
         (filter identity))))
(sh/defn check-checking :- [p/Pos] [board :- b/Board, who :- c/Color, alive :- st/Alive]
  (when (alive who) (if-let [king-pos (b/where-is-king board who)]
                      (threat-checking board king-pos alive {}))))
(defn new-king-pos-and-new-rook-pos [color castling]
  (let [color-segm*8 (* 8 (c/segm color))
        kfm-on-segm (+ p/kfm color-segm*8)
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
(defn after-sans-eval-death ;;:- StateOrImpossibility
  [{
    [from-rank from-file :as from] :from
    {:keys [board moats moves-next castling en-passant halfmoveclock fullmovenumber alive] :as before} :before
    :as m} ;; :- VecMove
]
  (let [nxtcolmvs (c/next-col moves-next)
        prvcolmvs (c/prev-col moves-next)
        to (v/addvec m from)]
    (if (= ::v/addition-error to) to
      (let [what (b/getb board from)
           tosq (b/getb board to)
           whatype (:type what)
           {:as new-state-after
            new-board :board
            new-en-passant :en-passant} (after-sans-eval-death-and-check m)]
       (if-not (v/is-castvec? m) new-state-after
               (cond (not (empty? (check-checking board moves-next alive))) :castling-over-check
                     (not (empty? (check-checking new-board moves-next alive))) :we-in-check
                     (not (empty? (check-checking (apply (partial b/mov new-board)
                                                         (new-king-pos-and-new-rook-pos moves-next (:castling m)))
                                                  moves-next alive))) :castling-over-check
                     :else new-state-after))))))
(sh/defn after [m ;; :- VecMove
]
  (let [sans-check (after-sans-eval-death m)]
    (if-not (map? sans-check) sans-check
            (let [{:keys [board moves-next alive]} sans-check
                  we (c/prev-col moves-next)]
              (if-not (empty? (check-checking board we alive)) :we-in-check
                      (eval-death sans-check))))))
(sh/defn generate-vecs ; :- #{BoundVec}
  ([figtype :- f/FigType
    {:keys [from prom] [rank-to :as to] :to :as ftp}] ;;:- (sh/either Desc DescMove)]
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
