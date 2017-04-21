-- :name create-st-table :! :raw
create table if not exists c3mst (
       id bigint auto_increment primary key,
       board binary(144) not null,
       moats set('w', 'g', 'b') not null, -- _ _ _ _ _ W G B
       movesnext enum('w', 'g', 'b') not null, -- w1 g2 b3
       castling set('wk', 'wq', 'gk', 'gq', 'bk', 'bq') not null, -- _ _ WK WQ GK GQ BK BQ
       enpassant_prev tinyint,
       enpassant_last tinyint,
       halfmoveclock tinyint not null,
       fullmovenumber smallint not null,
       alive set('w', 'g', 'b') not null, -- _ _ _ _ _ W G B
       constraint everything unique (
                  board, moats, movesnext, castling, enpassant_prev, enpassant_last, halfmoveclock, fullmovenumber, alive
       )
) ENGINE = InnoDB;

-- :name get-st-by-id :? :1
select * from c3mst
where id = :id

-- :name insert-new-st! :i! :n
insert into c3mst (board,
                   moats,
                   movesnext,
                   castling,
                   enpassant_prev,
                   enpassant_last,
                   halfmoveclock,
                   fullmovenumber,
                   alive)
values (:board,
        :moats,
        :movesnext,
        :castling,
        :enpassant_prev,
        :enpassant_last,
        :halfmoveclock,
        :fullmovenumber,
        :alive)
on duplicate key update id = last_insert_id(id);

-- :name create-gp-table :! :raw
create table if not exists c3mgp (
       id bigint auto_increment primary key,
       state bigint not null,
       created timestamp default current_timestamp,
       constraint foreign key (state) references c3mst (id) on update restrict
) ENGINE = InnoDB;

-- :name get-just-gp-by-id :? :1
select * from c3mgp
where id = :id;

-- :name get-gp-by-id :? :1
select gp.id as id, gp.created, gp.state,
                st.board, st.moats, st.movesnext, st.castling, st.enpassant_prev, st.enpassant_last,
                st.halfmoveclock, st.fullmovenumber, st.alive
from c3mgp gp
join c3mst st on st.id = gp.state;

-- :name insert-new-gp! :i! :n
insert into c3mgp (state) values(:state);

-- :name create-mv-table :! :raw
create table if not exists c3mmv (
       id bigint auto_increment primary key,
       fromto binary(4) not null,
       promotion enum('q', 'b', 'n', 'r'),
       beforegame bigint not null,
       aftergame bigint,--not null,
       -- who,
       constraint foreign key (beforegame) references c3mgp (id) on update restrict
) ENGINE = InnoDB;

-- :name get-just-mv-by-id :? :1
select * from c3mmv where id = :id;

-- :name get-just-mvs-by-before :?
select * from c3mmv where beforegame = :id;

-- :name get-mv-by-id-with-after :! :1
select mv.id, mv.fromto, mv.promotion, mv.beforegame, mv.aftergame,
       be.created as b_created, be.state as b_state,
       af.created as a_created, af.state as a_state,
       bs.board as b_board, bs.moats as b_moats, bs.movesnext as b_movesnext,
       bs.castling as b_castling, bs.enpassant_prev as b_enpassant_prev, bs.enpassant_last as b_enpassant_last,
       bs.halfmoveclock as b_halfmoveclock, bs.fullmovenumber as b_fullmovenumber, bs.alive as b_alive,
       sa.board as a_board, sa.moats as a_moats, sa.movesnext as a_movesnext,
       sa.castling as a_castling, sa.enpassant_prev as a_enpassant_prev, sa.enpassant_last as a_enpassant_last,
       sa.halfmoveclock as a_halfmoveclock, sa.fullmovenumber as a_fullmovenumber, sa.alive as a_alive,
from c3mmv mv
join c3mgp be on mv.beforegame = be.id
left join c3mgp af on mv.aftergame = be.id
join c3mst bs on be.state = bs.id
left join c3mst sa on af.state = sa.id;

-- :name insert-new-mv! :i! :n
insert into c3mmv (fromto, promotion, beforegame, aftergame)
values (:fromto, :prom, :beforegame, :aftergame);
