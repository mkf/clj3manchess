-- :name create-state-table
-- :command :execute
-- :result :raw
create table if not exists c3mst (
       id bigint auto_increment primary key,
       board binary(144), --null is newgame; input: best in hex; zeros are empty squares
       moats tinyint not null, -- _ _ _ _ _ W G B
       movesnext tinyint not null, -- w1 g2 b3
       castling tinyint not null, -- _ _ WK WQ GK GQ BK BQ
       enpassant_prev tinyint,
       enpassant_last tinyint,
       halfmoveclock tinyint not null,
       fullmovenumber smallint not null,
       alive tinyint not null, -- _ _ _ _ _ W G B
       constraint everything unique (
                  board, moats, movesnext, castling, enpassant_prev, enpassant_last, halfmoveclock, fullmovenumber, alive
       )
) ENGINE = InnoDB;

-- :name create-gp-table
-- :command :execute
-- :result :raw
create table if not exists c3mgp (
       id bigint auto_increment primary key,
       state bigint,
       created timestamp default current_timestamp,
       constraint foreign key (state) references c3mst (id) on update restrict
) ENGINE = InnoDB;
