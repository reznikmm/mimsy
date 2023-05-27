create table users (
  nickname varchar primary key,
  name     varchar not null,
  avatar   varchar,
  created timestamp default current_timestamp);

create table emails (
  email    varchar primary key,
  nickname varchar not null references users,
  main     char(1));

create table game_stations (
  station varchar not null primary key,
  name    varchar not null
);

insert into game_stations values ('library','Elementary');

create table game_missions (
  mission varchar not null primary key,
  station varchar not null references game_stations,
  name    varchar not null,
  points  integer not null
);

create table game_mission_votes (
  nickname varchar not null references users,
  mission  varchar not null references game_missions,
  vote     integer not null,
  primary key (nickname, mission)
);

create table solved_missions (
  nickname varchar not null references users,
  mission  varchar not null references game_missions,
  solved   timestamp not null default current_timestamp,
  primary key (nickname, mission)
);

create table solution_texts (
  nickname varchar not null references users,
  mission  varchar not null references game_missions,
  line     integer not null,
  text     varchar not null,
  primary key (nickname, mission, line)
);

insert into game_missions (mission, station, name, points) values
('multiply-intro', 'library', 'Multiply (Intro)', 5);

insert into game_missions (mission, station, name, points) values
('easy-unpack', 'library', 'Easy Unpack', 5);

insert into game_missions (mission, station, name, points) values
('first-word-simplified', 'library', 'First Word (simplified)', 5);

insert into game_missions (mission, station, name, points) values
('acceptable-password-i', 'library', 'Acceptable Password I', 5);

insert into game_missions (mission, station, name, points) values
('number-length', 'library', 'Number Length', 5);

insert into game_missions (mission, station, name, points) values
('end-zeros', 'library', 'End Zeros', 5);

insert into game_missions (mission, station, name, points) values
('backward-string', 'library', 'Backward String', 5);

insert into game_missions (mission, station, name, points) values
('remove-all-before', 'library', 'Remove All Before', 5);

insert into game_missions (mission, station, name, points) values
('all-upper-i', 'library', 'All Upper I', 5);

commit;