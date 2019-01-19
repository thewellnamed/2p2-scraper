CREATE TABLE posters (
   poster_id serial,
   name varchar(255) not null,
   orig_id integer
);

CREATE TABLE threads (
   thread_id serial,
   poster_id int not null,
   title text,
   url text,
   created timestamp,
   orig_id integer,
   forum_id integer  
);

CREATE TABLE posts (
   post_id serial primary key,
   thread_id int not null,
   poster_id int not null,
   created timestamp,
   content text,
   comment_id integer,
   forum_id integer,
   word_count integer,
   seq integer,
   fog_index float,
   flesch_ease float,
   flesch_grade float
);

CREATE TABLE words (
   forum_id int not null,
   thread_id int not null,
   poster_id int not null,
   post_id int not null,
   word varchar(20),
   count int
);

