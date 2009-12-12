-module(db_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(TEST_TABLE, items).
-define(TEST_TABLE_FIELDS, [name, power]).
-record(items, {name, power}).

setup() ->
	db:init().

teardown() ->
	mnesia:delete_table(?TEST_TABLE),
	mnesia:stop().

create_ram_table_test() ->
	setup(),
	?assertEqual({atomic, ok}, db:create_ram_table(?TEST_TABLE, ?TEST_TABLE_FIELDS)),
	teardown().

insert_test() ->
	setup(),
	db:create_ram_table(?TEST_TABLE, ?TEST_TABLE_FIELDS),
	R = #items{name=sword, power=10},
	?assertEqual({atomic, ok}, db:insert(R)),
	teardown().
	
select_test() ->
	setup(),
	db:create_ram_table(?TEST_TABLE, ?TEST_TABLE_FIELDS),
	R = #items{name=sword, power=10},
	db:insert(R),
	?assertEqual([#items{name=sword, power=10}], db:select(qlc:q([X || X <- mnesia:table(items)]))),
	teardown().
