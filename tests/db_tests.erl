-module(db_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(TEST_TABLE, items).
-define(TEST_TABLE_FIELDS, [name, power]).
-record(items, {name, power}).

setup() ->
	db:init().

teardown(_Value) ->
	mnesia:delete_table(?TEST_TABLE).

create_ram_table_test_() ->
	{"Creates an mnesia ram table", 
		setup, fun setup/0, fun teardown/1,
		fun() ->
			?assertEqual({atomic, ok}, db:create_ram_table(?TEST_TABLE, ?TEST_TABLE_FIELDS))
		end
	}.

insert_test_() ->
	{"Inserts an item record into a ram table", 
		setup, fun setup/0, fun teardown/1,
		fun() ->
			db:create_ram_table(?TEST_TABLE, ?TEST_TABLE_FIELDS),
			R = #items{name=sword, power=10},
			?assertEqual({atomic, ok}, db:insert(R))
		end
	}.
	
select_test_() ->
	{"Selects an item record from the ram table", 
		setup, fun setup/0, fun teardown/1,
		fun() ->
			db:create_ram_table(?TEST_TABLE, ?TEST_TABLE_FIELDS),
			R = #items{name=sword, power=10},
			db:insert(R),
			?assertEqual([#items{name=sword, power=10}], db:select(qlc:q([X || X <- mnesia:table(items)])))
		end
	}.
