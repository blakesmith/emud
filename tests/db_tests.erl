-module(db_tests).
-include_lib("eunit/include/eunit.hrl").

-define(TEST_TABLE, items).

setup() ->
	db:init().

teardown() ->
	mnesia:delete_table(?TEST_TABLE).

create_ram_table_test() ->
	setup(),
	?assertEqual({atomic, ok}, db:create_ram_table(?TEST_TABLE, [name, power])),
	teardown().
