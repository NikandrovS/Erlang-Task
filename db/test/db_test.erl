-module(db_test).

-include_lib("eunit/include/eunit.hrl").

crud_test() ->
    Record1 = {6, "Andrew", "Berlin"},
    Record2 = {6, "Mary", "Rome"},
    {Id, _, _} = Record2,
    Base_Name = myBase,
    ?assertEqual(ok, db:new(Base_Name)),
    ?assertEqual({ok, Record1}, db:create(Record1, Base_Name)),
    ?assertEqual({error, "Key already exists"}, db:create(Record1, Base_Name)),
    ?assertEqual({ok, Record2}, db:update(Record2, Base_Name)),
    ?assertEqual({ok, Record2}, db:read(Id, Base_Name)),
    ?assertEqual(ok, db:delete(Id, Base_Name)),
    ?assertEqual({error, "Key doesn't exist"}, db:delete(Id, Base_Name)),
    ?assertEqual({error, "Key doesn't exist"}, db:read(Id, Base_Name)),
    ?assertEqual({error, "Key doesn't exist"}, db:update(Record2, Base_Name)),
    ?assertEqual({ok, "Database was removed"}, db:close(Base_Name)),
    ok.
