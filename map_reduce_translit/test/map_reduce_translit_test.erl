-module(map_reduce_test_translit).

-include_lib("eunit/include/eunit.hrl").

file5_test() ->
    Data = map_reduce:start(["data1.txt",
                             "data2.txt",
                             "data3.txt",
                             "data4.txt",
                             "data5.txt"]),
    ?assertEqual(3, maps:get("a", Data)),
    ?assertEqual(1, maps:get("benzopila", Data)),
    ?assertEqual(4, maps:get("v", Data)),
    ?assertEqual(2, maps:get("vsluh", Data)),
    ?assertEqual(1, maps:get("zaintrigovan", Data)),
    ?assertEqual(1, maps:get("carya", Data)),
    ok.


file2_test() ->
    Data = map_reduce:start(["data1.txt", "data2.txt"]),
    ?assertEqual(2, maps:get("a", Data)),
    ?assertEqual(1, maps:get("benzopila", Data)),
    ?assertEqual(2, maps:get("v", Data)),
    ?assertEqual(error, maps:find("vsluh", Data)),
    ?assertEqual(1, maps:get("zaintrigovan", Data)),
    ?assertEqual(1, maps:get("carya", Data)),
    ok.


file1_test() ->
    Data = map_reduce:start(["data1.txt", "data777.txt"]),
    ?assertEqual(error, maps:find("a", Data)),
    ?assertEqual(1, maps:get("benzopila", Data)),
    ?assertEqual(2, maps:get("v", Data)),
    ?assertEqual(error, maps:find("vsluh", Data)),
    ?assertEqual(1, maps:get("zaintrigovan", Data)),
    ?assertEqual(error, maps:find("carya", Data)),
    ok.
