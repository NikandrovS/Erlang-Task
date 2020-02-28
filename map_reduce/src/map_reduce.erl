-module(map_reduce).
-author("SN").

-export([start/1]).

start(Files) ->
  Map = fun read_words/2,
  Reduce = fun count_words/2,
  M = maps:new(),
  % run map
  MapCount = run_map(Files, Map),
  MapResults = lists:sort(
    fun({LeftKey, _}, {RightKey, _}) -> LeftKey =< RightKey end,
    wait_results(MapCount, [])),
  % run reduce
  ReduceCount = run_reduce(MapResults, Reduce),
  ResultList = wait_results(ReduceCount, []),
  get_map(ResultList, M).

run_map(Files, Fun) ->
  lists:foreach(
    fun(Element) -> spawn_worker(Element, Fun) end, Files),
  length(Files).


run_reduce([{Key, Value} | SortedList], Fun) ->
  run_reduce(Key, [Value], SortedList, Fun, 0).

run_reduce(Key, ValueList,
    [{Key, Value} | SortedList], Fun, Count) ->
  run_reduce(Key, [Value | ValueList],
    SortedList, Fun, Count);

run_reduce(Key, ValueList,
    [{NewKey, Value} | SortedList], Fun, Count) ->
  spawn_worker({Key, ValueList}, Fun),
  run_reduce(NewKey, [Value], SortedList, Fun, Count + 1);

run_reduce(Key, ValueList, [], Fun, Count) ->
  spawn_worker({Key, ValueList}, Fun),
  Count + 1.


spawn_worker(Element, Fun) ->
  CurrentPID = self(),
  Emit =
    fun(Key, Val) ->
      CurrentPID ! {Key, Val}
    end,

  Worker =
    fun() ->
      Fun(Element, Emit),
      CurrentPID ! worker_done
    end,
  spawn(fun() -> Worker() end).


wait_results(0, Results) ->
  Results;
wait_results(Count, Results) ->
  receive
    {Key, Val} ->
      wait_results(Count, [{Key, Val} | Results]);
    worker_done ->
      wait_results(Count - 1, Results)
  end.


read_words(FileName, Emit) ->
  {Status, FileContent} = file:read_file(FileName),
  if
    Status == ok ->
      Words = string:tokens(erlang:binary_to_list(FileContent), " \t\n\r,.;:-!?\"'()");
    true -> Words = [{cant_get_file, FileName}]
  end,
  lists:foreach(
    fun(Word) ->
      if
        Word /= "" -> Emit(Word, 1);
        Word == "" -> false
      end
    end, Words).

count_words({Word, Counts}, Emit) ->
  Emit(Word, length(Counts)).


get_map([], Map) -> Map;

get_map([{Word, Num} | Rest], Map) ->
  NewMap = maps:put(Word, Num, Map),
  get_map(Rest, NewMap).
