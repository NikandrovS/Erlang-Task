-module(db).
-author("SN").

-export([new/1, create/2, read/2, update/2, delete/2, close/1]).

-type db() :: binary().

-type db_record() :: {key(), username(), city()}.
-type key() :: integer().
-type username() :: string().
-type city() :: string().

-type reason() :: term().


%% Return error if db exists
-spec new(db()) -> ok | {error, reason()}.

new(Table_Name) ->
  try
    ets:new(Table_Name, [set, ordered_set, named_table]),
    ets:insert(Table_Name, [
        {1, "Bob", "New York"},
        {2, "Helen", "Berlin"},
        {3, "Bill", "Paris"},
        {4, "Kate", "Minsk"},
        {5, "Ivan", "Moscow"}]),
    ok
  catch
    error:X ->
      {error, X}
  end.

%% Return error if the key exists
-spec create(db_record(), db()) -> {ok, db_record()} | {error, reason()}.

create(New_Record, Table_Name) ->
  try
    {Id, Username, City} = New_Record,
    Exist = read(Id, Table_Name),
    if
      Exist == {error, "Key doesn't exist"} -> ets:insert(Table_Name, {Id, Username, City}),
        {ok, New_Record};
      true -> {error, "Key already exists"}
    end
  catch
    error:X ->
      {error, X}
  end.

%% Return error if the key doesn't exist
-spec read(key(), db()) -> {ok, db_record()} | {error, reason()}.

read(Id, Table_Name) ->
  try
    Exist = ets:lookup(Table_Name, Id),
    if
      Exist == [] -> {error, "Key doesn't exist"};
      true -> [Record | _] = Exist,
        {ok, Record}
    end
  catch
    error:X ->
      {error, X}
  end.

%% Return error if the key doesn't exist
-spec update(db_record(), db()) -> {ok, db_record()} | {error, reason()}.

update(New_Record, Table_Name) ->
  try
    {Id, Username, City} = New_Record,
    Exist = read(Id, Table_Name),
    if
      Exist == {error, "Key doesn't exist"} -> Exist;
      true -> ets:insert(Table_Name, {Id, Username, City}),
        {ok, New_Record}
    end
  catch
    error:X ->
      {error, X}
  end.

%% Return error if the key doesn't exist
-spec delete(key(), db()) -> ok | {error, reason()}.

delete(Id, Table_Name) ->
  try
    Exist = read(Id, Table_Name),
    if
      Exist == {error, "Key doesn't exist"} -> Exist;
      true -> ets:delete(Table_Name, Id),
        ok
    end
  catch
    error:X ->
      {error, X}
  end.

close(Table_Name) ->
  ets:delete(Table_Name),
  {ok, "Database was removed"}.
