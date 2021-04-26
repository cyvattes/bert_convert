-module(bert_utils).

-include("bert_convert.hrl").
-include_lib("kernel/include/file.hrl").

-export([
  filename/2,
  read_file/1,
  receive_all/1,
  write_file/3,
  write_tables/2,
  write_tables/3
]).

%% public
-spec filename(path(), table()) -> list().

filename(Path, Table) ->
  Path ++ "/" ++ atom_to_list(Table) ++ ".bert2".

-spec read_file(filename()) -> {mtime(), list(table())}.
read_file(Filename) ->
  {ok, File} = file:read_file(Filename),
  {ok, #file_info {mtime = MTime}} = file:read_file_info(Filename, [{time, local}]),
  {MTime, binary_to_term(File)}.

-spec receive_all(integer()) -> ok.
receive_all(0) ->
  ok;
receive_all(N) ->
  receive
    done -> receive_all(N - 1)
  end.

-spec write_file(filename(), mtime(), terms()) -> result().
write_file(Output, MTime, Terms) ->
  Bytes = lists:map(
    fun (Term) ->
      Bin = term_to_binary(Term),
      [encode_varint(size(Bin)), Bin]
    end,
    Terms
  ),
  file:write_file(Output, Bytes, [raw, write]),
  case MTime of
    undefined -> ok;
    _ -> change_mtime(Output, MTime)
  end.

-spec write_tables(path(), list(table())) -> result().
write_tables(Path, Tables) ->
  write_tables(Path, undefined, Tables).

-spec write_tables(path(), mtime(), list(table())) -> result().
write_tables(_, _, []) ->
  ok;
write_tables(Path, MTime, [{Table, Terms} | Tables]) ->
  Output = filename(Path, Table),
  write_file(Output, MTime, Terms),
  write_tables(Path, MTime, Tables).

%% private
-spec change_mtime(filename(), mtime()) -> result().
change_mtime(_, undefined) ->
  ok;
change_mtime(Output, MTime) ->
  file:change_time(Output, MTime).

-spec encode_varint(integer()) -> integer().
encode_varint(I) ->
  encode_varint(I, []).

-spec encode_varint(integer(), list(integer())) -> integer().
encode_varint(I, Acc) when I =< 16#7F ->
  lists:reverse([I | Acc]);
encode_varint(I, Acc) ->
  LastSevenBits = (I - ((I bsr 7) bsl 7)),
  OtherBits = (I bsr 7),
  NewBit = LastSevenBits bor 16#80,
  encode_varint(OtherBits, [NewBit | Acc]).
