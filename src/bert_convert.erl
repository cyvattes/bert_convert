-module(bert_convert).

-include("bert_convert.hrl").
%-include("bert_utils.erl").
-include_lib("kernel/include/file.hrl").

-export([
  to_bert2/1
]).

%% term to bert2
-spec to_bert2(Filenames) -> result() when
  Filenames :: filename() | list(filename()).
to_bert2([]) ->
  ok;
to_bert2([X |_] = Wildcard) when is_integer(X) ->
  Filenames = filelib:wildcard(Wildcard),
  to_bert2(Filenames);
to_bert2([X | _] = Filenames) when is_list(X) ->
  PID = self(),
  lists:foreach(fun(Filename) ->
    spawn(fun() ->
      case filename:extension(Filename) of
        ".bert" -> bert_to_bert2(Filename);
        ".term" -> term_to_bert2(Filename);
        _ -> ok
      end,
      PID ! done
          end)
                end, Filenames),
  bert_utils:receive_all(length(Filenames)).

-spec bert_to_bert2(filename()) -> result().
bert_to_bert2(Filename) ->
  Path = filename:dirname(Filename),
  try
    bert_utils:read_file(Filename)
  of
    {MTime, Tables} -> bert_utils:write_tables(Path, MTime, Tables)
  catch
      C:E:S -> luger:error(
        "bert_convert",
        "Unable to process ~s.bert ~p",
        [Filename, {C, E, S}]
      )
  end.

-spec term_to_bert2(filename()) -> result().
term_to_bert2(Filename) ->
  Path = filename:dirname(Filename),
  {ok, Tables} = file:consult(Filename),
  bert_utils:write_tables(Path, Tables).
