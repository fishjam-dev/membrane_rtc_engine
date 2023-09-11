%% @private
%%% ===================================================================
%%% Version utils
%%% ===================================================================
-module(rebar3_hex_version).

-export([parse/1, increment/2, format/1]).

-type increment_type() :: patch | minor | major.

-spec parse(string() | binary()) -> {ok, verl:version_t()} | {error, term()}.
parse(Version) when is_list(Version) ->
    parse(rebar_utils:to_binary(Version));
parse(Version) when is_binary(Version) ->
    verl:parse(Version).

-spec increment(increment_type(), verl:version_t()) -> verl:version_t().
increment(patch, #{major := Maj, minor := Min, patch := Patch} = Version) ->
    Version#{major => Maj, minor => Min, patch => Patch + 1};
increment(minor, #{major := Maj, minor := Min} = Version) ->
    Version#{major => Maj, minor => Min + 1, patch => 0};
increment(major, #{major := Maj} = Version) ->
    Version#{major => Maj + 1, minor => 0, patch => 0}.

-spec format(verl:version_t()) -> binary().
format(#{major := Maj, minor := Min, patch := Patch}) ->
    MajBin = integer_to_binary(Maj),
    MinBin = integer_to_binary(Min),
    PatchBin = integer_to_binary(Patch),
    DotBin = <<".">>,
    <<MajBin/binary, DotBin/binary, MinBin/binary, DotBin/binary, PatchBin/binary>>.
