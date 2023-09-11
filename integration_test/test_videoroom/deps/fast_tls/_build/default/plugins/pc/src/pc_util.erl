-module(pc_util).

-export([ replace_extension/2
        , replace_extension/3
        , target_type/1
        , get_arch/0
        , wordsize/0
        , is_arch/1
        , strtok/2
        , strjoin/2
        , strchr/2
        ]).
-export_type([]).

%%%===================================================================
%%% API
%%%===================================================================

replace_extension(File, NewExt) ->
    OldExt = filename:extension(File),
    replace_extension(File, OldExt, NewExt).
replace_extension(File, OldExt, NewExt) ->
    filename:rootname(File, OldExt) ++ NewExt.

target_type(Target)  -> target_type1(filename:extension(Target)).
target_type1(".so")  -> drv;
target_type1(".dll") -> drv;
target_type1("")     -> exe;
target_type1(".exe") -> exe.

is_arch(ArchRegex) ->
    case re:run(get_arch(), ArchRegex, [{capture, none}]) of
        match ->
            true;
        nomatch ->
            false
    end.

%%
%% REBAR_TARGET_ARCH, if used, should be set to the "standard"
%% target string. That is a prefix for binutils tools.
%% "x86_64-linux-gnu" or "arm-linux-gnueabi" are good candidates
%% ${REBAR_TARGET_ARCH}-gcc, ${REBAR_TARGET_ARCH}-ld ...
%%
get_arch() ->
    Arch = os:getenv("REBAR_TARGET_ARCH"),
    Words = wordsize(Arch),
    rebar_utils:otp_release() ++ "-" ++ get_system_arch(Arch) ++ "-" ++ Words.

get_system_arch(Arch) when Arch =:= false; Arch =:= "" ->
    erlang:system_info(system_architecture);
get_system_arch(Arch) ->
    Arch.

wordsize() ->
    wordsize(os:getenv("REBAR_TARGET_ARCH")).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

wordsize(Arch) when Arch =:= false; Arch =:= "" ->
    native_wordsize();
wordsize(Arch) ->
    AllArchs = [
                {"i686","32"},
                {"i386","32"},
                {"arm","32"},
                {"aarch64", "64"},
                {"x86_64","64"}
               ],
    case match_wordsize(Arch, AllArchs) of
        false ->
            case cross_wordsize(Arch) of
                "" ->
                    env_wordsize(os:getenv("REBAR_TARGET_ARCH_WORDSIZE"));
                WordSize ->
                    WordSize
            end;
        {_, Wordsize} ->
            Wordsize
    end.

match_wordsize(Arch, [V={Match,_Bits}|Vs]) ->
    case re:run(Arch, Match, [{capture, none}]) of
        match ->
            V;
        nomatch ->
            match_wordsize(Arch, Vs)
    end;
match_wordsize(_Arch, []) ->
    false.

env_wordsize(Wordsize) when Wordsize =:= false;
                            Wordsize =:= "" ->
    rebar_log:log(
      warn, "REBAR_TARGET_ARCH_WORDSIZE not set, assuming 32\n", []),
    "32";
env_wordsize(Wordsize) ->
    case Wordsize of
        "16" -> Wordsize;
        "32" -> Wordsize;
        "64" -> Wordsize;
        _ ->
            rebar_log:log(
              warn, "REBAR_TARGET_ARCH_WORDSIZE bad value: ~p\n", [Wordsize]),
            "32"
    end.

%%
%% Find out the word size of the target by using Arch-gcc
%%
cross_wordsize(Arch) ->
    cross_sizeof(Arch, "void*").

%%
%% Find the size of target Type using a specially crafted C file
%% that will report an error on the line of the byte size of the type.
%%
cross_sizeof(Arch, Type) ->
    Compiler = if Arch =:= "" -> "cc";
                  true -> Arch ++ "-gcc"
               end,
    TempFile = mktempfile(".c"),
    ok = file:write_file(TempFile,
                         <<"int t01 [1 - 2*(((long) (sizeof (TYPE))) == 1)];\n"
                           "int t02 [1 - 2*(((long) (sizeof (TYPE))) == 2)];\n"
                           "int t03 [1 - 2*(((long) (sizeof (TYPE))) == 3)];\n"
                           "int t04 [1 - 2*(((long) (sizeof (TYPE))) == 4)];\n"
                           "int t05 [1 - 2*(((long) (sizeof (TYPE))) == 5)];\n"
                           "int t06 [1 - 2*(((long) (sizeof (TYPE))) == 6)];\n"
                           "int t07 [1 - 2*(((long) (sizeof (TYPE))) == 7)];\n"
                           "int t08 [1 - 2*(((long) (sizeof (TYPE))) == 8)];\n"
                           "int t09 [1 - 2*(((long) (sizeof (TYPE))) == 9)];\n"
                           "int t10 [1 - 2*(((long) (sizeof (TYPE))) == 10)];\n"
                           "int t11 [1 - 2*(((long) (sizeof (TYPE))) == 11)];\n"
                           "int t12 [1 - 2*(((long) (sizeof (TYPE))) == 12)];\n"
                           "int t13 [1 - 2*(((long) (sizeof (TYPE))) == 13)];\n"
                           "int t14 [1 - 2*(((long) (sizeof (TYPE))) == 14)];\n"
                           "int t15 [1 - 2*(((long) (sizeof (TYPE))) == 15)];\n"
                           "int t16 [1 - 2*(((long) (sizeof (TYPE))) == 16)];\n"
                         >>),
    Cmd = Compiler ++ " -DTYPE=\""++Type++"\" " ++ TempFile,
    ShOpts = [{use_stdout, false}, return_on_error],
    {error, {_,Res}} = rebar_utils:sh(Cmd, ShOpts),
    ok = file:delete(TempFile),
    case strtok(Res, ":") of
        [_, Ln | _] ->
            try list_to_integer(Ln) of
                NumBytes -> integer_to_list(NumBytes*8)
            catch
                error:_ ->
                    ""
            end;
        _ ->
            ""
    end.

strtok(Str, SepList) ->
    case erlang:function_exported(string, lexemes, 2) of
        true -> string:lexemes(Str, SepList);
        false -> apply(string, tokens, [Str, SepList])
    end.

%% Lifted lists:join/2 from OTP-19 to use with OTP-17 and OTP-18.
%% TODO: remove when pc requires OTP >=19.
lists_join(Sep, L) ->
    case erlang:function_exported(lists, join, 2) of
        true -> lists:join(Sep, L);
        false -> lists_join1(Sep, L)
    end.

lists_join1(_Sep, []) -> [];
lists_join1(Sep, [H|T]) -> [H|lists_join1_prepend(Sep, T)].

lists_join1_prepend(_Sep, []) -> [];
lists_join1_prepend(Sep, [H|T]) -> [Sep,H|lists_join1_prepend(Sep,T)].

strjoin(L, Sep) ->
    lists:flatten(lists_join(Sep, L)).

strchr(S, C) when is_integer(C) -> strchr(S, C, 1).
strchr([C|_Cs], C, I) -> I;
strchr([_|Cs], C, I) -> strchr(Cs, C, I+1);
strchr([], _C, _I) -> 0.

mktempfile(Suffix) ->
    {A,B,C} = rebar_now(),
    Dir = temp_dir(),
    File = "rebar_"++os:getpid()++
        integer_to_list(A)++"_"++
        integer_to_list(B)++"_"++
        integer_to_list(C)++Suffix,
    filename:join(Dir, File).

temp_dir() ->
    case os:type() of
        {win32, _} -> windows_temp_dir();
        _ -> "/tmp"
    end.

windows_temp_dir() ->
    case os:getenv("TEMP") of
        false ->
            case os:getenv("TMP") of
                false -> "C:/WINDOWS/TEMP";
                TMP -> TMP
            end;
        TEMP -> TEMP
    end.

rebar_now() ->
    case erlang:function_exported(erlang, timestamp, 0) of
        true ->
            erlang:timestamp();
        false ->
            %% erlang:now/0 was deprecated in 18.0. One solution to avoid the
            %% deprecation warning is to use
            %% -compile({nowarn_deprecated_function, [{erlang, now, 0}]}), but
            %% that would raise a warning in versions older than 18.0.  Calling
            %% erlang:now/0 via apply/3 avoids that.
            apply(erlang, now, [])
    end.

native_wordsize() ->
    try erlang:system_info({wordsize, external}) of
        Val ->
            integer_to_list(8 * Val)
    catch
        error:badarg ->
            integer_to_list(8 * erlang:system_info(wordsize))
    end.
