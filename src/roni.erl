-module(roni).

-export([
    compile/1,
    compile_file/1,
    render/2,
    render/3,
    render_file/2,
    render_file/3
]).

-include("include/roni.hrl").

compile(Bin) when is_binary(Bin) ->
    case make(Bin, []) of
        {ok, Template} ->
            {ok, Template};
        Else -> Else
    end;
compile(IoList) when is_list(IoList) ->
    make(iolist_to_binary(IoList), []).

compile_file(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    compile(Bin).

render(#roni_template{segs = Segs}, Vars) ->
    [substitute(Seg, Vars) || Seg <- Segs];
render(IoList, Vars) ->
    case compile(IoList) of
        {ok, Template} -> render(Template, Vars);
        Else -> Else
    end.

render(Mod, #roni_template{segs = Segs}, Vars) ->
    [substitute(Seg, {Mod, Vars}) || Seg <- Segs];
render(Mod, IoList, Vars) ->
    case compile(IoList) of
        {ok, Template} -> render(Mod, Template, Vars);
        Else -> Else
    end.

render_file(Filename, Vars) ->
    {ok, Bin} = file:read_file(Filename),
    render(Bin, Vars).

render_file(Mod, Filename, Vars) ->
    {ok, Bin} = file:read_file(Filename),
    render(Mod, Bin, Vars).

% private

make(Text, TerminalSymbols) ->
    make([], Text, TerminalSymbols).

make(Segs, <<>>, []) ->
    {ok, #roni_template{segs = lists:reverse(Segs)}};
make(_, <<>>, _) ->
    {error, unexpected_end_of_template};
make(Segs, <<$~, Bin/binary>>, TerminalSymbols) ->
    <<C, Tail/binary>> = Bin,
    case lists:member(C, TerminalSymbols) of
        true ->
            {ok, C, #roni_template{segs = lists:reverse(Segs)}, Tail};
        false ->
            case scan_for_op(Bin) of
                {ok, Op, Args, Rest} ->
                    make([{Op, Args} | Segs], Rest, TerminalSymbols);
                Else -> Else
            end
    end;
make(Segs, Bin, TerminalSymbols) ->
    {Literal, Rest} = scan_for_literal(Bin),
    make([{literal, Literal} | Segs], Rest, TerminalSymbols).

scan_for_op(Bin) ->
    case scan_for_terminal(Bin) of
        {ok, Arg, Terminal, Rest} ->
            case which_op(Terminal) of
                {ok, Op} -> Op(iolist_to_binary(Arg), Rest);
                Else -> Else
            end;
        Else -> Else
    end.

scan_for_terminal(Bin) ->
    case binary:match(Bin, [<<"~~">>, <<"~">>]) of
        nomatch ->
            {error, unexpected_end_of_template};
        {Pos, Len} ->
            LenToEnd = size(Bin) - (Pos + Len),
            Pre = binary:part(Bin, 0, Pos),
            Rest = binary:part(Bin, Pos + Len, LenToEnd),
            case Len of
                1 ->
                    <<Terminal, Rest2/binary>> = Rest,
                    {ok, Pre, Terminal, Rest2};
                2 ->
                    case scan_for_terminal(Rest) of
                        {ok, Pre2, Terminal, Rest2} ->
                            {ok, [Pre, $~, Pre2], Terminal, Rest2};
                        Else -> Else
                    end
            end
    end.

scan_for_literal(Bin) ->
    case binary:match(Bin, [<<"~~">>, <<"~">>]) of
        nomatch ->
            {Bin, <<>>};
        {Pos, _} ->
            Lit = binary:part(Bin, 0, Pos),
            case binary:part(Bin, Pos, size(Bin) - Pos) of
                <<$~, $~, Rest/binary>> ->
                    {Lit2, Rest2} = scan_for_literal(Rest),
                    {[Lit, $~, Lit2], Rest2};
                <<$~, _/binary>> = Rest ->
                    {Lit, Rest}
            end
    end.

which_op($;) -> {ok, fun basic/2};
which_op($?) -> {ok, fun either/2};
which_op($[) -> {ok, fun list/2};
which_op(${) -> {ok, fun format/2}.

basic(Arg, Rest) ->
    {ok, basic, Arg, Rest}.

either(Cond, Binary) ->
    case make(Binary, [$:, $;]) of
        {ok, $;, True, Rest} ->
            False = #roni_template{segs = []},
            {ok, either, {Cond, True, False}, Rest};
        {ok, $:, True, Rest} ->
            case make(Rest, [$;]) of
                {ok, _, False, Tail} ->
                    {ok, either, {Cond, True, False}, Tail};
                Else -> Else
            end;
        Else -> Else
    end.

list(Arg, Bin) ->
    case make(Bin, [$]]) of
        {ok, _, Template, Rest} -> {ok, list, {Arg, Template}, Rest};
        Else -> Else
    end.

format(Arg, Bin) ->
    case scan_for_terminal(Bin) of
        {ok, FmtTemplate, $}, Rest} ->
            {ok, format, {Arg, FmtTemplate}, Rest};
        {ok, S, Other, Rest} ->
            case format(Arg, Rest) of
                {ok, format, {Arg, S2}, Rest2} ->
                    FmtTemplate = iolist_to_binary([S, $~, Other, S2]),
                    {ok, format, {Arg, FmtTemplate}, Rest2};
                Else -> Else
            end;
        Else -> Else
    end.

substitute({literal, Bin}, _Vars) -> to_b(Bin);
substitute({basic, Var}, Vars) -> to_b(lookup(Var, Vars));
substitute({either, {Cond, TrueTemplate, FalseTemplate}}, Vars) ->
    case lookup(Cond, Vars) of
        true -> render(TrueTemplate, Vars);
        _ -> render(FalseTemplate, Vars)
    end;
substitute({list, {Var, ElementTemplate}}, Vars) ->
    List = lookup(Var, Vars),
    case Vars of
        {Mod, _} ->
            [render(Mod, ElementTemplate, Props) || Props <- List];
        _ ->
            [render(ElementTemplate, Props) || Props <- List]
    end;
substitute({format, {Var, Fmt}}, Vars) ->
    List = lookup(Var, Vars),
    io_lib:format(Fmt, List).


to_b(X) when is_binary(X) -> X;
to_b(X) when is_list(X)   -> iolist_to_binary(X);
to_b(X) when is_atom(X)   -> atom_to_binary(X, utf8);
to_b(X)                   -> iolist_to_binary(io_lib:format(<<"~p">>, [X])).

lookup(Var, {Mod, Vars}) ->
    Exports = Mod:module_info(exports),
    try
        FuncName = binary_to_existing_atom(Var, utf8),
        case lists:member({FuncName, 1}, Exports) of
            true -> Mod:FuncName(Vars);
            false -> lookup(Var, Vars)
        end
    catch
        error:badarg:_Stacktrace -> lookup(Var, Vars)
    end;
lookup(Var, Vars) when is_map(Vars) ->
    maps:get(Var, Vars, undefined);
lookup(Var, Vars) ->
    proplists:get_value(Var, Vars, undefined).
