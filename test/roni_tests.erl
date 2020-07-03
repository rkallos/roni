-module(roni_tests).

-include_lib("eunit/include/eunit.hrl").

-include("include/roni.hrl").

% the following exports are for testing with a callback module
-export([
    badarg/1,
    bar/1,
    items/1
]).

-define(template(Segs), #roni_template{segs = Segs}).

-define(scan_test(Template, Expected), ?_test(begin
    Actual = roni:make(Template, []),
    Expected = Actual
end)).

scan_test_() -> [
    ?scan_test(<<"foobar">>, {ok, ?template([
        {literal, <<"foobar">>}
    ])}),
    ?scan_test(<<"foo~bar~;">>, {ok, ?template([
        {literal, <<"foo">>},
        {basic, <<"bar">>}
    ])}),
    ?scan_test(<<"foo~bar~?baz~;">>, {ok, ?template([
        {literal, <<"foo">>},
        {either, {<<"bar">>, ?template([{literal, <<"baz">>}]), ?template([])}}
    ])}),
    ?scan_test(<<"foo~bar~?baz~:quux~;">>, {ok, ?template([
        {literal, <<"foo">>},
        {either, {
            <<"bar">>,
            ?template([{literal, <<"baz">>}]),
            ?template([{literal, <<"quux">>}])
        }}
    ])}),
    ?scan_test(<<"foo~bar~[ ~baz~;~]">>, {ok, ?template([
        {literal, <<"foo">>},
        {list, {<<"bar">>, ?template([{literal, <<" ">>}, {basic, <<"baz">>}])}}
    ])}),
    ?scan_test(<<"foo~bar~{~p~}">>, {ok, ?template([
        {literal, <<"foo">>},
        {format, {<<"bar">>, <<"~p">>}}
    ])})
].

-define(substitute_test(Template, Vars, Expected), ?_test(begin
    Actual = roni:render(Template, Vars),
    Expected = iolist_to_binary(Actual)
end)).

substitute_test_() -> [
    ?substitute_test(
        <<"foobar">>,
        [],
        <<"foobar">>
    ),
    ?substitute_test(
        <<"foo~bar~;">>,
        #{<<"bar">> => <<"bleh">>},
        <<"foobleh">>
    ),
    ?substitute_test(
        <<"foo~bar~?baz~;">>,
        #{<<"bar">> => true},
        <<"foobaz">>
    ),
    ?substitute_test(
        <<"foo~bar~?baz~:quux~;">>,
        #{},
        <<"fooquux">>
    ),
    ?substitute_test(
        <<"foo~bar~[ ~baz~;~]">>,
        #{<<"bar">> => [
            #{<<"baz">> => 1},
            #{<<"baz">> => 2},
            #{<<"baz">> => 3}
        ]},
        <<"foo 1 2 3">>
    ),
    ?substitute_test(
        <<"foo~bar~{~p~}">>,
        #{<<"bar">> => [[a,b,c]]},
        <<"foo[a,b,c]">>
    )
].

-define(mod_test(Template, Vars, Expected), ?_test(begin
    Actual = roni:render(Template, Vars),
    Expected = iolist_to_binary(Actual)
end)).

badarg(_) -> <<"baz">>.
bar(_) -> <<"baz">>.
items(_) -> [
    #{<<"num">> => 1},
    #{<<"num">> => 2},
    #{<<"num">> => 3}
].

mod_test_() -> [
    ?mod_test(
        <<"foo~bar~;">>,
        {?MODULE, []},
        <<"foobaz">>
    ),
    ?mod_test(
        <<"foo~blarg~;">>,
        {?MODULE, #{<<"blarg">> => <<"baz">>}},
        <<"foobaz">>
    ),
    ?mod_test(
        <<"foo~badarg~;">>,
        {?MODULE, []},
        <<"foobaz">>
    ),
    ?mod_test(
        <<"foo~items~[ ~num~;~]">>,
        {?MODULE, []},
        <<"foo 1 2 3">>
    )
].
