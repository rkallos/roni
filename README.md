roni
=====

Erlang string templating with macaroni-filled syntax. Based on [dactyl](https://github.com/basho/dactyl).

Build
-----

    $ rebar3 compile

Usage
-----

Compile a binary into a template:

    1> {ok, Template} = roni:compile(<<"my ~adjective~; template">>).

Render a template:

    2> IoList = roni:render(Template, #{<<"adjective">> => <<"noodly">>}).
    [<<"my ">>, <<"noodly">>, <<" template">>]
    3> iolist_to_binary(IoList).
    <<"my noodly template">>

Render a template without compiling it first:

    4> TemplateBin = <<"my ~adjective~; template">>.
    5> Bindings = [{<<"adjective">>, <<"noodly">>}].
    6> Rendered = iolist_to_binary(roni:render(TemplateBin, Bindings)).

You can also compile and/or render templates located in files with
`compile_file/1` and `render_file/2`.

Differences from dactyl
-----------------------

Roni makes extensive use of binaries and iolists, opting to use sub-binaries
instead of copying from the template string. Roni also adds the ability to use
maps for bindings instead of property lists.

Template syntax
---------------

100% ~~stolen~~ lovingly borrowed from dactyl, which supposedly borrowed from Common Lisp's `FORMAT` function.

Basic substitution:

    roni:render(<<"my ~adj~; template">>, #{<<"adj">> => <<"noodly">>})
    [<<"my ">>,<<"noodly">>,<<" template">>]


Conditional substitution:

    1> N = 5.
    2> roni:render(<<"I've got ~any~?~n~;~:zero~; problems">>,
           #{<<"any">> => N > 0, <<"n">> => N}).
    [<<"I've got ">>,[<<"5">>],<<" problems">>]

    3> roni:render(<<"dramatic pause: ~go~?you won't see this~;">>,
           #{<<"go">> => false}).
    [<<"dramatic pause: ">>,[]]

List substitution:

    1> Bindings = #{<<"things">> => [#{<<"num">> => 1},
                                     #{<<"num">> => 2},
                                     #{<<"num">> => 3}]}.
    2> Template = <<"my things: ~things~[Thing ~num~;. ~]">>.
    2> iolist_to_binary(roni:render(Template, Bindings)).
    <<"my things: Thing 1. Thing 2. Thing 3. ">>

Substitution with custom formatting (using `io_lib:format/2`):

    1> Bindings = #{<<"terms">> => [{a, 1}, {b, 2}, {c, 3}]}.
    2> Template = <<"my terms: ~terms~{~n1. ~p~n2. ~p~n3. ~p~n~}">>.
    3> iolist_to_binary(roni:render(Template, Bindings)).
    <<"my terms: \n1. {a,1}\n2. {b,2}\n3. {c,3}\n">>

Templating with a callback module
---------------------------------

Like [dactyl](https://github.com/basho/dactyl), roni allows the user to pass an
Erlang module in order to perform substitution. Bindings passed to `roni:render`
will be checked against the exports list of the callback module. Callback
functions must accept a single argument: The map of all bindings passed to
`roni:render`.

```erlang
-module(foo).

-export([bar/1, baz/1]).

bar(_) -> <<"bar">>.

baz(Bindings) -> io_lib:format("~p", [Bindings]).
```

    1> Template = <<"bar: ~bar~;. baz: ~baz~;">>.
    2> Bindings = {foo, #{<<"unused">> => <<"yup">>}}.
    3> iolist_to_binary(roni:render(Template, Bindings)).
    <<"bar: bar. baz: #{<<\"unused\">> => <<\"yup\">>}">>
