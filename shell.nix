{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  erlang = beam.interpreters.erlang_nox;
  rebar3 = beam.packages.erlang.rebar3;
in
mkShell {
  buildInputs = [ erlang rebar3 ];

  ERL_INCLUDE_PATH="${erlang}/lib/erlang/usr/include";
}
