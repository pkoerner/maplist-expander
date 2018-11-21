:- module(bench_vanilla, [bench1/1]).

:- use_module(library(lists)).

identity(X, X).

bench1(Vars) :-
    maplist(identity(1), Vars).
