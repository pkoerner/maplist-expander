:- module(bench_pimped, [bench2/1]).

:- use_module(maplist_expander).

identity(X, X).

bench2(Vars) :-
    maplist(identity(1), Vars).
