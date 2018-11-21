:- module(taist, [foo/2]).

:- use_module(maplist_expander).

foo(A,B) :-
    maplist(member, A, B).
