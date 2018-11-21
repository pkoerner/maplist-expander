:- module(taist, [foo/2]).

:- use_module(maplist_expander).

foo(A,B) :-
    maplist(member, A, B).


bar(B) :-
    maplist(member(1), B).
