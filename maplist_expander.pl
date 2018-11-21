:- module(maplist_expander, []).

% idea:
% replace maplists in predicates
%
% example:
% foo(A,B) :- maplist(member, A, B).
% should expand to:
%
% foo(A,B) :- $internal_no_maplisto_member_2(A,B).
% $internal_no_maplisto_member_2([], []).
% $internal_no_maplisto_member_2([H|T], [H2|T2]) :-
%     member(H, H2),
%     $internal_no_maplisto_member_2(T, T2).
%
% care has to be taken for given arguments.
%
% performance optimisation:
% if the lists are known, the steps can be inlined
% example:
% foo :- maplist(member, [1,2,3], [[1], [2], [1,2,3]).
% should expand to:
% foo :- member(1, [1]), member(2, [2]), member(3, [1,2,3]).

repeat(0, _, []).
repeat(N, Elem, [Elem|Res]) :-
    N1 is N-1,
    repeat(N1, Elem, Res).

generate_head_tail([], [], []).
generate_head_tail([[H|T]|VT], [H|HT], [T|TT]) :-
    generate_head_tail(VT, HT, TT).

generate_more_rules([Names, Acc], Module, InternalName, _, _, [Names, Acc]) :-
    % if a maplist already is expanded in this module,
    % we must not generate duplicate rules
    % (it would incorrectly generate more solutions)
    member(Module:InternalName, Names), !.
generate_more_rules([NamesIn, AccIn], Module, InternalName, MetaGoal, Arity, [[Module:InternalName,NamesIn],[Module:Goal1, Module:Goal2|AccIn]]) :-
    repeat(Arity, [], BaseCase),
    Goal1 =.. [InternalName|BaseCase],
    length(Vars, Arity),
    generate_head_tail(Vars, Heads, Tails),
    GoalHead =.. [InternalName|Vars],

    % if the goal is something like maplist(member(1), Xs),
    % i.e. some arguments are bound to the predicate,
    % they need to be passed as well
    MetaGoal =.. [MetaGoalTerm|Args],
    concat(Args, Heads, AllArgs),

    SubGoal =.. [MetaGoalTerm|AllArgs],
    Recursion =.. [InternalName|Tails],
    Goal2 = ':-'(GoalHead, (SubGoal, Recursion)).

gen_name_no_bound_args(MetaGoal, Arity, Name) :-
    atom_concat('$internal_no_maplisto_', MetaGoal, InternalTmp1),
    number_chars(Arity, ArityChars),
    atom_chars(ArityAtom, ArityChars),
    atom_concat('_', ArityAtom, InternalTmp2),
    atom_concat(InternalTmp1, InternalTmp2, Name).

replace_goal(MaplistGoal, Module, AdditionalRulesIn, AdditionalRulesOut, SubGoalOut) :-
    % for now: no performance optimisation
    % for now: only the easy part that expands 
    % maplist(member, _, _)
    % but not
    % maplist(member(a), _)
    MaplistGoal =.. [maplist,MetaGoal|Args],
    (atom(MetaGoal) ; MetaGoal = ':'(_Module, Goali), atom(Goali)),
    length(Args, Arity),
    gen_name_no_bound_args(MetaGoal, Arity, InternalName),
    generate_more_rules(AdditionalRulesIn, Module,InternalName, MetaGoal, Arity, AdditionalRulesOut),
    SubGoalOut =.. [InternalName|Args].



expand((A,B), Module, (NA, NB), AccIn, AccOut) :-
    expand(A, Module, NA, AccIn, AccTmp),
    expand(B, Module, NB, AccTmp, AccOut).
expand((A;B), Module, (NA; NB), AccIn, AccOut) :-
    expand(A, Module, NA, AccIn, AccTmp),
    expand(B, Module, NB, AccTmp, AccOut).
expand((A -> B), Module, (NA, NB), AccIn, AccOut) :-
    expand(A, Module, NA, AccIn, AccTmp),
    expand(B, Module, NB, AccTmp, AccOut).
expand(':-'(A, B), Module, ':-'(A, ExpandedB), AccIn, AccOut) :- !,
    expand(B, Module, ExpandedB, AccIn, AccOut).
expand(A, Module, NA, AccIn, AccOut) :-
    functor(A, maplist, _Arity),
    replace_goal(A, Module, AccIn, AccOut, NA).
expand(X, _Module, X, Acc, Acc).

:- multifile user:term_expansion/6.
user:term_expansion(Term1, _Layout1, Ids, [Term2|MoreTerms], [], [maplist_expander_token|Ids]) :-
    nonmember(maplist_expander_token, Ids),
    prolog_load_context(module, Module),
    expand(Term1, Module, Term2, [], MoreTerms),
    print(expanded_to(Term2, MoreTerms)),nl.
