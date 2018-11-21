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

:- dynamic countah/1.

countah(0).

inc_countah(Val) :-
    countah(Val),
    retract(countah(Val)),
    Val1 is Val+1,
    assert(countah(Val1)).

repeat(0, _, []).
repeat(N, Elem, [Elem|Res]) :-
    N1 is N-1,
    repeat(N1, Elem, Res).

generate_head_tail([], [], []).
generate_head_tail([[H|T]|VT], [H|HT], [T|TT]) :-
    generate_head_tail(VT, HT, TT).

my_atom_number(Atom, Number) :-
    (nonvar(Number)
      -> number_chars(Number, Chars), atom_chars(Atom, Chars)
       ; atom_chars(Atom, Chars), number_chars(Number, Chars)).

generate_more_rules(AccIn, Module, InternalName, MetaGoal, Arity, [Module:Goal1, Module:Goal2|AccIn]) :-
    repeat(Arity, [], BaseCase),
    Goal1 =.. [InternalName|BaseCase],
    length(Vars, Arity),
    generate_head_tail(Vars, Heads, Tails),
    GoalHead =.. [InternalName|Vars],

    % if the goal is something like maplist(member(1), Xs),
    % i.e. some arguments are bound to the predicate,
    % they need to be passed as well
    MetaGoal =.. [MetaGoalTerm|Args],
    append(Args, Heads, AllArgs),

    SubGoal =.. [MetaGoalTerm|AllArgs],
    Recursion =.. [InternalName|Tails],
    Goal2 = ':-'(GoalHead, (SubGoal, Recursion)).

gen_name(MetaGoal, Arity, Name) :-
    % generate something like
    % $internal_no_maplisto_member_1_123
    MetaGoal =.. [MetaGoalName|_],
    atom_concat('$internal_no_maplisto_', MetaGoalName, InternalTmp1),
    my_atom_number(ArityAtom, Arity),
    atom_concat('_', ArityAtom, InternalTmp2),
    atom_concat(InternalTmp1, InternalTmp2, InternalTmp3),
    inc_countah(Count),
    my_atom_number(CountAtom, Count),
    atom_concat('_', CountAtom, InternalTmp4),
    atom_concat(InternalTmp3, InternalTmp4, Name).


my_is_list(L) :-
    L == [] ;
    nonvar(L),
    L = [_|T],
    my_is_list(T).


all_args_have_known_length([]).
all_args_have_known_length([H|T]) :-
    my_is_list(H), all_args_have_known_length(T).

inline_calls([[]|_], _, _, true).
inline_calls(ArgLists, MetaGoalName, MetaGoalArgs, (Goal,SubGoalOut)) :-
    generate_head_tail(ArgLists, Heads, Tails),
    append(MetaGoalArgs, Heads, AllArgs),
    Goal =.. [MetaGoalName|AllArgs],
    inline_calls(Tails, MetaGoalName, MetaGoalArgs, SubGoalOut).

replace_goal(MaplistGoal, _, AdditionalRulesIn, AdditionalRulesIn, SubGoalOut) :-
    MaplistGoal =.. [maplist,MetaGoal|Args],
    all_args_have_known_length(Args), !,
    MetaGoal =.. [MetaGoalName|MetaGoalArgs],
    inline_calls(Args, MetaGoalName, MetaGoalArgs, SubGoalOut).
replace_goal(MaplistGoal, Module, AdditionalRulesIn, AdditionalRulesOut, SubGoalOut) :-
    % TODO: inline call entirely if length is statically known
    MaplistGoal =.. [maplist,MetaGoal|Args],
    length(Args, Arity),
    gen_name(MetaGoal, Arity, InternalName),
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
    %print(expanded_to(Term2, MoreTerms)),nl.
    true.
