:- use_module(bench_vanilla, [bench1/1]).
:- use_module(bench_pimped,  [bench2/1]).

run_bench1(0, _).
run_bench1(N, Vars) :-
    bench1(Vars),
    N1 is N-1,
    run_bench1(N1, Vars).

run_bench2(0, _).
run_bench2(N, Vars) :-
    bench2(Vars),
    N1 is N-1,
    run_bench2(N1, Vars).



bench :-
    length(Vars1, 300000),
    length(Vars2, 300000),
    
    statistics(walltime, _),
    
    run_bench1(100, Vars1),
    statistics(walltime, [_,RunTime1]),
    
    run_bench2(100, Vars2),
    statistics(walltime, [_,RunTime2]),

    format('runtime (vanilla): ~w~n', [RunTime1]),
    format('runtime (pimped):  ~w~n', [RunTime2]).

