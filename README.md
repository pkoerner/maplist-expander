# maplist-expander

## Rationale
In Prolog, `maplist` is a great predicate that I try to use whenever applicable.
Yet, there are two issues I identified:

- At least in SICStus Prolog, `maplist` is slowed compared to writing the loop manually. When preparing benchmarks that uses `maplist` with an identity function, I observed 10-20x speed-ups when calling the loop instead.
- There are only so many arities of `maplist` that are implemented (usually `maplist/2` to `maplist/4` - if, say, `maplist/6` is needed, one has to write it oneself).

However, I still like using `maplist`. Using term expansion, an explicit recursion can be _generated_ instead.
This keeps the nice, clean looks of using `maplist`, yet gives us the performance and allows us to stop worrying about limited arities.

## Usage

- Drop `maplist_expander.pl` into your project.
- Include the term expander by adding `:- use_module(maplist_expander).` to the top of your module.

Then, you're all set!

## Limitations

- Currently, this only works for SICStus Prolog. I might add a term expander for SWI Prolog in the future.
- No expansion happens in combination with other meta-predicates (e.g., `call`).
- It is not well-tested yet, so I hope you like experiments :-)
