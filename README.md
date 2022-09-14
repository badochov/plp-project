# Knowledge Compilation
## Project outline
Create a metainterpreter of a prolog language being able to calculate probabilities of queries.

## Method of Knowledge Compilation

1. A probabilistic logic program is first grounded by replacing all variables with ground literals of the appropriate sorts.
2. The grounded program is then rewritten into a boolean algebra expression which denotes a possible worlds.
3. The user-provided query is added to the boolean algebra expression, thus making a choice of possible worlds.
3. A BDD tree is built from the boolean algebra expression to be able to efficiently calculate the probability of the query.
4. The BDD is traversed with probability weights in a final calculation.
5. (if provided) Conditional probabilities are implemented by a usual Kolmogorov definition calculation.

An example of the type of bdd trees the program builds can be seen in `causal_sprinkler.svg`. This one built from the example program in `src/problog_examples/causal_sprinkler.pl`.

## Dependencies
`bdd_view.pl` has a `view_bdd/1` predicate that shows the built BDD in a gui graph. This requires the `prolog_graphviz` library, which can be installed with `swipl -g 'pack_install(prolog_graphviz)' -t halt` it requires `graphviz` to be installed.

An example query to build such a BDD and show it (from within src/ directory):
```pl
[bdd_view, problog_examples/program_3].

problog_collect(P),
ground_program(P, GL, GP),
progam_formula(GP, F),
construct_bdd(F, BDD),
view_bdd(BDD).
```

## Usage
Project exposes two public predicates:
- prob\3
- prob\2
prob\2 is prob\3 with 2nd argument set to true.

prob(+Query, +Evidence, -Probability).

## Tests
Implementation has multiple tests that can be run with `run_tests.sh` script.

## Examples
Are located in `src/problog_examples`.

## Authors
Hubert Badocha
Vytautas Mickus