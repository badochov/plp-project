# Knowledge Compilation
## Project outline
Create a metainterpreter of a prolog language being able to calculate probabilities of queries.

## Dependencies
`bdd_view.pl` has a `view_bdd/1` predicate that shows the built BDD in a gui graph. This requires the `prolog_graphviz` library, which can be installed with `swipl -g 'pack_install(prolog_graphviz)' -t halt`.

An example query to build such a BDD and show it (from within src/ directory):
```pl
[bdd_view, problog_examples/program_4].

problog_collect(P),
ground_program(P, GL, GP),
formula(GP, F),
construct_bdd(F, BDD,
view_bdd(BDD).
```

## Usage
Project exposes two public predicates:
- prob\3
- prob\2
prob\2 is prob\3 with 2nd argument set to true.

prob(+Query, +Evidence, -Probability).

## Authors
Hubert Badocha
Vytautas Mickus