# OCAML Project FFA and Max-Flow Min-Cost

In this project the Ford-Fulkerson Algorithm (FFA) or the Max-Flow Min-Cost Algorithm is used to match fresh Pokemon trainers to a list of available Pokemon.

## Input Data

Available Pokemon and adventure hungry pokemon trainers are stored in the csv format.

Trainers:
| ID | Name | TypePreference1 | TypePreference1 | TypePreference1 |
|----|------|-----------------|-----------------|-----------------|
| 1  | Ash  | Electric        | Fire            | Water           |

Pokemon:
| ID | Name       | Type1 | Type2   |
|----|------------|-------|---------|
| 79 | Slowpokesh | Water | Psychic |



Because manual data entry is tiresome, there is a script to generate random data:

generate data for tests: 

```python3 pokemon_match/random_choice.py <number of trainers> <trainer_file.csv> <number_of_pokemon> <pokemon_file.csv>```

or for example

```python3 pokemon_match/random_choice.py 20 pokemon_match/test1_trainer.csv 30 pokemon_match/test1_pokemon.csv```

## Installing Requirements

```opam install csv```

## Compiling

For the FFA variant where each preference is weighted the same:
```ocamlbuild -use-ocamlfind -package csv pokemon_ffa.native```

For the Max-Flow Min-Cost variant where each preference is weighted based on position in the list:
```ocamlbuild -use-ocamlfind -package csv pokemon_maxmin.native```

## Running the Algorithm

To run the ffa algorithm use:

```./pokemon_ffa.native <pokemon_file.csv> <trainer_file.csv> <resulting_graph.dot> <results.txt>```

or for example

```./pokemon_ffa.native pokemon_match/test1_pokemon.csv pokemon_match/test1_trainer.csv outgraph.dot results.txt```

to run the max-flow min-cost algorithm use

```./pokemon_maxmin.native <pokemon_file.csv> <trainer_file.csv> <resulting_graph.dot> <results.txt>```

or for example

```./pokemon_maxmin.native pokemon_match/test1_pokemon.csv pokemon_match/test1_trainer.csv outgraph.dot results.txt```

The results.txt file will contain what pokemon is matched to each trainer. While an optimal solution is found, not all trainers may get a pokemon :(

To view the output graph you can use
```dot -Tsvg outgraph.dot > outgraph.svg```
It may not be pretty if there are many pokemon and trainers.
