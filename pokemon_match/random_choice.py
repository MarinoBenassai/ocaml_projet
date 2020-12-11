import csv 
import sys
import random
import os

types = ["Bug", "Dark", "Dragon", "Electric", "Fighting", "Fire", "Flying", "Ghost", "Grass", "Ground", "Ice", "Normal", "Poison", "Psychic", "Rock", "Steel", "Water"]
if len(sys.argv) != 5:
    print("Usage: python random_choice.py <number of trainers> <trainer_file> <number of pokemon> <pokemon_file>")
    exit()
num_trainers = int(sys.argv[1])
trainer_file = sys.argv[2]
num_pokemon = int(sys.argv[3])
pokemon_file = sys.argv[4]


f = open(os.path.join(os.path.dirname(sys.argv[0]) ,"all_pokemon.csv"))
pokemon_reader = csv.reader(f, delimiter=",")
# inefficient but who cares
pokemon = []
for row in pokemon_reader:
    pokemon.append(row)
selected_pokemon = random.sample(pokemon, k=num_pokemon)#
f.close()


f2 = open(pokemon_file, 'w')
pokemon_writer = csv.writer(f2)
pokemon_writer.writerows(selected_pokemon)
f2.close()


f3 = open(os.path.join(os.path.dirname(sys.argv[0]) ,"random_names.csv"))
trainer_name_reader = csv.reader(f3, delimiter=",")
trainer_names = []
for t in trainer_name_reader:
    trainer_names.append(t)
f3.close()
selected_trainers = random.sample(trainer_names, k=num_trainers)

ids = list(range(num_trainers))
random.shuffle(ids)

f4 = open(trainer_file, 'w')
trainer_writer = csv.writer(f4)
for i, trainer in enumerate(selected_trainers):
    row = [ids[i], trainer[0]]
    for k in range(3):
        row.append(random.choice(types))
    trainer_writer.writerow(row)
f4.close()




    