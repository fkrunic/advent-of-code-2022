from collections import defaultdict
from random import randint

MK_LABELS = [0, 1, 2, 3]

MK_PROPERTIES = [
  {
    "op": lambda n: n * 19,
    "divisibility": 23,
    "throw_choice": [2, 3]
  },
  {
    "op": lambda n: n + 6,
    "divisibility": 19,
    "throw_choice": [2, 0]
  },
  {
    "op": lambda n: n * n,
    "divisibility": 13,
    "throw_choice": [1, 3]
  },
  {
    "op": lambda n: n + 3,
    "divisibility": 17,
    "throw_choice": [0, 1]
  }
]


def round(reducer, monkey_items, monkey_counters, monkey_residues):
  for label in MK_LABELS:
    items = monkey_items[label]

    for item in items:  # foldM
      item_index = item[0]
      item_worry = item[1]

      props = MK_PROPERTIES[label]
      modifier = props["op"]

      for monkey_index, prop in enumerate(MK_PROPERTIES):
        divisor = prop["divisibility"]
        residue_key = (item_index, monkey_index)
        existing_residue = monkey_residues[residue_key]
        monkey_residues[residue_key] = (modifier(existing_residue) // reducer) % divisor

      modified = (modifier(item_worry) // reducer) % divisor

      throw_choice_index = 0 if modified % props["divisibility"] == 0 else 1
      throw_target = props["throw_choice"][throw_choice_index]

      target_residue_key = (item_index, throw_target)
      residue_item = (item_index, monkey_residues[target_residue_key])

      monkey_items[throw_target].append(residue_item)
      monkey_counters[label] += 1

    monkey_items[label] = []


if __name__ == "__main__":

  mk_items = [
    [(1, 79), (2, 98)],
    [(3, 54), (4, 65), (5, 75), (6, 74)],
    [(7, 79), (8, 60), (9, 97)],
    [(10, 74)]
  ]

  mk_counters = defaultdict(int)
  mk_residues = defaultdict(int)

  for _ in range(20):
    round(3, mk_items, mk_counters, mk_residues)

  print(mk_items)
  print(mk_counters)
  print(mk_residues)