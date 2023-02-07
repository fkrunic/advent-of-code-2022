from collections import defaultdict

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


def round(reducer, monkey_items, monkey_counters):
  for label in MK_LABELS:
    items = monkey_items[label]

    for item in items:  # foldM
      props = MK_PROPERTIES[label]
      modified = props["op"](item) // reducer

      throw_choice_index = 0 if modified % props["divisibility"] == 0 else 1
      throw_target = props["throw_choice"][throw_choice_index]
      monkey_items[throw_target].append(modified) # State.modify in inner scope
      monkey_counters[label] += 1

    monkey_items[label] = [] # State.modify in outer scope


if __name__ == "__main__":

  mk_items = [
    [79, 98],
    [54, 65, 75, 74],
    [79, 60, 97],
    [74]
  ]

  mk_counters = defaultdict(int)

  for _ in range(20):
    round(1, mk_items, mk_counters)

  print(mk_items)
  print(mk_counters)