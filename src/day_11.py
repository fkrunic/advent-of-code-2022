def round(monkey_labels, monkey_items, monkey_properties):
  for label in monkey_labels:
    items = monkey_items[label]

    for item in items:
      props = monkey_properties[label]
      modified = props["op"](item) // 3

      throw_choice_index = 0 if modified % props["divisibility"] == 0 else 1
      throw_target = props["throw_choice"][throw_choice_index]
      monkey_items[throw_target].append(modified)

    monkey_items[label] = []


if __name__ == "__main__":
  mk_labels = [0, 1, 2, 3]

  mk_items = [
    [79, 98],
    [54, 65, 75, 74],
    [79, 60, 97],
    [74]
  ]

  mk_properties = [
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

  for _ in range(20):
    round(mk_labels, mk_items, mk_properties)

  print(mk_items)