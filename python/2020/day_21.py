from pathlib import Path


def main():
    text = (Path(__file__).parent / "../../input/2020/input_21.txt").read_text()
    recipes = []
    candidates = {}
    for line in text.splitlines():
        p1, p2 = line[:-1].split(" (contains ")
        ingredients = set(p1.split(' '))
        recipes.append(ingredients)
        allergens = set(p2.split(', '))
        for a in allergens:
            if a in candidates:
                candidates[a] &= ingredients
            else:
                candidates[a] = ingredients.copy()
    dangerous = {}
    while candidates:
        for allergen, ingredients in candidates.items():
            if len(ingredients) == 1:
                ingredient = ingredients.pop()
                dangerous[ingredient] = allergen
                del candidates[allergen]
                for s in candidates.values():
                    s.discard(ingredient)
                break
    print("Part 1:", sum(i not in dangerous for r in recipes for i in r))
    print("Part 2:", ','.join(sorted(dangerous, key=dangerous.get)))


if __name__ == "__main__":
    main()
