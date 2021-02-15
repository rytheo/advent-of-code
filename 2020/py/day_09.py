import itertools as it
from pathlib import Path


def main():
    text = (Path(__file__).parent / "../input/input_09.txt").read_text()
    nums = [int(n) for n in text.splitlines()]
    for i, n in enumerate(nums):
        if i < 25:
            continue
        if not any(n == a + b for (a, b) in it.combinations(nums[i-25:i], 2)):
            print("Part 1:", invalid := n)
            break
    lower = upper = 0
    total = nums[0]
    while total != invalid:
        if total < invalid:
            upper += 1
            total += nums[upper]
        else:
            total -= nums[lower]
            lower += 1
    print("Part 2:", min(nums[lower:upper+1]) + max(nums[lower:upper+1]))


if __name__ == "__main__":
    main()
