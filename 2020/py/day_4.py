import re
from pathlib import Path


fields = {'byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid'}
eye_colours = {'amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth'}
hrange = {
    'cm': range(150, 194),
    'in': range(59, 77),
}


def valid(passport: str, check_values: bool) -> bool:
    data = dict(pair.split(':') for pair in re.split(r'\s', passport))
    if not check_values:
        return data.keys() >= fields
    try:
        return all([
            data.keys() >= fields,
            1920 <= int(data['byr']) <= 2002,
            2010 <= int(data['iyr']) <= 2020,
            2020 <= int(data['eyr']) <= 2030,
            int(data['hgt'][:-2]) in hrange[data['hgt'][-2:]],
            re.fullmatch(r'#[\da-f]{6}', data['hcl']),
            data['ecl'] in eye_colours,
            re.fullmatch(r'\d{9}', data['pid']),
        ])
    except (KeyError, ValueError):
        return False


def main():
    text = (Path(__file__).parent / "../input/input_4.txt").read_text()
    passports = [p.strip() for p in text.split('\n\n')]
    print("Part 1:", sum(valid(p, False) for p in passports))
    print("Part 2:", sum(valid(p, True) for p in passports))


if __name__ == "__main__":
    main()
