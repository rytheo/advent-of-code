from pathlib import Path


def path_to_com(body: str, orbits: dict) -> list:
    path = [body]
    while body != 'COM':
        body = orbits[body]
        path.append(body)
    return path


def main():
    text = (Path(__file__).parent / "../../input/2019/input_06.txt").read_text()
    orbits = {}
    # Read in orbital data
    for line in text.splitlines():
        parent, child = line.strip().split(')')
        orbits[child] = parent

    orbit_count = 0
    # Count orbits
    for body in orbits:
        # Trace orbits back to the COM
        while body != 'COM':
            body = orbits[body]
            orbit_count += 1
    print("Part 1:", orbit_count)

    # Compute the shortest path between YOU and SAN
    you_path = path_to_com(orbits['YOU'], orbits)
    body = orbits['SAN']
    jump_count = 0
    # Jump until we intersect
    while body not in you_path:
        body = orbits[body]
        jump_count += 1
    # Add the index of the body on YOU's path
    jump_count += you_path.index(body)
    print("Part 2:", jump_count)


if __name__ == "__main__":
    main()
