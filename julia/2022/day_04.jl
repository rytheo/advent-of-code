function parse_ranges(line::AbstractString)::Tuple{UnitRange{UInt}, UnitRange{UInt}}
    m = match(r"(\d+)-(\d+),(\d+)-(\d+)", line)
    s1, e1, s2, e2 = (parse(UInt, c) for c in m.captures)
    s1:e1, s2:e2
end

function full_overlap(r1::UnitRange, r2::UnitRange)::Bool
    r1 ∩ r2 in [r1, r2]
end

function partial_overlap(r1::UnitRange, r2::UnitRange)::Bool
    length(r1 ∩ r2) > 0
end

function main()
    text = read("input/2022/input_04.txt", String)
    range_pairs = split(text) .|> parse_ranges
    for (p, has_overlap) in enumerate([full_overlap, partial_overlap])
        num_overlaps = count(has_overlap(a, b) for (a, b) in range_pairs)
        println("Part $p: $num_overlaps")
    end
end

main()
