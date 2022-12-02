function calorie_count(str::AbstractString)::UInt
    sum(parse(UInt, s) for s in eachsplit(str))
end

function main()
    text = read("input/2022/input_01.txt", String)
    counts = [calorie_count(s) for s in eachsplit(text, "\n\n")]
    partialsort!(counts, 3, rev=true)
    println("Part 1: $(counts[1])")
    println("Part 2: $(sum(counts[1:3]))")
end

main()
