function priority(c::Char)::UInt
    islowercase(c) ? c - 'a' + 1 :
    isuppercase(c) ? c - 'A' + 27 :
    error("invalid character")
end

function find_error(str::AbstractString)::Char
    front, back = str[1:end÷2], str[end÷2+1:end]
    only(Set(front) ∩ Set(back))
end

function find_badge(block::AbstractString)::Char
    only(intersect(split(block)...))
end

function main()
    text = read("input/2022/input_03.txt", String)
    p1 = split(text) .|> find_error .|> priority |> sum
    println("Part 1: $p1")
    p2 = eachmatch(r"(\w+\n){3}", text) .|> (m -> m.match) .|> find_badge .|> priority |> sum
    println("Part 2: $p2")
end

main()
