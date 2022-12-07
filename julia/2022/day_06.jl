import Base.Iterators as It

function find_marker(text::AbstractString, num_distinct::Int)::UInt
    It.filter(num_distinct:length(text)) do i
        allunique(@view text[i-num_distinct+1:i])
    end |> first
end

function main()
    text = read("input/2022/input_06.txt", String)
    for (p, num_distinct) in enumerate([4, 14])
        println("Part $p: $(find_marker(text, num_distinct))")
    end
end

main()
