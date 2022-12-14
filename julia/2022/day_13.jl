struct Packet
    contents::Vector{Union{Packet, Int}}
end

function parse_packet(io::Base.GenericIOBuffer)::Packet
    contents = []
    # Skip the open bracket
    skip(io, 1)
    while true
        next_char = peek(io, Char)
        # Close bracket: return packet
        if next_char == ']'
            skip(io, 1)
            return Packet(contents)
        # Open bracket: parse subpacket
        elseif next_char == '['
            push!(contents, parse_packet(io))
        # Digit: parse number
        elseif isdigit(next_char)
            num = 0
            while isdigit(peek(io, Char))
                num *= 10
                num += parse(Int, read(io, Char))
            end
            push!(contents, num)
        # Comma: do nothing
        elseif next_char == ','
            skip(io, 1)
        end
    end
end

Base.isless(a::Packet, b::Packet) = isless(a.contents, b.contents)
Base.isless(a::Packet, b::Int) = isless(a, Packet([b]))
Base.isless(a::Int, b::Packet) = isless(Packet([a]), b)
Base.isequal(a::Packet, b::Packet) = isequal(a.contents, b.contents)
Base.isequal(a::Int, b::Packet) = isequal(Packet([a]), b)
Base.isequal(a::Packet, b::Int) = isequal(a, Packet([b]))

function main()
    text = read("input/2022/input_13.txt", String)
    packets = [parse_packet(IOBuffer(line)) for line in eachsplit(text)]
    pairs = (packets[i:i+1] for i in 1:2:length(packets))
    p1 = findall(issorted, pairs) |> sum
    println("Part 1: $p1")
    push!(packets, Packet([2]), Packet([6]))
    sort!(packets)
    p2 = findfirst(isequal(2), packets) * findfirst(isequal(6), packets)
    println("Part 2: $p2")
end

main()
