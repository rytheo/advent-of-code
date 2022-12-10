const dir_map = Dict(
    "R" => [0, 1],
    "D" => [1, 0],
    "L" => [0, -1],
    "U" => [-1, 0],
)

struct Node
    position::Vector{Int}
    child::Union{Node, Nothing}
end

function parse_step(str::AbstractString)::Tuple{Vector{Int}, UInt}
    dir, dist = match(r"(\w) (\d+)", str)
    dir_map[dir], parse(UInt, dist)
end

function update_pos(node::Node, parent::Node)
    delta = parent.position .- node.position
    # If distance is 2 in any dimension, move up to 1 unit in each dimension towards parent
    if any(abs.(delta) .== 2)
        node.position .+= clamp.(delta, -1, 1)
        if !isnothing(node.child)
            update_pos(node.child, node)
        end
    end
end

function main()
    text = read("input/2022/input_09.txt", String)
    head = tail = Node([0, 0], nothing)
    # Extend the rope to 10 nodes
    for _ in 1:9
        head = Node([0, 0], head)
    end
    p1_visited = Set()
    p2_visited = Set()
    for (delta, dist) in parse_step.(split(text, '\n', keepempty=false))
        for _ in 1:dist
            head.position .+= delta
            update_pos(head.child, head)
            push!(p1_visited, copy(head.child.position))
            push!(p2_visited, copy(tail.position))
        end
    end
    println("Part 1: $(length(p1_visited))")
    println("Part 2: $(length(p2_visited))")
end

main()
