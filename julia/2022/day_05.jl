import Base.Iterators as It

const num_stacks = 9

function parse_stacks(str::AbstractString)::Vector{Vector{Char}}
    lines = split(str, '\n')
    stacks = [[] for _ in 1:num_stacks]
    # Loop starting from the bottom, skipping the index line
    for line in It.drop(It.reverse(lines), 1)
        # Loop over each crate column
        for (i, m) in enumerate(eachmatch(r"(?:\[(\w)\]|   ) ?", line))
            if !isnothing(m[1])
                push!(stacks[i], only(m[1]))
            end
        end
    end
    stacks
end

function parse_step(str::AbstractString)::Tuple{UInt, UInt, UInt}
    m = match(r"move (\d+) from (\d) to (\d)", str)
    tuple((parse(UInt, s) for s in m)...)
end

function do_steps(
    stacks::Vector{Vector{Char}},
    steps::AbstractVector{Tuple{UInt, UInt, UInt}};
    advanced::Bool
)::Vector{Vector{Char}}
    stacks = deepcopy(stacks)
    for (quantity, origin_i, dest_i) in steps
        origin, dest, = stacks[origin_i], stacks[dest_i]
        for _ in 1:quantity
            push!(dest, pop!(origin))
        end
        if advanced
            reverse!(dest, length(dest) - quantity + 1)
        end
    end
    stacks
end

function stack_tops(stacks::Vector{Vector{Char}})::String
    string(last.(stacks)...)
end

function main()
    text = read("input/2022/input_05.txt", String)
    state_str, steps_str = split(text, "\n\n")
    stacks = parse_stacks(state_str)
    steps = eachsplit(steps_str, '\n', keepempty=false) .|> parse_step
    for (p, advanced) in enumerate([false, true])
        solution = do_steps(stacks, steps, advanced=advanced) |> stack_tops
        println("Part $p: $solution")
    end
end

main()
