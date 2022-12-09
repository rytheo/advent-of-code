function lines_of_sight(grid::Matrix, index::CartesianIndex)::Vector
    row, col = Tuple(index)
    [
        grid[row-1:-1:1, col],
        grid[row+1:end, col],
        grid[row, col-1:-1:1],
        grid[row, col+1:end],
    ]
end

function is_visible(grid::Matrix, index::CartesianIndex)::Bool
    any(all(line .< grid[index]) for line in lines_of_sight(grid, index))
end

function line_score(tree_height::UInt, line::Vector{UInt})::UInt
    score = 0
    for height in line
        score += 1
        if height >= tree_height
            break
        end
    end
    score
end

function scenic_score(grid::Matrix, index::CartesianIndex)::UInt
    val = grid[index]
    prod(line_score(val, line) for line in lines_of_sight(grid, index))
end

function parse_line(line::AbstractString)::Vector{UInt}
    [parse(UInt, c) for c in line]
end

function main()
    text = read("input/2022/input_08.txt", String)
    grid = split(text) .|> parse_line |> Base.splat(hcat)
    num_visible = count(is_visible(grid, i) for i in CartesianIndices(grid))
    println("Part 1: $num_visible")
    high_score = maximum(scenic_score(grid, i) for i in CartesianIndices(grid))
    println("Part 2: $high_score")
end

main()
