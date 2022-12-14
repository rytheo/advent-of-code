const deltas = [
    CartesianIndex(0, 1),
    CartesianIndex(1, 0),
    CartesianIndex(0, -1),
    CartesianIndex(-1, 0),
]

function main()
    text = read("input/2022/input_12.txt", String)
    grid = split(text) .|> collect |> Base.splat(hcat)
    start = findfirst(isequal('S'), grid)
    finish = findfirst(isequal('E'), grid)
    # Fix the start and end elevations
    grid[start] = 'a'
    grid[finish] = 'z'
    costs = Dict(finish=>0)
    # Makeshift priority queue
    to_visit = [(0, finish)]
    # Search backwards to find the shortest path from every grid space
    while length(to_visit) > 0
        # Find the item with the smallest cost
        min_index = argmin(to_visit)
        # Swap it with the last item to avoid shifting the rest of the vector
        to_visit[end], to_visit[min_index] = to_visit[min_index], to_visit[end]
        steps, here = pop!(to_visit)
        for delta in deltas
            adj = here + delta
            if checkbounds(Bool, grid, adj) && !haskey(costs, adj) && grid[adj] - grid[here] >= -1
                push!(costs, adj=>steps + 1)
                push!(to_visit, (steps + 1, adj))
            end
        end
    end
    println("Part 1: $(costs[start])")
    shortest = minimum(costs[i] for i in findall(isequal('a'), grid) if haskey(costs, i))
    println("Part 2: $shortest")
end

main()
