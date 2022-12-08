const total_disk_space = 70_000_000
const update_size = 30_000_000

struct File
    size::UInt
end

struct Dir
    parent::Union{Dir, Nothing}
    contents::Dict{String, Union{Dir, File}}
end

size(file::File)::UInt = file.size
size(dir::Dir)::UInt = values(dir.contents) .|> size |> sum

function main()
    text = read("input/2022/input_07.txt", String)
    cwd = root = Dir(nothing, Dict())
    all_dirs = [root]
    for line in eachsplit(text, '\n', keepempty=false)
        cd, size, name = match(r"ls|cd (/|\.\.|\w+)|(dir|\d+) ([\w.]+)", line)
        if !isnothing(cd)
            if cd == "/"
                cwd = root
            elseif cd == ".."
                cwd = cwd.parent
            else
                cwd = cwd.contents[cd]
            end
        elseif !isnothing(size)
            if size == "dir"
                dir = Dir(cwd, Dict())
                cwd.contents[name] = dir
                push!(all_dirs, dir)
            else
                cwd.contents[name] = File(parse(UInt, size))
            end
        end
    end
    small_sum = sum(filter(x -> x <= 100_000, size.(all_dirs)))
    println("Part 1: $small_sum")
    free_space = total_disk_space - size(root)
    space_needed = update_size - free_space
    space_to_free = minimum(filter(x -> x >= space_needed, size.(all_dirs)))
    println("Part 2: $space_to_free")
end

main()
