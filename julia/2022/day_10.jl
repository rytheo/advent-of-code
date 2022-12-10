const crt_width = 40
const crt_height = 6

function main()
    text = read("input/2022/input_10.txt", String)
    sprite_xs = [1]
    x = 1
    for line in eachsplit(text, "\n", keepempty=false)
        v, _... = match(r"noop|addx (-?\d+)", line)
        if !isnothing(v)
            # Take an extra cycle for the add operation
            push!(sprite_xs, x)
            x += parse(Int, v)
        end
        push!(sprite_xs, x)
    end
    total = sum(i * sprite_xs[i] for i in 20:40:220)
    println("Part 1: $total")
    println("Part 2:")
    for y = 0:crt_height-1
        for x = 0:crt_width-1
            cycle = y * crt_width + x + 1
            sprite_x = sprite_xs[cycle]
            # Sprite is 3 pixels wide
            if x in sprite_x-1:sprite_x+1
                print("█")
            else
                print(" ")
            end
        end
        println()
    end
end

main()
