function score_line(line::AbstractString; advanced::Bool=false)::UInt
    # rock|loss = 0; paper|draw = 1; scissors|win = 2
    them = Int(line[1] - 'A')
    hint = Int(line[3] - 'X')
    if advanced
        outcome = hint
        # -1 to compute required offset from opponent's shape
        you = mod(them + outcome - 1, 3)
    else
        you = hint
        # +1 to rotate values from (0:draw,win:1,loss:2) to (0:loss,1:draw,2:win)
        outcome = mod(you - them + 1, 3)
    end
    you + 1 + outcome * 3
end

function main()
    text = read("input/2022/input_02.txt", String)
    for (part, advanced) in enumerate([false, true])
        total = sum(
            score_line(line, advanced=advanced)
            for line in eachsplit(text, '\n', keepempty=false)
        )
        println("Part $part: $total")
    end
end

main()
