mutable struct Monkey
    items::Vector{UInt}
    op::Char
    op_val::Union{UInt, Nothing}
    divisor::UInt
    true_id::UInt
    false_id::UInt
    num_inspections::UInt
end

function parse_monkey(str::AbstractString)::Monkey
    lines = split(str, '\n')
    m = match(r"new = old ([+*]) (\d+|old)", lines[3])
    op = only(m[1])
    if m[2] == "old"
        op_val = nothing
    else
        op_val = parse(UInt, m[2])
    end
    Monkey(
        [parse(UInt, s.match) for s in eachmatch(r"\d+", lines[2])],
        op,
        op_val,
        (parse(UInt, match(r"\d+", s).match) for s in lines[4:6])...,
        UInt(0)
    )
end

function do_round(monkeys::Vector{Monkey}; advanced::Bool)
    # We can mod by the LCM of all divisors without affecting throw decisions
    cap = lcm((m.divisor for m in monkeys)...)
    for monkey in monkeys
        monkey.num_inspections += length(monkey.items)
        for item in monkey.items
            b = something(monkey.op_val, item)
            item = monkey.op == '+' ? item + b : item * b
            if advanced
                item %= cap
            else
                item ÷= 3
            end
            dest = item % monkey.divisor == 0 ? monkey.true_id : monkey.false_id
            # Need to add 1 since Julia indices start at 1
            push!(monkeys[dest+1].items, item)
        end
        empty!(monkey.items)
    end
end

function business(monkeys::Vector{Monkey}; advanced::Bool)::UInt
    num_rounds = advanced ? 10_000 : 20
    monkeys = deepcopy(monkeys)
    for _ in 1:num_rounds
        do_round(monkeys, advanced=advanced)
    end
    partialsort!(monkeys, 2, by=(m -> m.num_inspections), rev=true)
    prod(m.num_inspections for m in monkeys[1:2])
end

function main()
    text = read("input/2022/input_11.txt", String)
    monkeys = [parse_monkey(s) for s in eachsplit(text, "\n\n", keepempty=false)]
    for (p, advanced) in enumerate([false, true])
        println("Part $p: $(business(monkeys, advanced=advanced))")
    end
end

main()
