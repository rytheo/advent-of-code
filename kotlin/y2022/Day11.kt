package y2022

import java.io.File

data class Monkey(
    val items: MutableList<ULong>,
    val inspect: (ULong) -> ULong,
    val divisor: ULong,
    val trueId: Int,
    val falseId: Int,
    var numInspected: ULong = 0U,
) {
    fun clone(): Monkey = this.copy(items = this.items.toMutableList())
}

fun findInt(s: String): Int = Regex("""\d+""").find(s)!!.value.toInt()

fun parseMonkey(s: String): Monkey {
    val lines = s.lines()
    val match = Regex("""new = old ([+*]) (\d+|old)""").find(lines[2])!!
    val (operator, operand) = match.destructured
    val binaryOp: (ULong, ULong) -> ULong = when (operator) {
        "+" -> ULong::plus
        "*" -> ULong::times
        else -> throw UnsupportedOperationException()
    }
    val unaryOp: (ULong) -> ULong = when (val opVal = operand.toULongOrNull()) {
        null -> { x -> binaryOp(x, x) }
        else -> { x -> binaryOp(x, opVal) }
    }
    return Monkey(
        Regex("""\d+""").findAll(lines[1]).map { it.value.toULong() }.toMutableList(),
        unaryOp,
        findInt(lines[3]).toULong(),
        findInt(lines[4]),
        findInt(lines[5]),
    )
}

fun business(monkeys: MutableList<Monkey>, advanced: Boolean): ULong {
    val numRounds = if (advanced) { 10_000 } else { 20 }
    val manageFn: (ULong) -> ULong = if (advanced) {
        val cap = monkeys.map { it.divisor }.reduce(ULong::times);
        { it % cap }
    } else {
        { it / 3U }
    }
    repeat(numRounds) {
        for (monkey in monkeys) {
            monkey.numInspected += monkey.items.size.toULong()
            for (item in monkey.items) {
                val newItem = manageFn(monkey.inspect(item))
                val dest = when (newItem % monkey.divisor) {
                    0UL -> monkey.trueId
                    else -> monkey.falseId
                }
                monkeys[dest].items.add(newItem)
            }
            monkey.items.clear()
        }
    }
    monkeys.sortByDescending { it.numInspected }
    return monkeys[0].numInspected * monkeys[1].numInspected
}

fun main() {
    val text = File("input/2022/input_11.txt").readText().trim()
    val monkeys = text.split("\n\n").map(::parseMonkey)
    for (part in 1..2) {
        val advanced = part == 2
        println("Part $part: ${business(monkeys.map(Monkey::clone).toMutableList(), advanced)}")
    }
}
