package y2022

import java.io.File

data class Step(val amount: Int, val start: Int, val end: Int)

fun parseState(str: String): Array<MutableList<Char>> {
    val numStacks = str.lines().last().count(Char::isDigit)
    val stacks: Array<MutableList<Char>> = Array(numStacks) { mutableListOf() }
    for (line in str.lines().dropLast(1).reversed()) {
        for ((index, match) in Regex("""\[(\w)] ?| {3,4}""").findAll(line).withIndex()) {
            val group = match.groups[1] ?: continue
            stacks[index].add(group.value[0])
        }
    }
    return stacks
}

private fun parseMotion(str: String): Step {
    val (amount, start, end) = Regex("\\d+").findAll(str).map { it.value.toInt() }.toList()
    return Step(amount, start - 1, end - 1)
}

fun doSteps(stacks: List<MutableList<Char>>, steps: Iterable<Step>, advanced: Boolean) {
    for ((amount, start, end) in steps) {
        val origin = stacks[start]
        val dest = stacks[end]
        repeat(amount) {
            dest.add(origin.removeLast())
        }
        if (advanced) dest.subList(dest.size - amount, dest.size).reverse()
    }
}

fun main() {
    val text = File("input/2022/input_05.txt").readText().trimEnd()
    val (stateStr, stepsStr) = text.split("\n\n")
    val stacks = parseState(stateStr)
    val steps = stepsStr.lines().map(::parseMotion)
    for ((part, advanced) in arrayOf(false, true).withIndex()) {
        val newStacks = stacks.map { it.toMutableList() }.also { doSteps(it, steps, advanced) }
        val solution = newStacks.map { it.last() }.joinToString("")
        println("Part ${part + 1}: $solution")
    }
}
