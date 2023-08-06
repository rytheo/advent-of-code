package y2022

import java.io.File

fun priority(c: Char): Int = when {
    c.isLowerCase() -> c - 'a' + 1
    c.isUpperCase() -> c - 'A' + 27
    else -> throw UnsupportedOperationException()
}

fun findError(sack: String): Char {
    val (front, back) = sack.chunked(sack.length / 2)
    return front.toSet().intersect(back.toSet()).first()
}

fun findBadge(sacks: Iterable<String>): Char =
    sacks.map(String::toSet).reduce(Set<Char>::intersect).first()

fun main() {
    val text = File("input/2022/input_03.txt").readText().trim()
    val p1 = text.lines().map(::findError).sumOf(::priority)
    println("Part 1: $p1")
    val p2 = text.lines().chunked(3).map(::findBadge).sumOf(::priority)
    println("Part 2: $p2")
}
