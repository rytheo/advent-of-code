package y2022

import java.io.File

fun parseRanges(line: String): Pair<IntRange, IntRange> {
    val (s1, e1, s2, e2) = line.split('-', ',').map(String::toInt)
    return Pair(s1..e1, s2..e2)
}

fun fullOverlap(r1: IntRange, r2: IntRange): Boolean =
    r1.first <= r2.first && r2.last <= r1.last || r2.first <= r1.first && r1.last <= r2.last

fun partialOverlap(r1: IntRange, r2: IntRange): Boolean =
    r1.first <= r2.last && r2.first <= r1.last

fun main() {
    val text = File("input/2022/input_04.txt").readText().trim()
    val rangePairs = text.lines().map(::parseRanges)
    for ((p, hasOverlap) in arrayOf(::fullOverlap, ::partialOverlap).withIndex()) {
        val numOverlaps = rangePairs.count { (a, b) -> hasOverlap(a, b) }
        println("Part ${p + 1}: $numOverlaps")
    }
}
