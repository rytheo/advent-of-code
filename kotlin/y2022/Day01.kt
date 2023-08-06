package y2022

import java.io.File

fun main() {
    val text = File("input/2022/input_01.txt").readText().trim()
    val counts = text.split("\n\n")
        .map { it.lines().sumOf(String::toUInt) }
        .sortedDescending()
    println("Part 1: ${counts[0]}")
    println("Part 2: ${counts.take(3).sum()}")
}
