package y2022

import java.io.File
import kotlin.math.abs

typealias Point = List<Int>

fun parseDir(dir: Char) = when (dir) {
    'R' -> listOf(1, 0)
    'D' -> listOf(0, -1)
    'L' -> listOf(-1, 0)
    'U' -> listOf(0, 1)
    else -> throw UnsupportedOperationException()
}

private fun parseMotion(step: String): Pair<Point, Int> =
    Pair(parseDir(step[0]), step.drop(2).toInt())

fun main() {
    val text = File("input/2022/input_09.txt").readText().trim()
    val rope = Array(10) { mutableListOf(0, 0) }
    val neckPoints = mutableSetOf<Point>()
    val tailPoints = mutableSetOf<Point>()
    for ((step, dist) in text.lines().map(::parseMotion)) {
        repeat(dist) {
            rope[0][0] += step[0]
            rope[0][1] += step[1]
            for (i in rope.indices.drop(1)) {
                val (front, back) = rope.slice(i-1..i)
                val dX = front[0] - back[0]
                val dY = front[1] - back[1]
                if (arrayOf(dX, dY).any { abs(it) == 2 }) {
                    back[0] += dX.coerceIn(-1..1)
                    back[1] += dY.coerceIn(-1..1)
                }
            }
            neckPoints.add(rope[1].toList())
            tailPoints.add(rope.last().toList())
        }
    }
    println("Part 1: ${neckPoints.size}")
    println("Part 2: ${tailPoints.size}")
}
