package y2022

import java.io.File

const val CRT_WIDTH = 40
const val CRT_HEIGHT = 6

fun main() {
    val text = File("input/2022/input_10.txt").readText().trim()
    val xs = mutableListOf<Int>()
    var spriteX = 1
    for (line in text.lines()) {
        xs.add(spriteX)
        Regex("""addx (-?\d+)""").find(line)?.let {
            xs.add(spriteX)
            spriteX += it.destructured.component1().toInt()
        }
    }
    val signalSum = (20..220 step 40).sumOf { it * xs[it - 1] }
    println("Part 1: $signalSum")
    println("Part 2:")
    for (y in 0..<CRT_HEIGHT) {
        for (x in 0..<CRT_WIDTH) {
            spriteX = xs[y * CRT_WIDTH + x]
            print(if (x in (spriteX-1)..(spriteX+1)) { "█" } else { " " })
        }
        println()
    }
}
