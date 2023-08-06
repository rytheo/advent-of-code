package y2022

import java.io.File

fun findMarker(str: String, markerSize: Int): Int =
    str.windowed(markerSize).withIndex()
        .find { (_, window) -> window.toSet().size == markerSize }!!
        .component1() + markerSize

fun main() {
    val text = File("input/2022/input_06.txt").readText().trim()
    for ((part, markerSize) in arrayOf(4, 14).withIndex()) {
        println("Part ${part + 1}: ${findMarker(text, markerSize)}")
    }
}
