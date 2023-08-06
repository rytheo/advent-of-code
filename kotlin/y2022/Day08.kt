package y2022

import java.io.File

fun linesOfSight(grid: List<List<Int>>, position: Pair<Int, Int>): Array<List<Int>> {
    val (y, x) = position
    return arrayOf(
        grid[y].slice(0..<x).reversed(),
        grid[y].slice(x+1..<grid[y].size),
        (0..<y).reversed().map { grid[it][x] },
        (y+1..<grid.size).map { grid[it][x] },
    )
}

fun isVisible(grid: List<List<Int>>, position: Pair<Int, Int>): Boolean {
    val height = grid[position.first][position.second]
    return linesOfSight(grid, position).any { line -> line.all { it < height } }
}

fun lineScore(treeHeight: Int, line: Iterable<Int>): Int {
    var score = 0
    for (height in line) {
        score += 1
        if (height >= treeHeight) break
    }
    return score
}

fun scenicScore(grid: List<List<Int>>, position: Pair<Int, Int>): Int {
    val height = grid[position.first][position.second]
    return linesOfSight(grid, position).map { lineScore(height, it) }.reduce(Int::times)
}

fun main() {
    val text = File("input/2022/input_08.txt").readText().trim()
    val grid = text.lines().map { it.map(Char::digitToInt) }
    val positions = grid.withIndex().flatMap { (y, row) -> row.indices.map { x -> Pair(y, x) } }
    val numVisible = positions.count { isVisible(grid, it) }
    println("Part 1: $numVisible")
    val highScore = positions.maxOf { scenicScore(grid, it) }
    println("Part 2: $highScore")
}
