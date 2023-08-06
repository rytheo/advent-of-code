package y2022

import java.io.File

const val DISK_CAPACITY = 70_000_000
const val UPDATE_SIZE = 30_000_000

data class Dir(var size: Int = 0)

fun main() {
    val text = File("input/2022/input_07.txt").readText().trim()
    val root = Dir()
    val allDirs = mutableListOf(root)
    val path = mutableListOf(root)
    for (line in text.lines()) {
        val match = Regex("""ls|cd (/|\.\.|\w+)|dir \w+|(\d+) .+""").find(line)!!
        val (cd, fileSize) = match.destructured
        if (cd.isNotEmpty()) when (cd) {
            "/" -> {}
            ".." -> path.removeLast()
            else -> {
                allDirs.add(Dir())
                path.add(allDirs.last())
            }
        } else if (fileSize.isNotEmpty()) {
            val size = fileSize.toInt()
            for (dir in path) {
                dir.size += size
            }
        }
    }
    val dirSizes = allDirs.map { it.size }
    val smallSum = dirSizes.filter { it <= 100_000 }.sum()
    println("Part 1: $smallSum")
    val freeSpace = DISK_CAPACITY - root.size
    val deficit = UPDATE_SIZE - freeSpace
    val dirSize = dirSizes.filter { it >= deficit }.minOrNull()!!
    println("Part 2: $dirSize")
}
