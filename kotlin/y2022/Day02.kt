package y2022

import java.io.File

fun scoreLine(line: String, advanced: Boolean): Int {
    // rock|loss = 0; paper|draw = 1; scissors|win = 2
    val them = line[0] - 'A'
    val hint = line[2] - 'X'
    val outcome: Int
    val you: Int
    if (advanced) {
        outcome = hint
        // -1 to compute required offset from opponent's shape
        you = (them + outcome - 1).mod(3)
    } else {
        you = hint
        // +1 to rotate values from (0:draw,1:win,2:loss) to (0:loss,1:draw,2:win)
        outcome = (you - them + 1).mod(3)
    }
    return you + 1 + outcome * 3
}

fun main() {
    val text = File("input/2022/input_02.txt").readText().trim()
    for ((part, advanced) in arrayOf(false, true).withIndex()) {
        val total = text.lines().sumOf { scoreLine(it, advanced) }
        println("Part ${part + 1}: $total")
    }
}
