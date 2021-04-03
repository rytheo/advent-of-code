import operator
from collections import deque
from dataclasses import dataclass, field
from typing import Callable, ClassVar


@dataclass
class CPU:
    binary_ops: ClassVar[dict[int, Callable[[int, int], int]]] = {
        1: operator.add,
        2: operator.mul,
        7: operator.lt,
        8: operator.eq,
    }

    mem: list[int]
    pos: int = 0
    input: deque[int] = field(default_factory=deque)
    output: deque[int] = field(default_factory=deque)

    def _get_op(self) -> tuple[int, list[int]]:
        inst = self.mem[self.pos]
        opcode = inst % 100
        modes = [(inst // 10 ** k) % 10 for k in range(2, 5)]
        return opcode, modes

    def _get_params(self, n: int, modes: list[int]) -> list[int]:
        params = self.mem[self.pos + 1: self.pos + 1 + n]
        return [p if m else self.mem[p] for p, m in zip(params, modes)]

    def run(self) -> int:
        while True:
            op, modes = self._get_op()
            if op == 99:  # Halt
                return op
            elif op == 3:  # Input
                if not self.input:
                    return op
                a = self.mem[self.pos + 1]
                self.mem[a] = self.input.popleft()
                self.pos += 2
            elif op == 4:  # Output
                self.output.extend(self._get_params(1, modes))
                self.pos += 2
            elif op in {5, 6}:  # Jump if true/false
                a, b = self._get_params(2, modes)
                self.pos = b if bool(a) == (op == 5) else self.pos + 3
            elif op in CPU.binary_ops:  # Combine two values
                (a, b), c = self._get_params(2, modes), self.mem[self.pos + 3]
                self.mem[c] = CPU.binary_ops[op](a, b)
                self.pos += 4
            else:
                raise ValueError(f"Unknown opcode: {op}")
