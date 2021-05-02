import operator
from collections import defaultdict, deque
from dataclasses import dataclass, field, InitVar
from typing import Callable, ClassVar


@dataclass
class CPU:
    binary_ops: ClassVar[dict[int, Callable[[int, int], int]]] = {
        1: operator.add,
        2: operator.mul,
        7: operator.lt,
        8: operator.eq,
    }

    program: InitVar[list[int]]
    mem: defaultdict[int, int] = field(default_factory=lambda: defaultdict(int))
    ip: int = 0
    ro: int = 0
    input: deque[int] = field(default_factory=deque)
    output: deque[int] = field(default_factory=deque)

    def __post_init__(self, program: list[int]):
        for i, v in enumerate(program):
            self.mem[i] = v

    def _get_op(self) -> tuple[int, list[int]]:
        inst = self.mem[self.ip]
        opcode = inst % 100
        modes = [(inst // 10 ** k) % 10 for k in range(2, 5)]
        return opcode, modes

    def _get_params(self, n: int, modes: list[int], write: bool=False) -> list[int]:
        params = [self.mem[self.ip + 1 + k] for k in range(n)]
        vals = []
        for i, (p, m) in enumerate(zip(params, modes)):
            # Check for position or relative mode
            if m != 1:
                # Apply relative offset
                if m == 2:
                    p += self.ro
                # Skip read if last param is a write address
                if not (i == n - 1 and write):
                    p = self.mem[p]
            vals.append(p)
        return vals

    def run(self) -> int:
        while True:
            op, modes = self._get_op()
            if op == 99:  # Halt
                return op
            elif op == 3:  # Input
                if not self.input:
                    return op
                a, = self._get_params(1, modes, write=True)
                self.mem[a] = self.input.popleft()
                self.ip += 2
            elif op == 4:  # Output
                self.output.extend(self._get_params(1, modes))
                self.ip += 2
            elif op in {5, 6}:  # Jump if true/false
                a, b = self._get_params(2, modes)
                self.ip = b if bool(a) == (op == 5) else self.ip + 3
            elif op == 9:  # Change relative offset
                self.ro += self._get_params(1, modes)[0]
                self.ip += 2
            elif op in CPU.binary_ops:  # Combine two values
                a, b, c = self._get_params(3, modes, write=True)
                self.mem[c] = CPU.binary_ops[op](a, b)
                self.ip += 4
            else:
                raise ValueError(f"Unknown opcode: {op}")
