# c-minux

Compiler for a C-like language. It uses bison for its grammer and outputs LLVM IR code, so that
furthur optimization and generating machine code can be done by LLVM.

## Run

You need a c compiler (clang or gcc), make, bison, and LLVM.
Write a program like `input.cmx`:

```c
int gcd(int a, int b) {
    if (b == 0)
        return a;
    return gcd(b, a % b);
}

int main(void) {
    int a;
    int b;
    a = input();
    b = input();
    print(gcd(a, b));
    return 0;
}
```

Compile into executable with `make output.exe` and then run it.

## References

- LLVM Language Reference, https://llvm.org/docs/LangRef.html
- LLVM-C, C interface to LLVM, https://llvm.org/doxygen/group__LLVMC.html
- Bison, https://www.gnu.org/software/bison/manual/html_node/index.html
