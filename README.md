## Mirthual machine, virtual machine that makes you happy.

# ATTENTION! **PROJECT IS UNFINISHED**

## Quick Start:
```console
$ make && ./build/masm ./masm/macro.masm
```

# Quick example of masm with macros:
```asm
#write arg1 arg2 oper out {
    push arg1
    push arg2
    oper
    dmp out
}

_start:
    write 666 246 isub 1
```
