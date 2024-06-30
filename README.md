## Mirthual machine, virtual machine that makes you happy.

# ATTENTION! **PROJECT IS UNFINISHED**

## Quick Start:
```console
$ make && ./build/masm ./masm/macro.masm
```

# Quick example of masm with macros:
```asm
#SYS_STDOUT 1

#write arg1 arg2 operation {
    push arg1
    push arg2
    operation
    dmp SYS_STDOUT
}

_start:
    write 34 35 iadd
    push 420
    dmp SYS_STDOUT
    halt
```
