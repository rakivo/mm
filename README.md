## Mirthual machine, virtual machine that makes you happy.

# ATTENTION! **PROJECT IS UNFINISHED**

## Quick Start:
```console
$ make && ./build/masm ./masm/macro.masm
```

# Quick example of masm with macros:
```asm
; You can define constants like in C.
#SYS_STDOUT 1

; Here is a showcase of how you can define a macro with a body.
; 'write' is the name, and everything before '{' is considered to be arguments of the macro.
#write arg1 arg2 operation out {
    push arg1
    push arg2
    operation
    dmp out
}

; `_start` is the default entry point, which, for now, you can't change.
_start:
    write 34 35 iadd SYS_STDOUT
    ret
```
