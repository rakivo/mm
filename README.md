## Mirthual machine, virtual machine that makes you happy.

# ATTENTION! **PROJECT IS UNFINISHED**

## Calculate the PI number in masm:
```console
$ make -j4 && ./build/masm ./masm/pi.masm
```

# For example you can take a look at how you can calculate the PI number in masm:
> Example from masm/pi.masm
```asm
#N 750000
#SYS_STDOUT 1

; calculate with current denominator
;        (4/n)
; acc + ^
#CALC_DENOMINATOR secop {
    fdiv
    secop
}

_start:
    push 4.0 ; acc (result of first division 4/1)
    push 3.0 ; denominator
    push N   ; counter

loop:
    swap 2 ; swap counter (top of stack) with current acc

    ; calculate next denominator
    push 4.0
    dup 2
    push 2.0
    fadd
    swap 3

    CALC_DENOMINATOR fsub

    push 4.0
    dup 2
    push 2.0
    fadd
    swap 3

    CALC_DENOMINATOR fadd

    ; decrement counter
    swap 2
    dec

    push 0
    cmp
    pop
    jne loop

    ; clean the stack and only have pi left
    pop
    pop

    dmp SYS_STDOUT ; print the value
```
