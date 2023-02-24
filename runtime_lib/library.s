.text
.global exit
exit:
    movq %rax, %rdi
    # 60 == exit
    movq $60, %rax
    syscall

.global readChar
readChar:
    pushq %rbp
    movq %rsp, %rbp

    # space for return value
    pushq $0

    # fd that is read from (stdin)
    movq $0, %rdi
    # buffer to read into
    movq %rsp, %rsi
    # count = 1
    movq $1, %rdx
    # 0 == read
    movq $0, %rax
    syscall
    # result in %rax (bytes read)
    # check if bytes read == 0 (EOF) or error (< 0) both return -1
    cmpq $0, %rax
    jle readCharL0
    jmp readCharL1
readCharL0:
    movq $-1, %rax
    # discard buffer on stack
    popq %rdi
    jmp readCharL2
readCharL1:
    # but we want the buffer where the read Char is, so we pop that into %rax
    popq %rax
readCharL2:
    popq %rbp
    ret

.global writeChar
writeChar:
    pushq %rbp
    movq %rsp, %rbp
    # push char to stack
    pushq 16(%rbp)
    # fd that is written to (stdout)
    movq $1, %rdi
    # write the first char on rsp (the char we pushed)
    movq %rsp, %rsi
    # count = 1
    movq $1, %rdx
    # 1 == write
    movq $1, %rax
    syscall
    # clear stack
    popq %rdx
    # result in %rax (bytes written)
    popq %rbp
    ret

.global writeInt
writeInt:
pushq %rbp
movq %rsp, %rbp
subq $480, %rsp
movq 16(%rbp), %rsi
movq $-9223372036854775808, %rdi
cmpq %rdi, %rsi
je L0
jmp L1
L0:
movq $45, %rcx    # move rightOp to scratch register
pushq %rcx    # arguments on stack
call writeChar    # do call
popq %rdi    # pop arguments
movq %rax, -232(%rbp)    # move retVal
movq $57, %rcx    # move rightOp to scratch register
pushq %rcx    # arguments on stack
call writeChar    # do call
popq %rdi    # pop arguments
movq %rax, -240(%rbp)    # move retVal
movq $50, %rcx    # move rightOp to scratch register
pushq %rcx    # arguments on stack
call writeChar    # do call
popq %rdi    # pop arguments
movq %rax, -248(%rbp)    # move retVal
movq $50, %rcx    # move rightOp to scratch register
pushq %rcx    # arguments on stack
call writeChar    # do call
popq %rdi    # pop arguments
movq %rax, -256(%rbp)    # move retVal
movq $51, %rcx    # move rightOp to scratch register
pushq %rcx    # arguments on stack
call writeChar    # do call
popq %rdi    # pop arguments
movq %rax, -264(%rbp)    # move retVal
movq $51, %rcx    # move rightOp to scratch register
pushq %rcx    # arguments on stack
call writeChar    # do call
popq %rdi    # pop arguments
movq %rax, -272(%rbp)    # move retVal
movq $55, %rcx    # move rightOp to scratch register
pushq %rcx    # arguments on stack
call writeChar    # do call
popq %rdi    # pop arguments
movq %rax, -280(%rbp)    # move retVal
movq $50, %rcx    # move rightOp to scratch register
pushq %rcx    # arguments on stack
call writeChar    # do call
popq %rdi    # pop arguments
movq %rax, -288(%rbp)    # move retVal
movq $48, %rcx    # move rightOp to scratch register
pushq %rcx    # arguments on stack
call writeChar    # do call
popq %rdi    # pop arguments
movq %rax, -296(%rbp)    # move retVal
movq $51, %rcx    # move rightOp to scratch register
pushq %rcx    # arguments on stack
call writeChar    # do call
popq %rdi    # pop arguments
movq %rax, -304(%rbp)    # move retVal
movq $54, %rcx    # move rightOp to scratch register
pushq %rcx    # arguments on stack
call writeChar    # do call
popq %rdi    # pop arguments
movq %rax, -312(%rbp)    # move retVal
movq $56, %rcx    # move rightOp to scratch register
pushq %rcx    # arguments on stack
call writeChar    # do call
popq %rdi    # pop arguments
movq %rax, -320(%rbp)    # move retVal
movq $53, %rcx    # move rightOp to scratch register
pushq %rcx    # arguments on stack
call writeChar    # do call
popq %rdi    # pop arguments
movq %rax, -328(%rbp)    # move retVal
movq $52, %rcx    # move rightOp to scratch register
pushq %rcx    # arguments on stack
call writeChar    # do call
popq %rdi    # pop arguments
movq %rax, -336(%rbp)    # move retVal
movq $55, %rcx    # move rightOp to scratch register
pushq %rcx    # arguments on stack
call writeChar    # do call
popq %rdi    # pop arguments
movq %rax, -344(%rbp)    # move retVal
movq $55, %rcx    # move rightOp to scratch register
pushq %rcx    # arguments on stack
call writeChar    # do call
popq %rdi    # pop arguments
movq %rax, -352(%rbp)    # move retVal
movq $53, %rcx    # move rightOp to scratch register
pushq %rcx    # arguments on stack
call writeChar    # do call
popq %rdi    # pop arguments
movq %rax, -360(%rbp)    # move retVal
movq $56, %rcx    # move rightOp to scratch register
pushq %rcx    # arguments on stack
call writeChar    # do call
popq %rdi    # pop arguments
movq %rax, -368(%rbp)    # move retVal
movq $48, %rcx    # move rightOp to scratch register
pushq %rcx    # arguments on stack
call writeChar    # do call
popq %rdi    # pop arguments
movq %rax, -376(%rbp)    # move retVal
movq $56, %rcx    # move rightOp to scratch register
pushq %rcx    # arguments on stack
call writeChar    # do call
popq %rdi    # pop arguments
movq %rax, -384(%rbp)    # move retVal
mov $20, %rax
addq $480, %rsp
popq %rbp
ret
L1:
movq $0, %rax
movq %rax, -224(%rbp)
movq 16(%rbp), %rsi
movq $0, %rdi
cmpq %rdi, %rsi
jl L2
jmp L3
L2:
movq $45, %rcx    # move rightOp to scratch register
pushq %rcx    # arguments on stack
call writeChar    # do call
popq %rdi    # pop arguments
movq %rax, -392(%rbp)    # move retVal
movq -392(%rbp), %rax
movq %rax, -216(%rbp)
movq $1, %rax
movq %rax, -224(%rbp)
movq $0, %rax
movq 16(%rbp), %rcx    # move rightOp to scratch register
subq %rcx, %rax
movq %rax, -400(%rbp)
movq -400(%rbp), %rax
movq %rax, 16(%rbp)
L3:
movq $0, %rax
movq %rax, -208(%rbp)
jmp L6
L4:
movq 16(%rbp), %rax
cqto
mov $10, %rdi
idivq %rdi
mov %rax, -408(%rbp)
movq -408(%rbp), %rax
movq %rax, -216(%rbp)
movq $10, %rax
movq -216(%rbp), %rcx    # move rightOp to scratch register
imulq %rcx, %rax
movq %rax, -416(%rbp)
movq 16(%rbp), %rax
movq -416(%rbp), %rcx    # move rightOp to scratch register
subq %rcx, %rax
movq %rax, -424(%rbp)
movq -208(%rbp), %rax
cltq
movq -424(%rbp), %r8
movq %r8, -200(%rbp, %rax, 8)    # STORE
movq -216(%rbp), %rax
movq %rax, 16(%rbp)
movq -208(%rbp), %rax
movq $1, %rcx    # move rightOp to scratch register
addq %rcx, %rax
movq %rax, -432(%rbp)
movq -432(%rbp), %rax
movq %rax, -208(%rbp)
L6:
movq 16(%rbp), %rsi
movq $0, %rdi
cmpq %rdi, %rsi
jg L4
jmp L5
L5:
movq -224(%rbp), %rax
movq -208(%rbp), %rcx    # move rightOp to scratch register
addq %rcx, %rax
movq %rax, -440(%rbp)
movq -440(%rbp), %rax
movq %rax, -224(%rbp)
movq -208(%rbp), %rsi
movq $0, %rdi
cmpq %rdi, %rsi
je L7
jmp L8
L7:
movq $48, %rcx    # move rightOp to scratch register
pushq %rcx    # arguments on stack
call writeChar    # do call
popq %rdi    # pop arguments
movq %rax, -448(%rbp)    # move retVal
movq -448(%rbp), %rax
movq %rax, -216(%rbp)
mov -224(%rbp), %rax
addq $480, %rsp
popq %rbp
ret
L8:
jmp L11
L9:
movq -208(%rbp), %rax
movq $1, %rcx    # move rightOp to scratch register
subq %rcx, %rax
movq %rax, -456(%rbp)
movq -456(%rbp), %rax
movq %rax, -208(%rbp)
movq -208(%rbp), %rax
cltq
movq -200(%rbp, %rax, 8), %rsi    # LOAD
movq %rsi, -464(%rbp)
movq $48, %rax
movq -464(%rbp), %rcx    # move rightOp to scratch register
addq %rcx, %rax
movq %rax, -472(%rbp)
movq -472(%rbp), %rcx    # move rightOp to scratch register
pushq %rcx    # arguments on stack
call writeChar    # do call
popq %rdi    # pop arguments
movq %rax, -480(%rbp)    # move retVal
movq -480(%rbp), %rax
movq %rax, -216(%rbp)
L11:
movq -208(%rbp), %rsi
movq $0, %rdi
cmpq %rdi, %rsi
jg L9
jmp L10
L10:
mov -224(%rbp), %rax
addq $480, %rsp
popq %rbp
ret
.global readInt
readInt:
pushq %rbp
movq %rsp, %rbp
subq $288, %rsp
movq $1, %rax
movq %rax, -32(%rbp)
call readChar    # do call
movq %rax, -200(%rbp)    # move retVal
movq -200(%rbp), %rax
movq %rax, -192(%rbp)
jmp L14
L12:
movq -192(%rbp), %rsi
movq $45, %rdi
cmpq %rdi, %rsi
je L15
jmp L16
L15:
movq $-1, %rax
movq %rax, -32(%rbp)
L16:
movq -192(%rbp), %rsi
movq $-1, %rdi
cmpq %rdi, %rsi
je L17
jmp L18
L17:
mov $0, %rax
addq $288, %rsp
popq %rbp
ret
L18:
call readChar    # do call
movq %rax, -208(%rbp)    # move retVal
movq -208(%rbp), %rax
movq %rax, -192(%rbp)
L14:
movq -192(%rbp), %rsi
movq $48, %rdi
cmpq %rdi, %rsi
jl L12
jmp L19
L19:
movq -192(%rbp), %rsi
movq $57, %rdi
cmpq %rdi, %rsi
jg L12
jmp L13
L13:
movq $0, %rax
movq %rax, -16(%rbp)
jmp L22
L20:
movq -192(%rbp), %rax
movq $48, %rcx    # move rightOp to scratch register
subq %rcx, %rax
movq %rax, -216(%rbp)
movq -16(%rbp), %rax
cltq
movq -216(%rbp), %r8
movq %r8, -184(%rbp, %rax, 8)    # STORE
movq -16(%rbp), %rax
movq $1, %rcx    # move rightOp to scratch register
addq %rcx, %rax
movq %rax, -224(%rbp)
movq -224(%rbp), %rax
movq %rax, -16(%rbp)
call readChar    # do call
movq %rax, -232(%rbp)    # move retVal
movq -232(%rbp), %rax
movq %rax, -192(%rbp)
L22:
movq -192(%rbp), %rsi
movq $48, %rdi
cmpq %rdi, %rsi
jge L23
jmp L21
L23:
movq -192(%rbp), %rsi
movq $57, %rdi
cmpq %rdi, %rsi
jle L20
jmp L21
L21:
movq -16(%rbp), %rax
movq $1, %rcx    # move rightOp to scratch register
subq %rcx, %rax
movq %rax, -240(%rbp)
movq -240(%rbp), %rax
movq %rax, -16(%rbp)
movq $0, %rax
movq %rax, -8(%rbp)
movq $1, %rax
movq %rax, -24(%rbp)
jmp L26
L24:
movq -16(%rbp), %rax
cltq
movq -184(%rbp, %rax, 8), %rsi    # LOAD
movq %rsi, -248(%rbp)
movq -248(%rbp), %rax
movq -24(%rbp), %rcx    # move rightOp to scratch register
imulq %rcx, %rax
movq %rax, -256(%rbp)
movq -8(%rbp), %rax
movq -256(%rbp), %rcx    # move rightOp to scratch register
addq %rcx, %rax
movq %rax, -264(%rbp)
movq -264(%rbp), %rax
movq %rax, -8(%rbp)
movq -24(%rbp), %rax
movq $10, %rcx    # move rightOp to scratch register
imulq %rcx, %rax
movq %rax, -272(%rbp)
movq -272(%rbp), %rax
movq %rax, -24(%rbp)
movq -16(%rbp), %rax
movq $1, %rcx    # move rightOp to scratch register
subq %rcx, %rax
movq %rax, -280(%rbp)
movq -280(%rbp), %rax
movq %rax, -16(%rbp)
L26:
movq -16(%rbp), %rsi
movq $0, %rdi
cmpq %rdi, %rsi
jge L24
jmp L25
L25:
movq -8(%rbp), %rax
movq -32(%rbp), %rcx    # move rightOp to scratch register
imulq %rcx, %rax
movq %rax, -288(%rbp)
movq -288(%rbp), %rax
movq %rax, -8(%rbp)
mov -8(%rbp), %rax
addq $288, %rsp
popq %rbp
ret

