	.text
	.file	"alloc_loop2.ll"
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
	.cfi_personality 3, __gcc_personality_v0
.Leh_func_begin0:
	.cfi_lsda 3, .Lexception0
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp7:
	.cfi_def_cfa_offset 16
	subq	$32, %rsp
.Ltmp8:
	.cfi_def_cfa_offset 48
.Ltmp9:
	.cfi_offset %rbx, -16
	movq	llvm_gc_root_chain(%rip), %rax
	movq	$__gc_main, 16(%rsp)
	movq	$0, 24(%rsp)
	movq	%rax, 8(%rsp)
	leaq	8(%rsp), %rax
	movq	%rax, llvm_gc_root_chain(%rip)
.Ltmp0:
	movl	$1048576, %edi          # imm = 0x100000
	callq	llvm_gc_initialize
.Ltmp1:
# BB#1:                                 # %entry.cont2
.Ltmp2:
	movl	$10, %edi
	callq	llvm_gc_allocate
.Ltmp3:
# BB#2:                                 # %entry.cont
	movq	%rax, 24(%rsp)
	movl	$10000000, %ebx         # imm = 0x989680
	.align	16, 0x90
.LBB0_3:                                # %AllocLoop
                                        # =>This Inner Loop Header: Depth=1
.Ltmp4:
	movl	$100, %edi
	callq	llvm_gc_allocate
.Ltmp5:
# BB#4:                                 # %AllocLoop.cont
                                        #   in Loop: Header=BB0_3 Depth=1
	decl	%ebx
	jne	.LBB0_3
# BB#5:                                 # %Exit
	movq	8(%rsp), %rax
	movq	%rax, llvm_gc_root_chain(%rip)
	xorl	%eax, %eax
	addq	$32, %rsp
	popq	%rbx
	retq
.LBB0_6:                                # %gc_cleanup
.Ltmp6:
	movq	8(%rsp), %rcx
	movq	%rcx, llvm_gc_root_chain(%rip)
	movq	%rax, %rdi
	callq	_Unwind_Resume
.Ltmp10:
	.size	main, .Ltmp10-main
	.cfi_endproc
.Leh_func_end0:
	.section	.gcc_except_table,"a",@progbits
	.align	4
GCC_except_table0:
.Lexception0:
	.byte	255                     # @LPStart Encoding = omit
	.byte	3                       # @TType Encoding = udata4
	.asciz	"\234"                  # @TType base offset
	.byte	3                       # Call site Encoding = udata4
	.byte	26                      # Call site table length
	.long	.Ltmp0-.Leh_func_begin0 # >> Call Site 1 <<
	.long	.Ltmp5-.Ltmp0           #   Call between .Ltmp0 and .Ltmp5
	.long	.Ltmp6-.Leh_func_begin0 #     jumps to .Ltmp6
	.byte	0                       #   On action: cleanup
	.long	.Ltmp5-.Leh_func_begin0 # >> Call Site 2 <<
	.long	.Leh_func_end0-.Ltmp5   #   Call between .Ltmp5 and .Leh_func_end0
	.long	0                       #     has no landing pad
	.byte	0                       #   On action: cleanup
	.align	4

	.type	llvm_gc_root_chain,@object # @llvm_gc_root_chain
	.section	.bss.llvm_gc_root_chain,"aGw",@nobits,llvm_gc_root_chain,comdat
	.weak	llvm_gc_root_chain
	.align	8
llvm_gc_root_chain:
	.quad	0
	.size	llvm_gc_root_chain, 8

	.type	__gc_main,@object       # @__gc_main
	.section	.rodata,"a",@progbits
	.align	8
__gc_main:
	.long	1                       # 0x1
	.long	0                       # 0x0
	.size	__gc_main, 8


	.section	".note.GNU-stack","",@progbits
