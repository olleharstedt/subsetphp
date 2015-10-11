; RUN: llc < %s
; REQUIRES: default_triple


declare i8* @llvm_gc_allocate(i32)
declare void @llvm_gc_initialize(i32)

declare void @llvm.gcroot(i8**, i8*)
declare void @llvm.gcwrite(i8*, i8*, i8**)

define i32 @main() gc "shadow-stack" {
entry:
	%A = alloca i8*

	call void @llvm_gc_initialize(i32 1048576)  ; Start with 1MB heap

  ;; void *A;
	call void @llvm.gcroot(i8** %A, i8* null)

  ;; A = gcalloc(10);
	%Aptr = call i8* @llvm_gc_allocate(i32 10)
	store i8* %Aptr, i8** %A

	br label %AllocLoop

AllocLoop:
	%i = phi i32 [ 0, %entry ], [ %indvar.next, %AllocLoop ]
  ;; Allocated mem: allocated memory is immediately dead.
	call i8* @llvm_gc_allocate(i32 100)
	
	%indvar.next = add i32 %i, 1
	%exitcond = icmp eq i32 %indvar.next, 10000000
	br i1 %exitcond, label %Exit, label %AllocLoop

Exit:
	ret i32 0
}

declare void @__main()
