; ModuleID = 'main2.c'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.C = type { i16, i32, %struct.C*, %struct.C* }

; Function Attrs: nounwind uwtable
; define i32 @main() #0 {
define i32 @main() gc "shadow-stack" {
  %1 = alloca i32, align 4
  %i = alloca i32, align 4
  %ptr = alloca %struct.C*, align 8
  store i32 0, i32* %1
  store i32 0, i32* %i, align 4
  br label %2

; <label>:2                                       ; preds = %12, %0
  %3 = load i32* %i, align 4
  %4 = icmp slt i32 %3, 1000
  br i1 %4, label %5, label %15

; <label>:5                                       ; preds = %2
  %6 = call i8* @llvm_gc_allocate(i32 24)
  %7 = bitcast i8* %6 to %struct.C*
  store %struct.C* %7, %struct.C** %ptr, align 8
  %8 = load %struct.C** %ptr, align 8
  %9 = getelementptr inbounds %struct.C* %8, i32 0, i32 0
  store i16 1, i16* %9, align 2
  %10 = load %struct.C** %ptr, align 8
  %11 = getelementptr inbounds %struct.C* %10, i32 0, i32 1
  store i32 1, i32* %11, align 4
  br label %12

; <label>:12                                      ; preds = %5
  %13 = load i32* %i, align 4
  %14 = add nsw i32 %13, 1
  store i32 %14, i32* %i, align 4
  br label %2

; <label>:15                                      ; preds = %2
  ret i32 0
}

declare i8* @llvm_gc_allocate(i32) #1

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"Ubuntu clang version 3.6.0-2ubuntu1~trusty1 (tags/RELEASE_360/final) (based on LLVM 3.6.0)"}
