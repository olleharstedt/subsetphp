#include <stdio.h>
#include <stdlib.h>

int main(void) {
  int arr[3] = {1, 2, 3};
  return 0;
}

/**
 
LLVM using memcpy
 
@main.arr = private unnamed_addr constant [3 x i32] [i32 1, i32 2, i32 3], align 4

; Function Attrs: nounwind uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4
  %arr = alloca [3 x i32], align 4
  store i32 0, i32* %1
  %2 = bitcast [3 x i32]* %arr to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %2, i8* bitcast ([3 x i32]* @main.arr to i8*), i64 12, i32 4, i1 false)
  ret i32 0
}

*/
