#include <stdio.h>
#include <stdlib.h>

typedef struct _s {
  int x, y;
} s;

int main(void) {
  s* s1 = malloc(sizeof(s));
  s1->x = 11;
  s1->y = 22;
  return 0;
}

/*
 
How test.php should work, storing x and y in struct.
 
define i32 @main() gc "shadow-stack" {
entry:
  call void @llvm_gc_initialize(i32 100000)
  %tmp = alloca {double, double}*, align 8
  %tmp1 = call noalias i8* @llvm_gc_allocate(i64 1024)
  %tmp11 = bitcast i8* %tmp1 to {double, double}* 
  store {double, double}* %tmp11, {double, double}** %tmp, align 8
  ;call void @llvm.gcroot({double, double}** %tmp, i8* null)
  ;%tmp2 = bitcast i8** %tmp to { double, double }*
  %0 = load {double, double}** %tmp, align 8
  %gep = getelementptr inbounds { double, double }* %0, i32 0, i32 0
  store double 1.100000e+01, double* %gep
  ;%tmp22 = bitcast i8** %tmp to { double, double }*
  %gep3 = getelementptr inbounds { double, double }* %0, i32 0, i32 1
  store double 2.200000e+01, double* %gep3
  ret i32 0
}

*/

/*
 
define i32 @main() gc "shadow-stack" {
entry:
  call void @llvm_gc_initialize(i32 100000)
  %tmp = alloca {double, double}*, align 8
  %tmp1 = call noalias i8* @llvm_gc_allocate(i64 1024)
  %tmp11 = bitcast i8* %tmp1 to {double, double}* 
  store {double, double}* %tmp11, {double, double}** %tmp, align 8
  ;call void @llvm.gcroot({double, double}** %tmp, i8* null)
  ;%tmp2 = bitcast i8** %tmp to { double, double }*
  %0 = load {double, double}** %tmp, align 8
  %gep = getelementptr inbounds { double, double }* %0, i32 0, i32 0
  store double 1.100000e+01, double* %gep
  ;%tmp22 = bitcast i8** %tmp to { double, double }*
  %gep3 = getelementptr inbounds { double, double }* %0, i32 0, i32 1
  store double 2.200000e+01, double* %gep3
  %gep5 = getelementptr inbounds { double, double }* %0, i32 0, i32 0
  %load = load double* %gep5
  %printd = call double @printd(double %load)
  ret i32 0
}

*/
