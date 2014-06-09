; ModuleID = 'test.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@globalB = global i32 0, align 4
@globalA = common global i32 0, align 4

; Function Attrs: nounwind uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4
  %localA = alloca i32, align 4
  %localB = alloca i32, align 4
  store i32 0, i32* %1
  store i32 2, i32* %localB, align 4
  %2 = load i32* %localB, align 4
  %3 = call i32 @square(i32 %2)
  store i32 %3, i32* %localA, align 4
  %4 = load i32* %localB, align 4
  %5 = add nsw i32 %4, 1
  store i32 %5, i32* @globalB, align 4
  call void @foo()
  %6 = load i32* %localA, align 4
  %7 = load i32* @globalA, align 4
  %8 = add nsw i32 %6, %7
  ret i32 %8
}

; Function Attrs: nounwind uwtable
define i32 @square(i32 %x) #0 {
  %1 = alloca i32, align 4
  store i32 %x, i32* %1, align 4
  %2 = load i32* %1, align 4
  %3 = load i32* %1, align 4
  %4 = mul nsw i32 %2, %3
  ret i32 %4
}

; Function Attrs: nounwind uwtable
define void @foo() #0 {
  %1 = load i32* @globalB, align 4
  %2 = add nsw i32 %1, 1
  store i32 %2, i32* @globalA, align 4
  ret void
}

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.4.1 (tags/RELEASE_34/dot1-final)"}
