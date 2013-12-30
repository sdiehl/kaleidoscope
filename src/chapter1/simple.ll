; ModuleID = 'simple'

declare i32 @getchar()

declare i32 @putchar(i32)

define i32 @add(i32 %a, i32 %b) {
  %1 = add i32 %a, %b
  ret i32 %1
}

define void @main() {
  %1 = call i32 @add(i32 0, i32 97)
  call i32 @putchar(i32 %1)
  ret void
}
