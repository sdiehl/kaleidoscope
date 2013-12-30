; ModuleID = 'minimal'

declare i32 @putchar(i32)

define void @main() {
  call i32 @putchar(i32 42)
  ret void
}
