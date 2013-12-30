def fib(x)
  if x < 3 then
    1
  else
    fib(x-1)+fib(x-2);

extern foo();
extern bar();
def baz(x) if x then foo() else bar();

extern putchard(char);
def printstar(n)
  for i = 1, i < n, 1.0 in
    putchard(42);  # ascii 42 = '*'

# print 100 '*' characters
printstar(100);
