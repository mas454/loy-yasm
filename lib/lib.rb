require("lib/read.rb")
require("lib/loy.rb")
def string?(a)
a.kind_of?(String)
end

def number?(o)
o.kind_of?(Numeric)
end

def symbol?(o)
o.kind_of?(Symbol)
end

def pair?(o)
o.kind_of?(Array)
end

def eq?(a, b)
a.equal?(b)
end

def null?(a)
a.nil?()
end

def open_input_file(path)
open(path, "r")
end

def map(fn, lis)
if null?(lis)
nil
else
[fn.call(lis[0]),map(fn, lis[1])]
end
end

def append(*lis)
app_list = nil
lis.map() {|li|
app_list = [li,app_list]
}
app_list
end

def append2(xs, ys)
if null?(xs)
ys
else
[xs[0],append2(xs[1], ys)]
end
end

def reverse(lis)
rev = lambda {|lis, retlis|
if null?(lis)
retlis
else
rev.call(lis[1], [lis[0],retlis])
end
}
rev.call(lis, nil)
end

def newarray(*arr)
arr
end

def memq(sym, lis)
if null?(lis)
false
elsif eq?(sym, lis[0])
lis
else
memq(sym, lis[1])
end
end

def caar(code)
code[0][0]
end

def cdar(code)
code[0][1]
end

def cddr(code)
code[1][1]
end

def cadr(code)
code[1][0]
end

def caddr(code)
code[1][1][0]
end

def cdadr(code)
code[1][0][1]
end

def caadr(code)
code[1][0][0]
end

def cadar(code)
code[0][1][0]
end

def a2l(array)
lamcall(lambda {|i, array, lis|
loop() {||
if array.length() == i
break
end
lis = [array.get(i),lis]
i = i + 1
}
lis
}, 0, array.reverse(), nil)
end

def memq(sym, lis)
if null?(lis)
false
elsif eq?(sym, lis[0])
lis
else
memq(sym, lis[1])
end
end

def transfer(ls)
if pair?(ls)
if pair?(ls[0])
if eq?(caar(ls), :"unquote")
list(:"cons", cadar(ls), transfer(ls[1]))
else
if eq?(caar(ls), :"unquote-splicing")
list(:"append2", cadar(ls), transfer(ls[1]))
else
list(:"cons", transfer(ls[0]), transfer(ls[1]))
end
end
else
list(:"cons", list(:"quote", ls[0]), transfer(ls[1]))
end
else
list(:"quote", ls)
end
end

