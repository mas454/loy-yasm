#!ruby -Ks
require 'lib/lib.rb'
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

nil
def null?(a)
a.nil?()
end

nil
def open_input_file(path)
open(path, "r")
end

nil
def map(fn, lis)
if null?(lis)
nil
else
[fn.call(lis[0]),map(fn, lis[1])]
end
end

nil
def append(*lis)
app_list = nil
lis.map() {|li|
app_list = [li,app_list]
}
app_list
end

nil
def append2(xs, ys=nil)
if null?(xs)
ys
else
[xs[0],append2(xs[1], ys)]
end
end

nil
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

nil
def newarray(*arr)
arr
end

nil
def memq(sym, lis)
if null?(lis)
false
elsif eq?(sym, lis[0])
lis
else
memq(sym, lis[1])
end
end

nil
def caar(code)
code[0][0]
end

nil
def cdar(code)
code[0][1]
end

nil
def cddr(code)
code[1][1]
end

nil
def cadr(code)
code[1][0]
end

nil
def caddr(code)
code[1][1][0]
end

nil
def cdadr(code)
code[1][0][1]
end

nil
def caadr(code)
code[1][0][0]
end

nil
def cadar(code)
code[0][1][0]
end

nil
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

nil
def memq(sym, lis)
if null?(lis)
false
elsif eq?(sym, lis[0])
lis
else
memq(sym, lis[1])
end
end

nil
def assoc(key, lis)
if null?(lis)
#f
else
if eq?(key, caar(lis))
lis[0]
else
assoc(key, lis[1])
end
end
end

nil
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
if null?(ls)
nil
else
list(:"quote", ls)
end
end
end

def list_ref(list, num)
loop() {||
if num == 0
return(list[0])
end
num = num - 1
list = list[1]
}
end

nil
nil
nil
nil
nil
nil
nil
nil
nil
nil
nil
nil
