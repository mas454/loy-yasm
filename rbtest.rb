require("lispu.rb")
eval "def con(a, b)
 lambda {|f|
        f.call(a, b)}
 end"
def car(f)
   f.call(lambda {|a, b|
		b})
end
x = con 10, 20
puts car x
class Metest
 def Metest.test
  puts "test"
 end
end
puts(if true
      "hello"
     "world"
     end,30)

obj=Metest.method(:test)
obj.call
#puts RubyVM::InstructionSequence.compile_file("rbtest.rb").disasm




