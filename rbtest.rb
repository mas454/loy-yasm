
puts RubyVM::InstructionSequence.compile_file("rbtest.rb").disasm
require "lispu.rb"

x = Test.new
x.abc(20)
