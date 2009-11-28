
puts RubyVM::InstructionSequence.compile_file("rbtest.rb").disasm
require "lispu.rb"

x = lambda {
         puts "hello, world"
        }
