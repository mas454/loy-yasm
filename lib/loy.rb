class Array
 def get(num)
  self[num]
 end	
end

def display(sexp, out=STDOUT)
 putlist sexp, out
end

def newline(out=STDOUT)
 out.print "\n"
end

def putlist(a, out)
 if a.kind_of?(Array)
  out.print '('
  putlist_loop a, out
 else
  out.print a
 end
end

def putlist_loop(a, out)
  if(a[1] != nil)
    putlist a[0], out
    out.print ' '
    putlist_loop(a[1], out)
  else
    putlist a[0], out
    out.print ")"
  end
end

def lamcall(lam, *args)
 lam.call(*args)
end

def list_loop(lis, num)
 if lis.length()==num
  nil
 else
  [lis.[](num),list_loop(lis,num+1)]
 end
end

def list(*b)
 list_loop(b,0)
end


def symbol?(o)
 o.kind_of?(Symbol)
end

def pair?(exp)
 exp.kind_of?(Array) 
end

def eq?(a, b)
 a==b
end
def null?(a)
 a.nil?()
end

S_READER = Reader.new

def read(i = STDIN)
 str=S_READER.read i
 S_READER.lex(str)
end
