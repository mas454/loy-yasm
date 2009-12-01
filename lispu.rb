def +(a, b)
 a + b
end
def putlist(a)
 print '('
 putlist_loop(a)   
end

def putlist_loop(a)
  if(a[1] != nil)
    print a[0]
    print ' '
    putlist_loop(a[1])
  else
    print a[0]
    print ")\n"
  end
end

class Test
def abc(a)
 puts a 
end
end
