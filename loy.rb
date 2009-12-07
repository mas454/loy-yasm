class Array
 def get(num)
  self[num]
 end	
end

def display(sexp)
 putlist sexp
 print "\n"
end

def putlist(a)
 if a.kind_of?(Array)
  print '('
  putlist_loop(a)
 else
  print a
 end
end

def putlist_loop(a)
  if(a[1] != nil)
    putlist a[0]
    print ' '
    putlist_loop(a[1])
  else
    putlist a[0]
    print ")"
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
