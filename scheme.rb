require 'yasm'

def scheme(text)
  s = eval text.gsub( /(\()|(\))|(\d+)|([^()\s]+)/ ) {
    case
      when $1 then "["
      when $2 then $'[/\S/] ? "]," : "]"
      when $3 then "#$3,"
      else         ":#$4,"
    end
  }

  iseq = YASM.toplevel {
    scm_compile s
    leave
  }
  
  iseq.eval
end

class YASM::SimpleDataBuilder
  def scm_compile(s)
    case s
    when Array
      op,*arg = s
      case op
      when :+
        scm_compile arg[0]
        arg[1..-1].each{|a| scm_compile a; send :+,1}
      when :-
        scm_compile arg[0]
        arg[1..-1].each{|a| scm_compile a; send :-,1}
      when :*
        scm_compile arg[0]
        arg[1..-1].each{|a| scm_compile a; send :*,1}
      when :/
        scm_compile arg[0]
        arg[1..-1].each{|a| scm_compile a; send :/,1}
      when :define
        mname, *mparam = arg[0]
        putnil
        definemethod mname, YASM.method(mname, mparam) {
          scm_compile arg[1]
          leave
        }
        putnil
      when :==
          scm_compile arg[0]; scm_compile arg[1]; send :==, 1
      when :<
        scm_compile arg[0]; scm_compile arg[1]; send :<, 1
      when :>
        scm_compile arg[0]; scm_compile arg[1]; send :>, 1
      when :<=
          scm_compile arg[0]; scm_compile arg[1]; send :<=, 1
      when :>=
          scm_compile arg[0]; scm_compile arg[1]; send :>=, 1
      when :if
          scm_compile arg[0]
        branchunless :else_part
        scm_compile arg[1]
        jump :end
        _ :else_part
        scm_compile arg[2]
        _ :end
      else
        putnil
        arg.each{|a| scm_compile a}
        call op, arg.size
      end
    when Symbol
      getlocal s
    else
      putobject s
    end
  end
end

if __FILE__ == $0

def cube(x)
  x * x * x
end
#p scheme('(cube 11)')

scheme <<EOS
  (define (norm x y)
     (+ (* x x) (* y y)))
EOS
p norm(3,4)  # ==> 25

scheme <<EOS
  (define (facs x)
     (if (== x 0) 1 (* x (facr (- x 1)))))
EOS
def facr(x)
  if x == 0 then 1 else x * facs(x-1) end
end
p facs(10), facr(10)   # 3628800 \n 3628800

end
