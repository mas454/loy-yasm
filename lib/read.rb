class Reader
  def init
    @indexOfLine = 0
    @lineLength = 0
    @charBuff = []
  end
  def initialize
    init
  end
  def read(io)
    sexp = ""
    lparen = 0
    while(!io.eof?)
      #ch = i.getc.chr #.id2name
      #str = io.gets.chop!
      str = io.gets.sub("\n", " ")
      i = 0
      while str[i] != nil
       if str[i] == "("
        lparen += 1

       elsif str[i] == ")"
        lparen -= 1
       elsif str[i] == ";"
        break
       end
       i += 1
       if (lparen == 0) & (str[i-1] != "'")
          sexp = sexp + str
          return sexp
       end
      end
      if !(str[i] == ";")
        sexp=sexp+str+" "
      end
    end
    sexp
  end
  def lex(s="")
    @indexOfLine = 0;
    @line = s;
    @lineLength = @line.length;
    getChar
    getSexp
  end
  def getChar
    if @line[@indexOfLine] != nil
      @ch = @line[@indexOfLine].chr
    end
    @indexOfLine += 1
  end
  def nextChar
    if @line[@indexOfLine] != nil
      nch = @line[@indexOfLine].chr
    else
      nch = nil
    end
    while @ch == " "
      if nch == ")" or nch == nil
	getChar
	break
      end
      if @indexOfLine >= @lineLength || @indexOfLine + 1 >= @lineLength
	break
      end
      @ch = nch
      @indexOfLine += 1
      if @line[@indexOfLine] != nil
	nch = @line[@indexOfLine].chr
      else
	nch = nil
      end
    end
    nch
  end
  def getSexp
    while @indexOfLine <= @lineLength
      case @ch
      when "(" 
	return makeList
      when "'"
	return makeQuote
      when "`"
        return makeQuasiquote
      when ","
        if @line[@indexOfLine] == "@"
          getChar
          return makeUnquote_splicing
        else
          return makeUnquote
        end
      when "-"
	return makeMinusNumber
      when "\""
	s = makeString
        return s
      else
	if /\s/ =~ @ch
	  #break
	elsif /[0-9]/ =~ @ch
	  return makeNumber
	else #end
          return makeSymbol
        end
      end
      getChar
    end
    return nil
  end
  
  def makeNumber
    str = ""
    b = false
    while @indexOfLine <= @lineLength
      if @ch == "(" || @ch == ")"
	break
      elsif (@ch == ".") & !b
        b = true
      elsif @ch == " "
	break
      elsif /[^0-9]/ =~  @ch && @ch != '-'
	@indexOfLine -= 1
	return makeSymbolInternal(str)
      end
      str.concat(@ch)
      getChar
    end
    if b
     str.to_f
    else
     str.to_i
    end
  end
  def makeMinusNumber
    nch = @line[@indexOfLine].chr
    if /[^0-9]/ =~ nch
      return makeSymbolInternal(@ch)
    end
    makeNumber
  end

  def makeSymbol
    makeSymbolInternal(@ch)
  end

  def makeSymbolInternal(str)
    while(@indexOfLine < @lineLength)
      getChar
      if @ch == "(" || @ch == ")"
	break
      elsif @ch == " "
	break
      end
      str.concat(@ch)
    end
    str.to_sym
  end
  def makeList
    getChar
    nextChar
    if @ch == ")"
      getChar
      return nil
    end
    top =  Array.new 2
    list = top
    while true
      list[0] = getSexp
      nextChar
      if @ch == ")"
	break
      elsif @indexOfLine == @lineLength
	return nil
      elsif @ch == "."
	getChar
	list[1] = getSexp
	getChar
	return top
      end
      list[1] = Array.new 2
      list = list[1]
    end
    
    getChar
    top
  end
  def makeQuote
    top = Array.new 2
    list = top
    qulist = Array.new 2
    list[0] = :quote
    getChar
    sexp = getSexp
    #if sexp.kind_of?(Array) 
     list[1] = [sexp, nil]
    #else
     #list[1] = sexp
    #end
    top
  end
  def makeQuasiquote
    top = Array.new 2
    list = top
    qulist = Array.new 2
    list[0] = :quasiquote
    getChar
    sexp = getSexp
    list[1] = [sexp, nil]
    top
  end
  
  def makeUnquote
    top = Array.new 2
    list = top
    qulist = Array.new 2
    list[0] = :unquote
    getChar
    sexp = getSexp
    list[1] = [sexp, nil]
    top
  end
  def makeUnquote_splicing
    top = Array.new 2
    list = top
    qulist = Array.new 2
    list[0] = :"unquote-splicing"
    getChar
    sexp = getSexp
    list[1] = [sexp, nil]
    top
  end
  def makeString
    str = ""
    while @indexOfLine < @lineLength
      getChar
      if @ch == "\\"
          str.concat(@ch)
          getChar
      elsif @ch == "\""
        getChar
	break
      end
      str.concat(@ch)
    end
    str
  end
end
#i=open("lib.loy", "r")
#r = Reader.new
#puts r.read(i)
#puts r.lex("(a . c)").to_s

