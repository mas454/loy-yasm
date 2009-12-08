class Reader
  def init
    @indexOfLine = 0
    @lineLength = 0
    @charBuff = []
  end
  def initialize
    init
  end
  def read(i)
    str = ""
    lparen = 0
    while(!i.eof?)
      ch = i.getc.chr #.id2name
      if ch == "("
        lparen += 1
      elsif ch == ")"
        lparen -= 1
        str.concat(")")
        if lparen == 0
          return str
        end
      end
      str = str.concat(ch)
    end
    str
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
      if nch == ")"
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
      when "-"
	return makeMinusNumber
      when "\""
	return makeString
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
    while @indexOfLine <= @lineLength
      if @ch == "(" || @ch == ")"
	break
      elsif @ch == " "
	break
      elsif /[^0-9]/ =~  @ch && ch != '-'
	@indexOfLine -= 1
	return makeSymbolInternal(str)
      end
      str.concat(@ch)
      getChar
    end
    str.to_i
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
    list[0] = :quote
    getChar
    list[1] = getSexp
    top
  end
  def makeString
    str = ""
    while @indexOfLine < @lineLength
      getChar
      if @ch == "\""
	break
      end
      str.concat(@ch)
    end
    str
  end
end
i=open("lib.loy", "r")
r = Reader.new
puts r.read(i)
#puts r.lex("(a . c)").to_s
