#
# racc inner file
#

$KCODE='u'

# keywords
TOKENS = [
  ["!=", :NOT_EQUAL],
  ["!==", :NOT_EQ],
  ["%=", :MOD_LET],
  ["&&", :LOGICAL_AND],
  ["&=", :AND_LET],
  ["*=", :MUL_LET],
  ["++", :INCREMENT],
  ["+=", :ADD_LET],
  ["--", :DECREMENT],
  ["-=", :SUB_LET],
  ["/=", :DIV_LET],
  ["<<", :SHIFT_LEFT],
  ["<<=", :SHIFT_LEFT_LET],
  ["<=", :LESS_EQUAL],
  ["==", :EQUAL],
  ["===", :EQ],
  [">=", :GRATER_EQUAL],
  [">>", :SHIFT_RIGHT],
  [">>=", :SHIFT_RIGHT_LET],
  [">>>", :U_SHIFT_RIGHT],
  [">>>=", :U_SHIFT_RIGHT_LET],
  ["^=", :NOT_LET],
  ["abstract", :ABSTRACT],
  ["boolean", :BOOLEAN],
  ["break", :BREAK],
#  ["byte", :BYTE],
  ["case", :CASE],
  ["catch", :CATCH],
  ["char", :CHAR],
  ["class", :CLASS],
  ["const", :CONST],
  ["continue", :CONTINUE],
  ["debugger", :DEBUGGER],
  ["default", :DEFAULT],
  ["delete", :DELETE],
  ["do", :DO],
  ["double", :DOUBLE],
  ["else", :ELSE],
  ["enum", :ENUM],
  ["export", :EXPORT],
  ["extends", :EXTENDS],
  ["false", :FALSE],
  ["final", :FINAL],
  ["finally", :FINALLY],
  ["float", :FLOAT],
  ["for", :FOR],
  ["function", :FUNCTION],
  ["goto", :GOTO],
  ["if", :IF],
  ["implements", :IMPLEMENTS],
  ["import", :IMPORT],
  ["in", :IN],
  ["instanceof", :INSTANCEOF],
  ["int", :INT],
#  ["interface", :INTERFACE],
  ["long", :LONG],
  ["native", :NATIVE],
  ["new", :NEW],
  ["null", :NULL],
  ["package", :PACKAGE],
  ["private", :PRIVATE],
  ["protected", :PROTECTED],
  ["public", :PUBLIC],
  ["return", :RETURN],
  ["short", :SHORT],
  ["static", :STATIC],
  ["super", :SUPER],
  ["switch", :SWITCH],
  ["synchronized", :SYNCHRONIZED],
  ["this", :THIS],
  ["throw", :THROW],
#  ["throws", :THROWS],
  ["transient", :TRANSIENT],
  ["true", :TRUE],
  ["try", :TRY],
  ["typeof", :TYPEOF],
  ["var", :VAR],
  ["void", :VOID],
  ["volatile", :VOLATILE],
  ["while", :WHILE],
  ["with", :WITH],
  ["|=", :OR_LET],
  ["||", :LOGICAL_OR]
]


# 正規表現が続く記号
REOK3 = /^===|^!==/
REOK2 = /^\|\||^\|=|^\^=|^>=|^==|^<=|^&=|^&&|^!=/
REOK1 = /^\}|^\||^\{|^\^|^\[|^\?|^=|^;|^:|^,|^\(|^&|^!/
    
# 正規表現が続かない記号
RENG4 = /^>>>=/;
RENG3 = /^>>>|^>>=|^<<=/
RENG2 = /^>>|^<<|^\/=|^\-=|^\-\-|^\+=|^\+\+|^\*=|^%=/
RENG1 = /^~|^\]|^>|^<|^\/|^.|^\-|^\+|^\*|^\)|^%/




####################################################
####################################################
####################################################


def initialize()

end

def isToken(str) 
  str = str.to_s
  TOKENS.each do |pair|
    if str == pair[0] then 
      return pair[1]
    end
  end
  return nil
end

def get()  # Scanner.prototype.get = function () {
  result = nil
  comment = false
  while true
    if @output.size > 0 then
      result = @output.shift
      break
    end

    if @buffer == nil or @buffer.size == 0 then
      if @lines.size == 0 then
        result = [0,nil]
        break
      end
      @buffer = @lines.shift
    end

    if comment then
      if @buffer =~ /(.*\*\/)/ then
        @buffer = $' 
        comment = false
      else
        @buffer = nil
      end
    else
      saveBuffer = @buffer.dup

      if @buffer =~ /^[ \t\r\n]+/ then
        @buffer = $'
      elsif @buffer =~ /^\/\//  then
        @buffer = nil
      elsif @buffer =~ /^\/\*/ then
        @buffer = $'
        comment = true
      elsif @buffer =~ /^"((?:[^"\\]|\\.)*)"/  then #"  # for ruby mode..
        @output.push([:STRING_LITERAL, $1])
        @buffer = $'
        @regexp = false
      elsif @buffer =~ /^'((?:[^'\\]|\\.)*)'/ then #'   # for ruby mode..
        @output.push([:STRING_LITERAL, $1]);
        @buffer = $'
        @regexp = false
      elsif @regexp and @buffer =~ /^(\/(?:[^\/\\]|\\.)*\/[igIG]*)/ then
        @output.push([:REGEXP_LITERAL, $1]);
        @buffer = $'
        @regexp = false
      elsif @buffer =~ /^(0x[0-9A-Fa-f]+)/ then
        @output.push([:NUMERIC_LITERAL, $1]);
        @buffer = $'
        @regexp = false
      elsif @buffer =~ /^([0-9]*\.[0-9]+(?:e|E)(?:\-|\+|)[0-9]+)/ then
        @output.push([:NUMERIC_LITERAL, $1]);
        @buffer = $'
        @regexp = false
      elsif @buffer =~ /^([0-9]+(?:e|E)(?:\-|\+|)[0-9]+)/ then 
        @output.push([:NUMERIC_LITERAL, $1]);
        @buffer = $'
        @regexp = false
      elsif @buffer =~ /^([0-9]*\.[0-9]+)/ then
        @output.push([:NUMERIC_LITERAL, $1]);   
        @buffer = $'
        @regexp = false
      elsif @buffer =~ /^([0-9]+)/ then 
        @output.push([:NUMERIC_LITERAL, $1]);
        @buffer = $'
        @regexp = false
      elsif @buffer =~ /^([_\$A-Za-z][_\$0-9A-Za-z]*)/ then
        token = $1
        @buffer = $'
        tokenId = isToken(token)
        if tokenId then
          @output.push([tokenId, token])
        else 
          @output.push([:IDENTIFIER, token]);
        end
        @regexp = false
      elsif @buffer =~ REOK3 or @buffer =~ REOK2 or @buffer =~ REOK1 then
        token = Regexp.last_match
        @buffer = $'
        tokenId = isToken(token)
        if tokenId then
          @output.push([tokenId, token])
        else
          @output.push([token[0], token]);
        end
        @regexp = true
      elsif @buffer =~ RENG4 or @buffer =~ RENG3 or @buffer =~ RENG2 or @buffer =~ RENG1 then
        token = Regexp.last_match
        @buffer = $'
        tokenId = isToken(token)
        if tokenId then
          @output.push([tokenId, token])
        else
          @output.push([token[0], token])
        end
        @regexp = false
      end
      if saveBuffer.size > 0 and saveBuffer == @buffer then
        raise "token error!"
      end
    end
  end
  return result
end



def ep(*args)
  STDERR.print *args
end
def lep(*args)
  STDERR.print *args
end

# opsym : :less とか
def pushbinop(opsym)
  r=pop(:exp)
  l=pop(:exp)
  push(:exp, [:binop, l, [:op, opsym], r] ) 
end

def push(*args)
  raise "push: cannot push empty array" if args.size == 0
  if typeof(args[0]) != Symbol then
    raise "push: first element must be a symbol. join:#{args.join('|')}" 
  end
#  ep "(#{args.join(':')}) "
  if args[0] == :lit then
    ep "(#{args[0]}=#{args[1]}) "
  else
    ep "(#{args[0]}) "
  end 
  @stack.push(args)
end

def pop(*args)
  top = @stack.pop()
  if !top then
    raise "pop: stack top is nil! args:'#{args}'"
  end
  if args[0] == nil then 
    return top
  else
    args.each do |sym|
      if sym == top[0] then
        return top
      end
    end
    # not found
    ep "\n==================\n"
    @stack.push(top)
    pp @stack
    raise "pop: found invlalid sym '#{top[0]}'(#{typeof(top[0])}) expected:#{args}"
  end
  return nil
end

def popstmt()
  return pop(:var)
end

# get return or break
def poplaststat()
  top = @stack.pop()
  if top and top[0] == :break or top[0] == :return then
    return top
  else
    raise "poplaststat: not found!"
  end
end

# get statements reversed
def mpopelems()
  return mpop(:stmt,:funcdecl)
end

def mpop(*symary)
  syms = {}
  symary.each do |sym| syms[sym] = true end
  out=[]
  while true
    top = @stack.pop
    break if !top
    if syms[top[0]] then
      out.push(top)
    else
      @stack.push(top)
      break
    end
  end
  if out.size==0 then
    ep "### mpop: output is empty : \n"
    pp @stack
    raise "FATAL"
  end
  return out.reverse
end


def next_token
  @q.shift
end

def on_error(t,v,values)
  pp @stack
  raise "ERROR: t:#{t} v:#{v} values:#{values}\n"
end

def escapestr(s)
#  STDERR.print "ESCAPE:#{s}\n"
  ary = s.split("")
  out = []
  ary.each do |ch|
    if ch == "\n" then
      out.push( "\\n" )
    elsif ch == "\"" then
      out.push( "\\\"")
    elsif ch == "\\" then 
      out.push( "\\\\" )
    else
      out.push(ch)
    end
  end
  return out.join("")
end

def ary2s(ary)
  raise "nil arg" if !ary
  out= "s(" 
  if typeof(ary[0])!=Symbol then
    ary.each do |v|
      ep "ARY:#{v}TYPE:#{typeof(v)}\n"
    end
    raise "first element have to be a symbol. type=#{typeof(ary[0])}"
  end
  ary.size.times do |i|
    o = ary[i]
    if typeof(o) == Symbol then
      out+= ":#{o}"
    elsif typeof(o) == Array then
      out+=ary2s(o)
    elsif typeof(o) == NilClass then
      out+="nil"
    elsif typeof(o) == String then
      out+= "\"" + escapestr(o) + "\""
    else
      out+= o.to_s
    end
    out+= "," if i < ary.size-1 
  end
  out+= ")"
  return out
end






def parse(s,fmt,exectest)

  s.gsub!("\\\n","")

  @lines = s.split("\n")
  @buffer = nil
  @parseOk = true #false
  @output = []
  @regexp = true
  @yylval = nil

#  @yydebug = true

  @q=[]   

  while true 
    ntk = get()
    lep "  #{ntk[0]}|#{ntk[1]}"
    if typeof(ntk[0]) != Symbol and typeof(ntk[0]) != String and typeof(ntk[0]) != Fixnum then
      raise "FATAL: invalid type:#{typeof(ntk[0])}" 
    end

    if ntk[0] == 0 then
      lep "EOF\n"
      @q.push([ false, '$end' ])
      break
    else
      @q.push(ntk)
    end
  end
  
  


  @stack = []

  do_parse

#  pp @stack

  topary = @stack.pop
  if @stack.size > 0 then
    ep "\n\nstack mismatch! size:#{@stack.size}\n\n"
    pp @stack
    raise "FATAL"
#    ep "FATAL\n"
  end


  ep "\n"

  pp topary
  if fmt =="s" then
    print ary2s(topary),"\n"
  elsif fmt =="a" then
    pp topary
  end
  if exectest then
    src = "$cnt=0\ndef s(*args)\n$cnt+= args.size\nend\n" + ary2s(topary) + "\nprint $cnt,'\n'\n"
    begin
      eval(src)
    rescue Exception => e
      STDERR.print "FATAL: parse error: #{e}\n",src
      exit 1
    end
  end

end


