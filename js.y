class JS
  # from http://www.h4.dion.ne.jp/~unkai/js/parser.jsy

#prechigh
#preclow

rule

program : {ep"program-empty "; push(:program) }
| elements { ep"program "; e=mpopelems(); push(*([:program]+e) ) }
;

elements : element { ep"elem-first "; }
| elements element { ep"elem-append "; }
;

element : statement { ep"elem-stat ";}
| funcdecl { ep"elem-funcdecl "; }
;

funcdecl : FUNCTION IDENTIFIER '(' paramlist ')' '{' funcbody '}' { ep"function-decl "; fb=pop(:funcbody); pl=pop(:paramlist); push(:funcdecl, val[1].to_sym, pl, fb) }
;

function_expression : FUNCTION IDENTIFIER '(' paramlist ')' '{' funcbody '}' { ep"function-expr "; fb=pop(:funcbody); pl=pop(:paramlist); push(:exp, [:function, val[1].to_sym, pl, fb ] ) }
| FUNCTION '(' paramlist ')' '{' funcbody '}' { ep"anonfunction-expr "; fb=pop(:funcbody); pl=pop(:paramlist); push(:exp, [:function, nil, pl,fb ] ) }
;

paramlist : { ep"paramlist-empty "; push(:paramlist) }
| IDENTIFIER { ep"paramlist-first-id "; push(:paramlist, val[0].to_sym ) }
| paramlist ',' IDENTIFIER { ep"paramlist-append-id "; pl = pop(:paramlist); pl.push(val[2].to_sym); push(*pl) }
;

funcbody : { ep"funcbody-empty "; push(:funcbody) }
| elements { ep"funcbody-elems "; e=mpopelems(); push(*([:funcbody]+e))}
;

stmtlist : statement { ep"stmtlist-first "; s=pop(:stmt); push(:stmtlist,s) }
| stmtlist statement { ep"stmtlist-append "; s=pop(:stmt); sl=pop(:stmtlist); sl.push(s); push(*sl) }
;

statement : block { ep"stmt-block "; b=pop(:block); push(:stmt,b) }
| funcdecl { ep"stmt-funcdecl "; f=pop(:funcdecl); push(:stmt, f) }
| var_statement { ep"stmt-var ";v=pop(:var); push(:stmt,v) }
| empty_statement { ep"stmt-empty "; push(:stmt) }
| expression_statement { ep"stmt-expr "; }
| if_statement { ep"stmt-if "; }
| iteration_statement { ep"stmt-iter "; }
| continue_statement { ep"stmt-cont "; }
| break_statement { ep"stmt-break "; }
| return_statement { ep"stmt-ret "; }
| with_statement { ep"stmt-with "; }
| labelled_statement { ep"stmt-labelled "; }
| switch_statement { ep"stmt-switch "; s=pop(:switch); push(:stmt, s) } 
| throw_statement { ep"stmt-throw "; t=pop(:throw); push(:stmt,t)}
| try_statement { ep"stmt-try "; t=pop(:try); push(:stmt,t)}
;

block : '{' '}' { ep"block-empty "; push(:block) }
| '{' stmtlist '}' { ep"block-stmtlist ";sl=pop(:stmtlist); ep"LEN:#{sl.size}\n" ; push(*([:block]+sl)) }
;


var_statement : VAR vardeclist ';' { ep"varstat "; l=pop(:varlist); push(:var, l ) }
| VAR vardeclist { ep"varstat-wo-semi "; l=pop(:varlist); push(:var, l) }
;

vardeclist : vardecl { ep"vardeclist-first "; decl=pop(:vardecl); push(:varlist,decl)  }
| vardeclist ',' vardecl { ep"vardeclist-append "; decl=pop(:vardecl); list=pop(:varlist); list.push(decl); push(*list) }
;


vardecl : IDENTIFIER initialiser { ep"vardecl-init "; init=pop(:init); push(:vardecl, val[0].to_sym, init ) }
| IDENTIFIER { ep"vardecl "; push(:vardecl, val[0].to_sym, nil ) }
;

initialiser : '=' assignment_expression { ep"init=asgn "; exp=pop(:exp); push(:init,exp) }
;


empty_statement : ';' { ep"emptystat "; }
;

expression_statement : expression semi_opt { ep"expr ";e=pop(:exp);  push(:stmt,e) }    
;



if_statement : IF '(' expression ')' statement ELSE statement { ep"if-else "; ef=pop(:stmt); et=pop(:stmt); e=pop(:exp); push(:stmt, [:if,e,et,ef])}
| IF '(' expression ')' statement { ep"if "; s=pop(:stmt); e=pop(:exp); push(:stmt, [:if,e,s,nil]) }
;

iteration_statement : DO statement WHILE '(' expression ')' ';' { ep"do "; e=pop(:exp); s=pop(:stmt); push(:stmt, [:do, e,s]) }
| WHILE '(' expression ')' statement { ep"while "; }
| FOR '(' expression_for1 ';' expression_for2 ';' expression_for3 ')' statement { ep"for3 "; s=pop(:stmt); f3=pop(:for3); f2=pop(:for2); f1=pop(:for1); push(:stmt, [:for, f1[1],f2[1],f3[1], s] ) }
| FOR '(' VAR vardeclist ';' expression_for2 ';' expression_for3 ')' statement { ep"for3var "; s=pop(:stmt); f3=pop(:for3); f2=pop(:for2); f1=pop(:varlist); push(:stmt, [:for,f1,f2[1],f3[1],s] ) }
| FOR '(' left_expression IN expression ')' statement { ep"forin "; s=pop(:stmt); tgt=pop(:exp); it=pop(:exp); push(:stmt, [:forin, it,tgt,s])}
| FOR '(' VAR vardecl IN expression ')' statement { ep"forvarin "; s=pop(:stmt); tgt=pop(:exp); v=pop(:vardecl); push(:stmt, [:forin,v,tgt,s])}
;

expression_for1: { ep"exfor1-empty "; push(:for1) }
| expression { ep"exfor1-expr "; push(:for1,pop(:exp)) }
;

expression_for2: { ep"exfor2-empty "; push(:for2) }
| expression { ep"exfor2-expr "; push(:for2,pop(:exp)) }
;

expression_for3: { ep"exfor3-empty "; push(:for3) }
| expression { ep"exfor3-expr "; push(:for3,pop(:exp)) }
;

continue_statement : CONTINUE IDENTIFIER semi_opt { ep"continue-with-id "; push(:stmt, [:continue,val[1].to_sym]) }
| CONTINUE semi_opt { ep"continue "; push(:stmt,[:continue,nil]) }
;

break_statement : BREAK IDENTIFIER semi_opt { ep"break-l "; push(:stmt,[:break, val[1].to_sym]) }
| BREAK semi_opt { ep"break "; push(:stmt,[:break,nil]) }
;

return_statement : RETURN expression semi_opt { ep"return1 "; e=pop(:exp); push(:stmt, [:return,e])  }
| RETURN semi_opt { ep"return2 "; push(:stmt, [:return]) }
;

semi_opt : { ep"semi-empty "; }
| ';' { ep"semi-semi "; }
;

with_statement : WITH '(' expression ')' statement { ep"with "; }
;

switch_statement : SWITCH '(' expression ')' case_block { ep"sw ";c=mpop(:case,:default); e=pop(:exp); push(*([:switch,e]+c)) }
;

case_block : '{' case_clauses_opt '}' { ep"case-wo-default "; }
| '{' case_clauses_opt default_clause case_clauses_opt '}' { ep"case-default "; }
;

case_clauses : case_clause { ep"case-first "; }
| case_clauses case_clause { ep"case-append "; }
;

case_clause : CASE expression ':' stmtlist_opt { ep"case ";sl=pop(:stmtlist); e=pop(:exp); push(:case,e,sl)  }
;

default_clause : DEFAULT ':' stmtlist_opt { ep"default "; sl=pop(:stmtlist); push(:default,sl) }
;

labelled_statement : IDENTIFIER ':' statement { ep"labelled "; s=pop(:stmt); push(:stmt, [:labelled, val[0].to_sym, s] ) }
;

throw_statement : THROW expression semi_opt { ep"throw "; e=pop(:exp); push(:throw,e) }
;

try_statement : TRY block catch { ep"try-catch "; b=pop(:block); push(:try,b,nil,nil) }
| TRY block finally { ep"try-finally "; f=pop(:finally); b=pop(:block); push(:try,b,nil,f) }
| TRY block catch finally { ep"try-catch-finally "; f=pop(:finally); c=pop(:catch);b=pop(:block); push(:try,b,c,f) }
;

catch : CATCH '(' IDENTIFIER ')' block { ep"catch "; b=pop(:block);push(:catch, val[2].to_sym, b)}
;

finally : FINALLY block { ep"finally "; b=pop(:block); push(:finally,b) }
;

stmtlist_opt : { ep"stmtlist-empty "; }
| stmtlist { ep"stmtlist "; }
;

case_clauses_opt : { ep"caseopt-empty "; }
| case_clauses { ep"caseopt "; }
;

literal : null_literal { ep"lit-null "; v=pop(:null); push(:lit,v) }
| boolean_literal { ep"lit-bool "; v=pop(); push(:lit,v) }
| NUMERIC_LITERAL { ep"lit-num "; push(:lit, val[0].to_f )}
| STRING_LITERAL { ep"lit-str "; push(:lit, [:str, val[0] ]) }
| REGEXP_LITERAL { ep"lit-regex "; push(:lit, [:regex, val[0]]) }
;

null_literal : NULL { ep"null-lit "; push(:null) }
;

boolean_literal : TRUE { ep"bool-true-lit "; push(:true) }
| FALSE { ep"bool-false-lit "; push(:false) }
;

primary_expression : THIS { ep"pexp-this "; push(:this) }
| IDENTIFIER { ep"pexp-id "; push(:exp, [:id, val[0].to_sym] ) }
| literal { ep"pexp-lit "; l=pop(:lit); push(:exp,l) }
| array_literal { ep"pexp-ary-lit "; lit=pop(:arylit); push(:exp,lit) }
| object_literal { ep"pexp-obj-lit "; lit=pop(:objlit); push(:exp,lit) }
| '(' expression ')' { ep"pexp-paren-exp "; }
;

array_literal : '[' elision_opt ']' { ep"ary-lit1 "; push(:arylit ) }
| '[' element_list ']' { ep"ary-lit2 "; el=pop(:elemlist); push(:arylit,el) }
| '[' element_list ',' elision_opt ']' { ep"ary-lit3 "; el=pop(:elemlist); push(:arylit,el) }
;

element_list : elision_opt assignment_expression { ep"el-first "; e=pop(:exp); push(:elemlist,e) }
| element_list ',' elision_opt assignment_expression { ep"el-append "; e=pop(:exp); el=pop(:elemlist); el.push(e); push(*el) }
;

elision_opt
: { ep"elis-empty "; }
| elision { ep"elis-opt "; }
;

elision : ',' { ep"elis-comma "; }
| elision ',' { ep"elis-elision "; }
;

object_literal : '{' '}' { ep"obj-empty "; push(:objlit, nil ) }
| '{' proplist '}' { ep"obj-lit "; pl=pop(:proplist); push(:objlit,pl) }
;

proplist : propname ':' assignment_expression { ep"proplist-first "; a=pop(:exp); n=pop(:propname); push(:proplist, [:prop, n,a])  }
| proplist ',' propname ':' assignment_expression { ep"proplist-append "; a=pop(:exp); n=pop(:propname); pl=pop(:proplist); pl.push([:prop,n,a]); push(*pl) }
;

propname : IDENTIFIER { ep"propname-id "; push(:propname, [:id, val[0].to_sym] ) }
| STRING_LITERAL { ep"propname-strlit "; push(:propname, [:lit, [:str, val[0]]])  }
| NUMERIC_LITERAL { ep"propname-numlit "; push(:propname, [:lit, val[0].to_f])}
;

member_expression : primary_expression { ep"mexp-pexp "; e=pop(:exp); push(*e)}
| function_expression { ep"mexp-func "; }
| member_expression '[' expression ']' { ep"mexp-mexp[] "; }
| member_expression '.' IDENTIFIER { ep"mexp-dot-id "; }
| NEW member_expression arguments { ep"mexp-new-mexp-args "; }
;

new_expression : member_expression { ep"newexp-memb "; }
| NEW new_expression { ep"newexp-new "; }
;

call_expression : member_expression arguments { ep"call-mem-args "; }
| call_expression arguments { ep"call-call-args "; }
| call_expression '[' expression ']' { ep"call-call-[exp] "; }
| call_expression '.' IDENTIFIER { ep"call-call-dot-id "; }
;

arguments : '(' ')' { ep"args-empty "; }
| '(' argument_list ')' { ep"args-arglist "; }
;

argument_list : assignment_expression { ep"arglist-first "; }
| argument_list ',' assignment_expression { ep"arglist-append "; }
;

left_expression : new_expression { ep"lefthand-new "; }
| call_expression { ep"lefthand-call "; }
;

postfix_expression : left_expression { ep"P0 "; }
| left_expression INCREMENT { ep"postfix-lefthand-incr "; left=pop(:exp); push(:exp,[:asgn, left, [:op, :equal], [:exp, [:increment, left.dup]]]) }
| left_expression DECREMENT { ep"postfix-lefthand-decl "; left=pop(:exp); push(:exp,[:asgn, left, [:op, :equal], [:exp, [:decrement, left.dup]]]) }
;

unary_expression : postfix_expression { ep"P1 "; }
| DELETE unary_expression { ep"unary-delete "; e=pop(:exp); push(:exp,[:unary, :delete, e]); }
| VOID unary_expression { ep"unary-void "; e=pop(:exp); push(:exp,[:unary, :void, e]); }
| TYPEOF unary_expression { ep"unary-typeof "; e=pop(:exp); push(:exp,[:unary, :typeof, e]); }
| INCREMENT unary_expression { ep"unary-increment "; e=pop(:exp); push(:exp,[:unary, :inrement, e]); }
| DECREMENT unary_expression { ep"unary-decrement "; e=pop(:exp); push(:exp,[:unary, :decrement, e]); }
| '+' unary_expression { ep"unary-+ "; e=pop(:exp); push(:exp,[:unary, :positive, e]); }
| '-' unary_expression { ep"unary-- "; e=pop(:exp); push(:exp,[:unary, :negative, e]); }
| '~' unary_expression { ep"unary-~ "; e=pop(:exp); push(:exp,[:unary, :nor, e]); }
| '!' unary_expression { ep"unary-! "; e=pop(:exp); push(:exp,[:unary, :not, e]); }
;

multiplicative_expression : unary_expression { ep"P2 "; }
| multiplicative_expression '*' unary_expression { ep"multiplicative-'*' "; pushbinop( :mul )}
| multiplicative_expression '/' unary_expression { ep"multiplicative-'/' "; pushbinop( :div )}
| multiplicative_expression '%' unary_expression { ep"multiplicative-'%' "; pushbinop( :mod )}
;

additive_expression : multiplicative_expression { ep"P3 "; }
| additive_expression '+' multiplicative_expression { ep"adtv+ "; pushbinop( :add ) }
| additive_expression '-' multiplicative_expression { ep"adtv- "; pushbinop( :sub ) }
;

shift_expression : additive_expression { ep"P4 "; }
| shift_expression SHIFT_LEFT additive_expression { ep"shift-sl "; pushbinop( :lshift ) }
| shift_expression SHIFT_RIGHT additive_expression { ep"shift-sr "; pushbinop( :rshift ) }
| shift_expression U_SHIFT_RIGHT additive_expression { ep"shift-sur "; pushbinop( :rushift ) }
;

relational_expression : shift_expression { ep"P5 "; }
| relational_expression '<' shift_expression { ep"rel-< "; pushbinop( :less ) }
| relational_expression '>' shift_expression { ep"rel-> "; pushbinop( :grater ) }
| relational_expression LESS_EQUAL shift_expression { ep"rel-less "; pushbinop( :leq ) }
| relational_expression GRATER_EQUAL shift_expression { ep"rel-grater "; pushbinop( :geq ) }
| relational_expression INSTANCEOF shift_expression { ep"rel-instanceof "; pushbinop( :instanceof) }
| relational_expression IN shift_expression { ep"rel-in "; pushbinop( :in ) }
;

equality_expression : relational_expression { ep"P6 "; }
| equality_expression EQUAL relational_expression { ep"eq "; pushbinop( :equal ) }
| equality_expression NOT_EQUAL relational_expression { ep"neql "; pushbinop( :notequal ) }
| equality_expression EQ relational_expression { ep"eq "; pushbinop( :eq ) }
| equality_expression NOT_EQ relational_expression { ep"neq "; pushbinop( :neq )}
;


bitwise_and_expression : equality_expression { ep"P7 "; }
| bitwise_and_expression '&' equality_expression { ep"bw-and "; pushbinop( :bitand ) }
;


bitwise_xor_expression : bitwise_and_expression { ep"P8 "; }
| bitwise_xor_expression '^' bitwise_and_expression { ep"bw-xor "; pushbinop( :bitxor ) }
;

bitwise_or_expression : bitwise_xor_expression { ep"P9 "; }
| bitwise_or_expression '|' bitwise_xor_expression { ep"bw-or "; pushbinop( :bitor ) }
;


logical_and_expression : bitwise_or_expression { ep"P10 "; }
| logical_and_expression LOGICAL_AND bitwise_or_expression { ep"&& "; er=pop(:exp); el=pop(:exp); push(:exp,[:logical_and,el,er]) }
;

logical_or_expression : logical_and_expression { ep"P11 "; }
| logical_or_expression LOGICAL_OR logical_and_expression { ep"|| "; er=pop(:exp); el=pop(:exp); push(:exp,[:logical_or,el,er]) }
;

conditional_expression : logical_or_expression { ep"P12 "; }
| logical_or_expression '?' assignment_expression ':' assignment_expression { ep"cond-3op "; f=pop(:exp); t=pop(:exp); cond=pop(:exp); push( :exp, [:cond,cond, t,f ] ) }
;

assignment_expression : conditional_expression { ep"P13 "; }
| left_expression assignment_operator assignment_expression { ep"asgnexp-left-op-asgn "; a=pop(:exp); op=pop(:op);left=pop(:exp); push(:exp, [:asgn,left,op,a]) }
;

assignment_operator : '=' { ep"asgnop-equal "; push(:op, :equal) }
| MUL_LET { ep"asgnop-mul-let "; push(:op, :mul_let ) }
| DIV_LET { ep"asgnop-div-let "; push(:op, :div_let ) }
| MOD_LET { ep"asgnop-mod-let "; push(:op, :mod_let ) }
| ADD_LET { ep"asgnop-add-let "; push(:op, :add_let ) }
| SUB_LET { ep"asgnop-sub-let "; push(:op, :sub_let ) }
| SHIFT_LEFT_LET { ep"asgnop-shift-left-let "; push(:op, :shift_left_let ) }
| SHIFT_RIGHT_LET { ep"asgnop-shift-right-let "; push(:op, :shift_right_let ) }
| U_SHIFT_RIGHT_LET { ep"asgnop-u-shift-right-let "; push(:op, :ushift_right_let ) }
| AND_LET { ep"asgnop-and-let "; push(:op, :and_let ) }
| NOT_LET { ep"asgnop-not-let "; push(:op, :not_let ) }
| OR_LET { ep"asgnop-or-let "; push(:op, :or_let ) }
;

expression : assignment_expression { ep"exp-asgn-first "; }
| expression ',' assignment_expression { ep"exp-asgn-append "; }
;




end

---- header = header.rb
---- inner = inner.rb
---- footer = footer.rb
