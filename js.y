class JS
  # from http://www.h4.dion.ne.jp/~unkai/js/parser.jsy

#prechigh
#preclow

rule

program : {ep"program-empty "; }
| elements { ep"program "; }
;

elements : element { ep"elem-first "; }
| elements element { ep"elem-append "; }
;

element : statement { ep"elem-stat "; }
| funcdecl { ep"elem-funcdecl "; }
;

funcdecl : FUNCTION IDENTIFIER '(' paramlist ')' '{' funcbody '}' { ep"function-decl "; }
;

function_expression : FUNCTION IDENTIFIER '(' paramlist ')' '{' funcbody '}' { ep"function-expr "; }
| FUNCTION '(' paramlist ')' '{' funcbody '}' { ep"anonfunction-expr "; }
;

paramlist : { ep"paramlist-empty "; }
| IDENTIFIER { ep"paramlist-first-id "; }
| paramlist ',' IDENTIFIER { ep"paramlist-append-id "; }
;

funcbody : { ep"funcbody-empty "; }
| elements { ep"funcbody-elems "; }
;

statlist : statement { ep"stmtlist-first "; }
| statlist statement { ep"stmtlist-append "; }
;

statement : block { ep"stmt-block ";}
| var_statement { ep"stmt-var "; }
| empty_statement { ep"stmt-empty "; }
| expression_statement { ep"stmt-expr "; }
| if_statement { ep"stmt-if "; }
| iteration_statement { ep"stmt-iter "; }
| continue_statement { ep"stmt-cont "; }
| break_statement { ep"stmt-break "; }
| return_statement { ep"stmt-ret "; }
| with_statement { ep"stmt-with "; }
| labelled_statement { ep"stmt-labelled "; }
| switch_statement { ep"stmt-switch "; } 
| throw_statement { ep"stmt-throw "; }
| try_statement { ep"stmt-try "; }
;

block : '{' '}' { ep"block-empty "; }
| '{' statlist '}' { ep"block-stmtlist "; }
;


var_statement : VAR vardeclist ';' { ep"varstat "; list=pop(:varlist); push(:var, list ) }
| VAR vardeclist { ep"varstat-wo-semi "; push(:var, [:vardeclist]) }
;

vardeclist : vardecl { ep"vardeclist-first "; decl=pop(:vardecl); push(:varlist,decl)  }
| vardeclist ',' vardecl { ep"vardeclist-append "; list=pop(:varlist); decl=pop(:vardecl); list.push(decl); push(*list) }
;


vardecl : IDENTIFIER initialiser { ep"vardecl-init "; init=pop(:init); push(:vardecl, :id,val[0].to_sym, init ) }
| IDENTIFIER { ep"vardecl "; init=pop(:init); push(:vardecl, :id,val[0].to_sym, init ) }
;

initialiser : '=' assignment_expression { ep"init=asgn "; exp=pop(:exp); push(:init,exp) }
;


empty_statement : ';' { ep"emptystat "; }
;

expression_statement : expression semi_opt { ep"expr "; }    
;



if_statement : IF '(' expression ')' statement ELSE statement { ep"if-else "; }
| IF '(' expression ')' statement { ep"if-if "; }
;

iteration_statement : DO statement WHILE '(' expression ')' ';' { ep"do "; }
| WHILE '(' expression ')' statement { ep"while "; }
| FOR '(' expression_opt ';' expression_opt ';' expression_opt ')' statement { ep"for3 "; }
| FOR '(' VAR vardeclist ';' expression_opt ';' expression_opt ')' statement { ep"for3var "; }
| FOR '(' left_hand_side_expression IN expression ')' statement { ep"forin "; }
| FOR '(' VAR vardecl IN expression ')' statement { ep"forvarin "; }
;

continue_statement : CONTINUE IDENTIFIER semi_opt { ep"continue-with-id "; }
| CONTINUE semi_opt { ep"continue "; }
;

break_statement : BREAK IDENTIFIER semi_opt { ep"break-with-id "; }
| BREAK semi_opt { ep"break "; }
;

return_statement : RETURN expression_opt semi_opt { ep"return ";  }
;

semi_opt : { ep"semi-empty "; }
| ';' { ep"semi-semi "; }
;

with_statement : WITH '(' expression ')' statement { ep"with "; }
;

switch_statement : SWITCH '(' expression ')' case_block { ep"sw "; }
;

case_block : '{' case_clauses_opt '}' { ep"case-wo-default "; }
| '{' case_clauses_opt default_clause case_clauses_opt '}' { ep"case-default "; }
;

case_clauses : case_clause { ep"case-first "; }
| case_clauses case_clause { ep"case-append "; }
;

case_clause : CASE expression ':' statlist_opt { ep"case "; }
;

default_clause : DEFAULT ':' statlist_opt { ep"default "; }
;

labelled_statement : IDENTIFIER ':' statement { ep"labelled-id-colon-stat "; }
;

throw_statement : THROW expression semi_opt { ep"throw "; }
;

try_statement : TRY block catch { ep"try-catch "; }
| TRY block finally { ep"try-finally "; }
| TRY block catch finally { ep"try-catch-finally "; }
;

catch : CATCH '(' IDENTIFIER ')' block { ep"catch "; }
;

finally : FINALLY block { ep"finally "; }
;

statlist_opt : { ep"stmtlist-empty "; }
| statlist { ep"stmtlist "; }
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
| IDENTIFIER { ep"pexp-id "; push(:id, val[0].to_sym ) }
| literal { ep"pexp-lit "; }
| array_literal { ep"pexp-ary-lit "; }
| object_literal { ep"pexp-obj-lit "; }
| '(' expression ')' { ep"pexp-paren-exp "; }
;

array_literal : '[' elision_opt ']' { ep"ary-lit-[elision] "; }
| '[' element_list ']' { ep"ary-lit-[elemlist] "; }
| '[' element_list ',' elision_opt ']' { ep"ary-list-[elemlist,elision] "; }
;

element_list : elision_opt assignment_expression { ep"elemlist-elision-asgnexp "; }
| element_list ',' elision_opt assignment_expression { ep"elemlist-elemlist-el-as "; }
;

elision_opt
: { ep"elision-opt-empty "; }
| elision { ep"elision-opt "; }
;

elision : ',' { ep"elision-comma "; }
| elision ',' { ep"elision-elision "; }
;

object_literal : '{' property_name_and_value_list_opt '}' { ep"obj-lit "; }
;

property_name_and_value_list_opt : { ep"proplist-opt "; }
| property_name_and_value_list { ep"proplist-opt-w-list "; }
;

property_name_and_value_list : property_name ':' assignment_expression { ep"propname-asgn-first "; }
| property_name_and_value_list ',' property_name ':' assignment_expression { ep"propname-asgn-append "; }
;

property_name : IDENTIFIER { ep"propname-id "; }
| STRING_LITERAL { ep"propname-strlit "; }
| NUMERIC_LITERAL { ep"propname-numlit "; }
;

member_expression : primary_expression { ep"mexp-pexp "; }
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

left_hand_side_expression : new_expression { ep"lefthand-new "; }
| call_expression { ep"lefthand-call "; }
;

postfix_expression : left_hand_side_expression { ep"postfix-lefthand "; }
| left_hand_side_expression INCREMENT { ep"postfix-lefthand-incr "; }
| left_hand_side_expression DECREMENT { ep"postfix-lefthand-decl "; }
;

unary_expression : postfix_expression { ep"unary-postfix "; }
| DELETE unary_expression { ep"unary-delete "; }
| VOID unary_expression { ep"unary-void "; }
| TYPEOF unary_expression { ep"unary-typeof "; }
| INCREMENT unary_expression { ep"unary-increment "; }
| DECREMENT unary_expression { ep"unary-decrement "; }
| '+' unary_expression { ep"unary-+ "; }
| '-' unary_expression { ep"unary-- "; }
| '~' unary_expression { ep"unary-~ "; }
| '!' unary_expression { ep"unary-! "; }
;

multiplicative_expression : unary_expression { ep"multiplicative-unary "; }
| multiplicative_expression '*' unary_expression { ep"multiplicative-'*' "; }
| multiplicative_expression '/' unary_expression { ep"multiplicative-'/' "; }
| multiplicative_expression '%' unary_expression { ep"multiplicative-'%' "; }
;

additive_expression : multiplicative_expression { ep"additive-multiplicative "; }
| additive_expression '+' multiplicative_expression { ep"additive-additive-plus "; }
| additive_expression '-' multiplicative_expression { ep"additive-additive-minus "; }
;

shift_expression : additive_expression { ep"shift-additive "; }
| shift_expression SHIFT_LEFT additive_expression { ep"shift-shift-left "; }
| shift_expression SHIFT_RIGHT additive_expression { ep"shift-shift-right "; }
| shift_expression U_SHIFT_RIGHT additive_expression { ep"shift-shift-uright "; }
;

relational_expression : shift_expression { ep"rel-shift "; }
| relational_expression '<' shift_expression { ep"rel-<-shift "; }
| relational_expression '>' shift_expression { ep"rel->-shift "; }
| relational_expression LESS_EQUAL shift_expression { ep"rel-less-shift "; }
| relational_expression GRATER_EQUAL shift_expression { ep"rel-grater-shift "; }
| relational_expression INSTANCEOF shift_expression { ep"rel-instanceof-shift "; }
| relational_expression IN shift_expression { ep"rel-in-shift "; }
;

equality_expression : relational_expression { ep"eq-rel "; }
| equality_expression EQUAL relational_expression { ep"eq-eq-rel "; }
| equality_expression NOT_EQUAL relational_expression { ep"eq-neql-rel "; }
| equality_expression EQ relational_expression { ep"rel-eq-rel "; }
| equality_expression NOT_EQ relational_expression { ep"rel-neq-rel "; }
;


bitwise_and_expression : equality_expression { ep"bw-eq "; }
| bitwise_and_expression '&' equality_expression { ep"bw-and "; }
;


bitwise_xor_expression : bitwise_and_expression { ep"bw-and "; }
| bitwise_xor_expression '^' bitwise_and_expression { ep"bw-xor "; }
;


bitwise_or_expression : bitwise_xor_expression { ep"bw-xor "; }
| bitwise_or_expression '|' bitwise_xor_expression { ep"bw-or-xor "; }
;


logical_and_expression : bitwise_or_expression { ep"logical-and-bor "; }
| logical_and_expression LOGICAL_AND bitwise_or_expression { ep"logical-and-and "; }
;

logical_or_expression : logical_and_expression { ep"logical-or-and "; }
| logical_or_expression LOGICAL_OR logical_and_expression { ep"logical-and-or-and "; }
;

conditional_expression : logical_or_expression { ep"cond-1 "; }
| logical_or_expression '?' assignment_expression ':' assignment_expression { ep"cond-2 "; }
;

assignment_expression : conditional_expression { ep"asgnexp-cond "; }
| left_hand_side_expression assignment_operator assignment_expression { ep"asgnexp-left-op-asgn "; }
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

expression : assignment_expression { ep"exp-asgn "; asgn=pop(:asgn); push(:exp, asgn ) }
| expression ',' assignment_expression { ep"asgn-exp-,-asgn "; }
;


expression_opt : { ep "exp-opt-empty "; }
| expression { ep"exp-opt "; }
;



end

---- header = header.rb
---- inner = inner.rb
---- footer = footer.rb
