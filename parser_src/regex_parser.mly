
%token <char> CHAR
%token <char> NZDIGIT
%token ALT
%token LPAR RPAR
%token LBRAC RBRAC COMMA
%token LBRACK RBRACK
%token STAR PLUS QMARK
%token HAT DOLLAR BACKSL DOT
%token COLONS LESS MORE EQUAL MINUS EXCL
%token LOWB UPB LOWD UPD LOWS UPS LOWW UPW
%token LOWF LOWN LOWR LOWT LOWV LOWK LOWX LOWU LOWP UPP
%token ZERO
%token EOF

%start <Regex.raw_regex> main
%type  <char> decimaldigit
%type  <string> decimaldigits
%type  <char> patterncharacter
%type  <char> syntaxcharacter
%type  <Regex.raw_regex> pattern
%type  <Regex.raw_regex> disjunction
%type  <Regex.raw_regex> alternative
%type  <Regex.raw_regex> term
%type  <Regex.raw_regex> assertion
%type  <Regex.raw_regex> atom
%type  <Regex.raw_regex> atomescape
%type  <Regex.quantifier> quantifier
%type  <Regex.counted_quantifier> counted_quantifier
%type  <Charclasses.char_group> characterclassescape
%type  <char> characterescape
%type  <char> controlescape
%type  <char> identityescape
%type  <string> decimalescape
%type  <Regex.character> characterclass
%type  <Charclasses.char_class> classcontents
%type  <Charclasses.char_class> nonemptyclassranges
%type  <Charclasses.char_class> nonemptyclassrangesnodash
%type  <Charclasses.char_class_elt> classatomnodash
%type  <char> classatomnodashchar
%type  <Charclasses.char_class_elt> classatom
%type  <Charclasses.char_class_elt> classescape

%%

decimaldigit:
  | nz=NZDIGIT { nz }
  | ZERO { '0' }

decimaldigits:
  | d1=decimaldigits d2=decimaldigit { d1 ^ String.make 1 d2 }
  | d=decimaldigit { String.make 1 d }
  

/* https://tc39.es/ecma262/#sec-patterns */

main: r=pattern EOF { r }

pattern:
  | d=disjunction { d }

disjunction:
  | a=alternative { a }
  | a=alternative ALT d=disjunction { Raw_alt(a,d) }

alternative:
  | a=alternative t=term { Raw_con(a,t) }
  | t=term { t } /* differs from the spec here */
  | { Raw_empty }

term:
  | a=assertion { a }
  | a=atom { a }
  | a=atom q=quantifier { Raw_quant(q,a) }
  | a=atom cq=counted_quantifier { Raw_count(cq,a) }
/* separation between usual quantifier and counted ones since they are separate in the OCaml engine AST. Could be changed if needed */

assertion:
  | HAT { Raw_anchor(BeginInput) }
  | DOLLAR { Raw_anchor(EndInput) }
  | BACKSL LOWB { Raw_anchor(WordBoundary) }
  | BACKSL UPB { Raw_anchor(NonWordBoundary) }
  | LPAR QMARK EQUAL d=disjunction RPAR { Raw_lookaround(Lookahead, d) }
  | LPAR QMARK EXCL d=disjunction RPAR { Raw_lookaround(NegLookahead, d) }
  | LPAR QMARK LESS EQUAL d=disjunction RPAR { Raw_lookaround(Lookbehind, d) }
  | LPAR QMARK LESS EXCL d=disjunction RPAR { Raw_lookaround(NegLookbehind, d) }

quantifier:
  | STAR { Star }
  | STAR QMARK { LazyStar }
  | PLUS { Plus }
  | PLUS QMARK { LazyPlus }
  | QMARK { QuestionMark }
  | QMARK QMARK { LazyQuestionMark }
/* do I have abug when parsing '(?=a)?' */
/* that's not a bug, the regex grammar doesn't say you can quantify assertions */

counted_quantifier:
  | LBRAC d=decimaldigits RBRAC { {min=int_of_string d; max=Some (int_of_string d); greedy=true} }
  | LBRAC d=decimaldigits RBRAC QMARK{ {min=int_of_string d; max=Some (int_of_string d); greedy=false} }
  | LBRAC d=decimaldigits COMMA RBRAC { {min=int_of_string d; max=None; greedy=true} }
  | LBRAC d=decimaldigits COMMA RBRAC QMARK { {min=int_of_string d; max=None; greedy=false} }
  | LBRAC dmin=decimaldigits COMMA dmax=decimaldigits RBRAC { {min=int_of_string dmin; max=Some (int_of_string dmax); greedy=true} }
  | LBRAC dmin=decimaldigits COMMA dmax=decimaldigits RBRAC QMARK { {min=int_of_string dmin; max=Some (int_of_string dmax); greedy=false} }

atom:
  | c=patterncharacter { Raw_character(Char c) }  
/* TODO: { for instance can be parsed as single char. But not (. I'm not sure where this is in the spec. Also I'm not sure why, if I add a similar rule for LBRAC, it does not work */
  | DOT { Raw_character(Dot) }
  | BACKSL a=atomescape { a }
  | c=characterclass { Raw_character c }
  | LPAR d=disjunction RPAR { Raw_capture d }
/* TODO: fail if there is a group specifier */
  | LPAR QMARK COLONS d=disjunction RPAR { d }

syntaxcharacter:
  | HAT { '^' }
  | DOLLAR { '$' }
  | BACKSL { '\\' }
  | DOT { '.' }
  | STAR { '*' }
  | PLUS { '+' }
  | QMARK { '?' }
  | LPAR { '(' }
  | RPAR { ')' }
  | LBRACK { '[' }
  | RBRACK { ']' }
  | LBRAC { '{' }
  | RBRAC { '}' }
  | ALT { '|' }

patterncharacter:
  | c=CHAR { c }
/* also adding all tokens that can be parsed as single characters */
  | LOWB { 'b' }
  | UPB { 'B' }
  | LOWD { 'd' }
  | UPD { 'D' }
  | LOWS { 's' }
  | UPS { 'S' }
  | LOWW { 'w' }
  | UPW { 'W' }
  | LOWF { 'f' }
  | LOWN { 'n' }
  | LOWR { 'r' }
  | LOWT { 't' }
  | LOWV { 'v' }
  | LOWK { 'k' }
  | LOWX { 'x' }
  | LOWU { 'u' }
  | LOWP { 'p' }
  | UPP { 'P' }
  | COMMA { ',' }
  | COLONS { ':' }
  | LESS { '<' }
  | MORE { '>' }
  | EQUAL { '=' }
  | MINUS { '-' }
  | EXCL { '!' }
  | LBRAC { '{' }
  | RBRAC { '}' }
/* TODO: still a bug when parsing for instance a{ */
  | LBRACK { '[' }
  | RBRACK { ']' } 
  | d=decimaldigit { d }


atomescape:
  | d=decimalescape { raise Regex.Unsupported_backref }
  | c=characterclassescape { Raw_character(Group c) }
  | c=characterescape { Raw_character(Char c) }
  | LOWK { raise Regex.Unsupported_named_groups }

characterescape:
  | c=controlescape { c }
  | ZERO { char_of_int 0 }
/* TODO: actually before raising the exception, it depends if there is a hexdigit sequence after the x, otherwise should be read as character x */
  | LOWX { raise Regex.Unsupported_hex }
  | LOWU { raise Regex.Unsupported_unicode }
  | i=identityescape { i }

controlescape:
  | LOWF { '\x0C' }
  | LOWN { '\n' }
  | LOWR { '\r' }
  | LOWT { '\t' }
  | LOWV { char_of_int 11 }

identityescape:
  | s=syntaxcharacter { s }
/* all other characters that represent themselves when escaped */
  | c=CHAR { c }
  | COMMA { ',' }
  | COLONS { ':' }
  | LESS { '<' }
  | MORE { '>' }
  | EQUAL { '=' }
  | MINUS { '-' }
  | EXCL { '!' }

decimalescape:
  | nz=NZDIGIT d=decimaldigits { String.make 1 nz ^ d }
  | nz=NZDIGIT { String.make 1 nz }

characterclassescape:
  | LOWD { Digit }
  | UPD { NonDigit }
  | LOWS { Space }
  | UPS { NonSpace }
  | LOWW { Word }
  | UPW { NonWord }
  | LOWP { raise Regex.Unsupported_prop }
  | UPP { raise Regex.Unsupported_prop }

characterclass:
  | LBRACK HAT c=classcontents RBRACK { NegClass c }
  | LBRACK c=classcontents RBRACK { Class c }

classcontents:
  | { [] }
  | n=nonemptyclassranges { n }

nonemptyclassranges:
  | a=classatom { [a] }
  | a=classatom n=nonemptyclassrangesnodash { a::n }
  | a1=classatom MINUS a2=classatom c=classcontents { (Charclasses.make_range a1 a2) @ c }

nonemptyclassrangesnodash:
  | a=classatom { [a] }
  | a=classatomnodash n=nonemptyclassrangesnodash { a::n }
  | a1=classatomnodash MINUS a2=classatom c=classcontents { (Charclasses.make_range a1 a2) @ c }
  


/* I'm removing the character groups \s \w... */
/* and making it a special rule */
/* otherwise I'm not sure how to type it for ranges */
/* Note that there still is a bug with e.g [a-\d] */

classatom:
  | MINUS { CChar '-' }
  | c=classatomnodash { c }

classatomnodash:
  | BACKSL c=classescape { c }
  | c=classatomnodashchar { CChar c }

classatomnodashchar:
/* all characters except \, ] and - */
  | HAT { '^' }
  | DOLLAR { '$' }
  | DOT { '.' }
  | STAR { '*' }
  | PLUS { '+' }
  | QMARK { '?' }
  | LPAR { '(' }
  | RPAR { ')' }
  | LBRACK { '[' }
  | LBRAC { '{' }
  | RBRAC { '}' }
  | ALT { '|' }
  | c=CHAR { c }
  | LOWB { 'b' }
  | UPB { 'B' }
  | LOWD { 'd' }
  | UPD { 'D' }
  | LOWS { 's' }
  | UPS { 'S' }
  | LOWW { 'w' }
  | UPW { 'W' }
  | LOWF { 'f' }
  | LOWN { 'n' }
  | LOWR { 'r' }
  | LOWT { 't' }
  | LOWV { 'v' }
  | LOWK { 'k' }
  | LOWX { 'x' }
  | LOWU { 'u' }
  | LOWP { 'p' }
  | UPP { 'P' }
  | COMMA { ',' }
  | COLONS { ':' }
  | LESS { '<' }
  | MORE { '>' }
  | EQUAL { '=' }
  | EXCL { '!' }
  | d=decimaldigit { d }

classescape:
  | LOWB { CChar (char_of_int 8) }	/* basckspace ascii character */
  | c=characterclassescape { CGroup c }
  | c=characterescape  { CChar c }
  | d=decimalescape { raise Regex.Unsupported_octal }


%%
