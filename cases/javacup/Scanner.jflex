package waebric;

/*
@author Rob van der Horst
@author Daniel Weggemans

JFlex specificatie voor Waebric.
*/

import java_cup.runtime.ComplexSymbolFactory;
import java_cup.runtime.ComplexSymbolFactory.Location;
import java_cup.runtime.Symbol;
import java.io.InputStreamReader;
import java.io.InputStream;

%%

/* 
   The name of the class JFlex will create will be Lexer.
   Will write the code to the file Lexer.java. 
*/
%class Lexer
%implements sym
%public
%unicode
/*
  The current line number can be accessed with the variable yyline
  and the current column number with the variable yycolumn.
*/
%line
%column
/* 
   Will switch to a CUP compatibility mode to interface with a CUP
   generated parser.
*/
%cup
//%cupdebug

/* ------------------------------ Declarations -------------------------------  
    
  Code between %{ and %}, both of which must be at the beginning of a
  line, will be copied letter to letter into the lexer class source.
  Here you declare member variables and functions that are used inside
  scanner actions.  
*/
%{
	private StringBuffer string = new StringBuffer();
	private int html_count = 0;
    private ComplexSymbolFactory symbolFactory;
    private int csline,cscolumn;
    
	public Lexer(ComplexSymbolFactory sf) {
		this(new InputStreamReader(System.in));
        symbolFactory = sf;
    }
    
    public Lexer(ComplexSymbolFactory sf, InputStream is) {
		this(is);
		symbolFactory = sf;
	}
            
    public Symbol symbol(String name, int code) {		
    	//System.out.println("state: " + yystate() + " name: " + name + " code: " + code + " line: " + (yyline+1));
		return symbolFactory.newSymbol(name, code,new Location(yyline+1,yycolumn+1-yylength()),new Location(yyline+1,yycolumn+1));
    }
    
    public Symbol symbol(String name, int code, Object lexem){
    	//System.out.println("state: " + yystate() + " name: " + name + " code: " + code + " lexem: " + lexem.toString() + " line: " + (yyline+1));		
		return symbolFactory.newSymbol(name, code, new Location(yyline+1, yycolumn +1), new Location(yyline+1,yycolumn+yylength()), lexem);
    }    
    
    private void error(String message) {
    	System.err.println("Error at line "+(yyline+1)+", column "+(yycolumn+1)+" : "+message);
	}
%}

%eofval{
    return symbolFactory.newSymbol("EOF",sym.EOF);
%eofval}

/* ------------------------------ Macro Declarations ---------------------------
    
  These declarations are regular expressions that will be used latter
  in the Lexical Rules Section.  
*/

/* A line terminator is a \r (carriage return), \n (line feed), or \r\n. */
LineTerminator		= \r|\n|\r\n

/* White space is a line terminator, space, tab, or line feed. */
WhiteSpace			= {LineTerminator} | [ \t\f]

/* Text */ 
Text				= "\"" {TextChar}* "\""
TextChar			= [^\000-\031\&\"\<] | {WhiteSpace} | {Amp} | {EscQuote} | {TextCharRef} | {TextEntityRef}
EscQuote 			= [\\] [\"]
Amp 				= [\&][^\#0-9a-zA-Z\_\:]
TextCharRef 		= "&#" [0-9]+ ";" | "&#x" [0-9a-fA-F]+ ";"
TextEntityRef 		= "&" [a-zA-Z\_\:] [a-zA-Z0-9\.\-\_\:]* ";" 

/* String */
StrChar 			= \n|\t| {EscQuote} | {Backslash} | {Decimal} | [^\000-\031\n\t\"\\]
Backslash			= [\\\\]
Decimal				= "\\" [0-9] [0-9] [0-9]
StrCon				= [\"] {StrChar}* [\"]

/* Comments */
Asterisk 			= "\*" [^\/]
CommentChar			= [^\*] | {Asterisk}
TraditionalComment  = "/*" {CommentChar}* "*/"
EndOfLineComment 	= "//" [^\n]* [\n]
Comment 			= {TraditionalComment} | {EndOfLineComment}

/* Expressions */
SymbolCon			= "'" {SymbolChar}*
SymbolChar			= [^\000-\031\)\ \t\n\r\;\,\>] /* 127-255 weggelaten. werkt niet met jflex */

/* IdentifierCon */
IdCon				= [A-Za-z][A-Za-z\-0-9]*

/* NatCon */
NatCon				= [0-9]+

/* Embedding */
PreText				= "\"" {TextChar}* "<"
PostText			= ">" {TextChar}* "\""
MidText				= ">" {TextChar}* "<"

/* Sites */
PathElement			= [^\ \t\n\r\.\/\\]+
FileExt				= [a-zA-Z0-9]+
Directory			= {PathElement}+
FileName			= {PathElement} "." {FileExt}
DirectorySeparator  = "/" [^\ \t\n\r\.\/\\]

%state SITE, COMMENT

%%

/* ------------------------Lexical Rules Section---------------------- 
   This section contains regular expressions and actions, i.e. Java
   code, that will be executed when the scanner matches the associated
   regular expression. */
   
   /* YYINITIAL is the state at which the lexer begins scanning.  So
   these regular expressions will only be matched if the scanner is in
   the start state YYINITIAL. */

<YYINITIAL> {
	/* keywords */
	"module"		{ return symbol("Module constant", MODULE); }
	"import"        { return symbol("Import", sym.IMPORT); }
	"site"          { yybegin(SITE); return symbol("Site", sym.SITE); }
	
	"def"           { return symbol("Def", sym.DEF); }
	"if"            { return symbol("If", sym.IF); }
	"else"          { return symbol("Else", sym.ELSE); }
	"each"          { return symbol("Each", sym.EACH); }
	"let"           { return symbol("Let", sym.LET); }	
	"in"			{ return symbol("In", sym.IN); }	
	"comment"       { yybegin(COMMENT); return symbol("Comment", sym.COMMENT); }
	"echo"          { return symbol("Echo", sym.ECHO); }
	"cdata"         { return symbol("CDATA", sym.CDATA); }
	"yield"         { return symbol("Yield", sym.YIELD); }		
	"list"          { return symbol("List", sym.LIST); }
	"record"        { return symbol("Record", sym.RECORD); }
	"string"        { return symbol("String", sym.STRING); }
			 
	/* literals */	
	{NatCon} 		{ return symbol("NatCon", sym.NATCON, new Integer(Integer.parseInt(yytext()))); }
	{SymbolCon}		{ return symbol("SymbolCon", sym.SYMBOLCON, yytext()); }
		
	{Text}			{ return symbol("Text", sym.TEXT, yytext()); }	
			
	{PreText}		{ return symbol("PreText", sym.PRETEXT, yytext()); }
	{PostText}		{ return symbol("PostText", sym.POSTTEXT, yytext()); }
	{MidText}		{ return symbol("MidText", sym.MIDTEXT, yytext()); }
							
	/* separators */
	"("             { return symbol("Left Parenthesis", sym.LPAREN); }
	")"             { return symbol("Right parenthesis", sym.RPAREN); }
	"{"             { return symbol("Left braces", sym.LBRACE); }
	"}"             { return symbol("Right braces", sym.RBRACE); }
	";"             { return symbol("Semicolon", sym.SEMI); }
	"."             { return symbol("Dot", sym.DOT); }
	","             { return symbol("Comma", sym.COMMA); }
	"#"             { return symbol("Number sign", sym.NUMBER_SIGN); }
	"$"             { return symbol("Dollar sign", sym.DOLLAR_SIGN); }
	"@"             { return symbol("At sign", sym.AT_SIGN); }
	"%"             { return symbol("Percent sign", sym.PERCENT_SIGN); }
	"="             { return symbol("Assign", sym.ASSIGN); }	
	"["           	{ return symbol("Left brackets", sym.LBRACKET); }
	"]"             { return symbol("Right brackets", sym.RBRACKET); }			
	"?"             { return symbol("Questionmark", sym.QUESTIONMARK); }
	"!"             { return symbol("Exclamation", sym.EXCLAMATION); }	
	"&&"            { return symbol("And", sym.AND); }
	"||"            { return symbol("Or", sym.OR); }		
	":"             { return symbol("Colon", sym.COLON); }			
	/*"+"             { return symbol("Plus", sym.PLUS); }*/	
}

/*
	Gedeelde state voor YYINITIAL en SITE. Dit om de markup tussen SITE en END te ondersteunen.
	TODO is dit volledig?	
*/
<YYINITIAL, SITE> {	
	"end"			{ yybegin(YYINITIAL); return symbol("End", sym.END); }
	":"             { return symbol("Colon", sym.COLON); }	
	"("             { return symbol("Left Parenthesis", sym.LPAREN); }
	")"             { return symbol("Right parenthesis", sym.RPAREN); }
	"#"             { return symbol("Number sign", sym.NUMBER_SIGN); }
	"$"             { return symbol("Dollar sign", sym.DOLLAR_SIGN); }
	"@"             { return symbol("At sign", sym.AT_SIGN); }
	"%"             { return symbol("Percent sign", sym.PERCENT_SIGN); }
	"/"             { return symbol("Slash", sym.SLASH); }
	
	{IdCon}    		{ return symbol("IdCon", sym.IDCON, yytext()); }
	
	{WhiteSpace}  	{ /* Negeren */  }
	{Comment}     	{ /* Negeren */ }	
}

/*
	Directory en Filename mogen alleen matchen tussen SITE en END.
	Omdat hier ook markup tussen kan zitten is er nog een gedeelde state
	gedefinieerd om ook de markup te ondersteunen.
*/
<SITE> {	
	/* Directory is alleen directory als gevolgd door een slash en niet een zooitje andere tekens */
	{Directory}/{DirectorySeparator}	{ return symbol("Directory", sym.DIRECTORY, yytext()); }	
	{FileName}							{ return symbol("Filename", sym.FILENAME, yytext()); }
}

<COMMENT> {
	{StrCon}		{ yybegin(YYINITIAL); return symbol("StrCon", sym.STRCON, yytext()); }	
}

/* error fallback */
.|\n            {  error("Illegal character <"+ yytext()+">"); }           
                
                
                
                
                
                
                
                