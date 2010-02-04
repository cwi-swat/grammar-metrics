/* --------------------------Usercode Section------------------------ */
package waebric;

import beaver.Symbol;
import beaver.Scanner;

import waebric.WaebricParser.Terminals;
%%

/* -----------------Options and Declarations Section----------------- */

/*
   The name of the class JFlex will create will be Lexer.
   Will write the code to the file Lexer.java.
*/
%class WaebricLexer
%extends Scanner

/*
  The current line number can be accessed with the variable yyline
  and the current column number with the variable yycolumn.
*/
%line
%column

%yylexthrow Scanner.Exception

%function nextToken
%type Symbol

%eofval{
	return nextToken(Terminals.EOF, "end-of-file");
%eofval}


/*
  Declarations

  Code between %{ and %}, both of which must be at the beginning of a
  line, will be copied letter to letter into the lexer class source.
  Here you declare member variables and functions that are used inside
  scanner actions.
*/
%{
	boolean bSITE=false;
	StringBuffer string = new StringBuffer(128);
	private Symbol nextToken(short id)
	{
		return new Symbol(id, yyline + 1, yycolumn + 1, yylength());
	}

	private Symbol nextToken(short id, Object value)
	{
		return new Symbol(id, yyline + 1, yycolumn + 1, yylength(), value);
	}
	
	private void Debug(String text)
	{
		//System.out.println(text);//commentariseer deze regel uit in productie
	}
%}


/*
  Macro Declarations

  These declarations are regular expressions that will be used latter
  in the Lexical Rules Section.
*/

/* A line terminator is a \r (carriage return), \n (line feed), or
   \r\n. */
LineTerminator = \r|\n|\r\n
InputCharacter = [^\r\n]

/* White space is a line terminator, space, tab, or line feed. */
WhiteSpace     = {LineTerminator} | [ \t\f]

/* comments */
Comment = {TraditionalComment} | {EndOfLineComment} |
          {DocumentationComment}

TraditionalComment = "/*" [^*] ~"*/" | "/*" "*"+ "/"
EndOfLineComment = "//" {InputCharacter}* {LineTerminator}?
DocumentationComment = "/*" "*"+ [^/*] ~"*/"

/* A literal integer is is a number beginning with a number between
   one and nine followed by zero or more numbers between zero and nine
   or just a zero.  */
Natcon = [0-9]+


TextChar = [^\x00-\x1F\&\"\<\x80-\xFF] | [\n\r\t] | {TextCharRefA} | {TextCharRefB} | {TextEntityRef} | [&]
SymbolChar = [^\x00-\x1F\)\ \t\n\r\;\,\>\x80-\xFF]


TextCharRefA = "&#" [0-9]+ ";" 
TextCharRefB = "&#x" [0-9a-fA-F]+ ";"
TextEntityRef  = "&" [a-zA-Z\_\:] [a-zA-Z0-9\.\-\_\:]* ";" 
FileExt = [a-zA-Z0-9]+
PathElement = [^\ \t\n\r\.\/\\]+
Directory = {PathElement} ("/" {PathElement})* 



/* identifiers */
IdentifierChars = [A-Za-z][A-Za-z\-0-9]*
ForbiddenKeywords = "if" | "comment" | "echo" | "cdata" | "each" | "let" | "module" | "import" | "def" | "end" | "site"
Identifier = !( !({IdentifierChars}) | ({ForbiddenKeywords}) )

SiteFilename = {PathElement} "." {FileExt}




%state SITE,STRING,STRCON,STRCON_INIT, PRETEXT, POSTMIDTEXT, DIRFILE, ARGUMENTS

%%
/* ------------------------Lexical Rules Section---------------------- */

/*
   This section contains regular expressions and actions, i.e. Java
   code, that will be executed when the scanner matches the associated
   regular expression. */

   /* YYINITIAL is the state at which the lexer begins scanning.  So
   these regular expressions will only be matched if the scanner is in
   the start state YYINITIAL. */

<YYINITIAL> {
  /* keywords */
  "module"                         { Debug("MODULE"); return nextToken(Terminals.MODULE); }
  "import"                         { Debug("IMPORT"); return nextToken(Terminals.IMPORT); }  
  "def"                            { Debug("DEF"); return nextToken(Terminals.DEF); }
  "end"                            { bSITE=false;
  									 Debug("END");
                                     return nextToken(Terminals.END);
                                   }
  "site"                           { Debug("SITE"); bSITE=true; yybegin(DIRFILE); return nextToken(Terminals.SITE); }
  "list"                           { Debug("LIST");  return nextToken(Terminals.LIST); }
  "record"                         { Debug("RECORD");  return nextToken(Terminals.RECORD); }
  "string"                         { Debug("STRING");  return nextToken(Terminals.STRING); }
  "if"                             { Debug("IF");   return nextToken(Terminals.IF); }
  "else"                           { Debug("ELSE");   return nextToken(Terminals.ELSE); }
  "in"				               { Debug("IN");  return nextToken(Terminals.IN); }

  
  "comment"                        { Debug("COMMENT"); string.setLength(0); yybegin(STRCON_INIT);  return nextToken(Terminals.COMMENT);  }
  "echo"                           { Debug("ECHO"); return nextToken(Terminals.ECHO); }
  "cdata"                          { Debug("CDATA"); return nextToken(Terminals.CDATA); }
  "each"                           { Debug("EACH"); return nextToken(Terminals.EACH); }
  "let"                            { Debug("LET"); return nextToken(Terminals.LET); }
  "yield"                          { Debug("YIELD"); return nextToken(Terminals.YIELD); }

   "\""                        { string.setLength(0); string.append( '\"' ); yybegin(PRETEXT); }
   ">"                        { string.setLength(0);string.append( '>' ); yybegin(POSTMIDTEXT); }

 
    
  /* separators and operators*/
  "("                            { Debug("LPAREN"); return nextToken(Terminals.LPAREN); }
  ")"                            { Debug("RPAREN"); return nextToken(Terminals.RPAREN); }
  "{"                            { Debug("LBRACE"); return nextToken(Terminals.LBRACE); }
  "}"                            { Debug("RBRACE"); return nextToken(Terminals.RBRACE); }
  "["                            { Debug("LBRACK"); return nextToken(Terminals.LBRACK); }
  "]"                            { Debug("RBRACK"); return nextToken(Terminals.RBRACK); }
  ";"                            { Debug("SEMICOLON"); if(bSITE){ yybegin(DIRFILE); } return nextToken(Terminals.SEMICOLON); }
  ","                            { Debug("COMMA"); return nextToken(Terminals.COMMA); }
  "."                            { Debug("DOT"); return nextToken(Terminals.DOT); }
  ":"                            { Debug("COLON"); return nextToken(Terminals.COLON); }
  "%"                            { Debug("MOD"); return nextToken(Terminals.MOD); }
  "@"                            { Debug("ADDCHAR"); return nextToken(Terminals.ADDCHAR); }    
  "/"                            { Debug("DIV"); return nextToken(Terminals.DIV); }
  "+"                            { Debug("PLUS"); return nextToken(Terminals.PLUS); }
  "?"                            { Debug("QUESTION"); return nextToken(Terminals.QUESTION); }
  "!"                            { Debug("NOT"); return nextToken(Terminals.NOT); }
  "&&"                           { Debug("ANDAND"); return nextToken(Terminals.ANDAND); }
  "||"                           { Debug("OROR"); return nextToken(Terminals.OROR); }
  "="                            { Debug("EQ");return nextToken(Terminals.EQ); }
  "$"                            { Debug("DOLLAR"); return nextToken(Terminals.DOLLAR); }
  "#"                            { Debug("HASH"); return nextToken(Terminals.HASH); }   
  
  "'" {SymbolChar}*              { Debug("SYMBOLCON " + yytext()); return nextToken(Terminals.SYMBOLCON, yytext() );}

 /* "#"{WhiteSpace}*{Identifier}   { Debug("HASHIDCON " + yytext()); String temp = yytext(); return nextToken(Terminals.HASHIDCON, temp.substring(1) ); }
  "."{WhiteSpace}*{Identifier}   { Debug("ATTDOTIDCON " + yytext()); String temp = yytext(); return nextToken(Terminals.ATTDOTIDCON, temp.substring(1) ); }
  "$"{WhiteSpace}*{Identifier}   { Debug("ATTDOLLARIDCON " + yytext()); String temp = yytext(); return nextToken(Terminals.ATTDOLLARIDCON, temp.substring(1) ); }
  ":"{WhiteSpace}*{Identifier}   { Debug("ATTCOLONIDCON " + yytext()); String temp = yytext(); return nextToken(Terminals.ATTCOLONIDCON, temp.substring(1) ); }
  "@"{WhiteSpace}*{Natcon}       { Debug("ADDCHARNATCON " + yytext()); String temp = yytext(); return nextToken(Terminals.ADDCHARNATCON, temp.substring(1) ); }
  "%"{WhiteSpace}*{Natcon}       { Debug("NATCON " + yytext()); String temp = yytext(); return nextToken(Terminals.NATCON, temp.substring(1) ); }
*/

  /* comments */
  {Comment}                      { /* ignore */ }

  /* whitespace */
  {WhiteSpace}                   { /* ignore */ }




  /* identifiers */
  {Identifier}              { Debug("IDCON " + yytext() ); return nextToken(Terminals.IDCON, yytext()); } 
    
  /* Natural numbers*/
  {Natcon}                       { Debug("NATCON " + yytext() ); return nextToken(Terminals.NATCON, yytext()); }  

}

<DIRFILE> {
  "end"                          { Debug("PushBackDirfile:" + yytext() ); yybegin(YYINITIAL); yypushback(3); }
  {Directory} /"/"               { Debug("DIRNAME " + yytext() ); return nextToken(Terminals.DIRNAME, yytext()); }
  "/"                            { Debug("DIV"); return nextToken(Terminals.DIV); }
  {SiteFilename}                 { Debug("FILENAME " + yytext() ); return nextToken(Terminals.FILENAME, yytext()); }
  ":"                            { Debug("COLON "); return nextToken(Terminals.COLON); }
  {Identifier}     	 	   { Debug("DIRFILE IDCON " + yytext() ); return nextToken(Terminals.IDCON, yytext() ); } 
 
  /* comments */
  {Comment}                      { /* ignore */ }

  /* whitespace */
  {WhiteSpace}                   { /* ignore */ }
  
  .					   {  Debug("PushBackDirfile:" + yytext() ); yybegin(YYINITIAL); yypushback(1); }
    
}

<STRCON_INIT> {
  "\""                           { yybegin(STRCON);  }
   /* comments */
  {Comment}                      { /* ignore */ }

  /* whitespace */
  {WhiteSpace}                   { /* ignore */ }
}

<STRCON> {

  "\""                           { Debug("STRCON " /*+ string.toString()*/ ); yybegin(YYINITIAL); return nextToken(Terminals.STRCON, string.toString() ); }	
  "\\t"                          { string.append( '\t' ); }
  "\\n"                          { string.append( '\n' ); }
  "\\\""                         { string.append( '\"' ); }
  "\\\\"                         { string.append( '\\' ); }
  "\\" [0-9][0-9][0-9]           { String temp = yytext(); string.append( temp.substring(1) ); }
  [^\x00-\x1F\n\t\"\\]           { string.append( yytext() ); }
}


<PRETEXT> {
	{TextChar}			   { string.append( yytext() ); }
	 "\\".				   { string.append( "\\" + yytext() ); }
      \"                       { Debug("TEXT " + string + yytext() ); yybegin(YYINITIAL); string.append(  yytext() );  return nextToken(Terminals.TEXT,  string.toString() ); }
      "<"                        { Debug("PRETEXT " + string+ yytext() ); yybegin(YYINITIAL); string.append(  yytext()  );  return nextToken(Terminals.PRETEXT, string.toString() ); }

}

<POSTMIDTEXT> {
	{TextChar} 			   { string.append( yytext() ); }
	 "\\".				   { string.append( "\\" + yytext() ); }
	\"                       { Debug("POSTTEXT " + string + yytext() );  yybegin(YYINITIAL);  string.append( '\"' );  return nextToken(Terminals.POSTTEXT, string.toString() ); }
	"<"                        { Debug("MIDTEXT " + string + yytext());  yybegin(YYINITIAL);  string.append( yytext());  return nextToken(Terminals.MIDTEXT,  string.toString() ); }
}


/* No token was found for the input so through an error.  Print out an
   Illegal character message with the illegal character that was found. */
[^]                    { throw new Scanner.Exception("Illegal character <"+yytext()+">"); }
