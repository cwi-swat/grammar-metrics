--
-- The Waebricc Lexer
--
%Options fp=WaebricLexer
%options single-productions
%options package=waebricparser
%options template=LexerTemplateD.g
%options filter=WaebricKWLexer.g
%options lalr=10

%Define
    --
    -- Definition of macro used in the included file LexerBasicMapB.g
    --
    $kw_lexer_class /.$WaebricKWLexer./

%End

%Include
    LexerBasicMap.g
%End

%Export

    SlComment
    MlComment
    SymbolLiteral
    CommentLiteral

    IDENTIFIER
	Path
    IntegerLiteral
    StringLiteral
    PreText
    MidText
    PostText
    OR_OR
    AND_AND
    PLUS
    NOT

    LPAREN
    RPAREN
    LBRACE
    RBRACE
    LBRACKET
    RBRACKET
    SEMICOLON
    SHARP
    COLON
    COMMA
    DOLLAR
    DOT
    EQUAL
    AT
    PERCENT
    
   
%End

%Terminals
    CtlCharNotWS

    LF   CR   HT   FF

    a    b    c    d    e    f    g    h    i    j    k    l    m
    n    o    p    q    r    s    t    u    v    w    x    y    z
    _

    A    B    C    D    E    F    G    H    I    J    K    L    M
    N    O    P    Q    R    S    T    U    V    W    X    Y    Z

    0    1    2    3    4    5    6    7    8    9

    AfterASCII   ::= '\u0080..\ufffe'
    Space        ::= ' '
    LF           ::= NewLine
    CR           ::= Return
    HT           ::= HorizontalTab
    FF           ::= FormFeed
    DoubleQuote  ::= '"'
    SingleQuote  ::= "'"
    Percent      ::= '%'
    VerticalBar  ::= '|'
    Exclamation  ::= '!'
    AtSign       ::= '@'
    BackQuote    ::= '`'
    Tilde        ::= '~'
    Sharp        ::= '#'
    DollarSign   ::= '$'
    Ampersand    ::= '&'
    Caret        ::= '^'
    Colon        ::= ':'
    SemiColon    ::= ';'
    BackSlash    ::= '\'
    LeftBrace    ::= '{'
    RightBrace   ::= '}'
    LeftBracket  ::= '['
    RightBracket ::= ']'
    QuestionMark ::= '?'
    Comma        ::= ','
    Dot          ::= '.'
    LessThan     ::= '<'
    GreaterThan  ::= '>'
    Plus         ::= '+'
    Slash        ::= '/'
    Minus        ::= '-'
    Star         ::= '*'
    LeftParen    ::= '('
    RightParen   ::= ')'
    Equal        ::= '='

%End

%Start
    Token
%End

%Rules

    ---------------------  Rules for Scanned Tokens --------------------------------
    -- The lexer creates an array list of tokens which is defined in the PrsStream class.
    -- A token has three attributes: a start offset, an end offset and a kind.
    -- 
    -- Only rules that produce complete tokens have actions to create token objects.
    -- When making a token, calls to the methods, $getToken(1) and $getRightSpan(), 
    -- provide the offsets (i.e. the span) of a rule's right hand side (rhs) and thus of the token.
    -- For a rule of the form A ::= A1 A2 ... An, the start offset of the rhs of A is given by
    -- $getToken(1) or by $getLeftSpan() and the end offset by $getRightSpan().
    --  
    -- Regarding rules for parsing in general, note that for a rhs symbol Ai, the 
    -- method $getToken(i) returns the location of the leftmost character derived from Ai.  
    -- The method $getLeftSpan(i) returns the same location unless Ai produces $empty in which case
    -- it returns the location of the last character derived before reducing Ai to $empty. 
    -- The method $getRightSpan(i) returns the location of the rightmost character derived from Ai 
    -- unless Ai produces $empty in which case it returns the location of the last character 
    -- derived before reducing Ai to $empty.
    --------------------------------------------------------------------------------
    
    CommentTToken ::= 'c' 'o' 'm' 'm' 'e' 'n' 't'
    /.$BeginAction
             checkForKeyWord();
      $EndAction
    ./
       
    CommentSToken ::= '"' '"' | '"' CommentString '"'
    /.$BeginAction
           makeToken($_CommentLiteral);
      $EndAction
    ./
    
    CommentTerminator ::= ';'
            /.$BeginAction
                    makeToken($_SEMICOLON);
          $EndAction
        ./
    
    WSOpt ::= %empty | WS
    
    
    Token ::= CommentTToken WSOpt CommentSToken WSOpt CommentTerminator    
    
    Token ::= Identifier
        /.$BeginAction
                    checkForKeyWord();
          $EndAction
        ./
        
    Token ::= Path
        /.$BeginAction
                    makeToken($_Path);
          $EndAction
        ./
        
    Token ::= '"' CombiBody
    	/.$BeginAction
    				if( getInputChars()[getRightSpan()] == '<')
    				{
    					makeToken($_PreText);
    				}
    				else
    				{
    					makeToken($_StringLiteral);
    				}
    	  $EndAction
    	./              

    Token ::= '>' TextBody '"'
        /.$BeginAction
                    makeToken($_PostText);
          $EndAction
        ./
    Token ::= '>' TextBody '<'
        /.$BeginAction
                    makeToken($_MidText);
          $EndAction
        ./
        
    Token ::= "'" SymbolCon
        /.$BeginAction
                    makeToken($_SymbolLiteral);
          $EndAction
        ./
        
    Token ::= IntegerLiteral
        /.$BeginAction
                    makeToken($_IntegerLiteral);
          $EndAction
        ./
    Token ::= '/' '*' Inside Stars '/'
        /.$BeginAction
                    makeComment($_MlComment);
          $EndAction
        ./
    Token ::= SLC
        /.$BeginAction
                    makeComment($_SlComment);
          $EndAction
        ./
    Token ::= WS -- White Space is scanned but not added to output vector
        /.$BeginAction
                    skipToken();
          $EndAction
        ./
    Token ::= '+'
        /.$BeginAction
                    makeToken($_PLUS);
          $EndAction
        ./
    Token ::= '@'
        /.$BeginAction
                    makeToken($_AT);
          $EndAction
        ./   
    Token ::= '%'
        /.$BeginAction
                    makeToken($_PERCENT);
          $EndAction
        ./    

    Token ::= '('
        /.$BeginAction
                    makeToken($_LPAREN);
          $EndAction
        ./
              
    Token ::= ')'
        /.$BeginAction
                    makeToken($_RPAREN);
          $EndAction
        ./

    Token ::= '='
        /.$BeginAction
                    makeToken($_EQUAL);
          $EndAction
        ./

    Token ::= ','
        /.$BeginAction
                    makeToken($_COMMA);
          $EndAction
        ./

    Token ::= ':'
        /.$BeginAction
                    makeToken($_COLON);
          $EndAction
        ./

    Token ::= ';'
        /.$BeginAction
                    makeToken($_SEMICOLON);
          $EndAction
        ./
        
    Token ::= '#'
        /.$BeginAction
                    makeToken($_SHARP);
          $EndAction
        ./

    Token ::= '$'
        /.$BeginAction
                    makeToken($_DOLLAR);
          $EndAction
        ./

    Token ::= '&' '&'
        /.$BeginAction
                    makeToken($_AND_AND);
          $EndAction
        ./

    Token ::= '|' '|'
        /.$BeginAction
                    makeToken($_OR_OR);
          $EndAction
        ./

    Token ::= '.'
        /.$BeginAction
                    makeToken($_DOT);
          $EndAction
        ./
        

    Token ::= '!'
        /.$BeginAction
                    makeToken($_NOT);
          $EndAction
        ./

    Token ::= '['
        /.$BeginAction
                    makeToken($_LBRACKET);
          $EndAction
        ./

    Token ::= ']'
        /.$BeginAction
                    makeToken($_RBRACKET);
          $EndAction
        ./

    Token ::= '{'
        /.$BeginAction
                    makeToken($_LBRACE);
          $EndAction
        ./

    Token ::= '}'
        /.$BeginAction
                    makeToken($_RBRACE);
          $EndAction
        ./
        

    IntegerLiteral -> Integer
                    | Integer LetterLl
                    | '0' LetterXx HexDigits
                    | '0' LetterXx HexDigits LetterLl

    Inside ::= Inside Stars NotSlashOrStar
             | Inside '/'
             | Inside NotSlashOrStar
             | %empty

    Stars -> '*'
           | Stars '*'

    SLC ::= '/' '/'
          | SLC NotEol

    TextBody ::= %empty
             | TextBody TextChar
             
    CombiBody ::= '"' | '<'
    	| TextChar CombiBody
    	
    
    		
    Integer -> Digit
             | Integer Digit

    HexDigits -> HexDigit
               | HexDigits HexDigit

    WSChar -> Space
            | LF
            | CR
            | HT
            | FF
    		
    Letter -> LowerCaseLetter
    	| UpperCaseLetter
        | '_'
        | '\u0080..\ufffe'

    LowerCaseLetter -> a | b | c | d | e | f | g | h | i | j | k | l | m |
                       n | o | p | q | r | s | t | u | v | w | x | y | z      
                       
    UpperCaseLetter -> A | B | C | D | E | F | G | H | I | J | K | L | M |
                       N | O | P | Q | R | S | T | U | V | W | X | Y | Z

    Digit -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

    OctalDigit -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7

    a..f -> a | b | c | d | e | f | A | B | C | D | E | F

    HexDigit -> Digit
              | a..f

    LetterLl -> 'L'
              | 'l'

    LetterXx -> 'X'
              | 'x'

    WS -> WSChar
        | WS WSChar
        
    Identifier -> Letter
                | Identifier Letter
                | Identifier Digit
                | Identifier '-'
    
    CommentString -> NotDQ
    	| CommentString NotDQ

    PathPrefix -> '.' '/' | '/'

    Path -> PathPrefix PathElement

    PathElement -> Letter | Digit
		| PathElement Letter
		| PathElement Digit
		| PathElement '.'
		| PathElement '/'

    SpecialNotStar -> '+' | '-' | '/' | '(' | ')' | '"' | '!' | '@' | '`' | '~' |
                      '%' | '&' | '^' | ':' | ';' | "'" | '\' | '|' | '{' | '}' |
                      '[' | ']' | '?' | ',' | '.' | '<' | '>' | '=' | '#'

    SpecialNotSlash -> '+' | '-' | -- exclude the star as well
                       '(' | ')' | '"' | '!' | '@' | '`' | '~' |
                       '%' | '&' | '^' | ':' | ';' | "'" | '\' | '|' | '{' | '}' |
                       '[' | ']' | '?' | ',' | '.' | '<' | '>' | '=' | '#'

    SpecialTextChar -> '+' | '-' | '/' | '(' | ')' | '*' | '!' | '@' | '`' | '~' |
                    '%' | '&' | '^' | ':' | ';' | "'" | '|' | '{' | '}' |
                    '[' | ']' | '?' | ',' | '.' | '=' | '#'
                    
    SpecialNotDQ -> '+' | '-' | '/' | '(' | ')' | '*' | '!' | '@' | '`' | '~' |
                    '%' | '&' | '^' | ':' | ';' | "'" | '|' | '{' | '}' |
                    '[' | ']' | '?' | ',' | '.' | '=' | '#' | '<' | '>'

    NotSlashOrStar -> Letter
                    | Digit
                    | SpecialNotSlash
                    | WSChar

    NotEol -> Letter
            | Digit
            | Space
            | '*'
            | SpecialNotStar
            | HT
            | FF
            | CtlCharNotWS

    NotDQ -> Letter
           | Digit
           | SpecialNotDQ
           | Space
           | HT
           | FF
           | EscapeSequence
           | '\' u HexDigit HexDigit HexDigit HexDigit
           | '\' OctalDigit
           
	 SymbolCon -> SymbolChar | SymbolCon SymbolChar
           
     SymbolChar -> Letter
     		| Digit
     		| SymbolCharSpecial
     		
     SymbolCharSpecial ->      '+' | '-' | '*' | '/' |
             '(' | '"' | '!' | '@' | '`' | '~' | "'" |
             '%' | '&' | '^' | ':' | '\' | '|' | '{' | '}' |
             '[' | ']' | '?' | '.' | '<' | '=' | '#'

    TextChar -> Letter
           | Digit
           | SpecialTextChar
           | Space
           | HT
           | FF
           | EscapeSequence
           | LF
           | CR
           | '\' u HexDigit HexDigit HexDigit HexDigit
           | '\' OctalDigit

    EscapeSequence ::= '\' b
                     | '\' t
                     | '\' n
                     | '\' f
                     | '\' r
                     | '\' '"'
                     | '\' "'"
                     | '\' '<'
                     | '\' '>'
                     | '\' '\'
%End
