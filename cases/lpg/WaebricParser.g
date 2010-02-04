%options ast_directory=./WaebricAst,automatic_ast=toplevel,var=nt,visitor=default
%options fp=WaebricParser,prefix=TK_
%options programming_language=java
%options package=waebricc
%options template=dtParserTemplateD.g
%options import_terminals=WaebricLexer.g
%options lalr=2
%options parent_saved
%options TRACE=FULL

%Terminals
   
    IDENTIFIER
    AND_AND
    OR_OR

    Path
    StringLiteral
    IntegerLiteral
    SymbolLiteral
    CommentLiteral
    PreText
    MidText
    PostText
           
    module 
    site 
    end 
    def 
    if 
    each 
    else 
    let 
    in 
    comment
    echo
    cdata
    yield
    list
    record
    string
    import
    
    AT        ::= '@'
    PERCENT   ::= '%'
    LPAREN    ::= '('
    RPAREN    ::= ')'
    LBRACE    ::= '{'
    RBRACE    ::= '}'
    LBRACKET  ::= '['
    RBRACKET  ::= ']'
    SEMICOLON ::= ';'
    COMMA     ::= ','
    DOLLAR    ::= '$'
    DOT       ::= '.'
    COLON     ::= ':'
    SHARP     ::= '#'
    EQUAL     ::= '='
    NOT       ::= '!'
	PLUS      ::= '+'    
%End


%Start
    Modules
%End

%Rules

	--
	-- Module
	--
	
	Modules$$Module ::= %empty
		| Modules Module

    Module ::= 'module' ModuleIds ModuleBlockOpt
    
    
    --
    -- ModuleId
    --

    ModuleIds$$ModuleId ::= ModuleId '.' ModuleIds 
    	| ModuleId
    
    ModuleId ::= Name
    
    
    --
    -- ModuleBlock
    --
    
    ModuleBlockOpt$$ModuleBlock ::= %empty 
    	| ModuleBlocks
    
    ModuleBlocks$$ModuleBlock ::= ModuleBlock ModuleBlocks 
    	| ModuleBlock
    
    ModuleBlock ::= Function 
    	| Site 
    	| Import
    
    	
    --
    -- Site
    --
    
    Site ::= 'site' MappingsOpt 'end'
    
    -- 
    -- Import 
    --
    
    Import ::= 'import' ModuleIds
    
        --
    -- Function
    --       
    Function$FunctionWithParameters ::= 'def' 'IDENTIFIER'$Name ParameterDeclOpt StatementOpt 'end'
    Function$FunctionWithoutParameters ::= 'def' 'IDENTIFIER'$Name StatementOpt 'end'

    --
    -- Parameter
    --       
    
    ParameterDeclOpt$$ParameterDecl ::= '(' ')' | '(' ParameterDecls ')'
    ParameterDecls$$ParameterDecl ::= ParameterDecl ',' ParameterDecls | ParameterDecl
    ParameterDecl ::= Name
    Name ::= 'IDENTIFIER'$Name
    
    --
    -- Mapping
    -- 
    
    MappingsOpt$$Mapping ::= %empty | Mappings
    Mappings$$Mapping ::= Mapping ';' Mappings | Mapping
    Mapping ::= Path$Path ':' Markup
    
    --
    -- Markup
    --
    
    Markup ::= Designator | Designator ArgumentsOpt
    MarkupOpt$$Markup ::= %empty | Markups
    Markups$$Markup ::= Markups Markup | Markup
    
    --
    -- Arguments
    --
    
    ArgumentsOpt$$Argument ::= '(' ')' | '(' Arguments ')'
    Arguments$$Argument ::= Argument ',' Arguments | Argument
    Argument ::= Var '=' Expression | Expression
        
    
    --
    -- Expression
    --
    
    ExpressionOpt$$Expression ::= %empty | Expressions
    Expressions$$Expression ::= Expression ',' Expressions | Expression
    
    Expression ::= 
    	  Var 
    	| ExpressionString 
    	| ExpressionCollection 
    	| ExpressionPair 
    	| ExpressionConstant
    	| ExpressionSymbol
    	| ExpressionPlus
    	
    ExpressionNoPlus ::=
    	  Var 
    	| ExpressionString 
    	| ExpressionCollection 
    	| ExpressionPair 
    	| ExpressionConstant
    	| ExpressionSymbol
    
   	ExpressionString ::= 'StringLiteral'$String
    ExpressionPlus ::= ExpressionNoPlus '+' Expression
    ExpressionConstant ::= 'IntegerLiteral'$IntegerString
    ExpressionSymbol ::= 'SymbolLiteral'$SymbolString    
    ExpressionCollection ::= '[' ExpressionOpt ']'
    ExpressionPair ::= '{' KeyValuePairOpt '}'
    
    -- 
    -- KeyValuePair
    --
    
    KeyValuePairOpt$$KeyValuePair ::= %empty | KeyValuePairs
    
    KeyValuePairs$$KeyValuePair ::= KeyValuePair ',' KeyValuePairs | KeyValuePair
    
    KeyValuePair ::= 'IDENTIFIER'$Key ':' Expression
    
    --
    -- Embedding
    --
   
    Embedding ::= 'PreText'$PreText Embed TextTail
    TextTail$TextTailMidText ::= 'MidText'$MidText Embed TextTail 
    TextTail$TextTailPostText ::= 'PostText'$PostText
    Embed ::= MarkupOpt Expression
    
    --
    -- Statement
    --
    
    StatementWithoutSubstatement ::=
		  StatementBlock
    	| StatementCData 
    	| StatementYield  	 
     	| StatementComment 
    	| StatementMarkup
    	| StatementEchoExpression 
    	| StatementEchoEmbedding 
    	| StatementLet 
    	
    StatementWithoutSubAndMarkup ::=
 		  StatementBlock
    	| StatementCData 
    	| StatementYield  	 
     	| StatementComment 
    	| StatementEchoExpression 
    	| StatementEchoEmbedding    
    	| StatementLet
        
    Statement ::= 
    	  StatementWithoutSubstatement
    	| StatementIf
    	| StatementIfElse
    	| StatementEach 
    	| StatementMarkupStatement
    	
    StatementNoShortIf ::=  
		  StatementWithoutSubstatement
		| StatementIfElseNoShortIf 
		| StatementEachNoShortIf
		| StatementMarkupStatementNoShortIf
    
    StatementWithoutMarkup ::= 
    	  StatementWithoutSubAndMarkup
    	| StatementIf
    	| StatementIfElse
    	| StatementEach

    StatementWithoutMarkupNoShortIf ::= 
    	  StatementWithoutSubAndMarkup
    	| StatementIfElseNoShortIf
    	| StatementEachNoShortIf    	
    	
    StatementIf ::= 'if' '(' Predicate ')' Statement 
    
    StatementIfElse ::= 'if' '(' Predicate ')' StatementNoShortIf  'else' Statement
    
    StatementIfElseNoShortIf ::=  'if' '(' Predicate ')' StatementNoShortIf  'else' StatementNoShortIf
    
    StatementEach ::= 'each' '(' Var ':' Expression ')' Statement
    StatementEachNoShortIf ::= 'each' '(' Var ':' Expression ')' StatementNoShortIf   
    
    StatementLet ::= 'let' Assignments 'in' StatementOpt 'end'
    
    StatementBlock ::= '{' StatementOpt '}'
    
    StatementOpt$$Statement ::= %empty | Statements
    Statements$$Statement ::=  Statements Statement | Statement
        
    StatementComment ::= 'comment' CommentLiteral$Comment ';'
    StatementEchoExpression ::= 'echo' Expression ';'
    StatementEchoEmbedding ::= 'echo' Embedding ';'
    StatementCData ::= 'cdata' Expression ';'
    StatementYield ::= 'yield' ';'
    
    StatementMarkup$StatementMarkup ::= Markup ';'
    StatementMarkup$StatementMarkupWithDesignator ::= Markups Designator ArgumentsOpt ';'
    StatementMarkup$StatementMarkupWithExpression ::= Markups Expression ';'
    StatementMarkup$StatementMarkupWithEmbedding ::= Markups Embedding ';'
    	
	StatementMarkupStatement ::= Markups StatementWithoutMarkup
	StatementMarkupStatementNoShortIf ::= Markups StatementWithoutMarkupNoShortIf
    
	--
	-- Type
	--
	
	Type$TypeList ::= 'list' 
	Type$TypeRecord ::= 'record' 
	Type$TypeString ::= 'string'
	
    --
    -- Predicate
    --	
    
    Predicate$PredicateCheckType ::= Expression | Expression '.' Type
    
    Predicate$PredicateNegate ::= '!' Predicate
    Predicate$PredicateAnd ::= '(' Predicate ')' 'AND_AND'$AndPredicate '(' Predicate ')'
    Predicate$PredicateOr ::= '(' Predicate ')' 'OR_OR'$OrPredicate '(' Predicate ')'
    
    --
    -- Var
    --
    
    Var ::= 'IDENTIFIER'$Name
    
    -- 
    -- Assignment
    --
    
    Assignments$$Assignment ::= Assignments Assignment | Assignment
    Assignment$AssignmentExpression ::= Var '=' Expression ';'
    Assignment$AssignmentStatement ::= 'IDENTIFIER'$Name ParameterDeclOpt '=' Statement   
    
    --
    -- Designator
    --

    Designator ::= 'IDENTIFIER'$Name AttributeOpt
    
    -- 
    -- Attribute
    --
    
    AttributeOpt$$Attribute ::= %empty | Attributes
    Attributes$$Attribute ::= Attribute Attributes | Attribute
    
    Attribute$AttributeSharp ::= '#' 'IDENTIFIER'$Name
    Attribute$AttributeDot ::= '.' 'IDENTIFIER'$Name
    Attribute$AttributeDollar ::= '$' 'IDENTIFIER'$Name
    Attribute$AttributeColon ::= ':' 'IDENTIFIER'$Name
    Attribute$AttributeWidth ::= '@' IntegerLiteral$Width
    Attribute$AttributeWidthHeight ::= '@' IntegerLiteral$Width '%' IntegerLiteral$Height

%End            