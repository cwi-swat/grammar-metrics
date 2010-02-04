/*
    Waebric Oslo implementation
    Implemented by Jeroen van Lieshout
*/
module Waebric
{
    export Waebric;
    
    
    language Waebric
    {
               
        //---Comments---
        @{Classification["Comment"]}
        token Comment = CommentMultipleLine | CommentLine;
        
        token CommentLine = "//" CommentLineContent*;
        
        token CommentLineContent = ^(
                '\u000A' | // New Line
                '\u000D' | // Carriage Return
                '\u0085' | // Next Line
                '\u2028' | // Line Separator
                '\u2029') // Paragraph Separator
        ;
        token CommentMultipleLine = "/*" CommentMultipleLineContent* "*/";
        token CommentMultipleLineContent = 
            ^('*') 
            | '*'  ^('/')
        ;
        
        //---WhiteSpace---
        token NewLine =
            '\u000A'   // New Line
            | '\u000D' // Carriage Return
            | '\u000A' // Line Feed
            | '\u0085' // Next Line
            | '\u2028' // Line Separator
            | '\u2029' // Paragraph Separator
        ;

        @{Classification["Whitespace"]}
        token Whitespace = WhitespaceCharacter+;
        token CR = '\u000D';    //Carriage Return
        token LF = '\u000A';    //Line Feed
        token HT = '\u0009';    //Horizontal Tab
        token VT = '\u000B';    //Vertical Tab
        token FF = '\u000C';    //Form Feed
        token Space = '\u0020'; //Space
        token Spaces = Space* | HT*;
        
        
        token WhitespaceCharacter =
            HT   // Horizontal Tab
            | VT // Vertical Tab
            | FF // Form Feed
            | Space // Space
            | NewLine
        ;     
        
        //---Separators---
        token Left_Paren = '(';
        token Right_Paren = ')';
        token Left_Brace = '{';
        token Right_Brace = '}';
        token Left_Bracket = '[';
        token Right_Bracket = ']';
        token Semi_Colon = ';';
        token Comma = ',';
        token Dot = '.';
        token Caret = '^';
        token Number_Sign = '#';
        token Dollar_Sign = '$';
        token At_Sign = '@';
        token Percent_Sign = '%';
        token Question_Mark = '?';
        token Exclam_Mark = '!';
        token And = '&&';
        token Or = '||';
           
        //---Operators---
        token Colon = ':';
        token Assign = '=';
        token Slash = '/';
        token Plus = '+';
        
        //---Helping Tokens---
        token Letter = 'a'..'z' | 'A'..'Z';
        token Digit = '0'..'9';
        token HexaDecimal = '0'..'9' | 'A'..'F' | 'a'..'f';
        token Minus = '-';
        token Amp = '&';
        token Esc_Quote = ('\\' | '\"') => '\\\"';
        token Str_Char = ("\\n" | "\\t" | "\\\"" | "\\\\" | "\\" Digit Digit Digit | '\u0020'..'\u0021' | '\u0023'..'\u005B' | '\u005D'..'\u007E');
        
        //---Text Tokens---
        token EscQuote = '\\' '\"';
        token Text_Char_Ref = '&#' Digit+ ';' | '&#x' HexaDecimal+ ';';
        token Text_Entity_Ref = '&' (Letter | '_' | '"') (Letter | Digit | '.' | '-' | '_' | ':')* ';';
        token Text_Char = (TextSymbolChar | Amp | Text_Char_Ref | Text_Entity_Ref | Space | CR | LF | HT);
        token TextSymbolChar = ('\u0020'..'\u0021' | '\u0023'..'\u0025' | '\u0027'..'\u003B' | '\u003D'..'\u007E');
        token Text = '"' t:(EscQuote | Text_Char)* '"' => t;        
        
        //---Identifier---
        token IdCon = Letter (Letter | Digit | Minus)*;
        token NatCon = Digit+;
        token SymbolCon = "'" s:SymbolChar* => s;
        token SymbolChar = ('\u0021'..'\u0028' | '\u002A'..'\u002B' | '\u002D'..'\u003A' | '\u003C'..'\u003D' | '\u003F'..'\u005C' | '\u005E'..'\u007C' | '\u007E'..'\u007F');
        token StrCon = '"' s:Str_Char* '"' => s;
        
        //---Path---
        token PathChar = ('\u0021'..'\u002D' | '\u002F'..'\u005A' | '\u005E'..'\u007E');
        token Filename = ('/' | './')  PathChar+ ^('/') '.' (Letter | Digit)+;
        
        //---Misc---       
        token CommentKey = "comment" Spaces t:StrCon Spaces ";" => t;
        token Pre_Text = '"' t:Text_Char* '<' => PreText[t];
        token Post_Text = '>' t:Text_Char* '"' => PostText[t];
        token Mid_Text = '>' t:Text_Char* '<' => MidText[t];
        
        //---Keyword Tokens---
        @{Classification["Keyword"]} final token ModuleKeyword = "module";
        @{Classification["Keyword"]} final token ImportKeyword = "import";
        @{Classification["Keyword"]} final token DefKeyword = "def";
        @{Classification["Keyword"]} final token EndKeyword = "end";
        @{Classification["Keyword"]} final token SiteKeyword = "site";
        @{Classification["Keyword"]} final token EchoKeyword = "echo";
        @{Classification["Keyword"]} final token EachKeyword = "each";
        @{Classification["Keyword"]} final token IfKeyword = "if";
        @{Classification["Keyword"]} final token ElseKeyword = "else";
        @{Classification["Keyword"]} final token YieldKeyword = "yield";
        @{Classification["Keyword"]} final token LetKeyword = "let";
        @{Classification["Keyword"]} final token InKeyword = "in";
        @{Classification["Keyword"]} final token CommentKeyword = "comment";
        @{Classification["Keyword"]} final token CDataKeyword = "cdata";
        @{Classification["Keyword"]} final token String = "string";
        @{Classification["Keyword"]} final token Record = "record";
        @{Classification["Keyword"]} final token List = "list";
        
        //---Waebric---
        //Starting symbol
        syntax Main 
            = m:Module
                => m;
        
        interleave Skippable 
            = Whitespace | Comment;
    
        //---Modules---
        syntax Import 
            = "import" m:ModuleId
                => Import[m];
        
        syntax ModuleElement 
            = i:Import 
                => i
            | s:Site 
                => s
            | f:FunctionDefinition
                => f;
        
        //{IdCon "."}+
        syntax ModuleId  
            = item:IdCon
                => ModuleId[item]
            | item:IdCon "." list: ModuleId 
                => ModuleId[item, valuesof(list)];
                
        syntax Module = "module" m:ModuleId e:ModuleElement*
                => Module[m,valuesof(e)];
     
        //---Sites---
        //site {Mapping ";"}* end
        syntax Site = "site" m:Mappings "end"
            => Site [valuesof(m)];
        
        //{Mapping ";"}
        syntax Mappings
            = item:Mapping
                => Mappings[item]
            | item:Mapping ";" list: Mappings
                => Mappings[item, valuesof(list)];
        
        //{Path ":" Markup}* -> Mapping
        syntax Mapping
            = p:Filename ":" m:Markup
                => Mapping[p, m];   
            
        //---Functions---
        syntax FunctionDefinition 
            = "def" i:IdCon f:Formals? s:StatementList "end"
                => FunctionDef[i,f,s];
                
        syntax StatementList 
            = item:Statement?
                => StatementList[item]
            | item:Statement list:StatementList
                => StatementList[item, valuesof(list)];
        
        syntax Formals = "(" f:Formal? ")"
            => Formals[valuesof(f)];
        
        syntax Formal 
            = item: IdCon
                => [item]
            | item: IdCon "," list: Formal
                => [item, valuesof(list)];
       
        //---Statements
        syntax Statement  
            = s:Statement_No_Markup
                => s
            | m:MarkupList s:Statement_No_Markup
                => MarkupStatStatement[m,s]
            | s:Statement_Markup_No_Statement
                => s;
            
        syntax Statement_No_Markup
            = "each" "(" i:IdCon ":" e:Expression ")" s:Statement
                => EachStatement[i,e,s]
            | "if" "(" p:Predicate ")" ts:Statement
                => IfStatement[p,TrueStatement[ts]]
            | "if" "(" p:Predicate ")" ts:Statement_No_Short_If "else" fs:Statement
                => IfElseStatement[p,TrueStatement[ts],FalseStatement[fs]]
            | s:Statement_No_Markup_No_Short_If
                => s;
        
        syntax Statement_No_Short_If
            = s:Statement_No_Markup_No_Short_If
                => s
            | s:Statement_Markup_No_Statement
                => s
            | "if" "(" p:Predicate ")" ts:Statement_No_Short_If "else" fs:Statement_No_Short_If 
                => IfElseStatement[p,TrueStatement[ts],FalseStatement[fs]];
        
        syntax Statement_Markup_No_Statement
            = m:Markup ";"
                => MarkupStatement[m]
            | m:MarkupsStatement
                => m
            | ml:MarkupList e:Embedding ";"
               => MarkupEmbeddingStatement[ml,e];
                
        syntax MarkupsStatement 
            = ml:MarkupList me:MarkupExpression ";" 
                => MarkupsStatement[ml, valuesof(me)]
            | m:Markup ";"
                => MarkupsStatement[m];        
        
        syntax MarkupExpression 
            = mc:MarkupCall
                => [mc]
            | e:Expression
                => [e];
        
        syntax Statement_No_Markup_No_Short_If 
            = "let" a:Assignment+ "in" s:StatementList "end"
                => LetStatement[Assignments[valuesof(a)],s]
            | "{" s:StatementList "}"
                => BlockStatement[s]
            | t:CommentKey
                => CommentStatement[t]
            | "echo" e:Expression ";"
                => EchoExpressionStatement[e]
            | "echo" e:Embedding ";"
                => EchoEmbeddingStatement[e]
            | "cdata" e:Expression ";"
                => CDataStatement[e]
            | "yield" ";"
                => YieldStatement[];
                
        syntax MarkupList
            = item:Markup
                => MarkupList[item]
            | item:Markup list:MarkupList
                => MarkupList[item,valuesof(list)];
        
        syntax Assignment 
                = f:FuncBindAssignment
                    => f
                | v:VarBindAssignment
                    => v;
                
        syntax VarBindAssignment 
            = i:IdCon "=" e:Expression ";"
                => VarBindAssignment[i,e];
                
        syntax FuncBindAssignment 
            = i:IdCon f:Formals "=" s:Statement
                => FuncBindAssignment[i,f,s];
                
        //---Predicates---
        syntax Predicate 
            = p:Predicate_No_And_Or
                => p
            | l:Predicate "&&" r:Predicate_No_And_Or
                => AndPredicate[l,r]
            | l:Predicate "||" r:Predicate_No_And_Or
                => OrPredicate[l,r];
        
        syntax Predicate_No_And_Or
            = e:Expression
                => ExpressionPredicate[e]
            | e:Expression "." t:Type "?"
                => IsAPredicate[e, t]
            | "!" p:Predicate_No_And_Or
                => NotPredicate[p];

        
        syntax Type = "string" | "record" | "list";

        //---Expressions---
        syntax Expression 
            = e:Expression_No_Plus
               => e
            | l:Expression "+" r:Expression_No_Plus 
                => CatExpression[l,r];
                
        syntax Expression_No_Plus 
            = i:IdCon
                => VarExpression[i]
            | e:Expression_No_Var
                => e;
        
        syntax Expression_No_Var
            = t:Text
                => TextExpression[t]
            | s:SymbolCon 
                => SymbolExpression[s]
            | n:NatCon
                => NatExpression[n]
            | e:Expression_No_Plus "." i:IdCon
                => FieldExpression[e,i]
            | "[" e:ExpressionList? "]"
                => ListExpression[valuesof(e)]
            | "{" k:KeyValuePairList? "}"
                => RecordExpression[valuesof(k)];
         
        syntax ExpressionList
            = item:Expression
                => [item]
            | item:Expression "," list:ExpressionList 
                => [item, valuesof(list)];
                
        syntax KeyValuePairList
            = item:KeyValuePair
                => [item]
            | list:KeyValuePairList "," item:KeyValuePair
                => [valuesof(list), item];
        
        syntax KeyValuePair
            = i:IdCon ":" e:Expression
                => KeyValuePair[i,e];
       
        //---Markup---
        syntax Markup 
            = d:Designator
                => MarkupTag[d]
            | mc:MarkupCall
               => mc;
        syntax MarkupCall 
            = d:Designator a:Arguments
                => MarkupCall[d,a];
        
        syntax Arguments 
            = "(" a:ArgumentList? ")"
                => Arguments[valuesof(a)];
                
        syntax ArgumentList 
            = item: Argument
                => [item]
            |  item:Argument "," list:ArgumentList
                => [item, valuesof(list)];
        
        syntax Argument 
            = i:IdCon "=" e:Expression
                => AttrArgument[i,e]
            | e:Expression
                => ExpressionArgument[e];        
        
        syntax Designator 
            = i:IdCon a:Attribute*
                => Designator[i,Attributes[valuesof(a)]];
        
        syntax Attribute
            = "#" i:IdCon
                => IdAttribute[i]
            | "." i:IdCon
                => ClassAttribute[i]
            | "$" i:IdCon
                => NameAttribute[i]
            | ":" i:IdCon
                => TypeAttribute[i]
            | "@" w:NatCon "%" h:NatCon
                => WidthHeightAttribute[w,h]
            | "@" w:NatCon
                => WidthAttribute[w];
                
        //---Embedding---
        syntax Embedding 
            = p:Pre_Text e:Embed t:TextTail
                => Embedding[p,e,t];
        
        syntax Embed 
            = ml:MarkupList? me:EmbedMarkupExpression 
                => Embed[ml, valuesof(me)]
            | m:Markup
                => Embed[null, m];
                
        syntax EmbedMarkupExpression 
            = mc:MarkupCall
                => [mc]
            | e:Expression
                => [e];
            
        syntax TextTail 
            = p:Post_Text
                => PostTextTail[p]
            | m:Mid_Text e:Embed t:TextTail
                => MidTextTail[m,e,t];

    }
}