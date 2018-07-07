%{
	#include "y.tab.h"
	#include "symboltb.h"
	int lineNum = 1;
	int colNum = 1;
	int scope = 0;
	int print = 0;
	void yyerror(char *);
	void printToken(char *);
%}

delim [\t ]
ws	{delim}+
letter	[A-Za-z]
digit [0-9]
id	{letter}({letter}|{digit}|_)*
num 	{digit}+(\.{digit}+)?(E[+-]?{digit}+)?
integer {digit}+
bcomment "/*"
strLiteral \"(\\.|[^"\\])*\"
charLiteral \'{letter}\'

%x COMMENT

%%

<INITIAL>{

{bcomment}	BEGIN(COMMENT);

"//".*	;

{ws}	{
		colNum += yyleng ;	
	}

"\n"	{
		lineNum++ ;
		colNum = 1;
	}

"class"	{
		colNum += yyleng ;
		printToken(yytext);
		return CLASS ;	
	}

"void"	{
		colNum += yyleng ;
		printToken(yytext);
		return VOID ;	
	}
	
"byte"	{
		colNum += yyleng ;
		printToken(yytext);
		return BYTE ; 	
	}

"short"	{
		colNum += yyleng ;	
		printToken(yytext);
		return SHORT;	
	}
    
"char"	{
		colNum += yyleng ;	
		printToken(yytext);
		return CHAR;	
	}

"int"	{
		colNum += yyleng ;
		printToken(yytext);
		return INT;	
	}

"long"	{
		colNum += yyleng ;	
		printToken(yytext);
		return LONG;	
	}

"float"	{
		colNum += yyleng ;	
		printToken(yytext);
		return FLOAT;	
	}

"double"	{
		colNum += yyleng ;	
		printToken(yytext);
		return DOUBLE;	
	}

"boolean"	{
		colNum += yyleng ;
		printToken(yytext);
		return BOOLEAN;	
	}

"String"	{
		colNum += yyleng ;	
		printToken(yytext);
		return STRING;	
	}

"public"	{
		colNum += yyleng ;	
		printToken(yytext);
		return PUBLIC;	
	}
	
"protected"	{
		colNum += yyleng ;	
		printToken(yytext);
		return PROTECTED;	
	}
	
"private"	{
		colNum += yyleng ;	
		printToken(yytext);
		return PRIVATE;	
	}
	
"static"	{
		colNum += yyleng ;	
		printToken(yytext);
		return STATIC;	
	}
	 
"abstract"	{
		colNum += yyleng ;	
		printToken(yytext);
		return ABSTRACT;	
	}
	
"final"	{
		colNum += yyleng ;	
		printToken(yytext);
		return FINAL;	
	}
	
"native"	{
		colNum += yyleng ;	
		printToken(yytext);
		return NATIVE;	
	}
	
"synchronized"	{
		colNum += yyleng ;	
		printToken(yytext);
		return SYNCHRONIZED;	
	}
	
"transient"	{
		colNum += yyleng ;	
		printToken(yytext);
		return TRANSIENT;	
	}
	
"volatile"	{
		colNum += yyleng ;	
		printToken(yytext);
		return VOLATILE;	
	}
	
"strictfp"	{
		colNum += yyleng ;	
		printToken(yytext);
		return STRICTFP;	
	}

"default"	{
		colNum += yyleng ;	
		printToken(yytext);
		return DEFAULT;	
	}

"case"	{
		colNum += yyleng ;	
		printToken(yytext);
		return CASE;	
	}
	
"main"	{
		colNum += yyleng ;	
		printToken(yytext);
		return MAIN;	
	}

"System"	{
		colNum += yyleng ;	
		printToken(yytext);
		return SYSTEM;	
	}

"out"	{
		colNum += yyleng ;	
		printToken(yytext);
		return OUT;	
	}

"println"	{
		colNum += yyleng ;	
		printToken(yytext);
		return PRINTLN;	
	}

"+"	{
		colNum += yyleng ;	
		printToken(yytext);
		return ADD;	
	}
	
"-"	{
		colNum += yyleng ;	
		printToken(yytext);
		return SUB;	
	}

"*"	{
		colNum += yyleng ;	
		printToken(yytext);
		return MUL;	
	}

"/"	{
		colNum += yyleng ;	
		printToken(yytext);
		return DIV;	
	}

"="	{
		colNum += yyleng ;
		printToken(yytext);
		return ASSIGN;	
	}

"."	{
		colNum += yyleng ;	
		printToken(yytext);
		return DOT;	
	}

";"	{
		colNum += yyleng ;
		printToken(yytext);
		return SEMI;	
	}

":"	{
		colNum += yyleng ;	
		printToken(yytext);
		return COLON;	
	}

","	{
		colNum += yyleng ;	
		printToken(yytext);
		return COMMA;	
	}

"("	{
		colNum += yyleng ;	
		printToken(yytext);
		return LBRACE;	
	}

")"	{
		colNum += yyleng ;
		printToken(yytext);
		return RBRACE;	
	}

"["	{
		colNum += yyleng ;	
		printToken(yytext);
		return LBRACK;	
	}

"]"	{
		colNum += yyleng ;	
		printToken(yytext);
		return RBRACK;	
	}
"{"	{
		colNum += yyleng ;
		scope += 1;
		printToken(yytext);
		return LPAREN;	
	}

"}"	{
		colNum += yyleng ;
		scope -= 1;
		printToken(yytext);
		return RPAREN;	
	}

"++"	{
		colNum += yyleng ;	
		printToken(yytext);
		return INC;	
	}
	
"--"	{
		colNum += yyleng ;	
		printToken(yytext);
		return DEC;	
	}


"&&"	{
		colNum += yyleng ;	
		printToken(yytext);
		return AND;	
	}
	
"||"	{
		colNum += yyleng ;	
		printToken(yytext);
		return OR;	
	}
"<="	{
		colNum += yyleng ;	
		printToken(yytext);
		return LE;	
	}
	
">="	{
		colNum += yyleng ;	
		printToken(yytext);
		return GE;	
	}

"!="	{
		colNum += yyleng ;	
		printToken(yytext);
		return NE;	
	}
	
"<"	{
		colNum += yyleng ;	
		printToken(yytext);
		return LT;	
	}

">"	{
		colNum += yyleng ;	
		printToken(yytext);
		return GT;	
	}
	
"=="	{
		colNum += yyleng ;	
		printToken(yytext);
		return EQUAL;	
	}

"?"	{
		colNum += yyleng ;	
		printToken(yytext);
		return QUESTION;	
	}
	
"~"	{
		colNum += yyleng ;	
		printToken(yytext);
		return TILDE;	
	}

"|"	{
		colNum += yyleng ;	
		printToken(yytext);
		return BITOR;	
	}
	
"&"	{
		colNum += yyleng ;	
		printToken(yytext);
		return BITAND;	
	}

"^"	{
		colNum += yyleng ;	
		printToken(yytext);
		return CARET;	
	}
	
"%"	{
		colNum += yyleng ;	
		printToken(yytext);
		return MOD;	
	}

"!"	{
		colNum += yyleng ;	
		printToken(yytext);
		return BANG;	
	}
	
">>>"	{
		colNum += yyleng ;	
		printToken(yytext);
		return URSHIFT;	
	}

"<<"	{
		colNum += yyleng ;	
		printToken(yytext);
		return LSHIFT;	
	}
	
">>"	{
		colNum += yyleng ;	
		printToken(yytext);
		return RSHIFT;	
	}

"null"	{
		colNum += yyleng ;	
		printToken(yytext);
		return NULLLITERAL;	
	}
	
"true"	{
		colNum += yyleng ;	
		printToken(yytext);
		return TRUE;	
	}
	
"false"	{
		colNum += yyleng ;	
		printToken(yytext);
		return FALSE;	
	}
	
"return"	{
		colNum += yyleng ;	
		printToken(yytext);
		return RETURN;	
	}

"import"	{
		colNum += yyleng ;	
		printToken(yytext);
		return IMPORT;	
	}
	
"package"	{
		colNum += yyleng ;	
		printToken(yytext);
		return PACKAGE;	
	}

"enum"	{
		colNum += yyleng ;	
		printToken(yytext);
		return ENUM;	
	}

"switch"	{
		colNum += yyleng ;	
		printToken(yytext);
		return SWITCH;	
	}

"break"	{
		colNum += yyleng ;	
		printToken(yytext);
		return BREAK;	
	}

"continue"	{
		colNum += yyleng ;	
		printToken(yytext);
		return CONTINUE;	
	}

{charLiteral}	{	
		colNum += yyleng ;
		printToken(yytext);
		yylval.charval = atoi(yytext);						
		return CHARACTERLITERAL;	
	}



{strLiteral}	{	
		colNum += yyleng ;
		printToken(yytext);
		yylval.strval = atoi(yytext);		
		return STRINGLITERAL;	
	}
	
{num}	{
		colNum += yyleng;
		printToken(yytext);
		yylval.intval = atoi(yytext);
		return NUM;	
	}

{id}	{	
		printToken(yytext);
		colNum += yyleng;
		yylval.strval = strdup(yytext);
		return ID;
	}

.	{yyerror("Synt\n");printf("Syntax Error at line %d , Column %d\n",lineNum,colNum);}
}

<COMMENT>{

"*/"	{BEGIN(INITIAL);};

"\n"	lineNum++ ;	
.	;

}

%%


int yywrap (void) {return 1;}


void printToken(char *token)
{
	if(print)
		printf("%s\n",token);

}
