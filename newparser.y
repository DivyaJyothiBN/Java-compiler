%{
	#include "symboltb.h"
	int yylex();
	void yyerror(char *);
	extern int colNum;
	extern int lineNum;
	extern int scope;
	char * newAddr();
	char *concate(char *,char * , char *);
	void addVariableType(char *type , char *name , char *value , int scope);
	void addValue(char *name , char *value);
	char *Quad(char *,char *,char *);
	char *convertToString(int val);
	void print3Code();
	int yydebug = 1;	

%}



%start CompilationUnit

%union
{
	struct symNode *node; 
	char *name;
	int count;
	struct 
	{
		struct 
		{
			char *addr;
			char *code;
		}icg;
		char *type;
		char *name;
		char *val;
	}attr;
	int intval;
	char *strval;
	char *charval;

};


%token <intval> NUM
%token <node> ID 
%token ASSIGN
%token BYTE SHORT INT LONG FLOAT CHAR DOUBLE BOOLEAN STRING
%token CLASS MAIN VOID
%token PUBLIC PROTECTED PRIVATE STATIC ABSTRACT FINAL NATIVE SYNCHRONIZED TRANSIENT VOLATILE STRICTFP 
%token DEFAULT CASE CONTINUE BREAK RETURN SWITCH
%token SYSTEM OUT PRINTLN
%token PACKAGE IMPORT ENUM
%token <strval>STRINGLITERAL 
%token <charval> CHARACTERLITERAL NULLLITERAL

%token INC DEC AND OR BITOR BITAND EQUAL TILDE QUESTION GT LT GE LE NE CARET MOD BANG URSHIFT LSHIFT RSHIFT
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token  ADD SUB MUL DIV SEMI COMMA DOT COLON
%token ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN AND_ASSIGN OR_ASSIGN XOR_ASSIGN MOD_ASSIGN LSHIFT_ASSIGN RSHIFT_ASSIGN URSHIFT_ASSIGN 
%token TRUE FALSE

%right ASSIGN ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN AND_ASSIGN XOR_ASSIGN OR_ASSIGN LSHIFT_ASSIGN RSHIFT_ASSIGN URSHIFT_ASSIGN
%left OR
%left AND
%left BITOR
%left BITAND
%left EQUAL NE
%left LT LE GT GE 
%left LSHIFT RSHIFT URSHIFT
%left ADD  SUB 
%left MUL DIV MOD
%right INC DEC TILDE BANG
%left LBRACK RBRACK LBRACE RBRACE DOT 

%type <attr> CompilationUnit PackageOrEmpty QualifiedIdentifier Identifier CompilationUnit1 CompilationUnit2
%type <attr> ImportDeclaration ImportDeclaration1 ImportDeclaration2 ClassEnumDeclaration ClassDeclaration 
%type <attr> EnumDeclaration Modifier ClassBody ClassBodyDeclaration BlockOrSemi Block BlockStatements BlockStatement
%type <attr> VariableDeclarationStatement VariableDeclarators AssignmentOrEmpty Variable ScalarType ArrayType
%type <attr> Brac Type BasicType Statement IdentifierOrEmpty SwitchBlockStatements SwitchBlockStatement SwitchLabel
%type <attr> EnumConstantName Expression BinExp UnaryExp InfixOp AssignmentOp RHSExpression
%type <attr> PrefixOp PostfixOp Primary ParExpression Literal BooleanLiteral
%type <attr> EnumConstantsOrEmpty EnumConstants EnumConstant ArgumentsOrEmpty Arguments ExpressionOrEmpty EnumBody
%type <attr> RHSExpression Primary BinExp UnaryExp 
%% 

CompilationUnit : PackageOrEmpty CompilationUnit1 {printf("Valid\n");YYACCEPT;} ;

PackageOrEmpty : PACKAGE QualifiedIdentifier SEMI 
	| ;
	
QualifiedIdentifier : ID {$$.name = $1;}
	| QualifiedIdentifier DOT Identifier {$$.name = concate($1.name , "." , $3.name);};
	
Identifier : ID {$$.name = $1;$$.type=$$.name;};

CompilationUnit1 : ImportDeclaration /*{printf("Import Done");}*/ CompilationUnit1
	| CompilationUnit2 ;
	
ImportDeclaration : IMPORT STATIC Identifier ImportDeclaration1 
		| IMPORT Identifier ImportDeclaration1 ;

ImportDeclaration1 : DOT Identifier SEMI 
		| ImportDeclaration2 ;

ImportDeclaration2 : DOT MUL SEMI 
		| SEMI;
		
CompilationUnit2 : CompilationUnit2 Modifier ClassEnumDeclaration
	| Modifier ClassEnumDeclaration
	;

ClassEnumDeclaration : ClassDeclaration
	| EnumDeclaration
	;
	
ClassDeclaration : CLASS Identifier ClassBody ;

Modifier: 
    PUBLIC 
    | PROTECTED 
    | PRIVATE 
    | STATIC 
    | ABSTRACT 
    | FINAL 
    | NATIVE 
    | SYNCHRONIZED 
    | TRANSIENT 
    | VOLATILE 
    | STRICTFP 
    ;
    
ClassBody : LPAREN ClassBodyDeclaration RPAREN ;

ClassBodyDeclaration : PUBLIC STATIC VOID MAIN LBRACE STRING Identifier  LBRACK RBRACK RBRACE  BlockOrSemi;

BlockOrSemi : SEMI 
	| Block ;
	
Block : LPAREN BlockStatements RPAREN ;

BlockStatements : BlockStatement BlockStatements 
	| ;
	
BlockStatement : VariableDeclarationStatement 
	| Statement ;
	
VariableDeclarationStatement : Type VariableDeclarators SEMI ;

VariableDeclarators : Variable AssignmentOrEmpty {$<attr>$.type = $<attr>0.type;addVariableType($<attr>$.type,$<attr>1.name,$2.val,scope);}
	| Variable {addVariableType($<attr>$.type,$<attr>1.name,NULL,scope);}COMMA VariableDeclarators ;
	
AssignmentOrEmpty : AssignmentOp RHSExpression {$$.val = $2.val;$$.icg.addr=Quad($1.icg.addr,$2.icg.addr,$<attr>0.icg.addr);}
	| {$$.val = NULL ;};
	
Variable : ScalarType {$$.type = $1.type;$$.val=$1.val;$$.icg.addr=$1.icg.addr;}
	| ArrayType {$$.type = $1.type;$$.val=$1.val;$$.icg.addr=$1.icg.addr;};
	
ScalarType : Identifier {$$.type = $1.type;$$.val=$1.name;$$.icg.addr=$1.name;}
	| QualifiedIdentifier {$$.type = $1.type;$$.val=$1.name;$$.icg.addr=$1.name;};
		
ArrayType : Identifier Brac {$$.type = $1.type;$$.val=$1.name;$$.icg.addr=$1.name;}
	| QualifiedIdentifier Brac {$$.type = $1.type;$$.val=$1.name;$$.icg.addr=$1.name;};
	
Brac : LBRACK RBRACK Brac
	| LBRACK RBRACK ;

Type : BasicType {$$.type = $1.type;}
	| STRING {$$.type = "STRING";}
	| Identifier {$$.type = $$.name;} ; 

BasicType : BYTE {$$.type = "BYTE";}
	| SHORT {$$.type = "SHORT";}
	| CHAR {$$.type = "CHAR";}
	| INT {$$.type = "INT";}
	| FLOAT {$$.type = "FLOAT";}
	| DOUBLE {$$.type = "DOUBLE";} 
	| BOOLEAN {$$.type = "BOOLEAN";}
	;
	
Statement : Block
	| SEMI 
	| Expression SEMI
	| SWITCH ParExpression LPAREN SwitchBlockStatements RPAREN
	| BREAK IdentifierOrEmpty SEMI
	;

IdentifierOrEmpty : Identifier
	| ;
	
SwitchBlockStatements : SwitchBlockStatement SwitchBlockStatement
	| ;

SwitchBlockStatement : SwitchLabel BlockStatements ;

SwitchLabel : CASE Expression COLON 
	| CASE EnumConstantName COLON
	| DEFAULT COLON 
	;

EnumConstantName : QualifiedIdentifier ;

Expression : Variable AssignmentOp RHSExpression {$$.val = "";/*printf("%s\n",$3.icg.addr);*/$$.icg.addr = Quad($2.icg.addr,$3.icg.addr,$1.icg.addr); if($1.type!=$3.type) printf("Invalid assignment\n %s %s\n",$1.type,$2.type); else{$1.type=$3.type;}}
	| RHSExpression {$$.val = "";};

InfixOp : OR {$$.icg.addr = "||";}
    | AND {$$.icg.addr = "&&";}
    | BITOR {$$.icg.addr = "|";}
    | BITAND {$$.icg.addr = "&";}
    | CARET {$$.icg.addr = "^";}
    | EQUAL {$$.icg.addr = "==";}
    | NE {$$.icg.addr = "!=";}
    | LT {$$.icg.addr = "<";}
    | GT {$$.icg.addr = ">";}
    | LE {$$.icg.addr = "<=";}
    | GE {$$.icg.addr = ">=";}
    | LSHIFT {$$.icg.addr = "<<=";}
    | RSHIFT {$$.icg.addr = ">>=";}
    | URSHIFT {$$.icg.addr = ">>>=";}
    | ADD {$$.icg.addr = "+";}
    | SUB {$$.icg.addr = "-";}
    | MUL {$$.icg.addr = "*";}
    | DIV {$$.icg.addr = "/";}
    | MOD {$$.icg.addr = "%";}
	;

AssignmentOp : ASSIGN {$$.icg.addr = "=";}
    |ADD_ASSIGN {$$.icg.addr = "+=";}
    |SUB_ASSIGN {$$.icg.addr = "||";}
    |MUL_ASSIGN {$$.icg.addr = "||";}
    |DIV_ASSIGN {$$.icg.addr = "||";}
    |AND_ASSIGN {$$.icg.addr = "||";}
    |OR_ASSIGN {$$.icg.addr = "||";}
    |XOR_ASSIGN {$$.icg.addr = "||";}
    |MOD_ASSIGN {$$.icg.addr = "||";}
    |LSHIFT_ASSIGN {$$.icg.addr = "||";}
    |RSHIFT_ASSIGN {$$.icg.addr = "||";}
    |URSHIFT_ASSIGN {$$.icg.addr = "||";}
    ;
	
RHSExpression : Primary {/*printf("%s\n",$1.icg.addr);*/$$.val = $1.val;$$.icg.addr = $1.icg.addr;}
		| BinExp {$$.val = "";/*printf("addr 1 = %s\n",$1.icg.addr);*/$$.icg.addr=$1.icg.addr;$$.type=$1.type;}
		| UnaryExp {$$.val = "";$$.icg.addr = $1.icg.addr;}
	//	| Variable {$$.val = $1.val;};

BinExp : RHSExpression InfixOp RHSExpression {$$.val = $1.val;$$.icg.addr = Quad($2.icg.addr,$1.icg.addr,$3.icg.addr);if($1.type != $3.type) printf("Invalid type for operator\n");else {$$.type=$1.type;}/*printf("addr 1 = %s\n",$$.icg.addr);*/}
	;
	
UnaryExp : Variable PostfixOp {$$.val = $1.val ;$$.icg.addr=Quad("=",$2.icg.addr,$1.icg.addr);} 
	| PrefixOp Variable {$$.val = $1.val;$$.icg.addr=Quad($1.icg.addr,$2.icg.addr,NULL);} 
	;		
	
PrefixOp : INC {$$.icg.addr = "++";}
	| DEC {$$.icg.addr = "--";}
	| ADD {$$.icg.addr = "+";}
	| SUB {$$.icg.addr = "-";}
	;

PostfixOp : INC {$$.icg.addr = Quad("+",$<attr>0.icg.addr,$<attr>0.icg.addr);}
	| DEC {$$.icg.addr = Quad("-",$<attr>0.icg.addr,$<attr>0.icg.addr);;}
	;
	
Primary : Literal {$$.val = $1.val;$$.icg.addr=$1.icg.addr;$$.type=$1.type;}
	| Identifier {$$.val = $1.name;$$.icg.addr=$1.name;$$.type=$1.type;}
	| QualifiedIdentifier {$$.val = $1.name;$$.icg.addr=$1.name;$$.type=$1.type;}
	| ParExpression {$$.val = $1.val;$$.icg.addr=$1.icg.addr;$$.type=$1.type;}
	;
	
ParExpression : LBRACE Expression RBRACE {$$.val = $2.val;$$.icg.addr = $2.icg.addr;$$.type=$2.type;};

Literal : NUM {$$.val = $1;$$.icg.addr = convertToString($1);$$.type="num";}
	| STRINGLITERAL {$$.val = $1;$$.icg.addr=$1;$$.type="string";}
	| CHARACTERLITERAL {$$.val = $1;$$.icg.addr=$1;$$.type="charecter";}
	| NULLLITERAL {$$.val = $1;$$.icg.addr=$1;$$.type="null";}
	| BooleanLiteral {$$.val = $1.val;$$.icg.addr=$1.icg.addr;$$.type="bool";}
	;

BooleanLiteral : TRUE {$$.val = "TRUE";$$.icg.addr = "TRUE ";}
	| FALSE {$$.val = "FALSE";$$.icg.addr = "FALSE ";}; 

EnumDeclaration : ENUM Identifier EnumBody ;

EnumBody : LPAREN EnumConstantsOrEmpty RPAREN ;

EnumConstantsOrEmpty : EnumConstants
	| ;
	
EnumConstants : EnumConstants COMMA EnumConstant 
	| EnumConstant ;
	
EnumConstant : Identifier ArgumentsOrEmpty ;

ArgumentsOrEmpty : Arguments 
	| ;
	
Arguments : LBRACE ExpressionOrEmpty RBRACE ;

ExpressionOrEmpty : Expression 
	| ;
	
%%

int main()
{
	initSymbolTable();
	yyparse();
	printSymTable();
	print3Code();
	//printf("exit");
	exit(0);
}

void yyerror(char *err)
{
	printf("%s\n",err);
	exit(0);
}

char * newAddr()
{
	static int count = 1;
	char *tmp = (char *)malloc(sizeof(char)*3);
	sprintf(tmp,"t%d",count);
	count += 1 ;
	//printf("Temp create for %s \n",tmp);
	return tmp;
}

char *concate(char *str1 , char *str2 , char *str3)
{
	char *str = (char *)calloc(sizeof(char) , strlen(str1)*30);
	strcpy(str,str1);
	strcat(str,str2);
	strcat(str,str3);
	return str;
}

void addVariableType(char *type , char *name , char *value , int scope)
{
	//printf("val = %s\n",value);
	struct symNode *tmp = lookandcreateNode(name , 0 , 0 , scope);
	if(tmp != NULL)
		symbolInsert(tmp);
	addType(name,scope,type,value);
}

char *Quad(char *op , char *arg1 , char *arg2)
{
	printf("%s , %s , %s \n",arg1,arg2,op);
	struct Quadruple *tmp = (struct Quadruple *)calloc(sizeof(struct Quadruple),1);
	if(strcmp(op,"++")==0 || strcmp(op,"--")==0)
	{
		tmp->arg1 = strdup(arg1);
		tmp->arg2 = strdup(arg1);
		tmp->op = strdup(op+1);
		tmp->res = newAddr();
		Quad("=",tmp->res,tmp->arg1);
	}
	else
	{	
		tmp->op = strdup(op);
		tmp->arg1 = strdup(arg1);
		if(strcmp(op,"=")==0)
		{
			tmp->res = strdup(arg2);
			tmp->arg2 = "";
		}
		else if(arg2==NULL)
		{
			tmp->res = newAddr();
			tmp->arg2 = "";
		}
		else
		{
			tmp->arg2 = strdup(arg2);
			tmp->res = newAddr();
		}
	}	
	tmp->next = NULL;
	if(quadHead == NULL)
		quadHead = tmp;
	else
	{
		struct Quadruple *tmp1 = quadHead;
		while(tmp1->next != NULL)
		{
			tmp1 = tmp1->next;
		}
		tmp1->next = tmp;
	}
	return tmp->res;
}


void print3Code()
{
	printf("3 Address Code : \n");
	struct Quadruple *tmp = quadHead;
	while(tmp!= NULL)	
	{
		if(strcmp(tmp->arg2,"")==0)
		{
			if(strcmp(tmp->op,"+")==0 || strcmp(tmp->op,"-")==0)
				printf("%s %s %s %s\n",tmp->res,"=",tmp->op,tmp->arg1);
			else
				printf("%s %s %s \n",tmp->res,tmp->op,tmp->arg1);
		}
		else
			printf("%s %s %s %s %s\n",tmp->res,"=",tmp->arg1,tmp->op,tmp->arg2);
		tmp = tmp->next;
	}
}

char *convertToString(int val)
{
	char *str = (char *)calloc(sizeof(char),20);
	sprintf(str,"%d",val);
	return str;
}
