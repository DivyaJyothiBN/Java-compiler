#include <stdio.h>
#include <stdlib.h>
#include<string.h>
#include<fcntl.h>
#include <stdbool.h>

#define MAX_ENTRIES 100
typedef enum { _INTEGER , _FLOAT , _STRING , _BOOLEAN , _ARRAY , _CLASS , _ENUM}TYPE;

union value
	{
		int intval;
		float floatval;
		char *strval;
		bool boolval;
	};


struct Type
{
	//TYPE typeName;
	char typeName[15];
};

struct symNode
{
	char *name;
	//struct Type *type;
	char *type;
	union value *val ; 
	int lineNum;
	int colNum;
	int scope;
	/*struct symNode *prev;*/
	struct symNode *next;
};


struct symTable
{
	struct symNode **sentry;
	int size;
};

struct symTable symTable; 

void initSymbolTable();
void symbolInsert(struct symNode *newNode);
int hashFunction(char *name);
struct symNode *createNode(char *name , int lineNum , int colNum , int scope);
struct symNode *lookandcreateNode(char *name , int lineNum , int colNum , int scope);
struct symNode *lookNode(char *name , int scope);
void printSymTable();

void InsertTypeValue(char *name  , int scope , void *val, char *type);
void addType(char *name , int scope , char *type,char *value);



struct Quadruple
{
	char *op;
	char *arg1;
	char *arg2;
	char *res;
	struct Quadruple *next;
}*quadHead;
