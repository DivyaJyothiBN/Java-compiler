#include "symboltb.h"

void initSymbolTable()
{
	symTable.sentry = (struct symNode **)malloc(sizeof(struct symNode *) * MAX_ENTRIES);
	symTable.size = 0; 
	quadHead = NULL;	
}

void symbolInsert(struct symNode *newNode)
{
	int loc = hashFunction(newNode->name);	
	//fprintf(stdout , "location for %s = %d \n",newNode->name , loc);	
	if(symTable.sentry[loc] == NULL)
	{
//		fprintf(stdout , "%s location  %d was null \n",newNode->name , loc);	
		symTable.sentry[loc] = newNode;
		symTable.size += 1;
		//if(loc != 40)printf("Location 40 contains = %s\n",symTable.sentry[29]->name);
	}
	else
	{
		struct symNode *tmp = symTable.sentry[loc]; 
		while(tmp->next!=NULL)
		{
			tmp = tmp->next ;
		}
		tmp->next = newNode;
		//printSymTable();
		//printf("tmp->val = %s , tmp->next->val = %s\n",tmp->name,(tmp->next)->name);
		
		symTable.size += 1;
//		printf("size = %d\n",symTable.size );
	}	
}

int hashFunction(char *name)
{
	int ret = 0;
	int length = strlen(name);
	for(int i=0 ; i<length ; ++i)
	{
		ret += name[i] - '0';
		//printf("Converted value = %d \n",ret);
	}
	return ret % MAX_ENTRIES;
}

struct symNode *createNode(char *name , int lineNum , int colNum , int scope)
{
	struct symNode *tmp = (struct symNode *)malloc(sizeof(struct symNode));
	tmp->name = (char *)malloc(sizeof(char) * (strlen(name) + 1) );
	strcpy(tmp->name , name);
	//printf("%s creating \n",tmp->name);
	tmp->type = NULL;
	tmp->val = NULL;
	tmp->lineNum = lineNum;
	tmp->colNum = colNum;
	tmp->scope = scope;
	tmp->next = NULL;
//	printf("tmp->next = %d",tmp->next);
	return tmp;
}


struct symNode *lookandcreateNode(char *name , int lineNum , int colNum , int scope)
{
	struct symNode *tmp = lookNode(name,scope);
	if(tmp == NULL)
	{
		//printf("Creating node for %s\n",name);
		return createNode(name , lineNum , colNum , scope);
	}
	else
	{
	//	printf("tmp->next = %d",tmp->next);
		return NULL;
	}
}

struct symNode *lookNode(char *name , int scope)
{
	int size = symTable.size;
	int loc = hashFunction(name);			
	if(symTable.sentry[loc] == NULL)
		return NULL;
	else	
	{
		struct symNode *tmp = symTable.sentry[loc];
		while(tmp!=NULL)
		{
			if((strcmp(tmp->name,name)==0) && tmp->scope == scope)
				return tmp;
			tmp = tmp->next;
		}
		return NULL;
	}
	
}


void addType(char *name , int scope , char *type , char *value)
{
//	printf("Add type for %s\n",value);
	struct symNode *node = lookNode(name , scope);
	//printf("node = %s \n",node->name);
	node->type = (char *)calloc(sizeof(char),strlen(type));
	sprintf(node->type,"%s",type);
	if(value != NULL)
	{
		node->val = (union value *)calloc(sizeof(union value),1);
		if(strcmp(type,"INT")==0 || strcmp(type,"SHORT")==0)
			node->val->intval = atoi(value);
		else if(strcmp(type,"CHAR")==0)	
			node->val->strval = strdup(value);
		else if(strcmp(type,"STRING")==0)	
			node->val->strval = strdup(value);	
		else if(strcmp(type,"FLOAT")==0)	
			node->val->floatval = atoi(value);	
		else if(strcmp(type,"DOUBLE")==0)	
			node->val->floatval = atoi(value);	
		else if(strcmp(type,"BYTE")==0)	
			node->val->intval = atoi(value);
		else
		{
			node->val->strval = strdup(value);
		}	
	}
}

void printSymTable()
{
//	int size = symTable.size;
//	printf("size = %d \n",size);
	printf("Symbol Table consists\n");
	printf("----------------------------------------------------------------------------------------------------------------------------------------\n");
	printf("| \t NAME \t\t|\t  LINENUM \t|\t  COLUMNNUM \t|\t  SCOPE \t|\t TYPE \t\t|\t VALUE |\n");
	printf("----------------------------------------------------------------------------------------------------------------------------------------\n");

	for(int i=0; i<MAX_ENTRIES ; ++i)
	{
		//printf("i = %d\n",i);
		struct symNode *tmp = symTable.sentry[i];
		while(tmp!=NULL)
		{
			char *type = tmp->type;
			char *val = NULL;
			if(tmp->val != NULL)
			{
				if(strcmp(type,"INT")==0 || strcmp(type,"SHORT")==0)
					val = strdup((char)tmp->val->intval); 
				else if(strcmp(type,"CHAR")==0)	
					val = strdup(tmp->val->strval);
				else if(strcmp(type,"STRING")==0)	
					val = strdup(tmp->val->strval);
				else if(strcmp(type,"FLOAT")==0)	
					val = strdup((char)tmp->val->floatval);	
				else if(strcmp(type,"DOUBLE")==0)	
					val = strdup((char)tmp->val->floatval);	
				else if(strcmp(type,"BYTE")==0)	
					val = strdup((char)tmp->val->intval);
				else
				{
					val = strdup(tmp->val->strval);
				}	
			}
			printf("|%15s \t|\t\t %3d \t|\t\t %3d \t|\t\t %3d \t|\t %10s \t|\t %5s |\n", tmp->name , tmp->lineNum , tmp->colNum , tmp->scope ,tmp->type , val);
			tmp = tmp->next;
		}
	}
	printf("----------------------------------------------------------------------------------------------------------------------------------------\n");

}
