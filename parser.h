#pragma once

#include <string>
#include <vector>
#include <list>
#include <set>
using namespace std;

const char* l4_keys[] = {"+", "-", "*", "/", ">", "<", "=", "and", "or", "not", "define", "lambda", "let", "if", "cond", "else", "cons", "car", "cdr", "display"};

struct LexComponent
{
	enum {NUM, STR, KEY, TXT};

	int type;
	string token;
	double value;
	int line;

	void Print();
};

class LexAnalyzer
{
public:
	void SetSourceFile(const string &file) {mFileName = file;}
	void AddKey(const string &key) {mKeys.insert(key);}
	void Parse();
	void Print();
	list<LexComponent>& GetComponents() {return mComponents;}

private:
	string mFileName;
	list<LexComponent> mComponents;
	set<string> mKeys;
};

struct SyntaxComponent
{
	int count;
	union
	{
		LexComponent *data;
		list<SyntaxComponent*> *children;
	};
};

class SyntaxAnalyzer
{
public:
	void MakeTree(const list<LexComponent> &components);
	void ClearTree();
	void PrintTree();
	SyntaxComponent* GetTree() {return root;}
private:
	typedef list<LexComponent>::const_iterator TreeIterator;
	SyntaxComponent* MakeTreeRecursive(TreeIterator &cur, TreeIterator end);
	void ClearTreeTreeRecursive(SyntaxComponent *node);
	void PrintTreeRecursive(SyntaxComponent *node, string prefix);
	SyntaxComponent *root;
};

struct EnvironmentInfo;

struct FunctionInfo
{
	SyntaxComponent *parameter;
	SyntaxComponent *body;
	EnvironmentInfo *env;
};

struct SymbolInfo
{
	SymbolInfo():next(NULL){}
	SymbolInfo* Copy();

	enum {BOOL, NUM, TXT, PAIR, SYS, FUN};

	int type;
	string name;
	union
	{
		bool flag;
		double value;
		FunctionInfo* func;
		string* text;
		SymbolInfo* pdata[2];
	};

	SymbolInfo *next;
};

struct EnvironmentInfo
{
	EnvironmentInfo():head(NULL){}

	SymbolInfo *head;
	string name;

	void AddSymbol(SymbolInfo *sym);
	SymbolInfo* FindSymbol(string name);
	void Print();
};

class Interpreter
{
public:
	Interpreter():debug(false){};
	void Run(SyntaxComponent *tree);
	bool debug;

private:
	SymbolInfo* Evaluate(SyntaxComponent *node);
	SymbolInfo* Define(SyntaxComponent *node);
	SymbolInfo* Call(SyntaxComponent *node);
	SymbolInfo* Arithmetic(SyntaxComponent *node);
	SymbolInfo* Logic(SyntaxComponent *node);
	SymbolInfo* Condition(SyntaxComponent *node);
	SymbolInfo* Let(SyntaxComponent *node);
	SymbolInfo* Pair(SyntaxComponent *node);
	SymbolInfo* SysFunc(SyntaxComponent *node);

	list<EnvironmentInfo*> currentEnvironment;
};