#pragma once

#include <string>
#include <vector>
#include <list>
#include <set>
using namespace std;

const char* l4_keys[] = {"+", "-", "*", "/", "%", ">", "<", "=", "and", "or", "not", "define", "lambda", "set!", "let", "if", "cond", "else", "true", "false", "cons", "car", "cdr", "pair?", "list", "null", "null?", "display", "newline", "random"};

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
	void Print();

	enum {BOOL, NUM, TXT, PAIR, NIL, SYS, FUN, LAMBDA};

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

	enum {NOT_USE, IN_TABLE, TO_USE};
	int useState;
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
	enum{
		DEBUG_FUNC_CALL = 1,
		DEBUG_SYMBOL_MARK = 1 << 1,
		DEBUG_SYMBOL_CLEAR = 1 << 2,
	};

	Interpreter():debug(0), maxSymbolNum(128){};
	void Run(SyntaxComponent *tree);
	int debug;

private:
	SymbolInfo* Evaluate(SyntaxComponent *node);
	SymbolInfo* Define(SyntaxComponent *node);
	SymbolInfo* Let(SyntaxComponent *node);
	SymbolInfo* Call(SyntaxComponent *node);
	SymbolInfo* Arithmetic(SyntaxComponent *node);
	SymbolInfo* Logic(SyntaxComponent *node);
	SymbolInfo* Condition(SyntaxComponent *node);
	SymbolInfo* Assign(SyntaxComponent *node);
	SymbolInfo* Pair(SyntaxComponent *node);
	SymbolInfo* List(SyntaxComponent *node);
	SymbolInfo* MiscFunc(SyntaxComponent *node);

	SymbolInfo* CopySymbol(SymbolInfo *sym);
	SymbolInfo* NewSymbol();
	void ReleaseSymbol(SymbolInfo *sym);
	void CheckSymbols();
	void MarkEnvironment(EnvironmentInfo *env, string prefix);
	void MarkSymbol(SymbolInfo *sym, string prefix);
	void ClearSymbols(bool force = false);
	unsigned int maxSymbolNum;

	SyntaxComponent *lastResult;
	list<EnvironmentInfo*> currentEnvironment;
	list<EnvironmentInfo*> tmpEnvironment;
	list<SymbolInfo*> symbolRecord;
};