#pragma once

#include <string>
#include <vector>
#include <list>
#include <forward_list>
#include <set>
using namespace std;

const char* l4_keys[] = {"+", "-", "*", "/", "%", ">", "<", "=", "and", "or", "not", "define", "lambda", "set!", "let", "if", "cond", "else", "true", "false", "cons", "car", "cdr", "pair?", "list", "null", "null?", "display", "newline", "random"};

const int SYMBOL_TABLE_MIN_SIZE = 128;

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
	SyntaxComponent *body;
	EnvironmentInfo *env;
};

struct SymbolInfo
{
	SymbolInfo():next(NULL){}
	virtual ~SymbolInfo(){};
	void Print();

	enum {BOOL, NUM, TXT, PAIR, NIL, SYS, FUN, LAMBDA, ENV};

	int type;
	string name;
	union
	{
		bool flag;
		double value;
		FunctionInfo* func;
		string* text;
		SymbolInfo* pdata[2];
		EnvironmentInfo* parent;
	};

	SymbolInfo *next;

	enum {NOT_USE, IN_TABLE, TO_USE, EMPTY};
	int useState;
};

struct EnvironmentInfo:public SymbolInfo
{
	void AddSymbol(SymbolInfo *sym);
	SymbolInfo* FindSymbol(string name);
	void Print(bool topLevel = true);
};

class Interpreter
{
public:
	enum{
		DEBUG_FUNC_CALL = 1,
		DEBUG_SYMBOL_MARK = 1 << 1,
		DEBUG_SYMBOL_CLEAR = 1 << 2,
		DEBUG_CALC_TIME = 1 << 3,
		DEBUG_ENV_SYMBOL = 1 << 4,
	};

	Interpreter():debug(0), symbolNumThreshold(SYMBOL_TABLE_MIN_SIZE){};
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
	EnvironmentInfo* NewEnvironment();
	void ReleaseSymbol(SymbolInfo *sym);
	void CheckSymbols();
	void MarkEnvironment(EnvironmentInfo *env, string prefix);
	void MarkSymbol(SymbolInfo *sym, string prefix);
	void ClearSymbols(bool force = false);
	int symbolNumThreshold;

	SyntaxComponent *lastResult;
	list<EnvironmentInfo*> currentEnvironment;
	list<EnvironmentInfo*> tmpEnvironment;
	forward_list<SymbolInfo*> symbolRecord;
	int symbolRecordSize;
	int emptySymbolSize;

	double totalTime;
	double markTime;
	double clearTime;
	int collectCount;
};