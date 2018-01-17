#pragma once

#include <string>
#include <vector>
#include <list>
#include <set>
using namespace std;

struct LexComponent
{
	enum {INT, NUM, STR, KEY};

	int type;
	string token;
	int value;
	float fvalue;
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
private:
	typedef list<LexComponent>::const_iterator TreeIterator;
	SyntaxComponent* MakeTreeRecursive(TreeIterator &cur, TreeIterator end);
	void ClearTreeTreeRecursive(SyntaxComponent *node);
	void PrintTreeRecursive(SyntaxComponent *node, string prefix);
	SyntaxComponent *root;
};