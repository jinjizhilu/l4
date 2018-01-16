#pragma once

#include <string>
#include <vector>
#include <set>
using namespace std;

struct LexComponent
{
	enum {INT, FLOAT, STR, KEY};

	int type;
	string token;
	int value;
	float fvalue;
};

class LexAnalyzer
{
public:
	void SetSourceFile(const string &file) {mFileName = file;}
	void AddKey(const string &key) {mKeys.insert(key);}
	void Run();
	void Print();

private:
	string mFileName;
	vector<LexComponent> mComponents;
	set<string> mKeys;
};