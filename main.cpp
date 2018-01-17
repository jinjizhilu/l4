#include <iostream>
#include <fstream>
#include <cstdio>
#include <windows.h>
#include "parser.h"
using namespace std;

const char* keys[] = {"+", "-", "*", "/", "define", "lambda", "if", "cons", "car", "cdr"};

void LexComponent::Print()
{
	printf("[%.3s] ", &"INT,NUM,STR,KEY"[type * 4]);

	switch(type)
	{
	case KEY:
	case STR:
		cout << token << ";" << endl; break;
	case INT:
		cout << value << ";" << endl; break;
	case NUM:
		cout << fvalue << ";" << endl; break;
	}
}

void LexAnalyzer::Parse()
{
	int line = 0, value;
	char token;
	ifstream fin(mFileName);

	while (fin.peek() != -1)
	{
		token = fin.get();

		if (token == '\n')
		{
			++line;
		}
		else if (token == '#')
		{
			while (fin.peek() != -1 && fin.peek() != '\n')
			{
				fin.get();
			}
		}
		else if (token == '(' || token ==')')
		{
			LexComponent item;
			item.type = LexComponent::KEY;
			item.token = token;
			item.line = line;
			mComponents.push_back(item);
		}
		else if ((token == '-' && fin.peek() >= '0' && fin.peek() <= '9')
			|| (token >= '0' && token <= '9'))
		{
			LexComponent item;
			bool negative = false;

			if (token == '-')
			{
				token = fin.peek();
				negative = true;
			}

			value = token - '0';
			while (fin.peek() >= '0' && fin.peek() <= '9')
			{
				token = fin.get();
				value = value * 10 + token - '0';
			}
			item.type = LexComponent::INT;
			item.value = value;
			item.line = line;

			if (fin.peek() == '.')
			{
				float fvalue = (float)value, base = 0.1f;
				fin.get();

				while (fin.peek() >= '0' && fin.peek() <= '9')
				{
					token = fin.get();
					fvalue += (token - '0') * base;
					base /= 10;
				}
				item.type = LexComponent::NUM;
				item.fvalue = fvalue;
			}

			if (negative)
			{
				item.value = -item.value;
				item.fvalue = -item.fvalue;
			}
			mComponents.push_back(item);
		}
		else if (token > 32 && token < 127 && token != ')')
		{
			LexComponent item;
			item.type = LexComponent::STR;
			item.token = token;
			item.line = line;

			while (fin.peek() > 32 && fin.peek() < 127 && fin.peek() != ')')
			{
				item.token += fin.get();
			}

			if (mKeys.find(item.token) != mKeys.end())
			{
				item.type = LexComponent::KEY;
			}
			mComponents.push_back(item);
		}
	}
}

void LexAnalyzer::Print()
{
	cout << "========================================" << endl;
	cout << "lex analyze result:" << endl;
	for (auto it=mComponents.begin(); it != mComponents.end(); ++it)
	{
		it->Print();	
	}
	cout << "========================================" << endl;
}

void SyntaxAnalyzer::MakeTree(const list<LexComponent> &componets)
{
	root = new SyntaxComponent;
	root->count = 0;
	root->children = new list<SyntaxComponent*>;
	TreeIterator cur = componets.cbegin(), end = componets.cend();

	while (cur != end)
	{
		root->children->push_back(MakeTreeRecursive(cur, end));
		root->count += 1;
	}
}

SyntaxComponent* SyntaxAnalyzer::MakeTreeRecursive(TreeIterator &cur, TreeIterator end)
{
	SyntaxComponent *node = new SyntaxComponent;
	node->count = 0;

	if (cur->token == "(")
	{
		++cur;
		node->children = new list<SyntaxComponent*>;

		while (cur != end && cur->token != ")")
		{
			node->children->push_back(MakeTreeRecursive(cur, end));
			node->count += 1;
		}

		if (cur->token != ")")
		{
			printf("unmatched parenthesis in line %d.\n", cur->line);
		}

		++cur;
	}
	else
	{
		node->data = new LexComponent;
		*(node->data) = *cur;
		++cur;
	}
	return node;
}

void SyntaxAnalyzer::ClearTree()
{
	ClearTreeTreeRecursive(root);
	delete root;
	root = NULL;
}

void SyntaxAnalyzer::ClearTreeTreeRecursive(SyntaxComponent *node)
{
	if (node->count == 0)
	{
		delete node->data;
		node->data = NULL;
	}
	else
	{
		for (auto it=node->children->begin(); it != node->children->end(); ++it)
		{
			ClearTreeTreeRecursive(*it);
		}
		delete node->children;
		node->children = NULL;
	}
}

void SyntaxAnalyzer::PrintTree()
{
	cout << "========================================" << endl;
	cout << "syntax tree:" << endl;
	if (root != NULL)
	{
		PrintTreeRecursive(root, "");
	}
	cout << "========================================" << endl;
}

void SyntaxAnalyzer::PrintTreeRecursive(SyntaxComponent *node, string prefix)
{
	if (node->count == 0)
	{
		cout << prefix;
		node->data->Print();
	}
	else
	{
		cout << prefix << "(" << endl;
		for (auto it=node->children->begin(); it != node->children->end(); ++it)
		{
			PrintTreeRecursive(*it, prefix + "  ");
		}
		cout << prefix << ")" << endl;
	}
}

int main(int argc, char **argv)
{
	if (argc < 2)
	{
		cout << "no source file found!" << endl;
		return 1;
	}

	LexAnalyzer lex;
	string src_file(argv[1]);
	
	for (size_t i=0; i<sizeof(keys)/4; ++i)
	{
		lex.AddKey(keys[i]);
	}

	lex.SetSourceFile(src_file);
	lex.Parse();
	lex.Print();

	SyntaxAnalyzer syntax;
	syntax.MakeTree(lex.GetComponents());
	syntax.PrintTree();

	syntax.ClearTree();

	system("pause");
	return 0;
}