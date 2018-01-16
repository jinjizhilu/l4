#include <iostream>
#include <fstream>
#include <cstdio>
#include <windows.h>
#include "parser.h"
using namespace std;

const char* keys[] = {"+", "-", "*", "/", "define", "lambda", "if", "cons", "car", "cdr"};

void LexAnalyzer::Run()
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
				item.type = LexComponent::FLOAT;
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
	for (auto it=mComponents.begin(); it != mComponents.end(); ++it)
	{
		printf("%.3s ", &"INT,FLT,STR,KEY"[it->type * 4]);

		switch(it->type)
		{
		case LexComponent::KEY:
		case LexComponent::STR:
			cout << it->token << ";" << endl; break;
		case LexComponent::INT:
			cout << it->value << ";" << endl; break;
		case LexComponent::FLOAT:
			cout << it->fvalue << ";" << endl; break;
		}
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
	lex.Run();
	lex.Print();

	system("pause");
	return 0;
}