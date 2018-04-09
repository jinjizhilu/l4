#include <iostream>
#include <fstream>
#include <cstdio>
#include <cassert>
#include <windows.h>
#include "parser.h"
using namespace std;

void LexComponent::Print()
{
	printf("[%.3s] ", &"NUM,STR,KEY,TXT"[type * 4]);

	switch(type)
	{
	case KEY:
	case TXT:
	case STR:
		cout << token << ";" << endl; break;
	case NUM:
		cout << value << ";" << endl; break;
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
			item.type = LexComponent::NUM;
			item.value = value;
			item.line = line;

			if (fin.peek() == '.')
			{
				double fvalue = value, base = 0.1;
				fin.get();

				while (fin.peek() >= '0' && fin.peek() <= '9')
				{
					token = fin.get();
					fvalue += (token - '0') * base;
					base /= 10;
				}
				item.value = fvalue;
			}

			if (negative)
			{
				item.value = -item.value;
			}
			mComponents.push_back(item);
		}
		else if (token == '"')
		{
			LexComponent item;
			item.type = LexComponent::TXT;
			item.token = token;
			item.line = line;

			while (fin.peek() != -1 && fin.peek() != '"')
			{
				item.token += fin.get();
			}
			item.token += fin.get();
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

		if (cur == end)
		{
			cout << "expected end of file" << endl;
			return node;
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

SymbolInfo* SymbolInfo::Copy()
{
	SymbolInfo *result = new SymbolInfo;
	result->type = type;
	result->name = name;
	result->value = value;
	result->next = next;

	return result;
}

void EnvironmentInfo::AddSymbol(SymbolInfo *sym)
{
	sym->next = head;
	head = sym;
}

SymbolInfo* EnvironmentInfo::FindSymbol(string name)
{
	SymbolInfo *p = head;
	while (p != NULL)
	{
		if (p->name == name)
		{
			break;
		}
		p = p->next;
	}
	if (p == NULL)
	{
		cout << "unrecognized symbol: " << name << endl;
	}
	return p;
}

void EnvironmentInfo::Print()
{
	cout << "environment " << name << endl;
	cout << "{" << endl;

	SymbolInfo *p = head;
	while (p != NULL)
	{
		cout << "\t" << p->name;
		switch(p->type)
		{
		case SymbolInfo::BOOL:
			cout << " = " << (p->flag ? "true" : "false") << endl; break;
		case SymbolInfo::NUM:
			cout << " = " << p->value << endl; break;
		case SymbolInfo::FUN:
			cout << " = FUNC" << endl; break;
		case SymbolInfo::PAIR:
			cout << " = PAIR" << endl; break;
		}
		p = p->next;
	}
	cout << "}" << endl;
}

void Interpreter::Run(SyntaxComponent *tree)
{
	assert(tree->count > 0);

	currentEnvironment.clear();
	currentEnvironment.push_front(new EnvironmentInfo);
	currentEnvironment.front()->name = "global";
	lastResult = NULL;

	for (auto it=tree->children->begin(); it != tree->children->end(); ++it)
	{
		SymbolInfo *result = Evaluate(*it);

		if (result != NULL && result->type == SymbolInfo::NUM)
		{
			cout << result->value << endl;
		}
	}
}

SymbolInfo* Interpreter::Evaluate(SyntaxComponent *node)
{
	bool flag = true;
	SymbolInfo *result = NULL;
	int envCount = 0;

	while (flag)
	{
		flag = false;
		lastResult = NULL;

		if (node->count == 0)
		{
			if (node->data->type == LexComponent::NUM)
			{
				SymbolInfo *sym = new SymbolInfo;
				sym->type = SymbolInfo::NUM;
				sym->value = node->data->value;
				result = sym;
			}
			else if (node->data->type == LexComponent::TXT)
			{
				SymbolInfo *sym = new SymbolInfo;
				sym->type = SymbolInfo::TXT;
				sym->text = new string;
				*sym->text = node->data->token;
				result = sym;
			}
			else if (node->data->type == LexComponent::STR)
			{
				SymbolInfo *sym = currentEnvironment.front()->FindSymbol(node->data->token);
				assert(sym != NULL);
				result = sym;
			}
			else
			{
				cout << "error in statement at line " << node->data->line << "!" << endl;
			}
		}
		else
		{
			assert(node->count > 0);
			SyntaxComponent *first = node->children->front();

			if (first->count == 0) // common call
			{
				LexComponent *op = first->data;
				if (op->type == LexComponent::KEY)
				{
					if (op->token == "define" || op->token == "lambda")
					{
						result = Define(node);
					}
					else if (op->token == "set!")
					{
						result = Assign(node);
					}
					else if (op->token == "let")
					{
						result = Let(node);
					}
					else if (op->token == "if" || op->token == "cond")
					{
						result = Condition(node);

						if (lastResult != NULL)
						{
							node = lastResult;
							flag = true;
						}
					}
					else if (op->token == "+" || op->token == "-" || op->token == "*" || op->token == "/" || op->token == "%")
					{
						result = Arithmetic(node);
					}
					else if (op->token == ">" || op->token == "<" || op->token == "=" || op->token == "and" || op->token == "or" || op->token == "not")
					{
						result = Logic(node);
					}
					else if (op->token == "cons" || op->token == "car" || op->token == "cdr")
					{
						result = Pair(node);
					}
					else if (op->token == "display" || op->token == "newline")
					{
						result = SysFunc(node);
					}
					else
					{
						cout << "unrecognized key: " << node->data->token << "at line " << node->data->line << "!" << endl;
					}
				}
				else
				{
					assert(op->type == LexComponent::STR);
					result = Call(node);

					if (lastResult != NULL)
					{
						node = lastResult;
						flag = true;
						++envCount;
					}
				}
			}
			else // lambda call
			{
				assert(first->count > 0);
				result = Call(node);

				if (lastResult != NULL)
				{
					node = lastResult;
					flag = true;
					++envCount;
				}
			}
		}
	}
	for (int i=0; i<envCount; ++i)
	{
		delete currentEnvironment.front();
		currentEnvironment.pop_front();
	}

	return result;
}

SymbolInfo* Interpreter::Define(SyntaxComponent *node)
{
	// (define x 1)
	//
	// (define (f x)
	//   (define y 1)
	//   (+ x 2))
	//
	// (lambda (x) 1)

	assert(node->count >= 3);

	auto it = node->children->begin();
	string op = (*it)->data->token;
	SyntaxComponent *variable = *++it;

	if (variable->count == 0) // variable define
	{
		assert(node->count == 3);
		SymbolInfo *sym = Evaluate(*++it);
		assert(sym != NULL);
		sym = sym->Copy();

		assert(variable->data->type == LexComponent::STR);
		sym->name = variable->data->token;

		currentEnvironment.front()->AddSymbol(sym);
	}
	else // function define
	{
		SymbolInfo *sym = new SymbolInfo;
		sym->type = SymbolInfo::FUN;
		sym->func = new FunctionInfo;
		sym->name = "lambda";
		
		if (op == "define")
		{
			SyntaxComponent *name = variable->children->front();
			assert(name->data->type == LexComponent::STR);
			sym->name = name->data->token;
		}

		EnvironmentInfo *env = new EnvironmentInfo;
		env->head = currentEnvironment.front()->head;
		env->name = sym->name;
		sym->func->env = env;
		sym->func->body = node;

		if (op == "define")
		{
			currentEnvironment.front()->AddSymbol(sym);
			sym->func->env->AddSymbol(sym);
		}
		else if (op == "lambda")
		{
			return sym;
		}
	}
	return NULL;
}

SymbolInfo* Interpreter::Assign(SyntaxComponent *node)
{
	assert(node->count == 3);

	auto it = node->children->begin();
	SyntaxComponent *variable = *++it;
	assert(variable->count == 0 && variable->data->type == LexComponent::STR);

	SymbolInfo *sym = Evaluate(variable);
	SymbolInfo *result = Evaluate(*++it);
	assert(sym != NULL && result != NULL);

	sym->type = result->type;
	sym->value = result->value;

	return NULL;
}

SymbolInfo* Interpreter::Call(SyntaxComponent *node)
{
	// (define (f x)
	//   (define y 1)
	//   (+ x 2))
	//
	// (lambda (x) 1)

	assert(node->count > 0);

	SymbolInfo *sym, *tmp, *result = NULL;
	auto it = node->children->begin();
	SyntaxComponent *first = *it;

	if (first->count == 0) // direct call by name
	{
		string funcName = first->data->token;
		sym = currentEnvironment.front()->FindSymbol(funcName);
		assert(sym != NULL && sym->type == SymbolInfo::FUN);
	}
	else // call by lambda or function result
	{
		assert(first->count > 0);
		sym = Evaluate(first);
		assert(sym != NULL && sym->type == SymbolInfo::FUN);
	}

	if (debug)
	{
		cout << "calling " << sym->name << endl;
	}

	EnvironmentInfo *env = new EnvironmentInfo;
	env->head = sym->func->env->head;
	env->name = sym->name;

	SyntaxComponent *function = sym->func->body;
	auto fIt = function->children->begin();
	
	// handle paremeters
	auto pIt = (*++fIt)->children->begin();
	if (sym->name == (*pIt)->data->token)
	{
		++pIt; // skip function name
	}

	while (++it != node->children->end() && pIt != (*fIt)->children->end())
	{
		tmp = Evaluate(*it);
		assert(tmp != NULL);
		tmp = tmp->Copy();

		LexComponent *parameter = (*pIt)->data;
		assert((*pIt)->count == 0 && parameter->type == LexComponent::STR);
		tmp->name = parameter->token;
		++pIt;
		
		env->AddSymbol(tmp);
	}
	assert(it == node->children->end() && pIt == (*fIt)->children->end());

	// handle function body
	currentEnvironment.push_front(env);

	if (debug)
	{
		currentEnvironment.front()->Print();
	}

	assert(++fIt != function->children->end());

	while (*fIt != function->children->back())
	{
		result = Evaluate(*fIt);
		++fIt;
	}
	lastResult = *fIt;

	return NULL;
}

SymbolInfo* Interpreter::Arithmetic(SyntaxComponent *node)
{
	assert (node->count >= 3);
	auto it = node->children->begin();
	string op = (*it)->data->token;

	SymbolInfo *tmp, *result = new SymbolInfo;
	result->type = SymbolInfo::NUM;

	tmp = Evaluate(*++it);
	assert(tmp != NULL && tmp->type == SymbolInfo::NUM);
	result->value = tmp->value;

	while (++it != node->children->end())
	{
		tmp = Evaluate(*it);
		assert(tmp != NULL && tmp->type == SymbolInfo::NUM);

		if (op == "+")
		{
			result->value += tmp->value;
		}
		else if (op == "-")
		{
			result->value -= tmp->value;
		}
		else if (op == "*")
		{
			result->value *= tmp->value;
		}
		else if (op == "/")
		{
			result->value /= tmp->value;
		}
		else if (op == "%")
		{
			assert(node->count == 3);
			result->value = int(result->value) % int(tmp->value);
		}
	}
	return result;
}

SymbolInfo* Interpreter::Logic(SyntaxComponent *node)
{
	auto it = node->children->begin();
	string op = (*it)->data->token;

	SymbolInfo *result = new SymbolInfo;
	result->type = SymbolInfo::BOOL;

	if (op == ">" || op == "<" || op == "=")
	{
		assert(node->count == 3);

		SymbolInfo *first = Evaluate(*++it);
		SymbolInfo *second = Evaluate(*++it);
		assert(first != NULL && first->type == SymbolInfo::NUM);
		assert(second != NULL && second->type == SymbolInfo::NUM);

		if (op == ">")
		{
			result->flag = (first->value > second->value);
		}
		else if (op == "<")
		{
			result->flag =  (first->value < second->value);
		}
		else if (op == "=")
		{
			result->flag =  (first->value == second->value);
		}
	}
	else if (op == "and" || op == "or")
	{
		assert(node->count >= 3);

		SymbolInfo *tmp = Evaluate(*++it);
		assert(tmp != NULL && tmp->type == SymbolInfo::BOOL);
		result->flag = tmp->flag;

		while (++it != node->children->end())
		{
			if ((result->flag == true && op == "or") || (result->flag == false && op == "and"))
			{
				break;
			}

			tmp = Evaluate(*it);
			assert(tmp != NULL && tmp->type == SymbolInfo::BOOL);

			if (op == "and")
			{
				result->flag = result->flag && tmp->flag;
			}
			else if (op == "or")
			{
				result->flag = result->flag || tmp->flag;
			}
		}

	}
	else if (op == "not")
	{
		assert(node->count == 2);

		SymbolInfo *first = Evaluate(*++it);
		assert(first != NULL && first->type == SymbolInfo::BOOL);

		result->flag = !first;
	}
	return result;
}

SymbolInfo* Interpreter::Condition(SyntaxComponent *node)
{
	// (if (= a 1)
	//    0 
	//    1)
	//
	// (cond ((= a 1) 0)
	//       ((> a 1) 1)
	//       ...
	//       (else -1))

	SymbolInfo *result = NULL;
	auto it = node->children->begin();
	string op = (*it)->data->token;

	if (op == "if")
	{
		assert(node->count == 4);
		SymbolInfo *first = Evaluate(*++it);
		assert(first != NULL && first->type == SymbolInfo::BOOL);

		++it;
		if (first->flag)
		{
			lastResult = *it;
		}
		else
		{
			lastResult = *++it;
		}
	}
	else if (op == "cond")
	{
		assert(node->count >= 2);
		SymbolInfo *flag;

		while (++it != node->children->end())
		{
			SyntaxComponent *line = *it;
			assert(line->count == 2);

			if (line->children->front()->count > 0) // normal condition
			{
				flag = Evaluate(line->children->front());
				assert(flag != NULL && flag->type == SymbolInfo::BOOL);

				if (flag->flag)
				{
					lastResult = line->children->back();
					break;
				}
			}
			else // else condition
			{
				SyntaxComponent *first = line->children->front();
				assert(first->count == 0 && first->data->token == "else" && ++it == node->children->end());
				lastResult = line->children->back();
				break;
			}
		}
	}
	return result;
}

SymbolInfo* Interpreter::Let(SyntaxComponent *node)
{
	// (let ((v1 1)
	//       (v2 2))
	//      (+ v1 v2))

	SymbolInfo *result = NULL;
	auto it = node->children->begin();
	assert((*it)->data->token == "let");

	EnvironmentInfo *env = new EnvironmentInfo;
	env->head = currentEnvironment.front()->head;
	env->name = currentEnvironment.front()->name + ".let";
	currentEnvironment.push_front(env);

	++it;
	assert((*it)->count > 0);

	auto it2 = (*it)->children->begin();
	while (it2 != (*it)->children->end())
	{
		SyntaxComponent *line = *it2++;
		assert(line->count == 2);
		
		SyntaxComponent *variable = line->children->front();
		assert(variable->count == 0 && variable->data->type == LexComponent::STR);

		SymbolInfo *sym = Evaluate(line->children->back());
		sym->name = variable->data->token;

		currentEnvironment.front()->AddSymbol(sym);
	}
	
	while (++it != node->children->end())
	{
		result = Evaluate(*it);
	}

	delete currentEnvironment.front();
	currentEnvironment.pop_front();

	return result;
}

SymbolInfo* Interpreter::Pair(SyntaxComponent *node)
{
	auto it = node->children->begin();
	string op = (*it)->data->token;
	SymbolInfo *result = NULL;

	if (op == "cons")
	{
		assert(node->count == 3);
		SymbolInfo *first = Evaluate(*++it);
		SymbolInfo *second = Evaluate(*++it);

		result = new SymbolInfo;
		result->type = SymbolInfo::PAIR;
		result->pdata[0] = first;
		result->pdata[1] = second;
	}
	else if (op == "car")
	{
		assert(node->count == 2);
		SymbolInfo *sym = Evaluate(*++it);
		assert(sym != NULL && sym->type == SymbolInfo::PAIR);
		result = sym->pdata[0];
	}
	else if (op == "cdr")
	{
		assert(node->count == 2);
		SymbolInfo *sym = Evaluate(*++it);
		assert(sym != NULL && sym->type == SymbolInfo::PAIR);
		result = sym->pdata[1];
	}
	return result;
}

SymbolInfo* Interpreter::SysFunc(SyntaxComponent *node)
{
	auto it = node->children->begin();
	string op = (*it)->data->token;
	SymbolInfo *result = NULL;

	if (op == "display")
	{
		assert(node->count > 1);
		while (++it != node->children->end())
		{
			SymbolInfo *sym = Evaluate(*it);
			assert(sym != NULL);

			if (sym->type == SymbolInfo::NUM)
			{
				cout << sym->value;
			}
			else if (sym->type == SymbolInfo::TXT)
			{
				cout << sym->text->substr(1, sym->text->size() - 2);
			}
			else
			{
				cout << "wrong parameter for display" << endl;
			}
		}
	}
	else if (op == "newline")
	{
		assert(node->count == 1);
		cout << endl;
	}

	return result;
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
	src_file = "test/fix-point.rkt";
	
	for (size_t i=0; i<sizeof(l4_keys)/4; ++i)
	{
		lex.AddKey(l4_keys[i]);
	}

	lex.SetSourceFile(src_file);
	lex.Parse();
	//lex.Print();

	SyntaxAnalyzer syntax;
	syntax.MakeTree(lex.GetComponents());
	//syntax.PrintTree();

	Interpreter vm;
	//vm.debug = true;
	vm.Run(syntax.GetTree());

	syntax.ClearTree();

	system("pause");
	return 0;
}