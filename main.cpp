#include <iostream>
#include <fstream>
#include <cstdio>
#include <ctime>
#include <cstdlib>
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
				token = fin.get();
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

void SymbolInfo::Print()
{
	cout << name;

	switch(type)
	{
	case SymbolInfo::BOOL:
		cout << " = " << (flag ? "true" : "false") << endl; break;
	case SymbolInfo::NUM:
		cout << " = " << value << endl; break;
	case SymbolInfo::TXT:
		cout << " = " << *(text) << endl; break;
	case SymbolInfo::FUN:
		cout << " = FUNC" << endl; break;
	case SymbolInfo::LAMBDA:
		cout << " = LAMBDA" << endl; break;
	case SymbolInfo::PAIR:
		cout << " = PAIR" << endl; break;
	case SymbolInfo::NIL:
		cout << " = NULL" << endl; break;
	default:
		assert("unrecognized symbol type" == 0);
	}
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
		cout << "\t";
		p->Print();
		p = p->next;
	}
	cout << "}" << endl;
}

void Interpreter::Run(SyntaxComponent *tree)
{
	assert(tree->count > 0);

	totalTime = markTime = clearTime = 0;
	clock_t startTime = clock();

	srand(unsigned int(time(NULL)));
	currentEnvironment.push_front(new EnvironmentInfo);
	currentEnvironment.front()->name = "global";
	lastResult = NULL;
	symbolNumThreshold = SYMBOL_TABLE_MIN_SIZE;
	symbolRecordSize = 0;
	emptySymbolSize = 0;
	collectCount = 0;

	for (auto it=tree->children->begin(); it != tree->children->end(); ++it)
	{
		SymbolInfo *result = Evaluate(*it);

		if (result != NULL)
		{
			if (result->type == SymbolInfo::NUM)
			{
				cout << result->value << endl;
			}
			else if (result->type == SymbolInfo::BOOL)
			{
				cout << (result->flag ? "true" : "false") << endl;
			}
			else if (result->type == SymbolInfo::PAIR)
			{
				cout << "(";

				SymbolInfo **p = &result;
				while ((*p)->type != SymbolInfo::NIL)
				{
					assert((*p)->pdata[0]->type == SymbolInfo::NUM);
					cout << (*p)->pdata[0]->value << " ";
					p = &(*p)->pdata[1];
				}
				cout << ")" << endl;
			}
		}
		ReleaseSymbol(result);
	}

	delete currentEnvironment.front();
	currentEnvironment.pop_front();

	ClearSymbols(true);
	totalTime = double(clock() - startTime) / CLOCKS_PER_SEC;

	if (debug & DEBUG_CALC_TIME)
	{
		printf("total time: %.4lf\n", totalTime);
		printf("mark time: %.4lf\n", markTime);
		printf("clear time: %.4lf\n", clearTime);
		printf("collect count: %d\n", collectCount);
	}
}

SymbolInfo* Interpreter::Evaluate(SyntaxComponent *node)
{
	bool flag = true;
	SymbolInfo *result = NULL;
	int envCount = 0;
	list<EnvironmentInfo*> envToDelete;

	while (flag)
	{
		if (symbolRecordSize - emptySymbolSize > symbolNumThreshold) // garbage collection
		{
			CheckSymbols();
			ClearSymbols();
		}

		flag = false;
		lastResult = NULL;

		if (node->count == 0)
		{
			if (node->data->type == LexComponent::NUM)
			{
				SymbolInfo *sym = NewSymbol();
				sym->type = SymbolInfo::NUM;
				sym->value = node->data->value;
				result = sym;
			}
			else if (node->data->type == LexComponent::TXT)
			{
				SymbolInfo *sym = NewSymbol();
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
			else if (node->data->type == LexComponent::KEY)
			{
				if (node->data->token == "true" || node->data->token == "false")
				{
					SymbolInfo *sym = NewSymbol();
					sym->type = SymbolInfo::BOOL;
					sym->flag = (node->data->token == "true");
					result = sym;
				}
				else if (node->data->token == "null")
				{
					SymbolInfo *sym = NewSymbol();
					sym->type = SymbolInfo::NIL;
					result = sym;
				}
				else
				{

					//cout << "illeagal use of key word \"" << node->data->token << "\"" << endl;
				}
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
					else if (op->token == "cons" || op->token == "car" || op->token == "cdr" || op->token == "pair?")
					{
						result = Pair(node);
					}
					else if (op->token == "list" || op->token == "null?")
					{
						result = List(node);
					}
					else if (op->token == "display" || op->token == "newline" || op->token == "random")
					{
						result = MiscFunc(node);
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
						envToDelete.push_front(currentEnvironment.front());
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
					envToDelete.push_front(currentEnvironment.front());
				}
			}
		}
		for (auto eIt=envToDelete.begin(); eIt != envToDelete.end(); ++eIt)
		{
			if ((*eIt) != currentEnvironment.front())
			{
				--envCount;
				currentEnvironment.remove(*eIt);
				delete (*eIt);
				(*eIt) = NULL;
			}
		}
		envToDelete.remove(NULL);
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
		ReleaseSymbol(sym);
		sym = CopySymbol(sym);

		assert(variable->data->type == LexComponent::STR);
		sym->name = variable->data->token;

		currentEnvironment.front()->AddSymbol(sym);
		ReleaseSymbol(sym);
	}
	else // function define
	{
		SymbolInfo *sym = NewSymbol();
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
			ReleaseSymbol(sym);
		}
		else if (op == "lambda")
		{
			sym->type = SymbolInfo::LAMBDA;
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

	ReleaseSymbol(sym);
	ReleaseSymbol(result);

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
		assert(sym != NULL && (sym->type == SymbolInfo::FUN || sym->type == SymbolInfo::LAMBDA));
	}
	else // call by lambda or function result
	{
		assert(first->count > 0);
		sym = Evaluate(first);
		assert(sym != NULL && (sym->type == SymbolInfo::FUN || sym->type == SymbolInfo::LAMBDA));
	}

	if (debug & Interpreter::DEBUG_FUNC_CALL)
	{
		cout << "calling " << sym->name << endl;
	}

	EnvironmentInfo *env = new EnvironmentInfo;
	env->head = sym->func->env->head;
	env->name = sym->name;
	tmpEnvironment.push_front(env); // for garbage collection

	SyntaxComponent *function = sym->func->body;
	auto fIt = function->children->begin();
	
	// handle paremeters
	auto pIt = (*++fIt)->children->begin();
	if (sym->type == SymbolInfo::FUN)
	{
		++pIt; // skip function name
	}
	ReleaseSymbol(sym);

	while (++it != node->children->end() && pIt != (*fIt)->children->end())
	{
		tmp = Evaluate(*it);
		assert(tmp != NULL);
		ReleaseSymbol(tmp);
		tmp = CopySymbol(tmp);

		LexComponent *parameter = (*pIt)->data;
		assert((*pIt)->count == 0 && parameter->type == LexComponent::STR);
		tmp->name = parameter->token;
		++pIt;
		
		env->AddSymbol(tmp);
		ReleaseSymbol(tmp);
	}
	assert(it == node->children->end() && pIt == (*fIt)->children->end());

	// handle function body
	assert(env == tmpEnvironment.front());
	tmpEnvironment.pop_front();
	currentEnvironment.push_front(env);

	if (debug & Interpreter::DEBUG_FUNC_CALL)
	{
		currentEnvironment.front()->Print();
	}

	++fIt;
	assert(fIt != function->children->end());

	while (*fIt != function->children->back())
	{
		result = Evaluate(*fIt);
		ReleaseSymbol(result);
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

	SymbolInfo *tmp, *result = NewSymbol();
	result->type = SymbolInfo::NUM;

	tmp = Evaluate(*++it);
	assert(tmp != NULL && tmp->type == SymbolInfo::NUM);
	result->value = tmp->value;
	ReleaseSymbol(tmp);

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
		ReleaseSymbol(tmp);
	}
	return result;
}

SymbolInfo* Interpreter::Logic(SyntaxComponent *node)
{
	auto it = node->children->begin();
	string op = (*it)->data->token;

	SymbolInfo *result = NewSymbol();
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
		ReleaseSymbol(first);
		ReleaseSymbol(second);
	}
	else if (op == "and" || op == "or")
	{
		assert(node->count >= 3);

		SymbolInfo *tmp = Evaluate(*++it);
		assert(tmp != NULL && tmp->type == SymbolInfo::BOOL);
		result->flag = tmp->flag;
		ReleaseSymbol(tmp);

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
			ReleaseSymbol(tmp);
		}

	}
	else if (op == "not")
	{
		assert(node->count == 2);

		SymbolInfo *first = Evaluate(*++it);
		assert(first != NULL && first->type == SymbolInfo::BOOL);

		result->flag = !first;
		ReleaseSymbol(first);
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
		ReleaseSymbol(first);
	}
	else if (op == "cond")
	{
		assert(node->count >= 2);
		SymbolInfo *sym;

		while (++it != node->children->end())
		{
			SyntaxComponent *line = *it;
			assert(line->count == 2);

			if (line->children->front()->count > 0) // normal condition
			{
				sym = Evaluate(line->children->front());
				assert(sym != NULL && sym->type == SymbolInfo::BOOL);

				if (sym->flag)
				{
					lastResult = line->children->back();
					ReleaseSymbol(sym);
					break;
				}
				ReleaseSymbol(sym);
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
		ReleaseSymbol(sym);
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

		result = NewSymbol();
		result->name = "pair";
		result->type = SymbolInfo::PAIR;
		result->pdata[0] = CopySymbol(first);
		result->pdata[1] = CopySymbol(second);
		ReleaseSymbol(first);
		ReleaseSymbol(second);
	}
	else if (op == "car")
	{
		assert(node->count == 2);
		SymbolInfo *sym = Evaluate(*++it);
		assert(sym != NULL && sym->type == SymbolInfo::PAIR);
		result = sym->pdata[0];
		ReleaseSymbol(sym);
	}
	else if (op == "cdr")
	{
		assert(node->count == 2);
		SymbolInfo *sym = Evaluate(*++it);
		assert(sym != NULL && sym->type == SymbolInfo::PAIR);
		result = sym->pdata[1];
		ReleaseSymbol(sym);
	}
	else if (op == "pair?")
	{
		assert(node->count == 2);
		SymbolInfo *sym = Evaluate(*++it);
		assert(sym != NULL);

		result = NewSymbol();
		result->type = SymbolInfo::BOOL;
		result->flag = (sym->type == SymbolInfo::PAIR);
		ReleaseSymbol(sym);
	}
	return result;
}

SymbolInfo* Interpreter::List(SyntaxComponent *node)
{
	auto it = node->children->begin();
	string op = (*it)->data->token;
	SymbolInfo *result = NULL;

	if (op == "null?")
	{
		assert(node->count == 2);
		SymbolInfo *sym = Evaluate(*++it);
		assert(sym != NULL);

		result = NewSymbol();
		result->type = SymbolInfo::BOOL;
		result->flag = (sym->type == SymbolInfo::NIL);
		ReleaseSymbol(sym);
	}
	else if (op == "list")
	{
		SymbolInfo **last = &result;
		SymbolInfo *tail = NewSymbol();
		tail->type = SymbolInfo::NIL;

		while (++it != node->children->end())
		{
			SymbolInfo *sym = Evaluate(*it);
			assert(sym != NULL);

			(*last) = NewSymbol();
			(*last)->type = SymbolInfo::PAIR;
			(*last)->name = "pair";
			(*last)->pdata[0] = CopySymbol(sym);
			ReleaseSymbol(sym);

			last = &(*last)->pdata[1];
		}
		(*last) = tail;
	}
	return result;
}

SymbolInfo* Interpreter::MiscFunc(SyntaxComponent *node)
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
			ReleaseSymbol(sym);
		}
	}
	else if (op == "newline")
	{
		assert(node->count == 1);
		cout << endl;
	}
	else if (op == "random")
	{
		assert(node->count == 2);
		SymbolInfo *sym = Evaluate(*++it);
		assert(sym != NULL && sym->type == SymbolInfo::NUM);

		result = NewSymbol();
		result->type = SymbolInfo::NUM;
		result->value = int(rand() * sym->value);
		ReleaseSymbol(sym);
	}

	return result;
}

SymbolInfo* Interpreter::CopySymbol(SymbolInfo *sym)
{
	SymbolInfo *result = NewSymbol();
	result->type = sym->type;
	result->name = sym->name;
	result->value = sym->value;
	result->next = sym->next;

	if (sym->type == SymbolInfo::FUN || sym->type == SymbolInfo::LAMBDA)
	{
		result->func = new FunctionInfo;
		result->func->parameter = sym->func->parameter;
		result->func->body = sym->func->body;
		result->func->env = new EnvironmentInfo;
		result->func->env->head = sym->func->env->head;
	}
	else if (sym->type == SymbolInfo::TXT)
	{
		result->text = new string;
		*result->text = *sym->text;
	}

	return result;
}

SymbolInfo* Interpreter::NewSymbol()
{
	SymbolInfo *result = NULL;

	if (emptySymbolSize > 0)
	{
		for (auto it=symbolRecord.begin(); it != symbolRecord.end(); ++it)
		{
			if ((*it)->useState == SymbolInfo::EMPTY)
			{
				result = (*it);
				--emptySymbolSize;
				break;
			}
		}
		assert(result != NULL);
	}
	else
	{
		result = new SymbolInfo;
		symbolRecord.push_front(result);
		++symbolRecordSize;
	}
	result->useState = SymbolInfo::TO_USE;
	return result;
}

void Interpreter::ReleaseSymbol(SymbolInfo *sym)
{
	if (sym != NULL)
	{
		sym->useState = SymbolInfo::NOT_USE;

		if (sym->type == SymbolInfo::PAIR)
		{
			ReleaseSymbol(sym->pdata[0]);
			ReleaseSymbol(sym->pdata[1]);
		}
	}
}

void Interpreter::CheckSymbols()
{
	clock_t startTime = clock();

	for (auto it=symbolRecord.begin(); it != symbolRecord.end(); ++it)
	{
		if ((*it)->useState != SymbolInfo::TO_USE && (*it)->useState != SymbolInfo::EMPTY) // return value, local variable still in use, etc
		{
			(*it)->useState = SymbolInfo::NOT_USE;
		}
	}

	if (debug & Interpreter::DEBUG_SYMBOL_MARK)
	{
		cout << "==start symbol mark==" << endl;
	}

	for (auto eIt=currentEnvironment.begin(); eIt != currentEnvironment.end(); ++eIt)
	{
		MarkEnvironment(*eIt, "");
	}

	for (auto eIt2=tmpEnvironment.begin(); eIt2 != tmpEnvironment.end(); ++eIt2)
	{
		MarkEnvironment(*eIt2, "tmp");
	}

	if (debug & Interpreter::DEBUG_SYMBOL_MARK)
	{
		cout << "==end symbol mark==" << endl;
	}

	++collectCount;
	markTime += double(clock() - startTime) / CLOCKS_PER_SEC;
}

void Interpreter::MarkEnvironment(EnvironmentInfo *env, string prefix)
{
	SymbolInfo *sym = env->head;
	while (sym != NULL)
	{
		if (debug & Interpreter::DEBUG_SYMBOL_MARK)
		{
			prefix += "." + sym->name;
		}
		MarkSymbol(sym, prefix);
		sym = sym->next;
	}
}

void Interpreter::MarkSymbol(SymbolInfo *sym, string prefix)
{
	assert(sym != NULL);
	if (sym->useState == SymbolInfo::NOT_USE) // avoid repertitive mark operation
	{
		if (debug & Interpreter::DEBUG_SYMBOL_MARK)
		{
			cout << "  " << prefix << ".";
			sym->Print();
		}

		sym->useState = SymbolInfo::IN_TABLE;

		if (sym->type == SymbolInfo::PAIR)
		{
			if (debug & Interpreter::DEBUG_SYMBOL_MARK)
			{
				prefix += "." + sym->name;
			}
			MarkSymbol(sym->pdata[0], prefix);
			MarkSymbol(sym->pdata[1], prefix);
		}
		else if (sym->type == SymbolInfo::FUN || sym->type == SymbolInfo::LAMBDA)
		{
			MarkEnvironment(sym->func->env, prefix);
		}
	}
}

void Interpreter::ClearSymbols(bool force)
{
	clock_t startTime = clock();
	emptySymbolSize = 0;

	for (auto it=symbolRecord.begin(); it != symbolRecord.end(); ++it)
	{
		if ((*it)->useState == SymbolInfo::EMPTY)
		{
			++emptySymbolSize;
			continue;
		}
		if ((*it)->useState == SymbolInfo::NOT_USE || force)
		{
			if ((*it)->type == SymbolInfo::FUN || (*it)->type == SymbolInfo::LAMBDA)
			{
				delete((*it)->func->env);
				delete (*it)->func;
			}
			else if ((*it)->type == SymbolInfo::TXT)
			{
				delete (*it)->text;
			}
			++emptySymbolSize;
			(*it)->useState = SymbolInfo::EMPTY;
		}
	}

	if (force)
	{
		for (auto sIt=symbolRecord.begin(); sIt != symbolRecord.end(); ++sIt)
		{
			delete (*sIt);
		}
		symbolRecord.clear();
		emptySymbolSize = symbolRecordSize = 0;
	}

	float clearRatio = float(emptySymbolSize) / symbolRecordSize;
	if (clearRatio < 0.3 || symbolNumThreshold < SYMBOL_TABLE_MIN_SIZE) // adjust garbage collection threshold dynamically
	{
		symbolNumThreshold *= 2;
	}
	else if (clearRatio > 0.7 && symbolNumThreshold >= SYMBOL_TABLE_MIN_SIZE)
	{
		symbolNumThreshold /= 2;
	}

	if (debug & Interpreter::DEBUG_SYMBOL_CLEAR)
	{
		cout << "maxSymbolNum: " << symbolNumThreshold << ", clear " << emptySymbolSize << " symbols, ratio: " << clearRatio * 100 << "%" << endl;
	}
	clearTime += double(clock() - startTime) / CLOCKS_PER_SEC;
}

int main(int argc, char **argv)
{
	if (argc < 2)
	{
		cout << "no source file found!" << endl;
		return 1;
	}

	LexAnalyzer lex;
	string lib_file("l4.lib");
	string src_file(argv[1]);
	src_file = "test/integral.rkt";
	
	for (size_t i=0; i<sizeof(l4_keys)/4; ++i)
	{
		lex.AddKey(l4_keys[i]);
	}

	// load lib
	lex.SetSourceFile(lib_file);
	lex.Parse();

	// load src file
	lex.SetSourceFile(src_file);
	lex.Parse();
	//lex.Print();

	SyntaxAnalyzer syntax;
	syntax.MakeTree(lex.GetComponents());
	//syntax.PrintTree();

	Interpreter vm;
	vm.debug = 0;//Interpreter::DEBUG_CALC_TIME;// | Interpreter::DEBUG_SYMBOL_CLEAR ;//Interpreter::DEBUG_SYMBOL_CLEAR | Interpreter::DEBUG_FUNC_CALL | Interpreter::DEBUG_SYMBOL_MARK;
	//for (int i=0; i<10; ++i)
	vm.Run(syntax.GetTree());

	syntax.ClearTree();

	system("pause");
	return 0;
}