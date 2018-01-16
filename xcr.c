#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <string.h>

int debug;    // print the executed instructions
int assembly; // print out the assembly and source

int token; // current token

// instructions
enum { LEA ,IMM ,JMP ,CALL,JZ  ,JNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PUSH,
       OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,
       OPEN,READ,CLOS,PRTF,MALC,MSET,MCMP,MCPY,FREE,EXIT };

// tokens and classes (operators last and in precedence order)
// copied from c4
enum {
  C_Num = 128, C_Fun, C_Sys, C_Glo, C_Loc,
  Id, Num, 
  Char, Else, Enum, If, Int, Return, Sizeof, Struct, While,
  Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr, Add, Sub, Mul, Div, Mod, Inc, Dec, Brak, Point, Member
};

// types of variable/function
enum { CHAR, INT, STRUCT, PTR };

// type of declaration.
enum {Global, Local};

struct SymbolProperty {
	int structId, type, category, value, count;
	struct SymbolProperty *next;
};

struct Symbol {
	int token, hash, *funcAddr;
	char *name;
	struct SymbolProperty *item;
};

struct StructMember {
	int id, type, addr, structId, count;
	struct StructMember *next;
};

struct StructHeader {
	int id, count, size;
	struct StructMember *item;
};

struct VirtualMachineState {
	int *pc, *bp, *sp;
};

struct DebugInfo {
	int *old_text, *debug_line, *text_head, last_text;
};

struct CompileInfo {
	int *text, *stack;
	char *data;
	struct DebugInfo dinfo;
};

struct CompileInfo cinfo;

char *src, *old_src;  // pointer to source code string;

struct Symbol symbols_table[1000];	// current parsed ID
int symbol_id;					// symbol table

struct StructHeader structs_table[64];
int struct_id;

int	line,        // line number of source code
    token_val,   // value of current token (mainly for number)
	pos_local;	 // position of local variables on the cinfo.stack.

int last_struct;// to keep value between recursive expression processing

// function frame
//
// 0: arg 1
// 1: arg 2
// 2: arg 3
// 3: return address
// 4: old bp pointer  <- index_of_bp
// 5: local var 1
// 6: local var 2
int index_of_bp; // index of bp pointer on cinfo.stack

void local_variable();

void next() {
    char *last_pos;
    int hash;

	while (cinfo.dinfo.last_text <= cinfo.text - cinfo.dinfo.text_head) {
		cinfo.dinfo.debug_line[cinfo.dinfo.last_text] = line;
		cinfo.dinfo.last_text++;
	}

    while (token = *src) {
        ++src;

        if (token == '\n') {
            if (assembly) {
                // print compile info
                printf("%d: %.*s", line, src-old_src, old_src);
                old_src = src;

                while (cinfo.dinfo.old_text < cinfo.text) {
                    printf("%8.4s", & "LEA ,IMM ,JMP ,CALL,JZ  ,JNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PUSH,"
                                      "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
                                      "OPEN,READ,CLOS,PRTF,MALC,MSET,MCMP,FREE,EXIT"[*++cinfo.dinfo.old_text * 5]);

                    if (*cinfo.dinfo.old_text <= ADJ)
                        printf(" %d\n", *++cinfo.dinfo.old_text);
                    else
                        printf("\n");
                }
            }
            ++line;
        }
        else if (token == '#') {
            // skip macro, because we will not support it
            while (*src != 0 && *src != '\n') {
                src++;
            }
        }
        else if ((token >= 'a' && token <= 'z') || (token >= 'A' && token <= 'Z') || (token == '_')) {

            // parse identifier
            last_pos = src - 1;
            hash = token;

            while ((*src >= 'a' && *src <= 'z') || (*src >= 'A' && *src <= 'Z') || (*src >= '0' && *src <= '9') || (*src == '_')) {
                hash = hash * 147 + *src;
                src++;
            }

            // look for existing identifier, linear search
			symbol_id = 0;
            while (symbols_table[symbol_id].token) {
                if (symbols_table[symbol_id].hash == hash && !memcmp((char *)(symbols_table[symbol_id].name), last_pos, src - last_pos)) {
                    //found one, return
                    token = symbols_table[symbol_id].token;
                    return;
                }
				++symbol_id;


				if (symbol_id >= 1000) {
					printf("%d: too much symbols.\n", line);
					exit(-1);
				}
            }

            // store new ID
            symbols_table[symbol_id].name = last_pos;
            symbols_table[symbol_id].hash = hash;
			symbols_table[symbol_id].item = malloc(sizeof(struct SymbolProperty));
			memset(symbols_table[symbol_id].item, 0, sizeof(struct SymbolProperty));

            token = symbols_table[symbol_id].token = Id;
            return;
        }
        else if (token >= '0' && token <= '9') {
            // parse number, three kinds: dec(123) hex(0x123) oct(017)
            token_val = token - '0';
            if (token_val > 0) {
                // dec, starts with [1-9]
                while (*src >= '0' && *src <= '9') {
                    token_val = token_val*10 + *src++ - '0';
                }
            } else {
                // starts with number 0
                if (*src == 'x' || *src == 'X') {
                    //hex
                    token = *++src;
                    while ((token >= '0' && token <= '9') || (token >= 'a' && token <= 'f') || (token >= 'A' && token <= 'F')) {
                        token_val = token_val * 16 + (token & 15) + (token >= 'A' ? 9 : 0);
                        token = *++src;
                    }
                } else {
                    // oct
                    while (*src >= '0' && *src <= '7') {
                        token_val = token_val*8 + *src++ - '0';
                    }
                }
            }

            token = Num;
            return;
        }
        else if (token == '/') {
            if (*src == '/') {
                // skip comments
                while (*src != 0 && *src != '\n') {
                    ++src;
                }
            } else {
                // divide operator
                token = Div;
                return;
            }
        }
        else if (token == '"' || token == '\'') {
            // parse string literal, currently, the only supported escape
            // character is '\n', store the string literal into cinfo.data.
            last_pos = cinfo.data;
            while (*src != 0 && *src != token) {
                token_val = *src++;
                if (token_val == '\\') {
                    // escape character
                    token_val = *src++;
                    if (token_val == 'n') {
                        token_val = '\n';
                    }
                }

                if (token == '"') {
                    *cinfo.data++ = token_val;
                }
            }

            src++;
            // if it is a single character, return Num token
            if (token == '"') {
                token_val = (int)last_pos;
            } else {
                token = Num;
            }

            return;
        }
        else if (token == '=') {
            // parse '==' and '='
            if (*src == '=') {
                src ++;
                token = Eq;
            } else {
                token = Assign;
            }
            return;
        }
        else if (token == '+') {
            // parse '+' and '++'
            if (*src == '+') {
                src ++;
                token = Inc;
            } else {
                token = Add;
            }
            return;
        }
        else if (token == '-') {
            // parse '-' and '--'
            if (*src == '-') {
                src ++;
                token = Dec;
			} else if (*src == '>') {
				src ++;
				token = Member;
            } else {
                token = Sub;
            }
            return;
        }
        else if (token == '!') {
            // parse '!='
            if (*src == '=') {
                src++;
                token = Ne;
            }
            return;
        }
        else if (token == '<') {
            // parse '<=', '<<' or '<'
            if (*src == '=') {
                src ++;
                token = Le;
            } else if (*src == '<') {
                src ++;
                token = Shl;
            } else {
                token = Lt;
            }
            return;
        }
        else if (token == '>') {
            // parse '>=', '>>' or '>'
            if (*src == '=') {
                src ++;
                token = Ge;
            } else if (*src == '>') {
                src ++;
                token = Shr;
            } else {
                token = Gt;
            }
            return;
        }
        else if (token == '|') {
            // parse '|' or '||'
            if (*src == '|') {
                src ++;
                token = Lor;
            } else {
                token = Or;
            }
            return;
        }
        else if (token == '&') {
            // parse '&' and '&&'
            if (*src == '&') {
                src ++;
                token = Lan;
            } else {
                token = And;
            }
            return;
        }
        else if (token == '^') {
            token = Xor;
            return;
        }
        else if (token == '%') {
            token = Mod;
            return;
        }
        else if (token == '*') {
            token = Mul;
            return;
        }
        else if (token == '[') {
            token = Brak;
            return;
        }
        else if (token == '?') {
            token = Cond;
            return;
        }
		else if (token == '.') {
			token = Point;
			return;
		}
        else if (token == '~' || token == ';' || token == '{' || token == '}' || token == '(' || token == ')' || token == ']' || token == ',' || token == ':') {
            // directly return the character as token;
            return;
        }
    }
}

void match(int tk) {
    if (token == tk) {
        next();
    } else {
        printf("%d: expected token: %d\n", line, tk);
        exit(-1);
    }
}

int find_struct(int id) {
	int struct_id = 0;

	while (structs_table[struct_id].id != 0) {
		if (structs_table[struct_id].id  == id) {
			return struct_id;
		}
		++struct_id;
	}
	return -1;
}

int find_struct_member(int struct_item, int id) {
	struct StructMember *member;
	member = structs_table[struct_item].item;

	while (member != 0) {
		if (member->id == id) {
			return (int)member;
		}
		member = member->next;
	}
	return 0;
}

int get_size(int type, int data) {
	if (type >= PTR) {
		return sizeof(int);
	}
	else if (type == CHAR) {
		return sizeof(char);
	}
	else if (type == INT) {
		return sizeof(int);
	}
	else if (type == STRUCT) {
		return structs_table[data].size;
	}
	return 0;
} 

int align_to_int(int address) {
	address = (address + sizeof(int) - 1) / sizeof(int);
	return address;
}

int assign_type_check(int left, int right, int left_data, int right_data) {
	// pointer assign
	if (left >= PTR && right == INT) {
		return 1;
	}
	// char & int
	if (left <= INT && right <= INT) {
		return 1;
	}
	// general assign
	if (left != right) {
		return 0;
	}
	// struct(pointer) assign
	if (left % PTR == STRUCT && left_data != right_data) {
		return 0;
	}
	return 1;
}

int op_assign_begin(int *new_level) {
	if (token == Assign) {
		match(Assign);

		if (*cinfo.text == LC || *cinfo.text == LI) {
			*(cinfo.text + 1) = *cinfo.text;
			*cinfo.text = PUSH;
			++cinfo.text;
			*new_level = Assign;
		}
		else {
			printf("%d: bad lvalue in assignment\n", line);
			exit(-1);
		}
		return 1;
	}
	return 0;
}

void op_assign_end(int op_assign, int type) {
	if (op_assign == 1) {
		*++cinfo.text = (type == CHAR) ? SC : SI;
	}
}

void push_symbol_property(struct Symbol *cur) {
	struct SymbolProperty *new_item;
	new_item = malloc(sizeof(struct SymbolProperty));
	memset(new_item, 0, sizeof(struct SymbolProperty));

	new_item->next = cur->item;
	cur->item = new_item;
}

void pop_symbol_property(struct Symbol *cur) {
	struct SymbolProperty *old_item;
	old_item = cur->item;
	cur->item = cur->item->next;
	free(old_item);
}

int expression(int level) {
    // expressions have various format.
    // but majorly can be divided into two parts: unit and operator
    // for example `(char) *a[10] = (int *) func(b > 0 ? 10 : 20);
    // `a[10]` is an unit while `*` is an operator.
    // `func(...)` in total is an unit.
    // so we should first parse those unit and unary operators
    // and then the binary ones
    //
    // also the expression can be in the following types:
    //
    // 1. unit_unary ::= unit | unit unary_op | unary_op unit
    // 2. expr ::= unit_unary (bin_op unit_unary ...)

    // unit_unary()
    int expr_type, id = -1, tmp, *addr, struct_item, struct_tmp, op_assign, new_level;
	struct StructMember *member;

    {
        if (!token) {
            printf("%d: unexpected token EOF of expression\n", line);
            exit(-1);
        }
        if (token == Num) {
            match(Num);

            // emit code
            *++cinfo.text = IMM;
            *++cinfo.text = token_val;
            expr_type = INT;
        }
        else if (token == '"') {
            // continous string "abc" "abc"


            // emit code
            *++cinfo.text = IMM;
            *++cinfo.text = token_val;

            match('"');
            // store the rest strings
            while (token == '"') {
                match('"');
            }

            // append the end of string character '\0', all the cinfo.data are default
            // to 0, so just move cinfo.data one position forward.
			// align to 4 byte
            cinfo.data = (char *)(align_to_int((int)cinfo.data + sizeof(char)) * sizeof(int));
            expr_type = PTR;
        }
        else if (token == Sizeof) {
            // sizeof is actually an unary operator
            // now only `sizeof(int)`, `sizeof(char)` and `sizeof(*...)` are
            // supported.
            match(Sizeof);
            match('(');

            expr_type = INT;
			if (token == Char) expr_type = CHAR;
			if (token == Struct) expr_type = STRUCT;
			match(token);

			if (expr_type == STRUCT) {
				struct_item = find_struct(symbol_id);
				match(Id);
			}

            while (token == Mul) {
                match(Mul);
                expr_type += PTR;
            }

            match(')');

            // emit code
            *++cinfo.text = IMM;
			if (expr_type != STRUCT) {
				*++cinfo.text = get_size(expr_type, 0);
			}
			else {
				*++cinfo.text = get_size(expr_type, struct_item);
			}

            expr_type = INT;
        }
        else if (token == Id) {
            // there are several type when occurs to Id
            // but this is unit, so it can only be
            // 1. function call
            // 2. Enum variable
            // 3. global/local variable

			id = symbol_id;
            match(Id);

            if (token == '(') {
                // function call
                match('(');

                // pass in arguments
                tmp = 0; // number of arguments
                while (token != ')') {
                    expression(Assign);
                    *++cinfo.text = PUSH;
                    tmp ++;

                    if (token == ',') {
                        match(',');
                    }

                }
                match(')');

                // emit code
                if (symbols_table[id].item->category == C_Sys) {
                    // system functions
                    *++cinfo.text = symbols_table[id].item->value;
                }
                else if (symbols_table[id].item->category == C_Fun) {
                    // function call
                    *++cinfo.text = CALL;
					*++cinfo.text = symbols_table[id].item->value;
                }
                else {
                    printf("%d: bad function call\n", line);
                    exit(-1);
                }

                // clean the cinfo.stack for arguments
                if (tmp > 0) {
                    *++cinfo.text = ADJ;
                    *++cinfo.text = tmp;
                }
                expr_type = symbols_table[id].item->type;
            }
            else if (symbols_table[id].item->category == C_Num) {
                // enum variable
                *++cinfo.text = IMM;
                *++cinfo.text = symbols_table[id].item->value;
                expr_type = INT;
            }
            else {
                // variable
                if (symbols_table[id].item->category == C_Loc) {
                    *++cinfo.text = LEA;
                    *++cinfo.text = index_of_bp - symbols_table[id].item->value;
                }
                else if (symbols_table[id].item->category == C_Glo) {
                    *++cinfo.text = IMM;
                    *++cinfo.text = symbols_table[id].item->value;
                }
                else {
                    printf("%d: undefined variable\n", line);
                    exit(-1);
                }

				last_struct = find_struct(symbols_table[id].item->structId);

				if (symbols_table[id].item->count == 0)
				{
					// emit code, default behaviour is to load the value of the
					// address which is stored in `ax`
					expr_type = symbols_table[id].item->type;
					*++cinfo.text = (expr_type == Char) ? LC : LI;
				}
				else {
					// for array type, value is address, do not need LC/LI
					expr_type = symbols_table[id].item->type + PTR;
				}
            }
        }
        else if (token == '(') {
            // cast or parenthesis
            match('(');
            if (token == Int || token == Char || token == Struct) {
				tmp  = INT;
				if (token == Char) tmp = CHAR;
				if (token == Struct) tmp = STRUCT;
                match(token);

				if (tmp == STRUCT) {
					id = symbol_id;
					match(Id);
				}

                while (token == Mul) {
                    match(Mul);
                    tmp += PTR;
                }

                match(')');

                expression(Inc); // cast has precedence as Inc(++)

				last_struct = find_struct(id);
                expr_type  = tmp;
            } else {
                // normal parenthesis
                expression(Assign);
                match(')');
            }
        }
        else if (token == Mul) {
            // dereference *<addr>
            match(Mul);
            expression(Inc); // dereference has the same precedence as Inc(++)

            if (expr_type >= PTR) {
                expr_type -= PTR;
            } else {
                printf("%d: bad dereference\n", line);
                exit(-1);
            }

            *++cinfo.text = (expr_type == CHAR) ? LC : LI;
        }
        else if (token == And) {
            // get the address of
            match(And);
            expression(Inc); // get the address of
            if (*cinfo.text == LC || *cinfo.text == LI) {
                cinfo.text --;
            } else {
                printf("%d: bad address of\n", line);
                exit(-1);
            }

            expr_type += PTR;
        }
        else if (token == '!') {
            // not
            match('!');
            expression(Inc);

            // emit code, use <expr> == 0
            *++cinfo.text = PUSH;
            *++cinfo.text = IMM;
            *++cinfo.text = 0;
            *++cinfo.text = EQ;

            expr_type = INT;
        }
        else if (token == '~') {
            // bitwise not
            match('~');
            expression(Inc);

            // emit code, use <expr> XOR -1
            *++cinfo.text = PUSH;
            *++cinfo.text = IMM;
            *++cinfo.text = -1;
            *++cinfo.text = XOR;

            expr_type = INT;
        }
        else if (token == Add) {
            // +var, do nothing
            match(Add);
            expression(Inc);

            expr_type = INT;
        }
        else if (token == Sub) {
            // -var
            match(Sub);

            if (token == Num) {
                *++cinfo.text = IMM;
                *++cinfo.text = -token_val;
                match(Num);
            } else {

                *++cinfo.text = IMM;
                *++cinfo.text = -1;
                *++cinfo.text = PUSH;
                expression(Inc);
                *++cinfo.text = MUL;
            }

            expr_type = INT;
        }
        else if (token == Inc || token == Dec) {
            tmp = token;
            match(token);
            expression(Inc);
            if (*cinfo.text == LC) {
                *cinfo.text = PUSH;  // to duplicate the address
                *++cinfo.text = LC;
            } else if (*cinfo.text == LI) {
                *cinfo.text = PUSH;
                *++cinfo.text = LI;
            } else {
                printf("%d: bad lvalue of pre-increment\n", line);
                exit(-1);
            }
            *++cinfo.text = PUSH;
            *++cinfo.text = IMM;
            *++cinfo.text = (expr_type > PTR) ? sizeof(int) : sizeof(char);
            *++cinfo.text = (tmp == Inc) ? ADD : SUB;
            *++cinfo.text = (expr_type == CHAR) ? SC : SI;
        }
		else if (token == Assign) {
			// variable assign in define mode
		}
        else {
            printf("%d: bad expression\n", line);
            exit(-1);
        }
    }

    // binary operator and postfix operators.
    {
        while (token >= level) {
            // handle according to current operator's precedence
			op_assign = 0;
            tmp = expr_type;
            if (token == Assign) {
                // var = expr;
                match(Assign);
                if (*cinfo.text == LC || *cinfo.text == LI) {
                    *cinfo.text = PUSH; // save the lvalue's pointer
                } else {
                    printf("%d: bad lvalue in assignment\n", line);
                    exit(-1);
                }

				struct_tmp = last_struct;

                expression(Assign);

				if (assign_type_check(tmp, expr_type, struct_tmp, last_struct) == 0) {
					printf("%d: unmatched type in assign, %d, %d\n", line, struct_tmp, last_struct);
					exit(-1);
				}
				if (tmp == STRUCT && expr_type == STRUCT) {
					if (struct_tmp == last_struct)
					{
						*cinfo.text = PUSH;
						*++cinfo.text = IMM;
						*++cinfo.text = structs_table[last_struct].size;
						*++cinfo.text = PUSH;
						*++cinfo.text = MCPY;
					}
				}
				else {
					*++cinfo.text = (tmp == CHAR) ? SC : SI;
				}
				expr_type = tmp;
            }
            else if (token == Cond) {
                // expr ? a : b;
                match(Cond);
                *++cinfo.text = JZ;
                addr = ++cinfo.text;
                expression(Assign);
                if (token == ':') {
                    match(':');
                } else {
                    printf("%d: missing colon in conditional\n", line);
                    exit(-1);
                }
                *addr = (int)(cinfo.text + 3);
                *++cinfo.text = JMP;
                addr = ++cinfo.text;
                expression(Cond);
                *addr = (int)(cinfo.text + 1);
            }
            else if (token == Lor) {
                // logic or
                match(Lor);
                *++cinfo.text = JNZ;
                addr = ++cinfo.text;
                expression(Lan);
                *addr = (int)(cinfo.text + 1);
                expr_type = INT;
            }
            else if (token == Lan) {
                // logic and
                match(Lan);
                *++cinfo.text = JZ;
                addr = ++cinfo.text;
                expression(Or);
                *addr = (int)(cinfo.text + 1);
                expr_type = INT;
            }
            else if (token == Or) {
                // bitwise or
                match(Or);

				new_level = Xor;
				op_assign = op_assign_begin(&new_level);

                *++cinfo.text = PUSH;
                expression(new_level);
                *++cinfo.text = OR;
                expr_type = INT;

				op_assign_end(op_assign, tmp);
            }
            else if (token == Xor) {
                // bitwise xor
                match(Xor);

				new_level = And;
				op_assign = op_assign_begin(&new_level);

                *++cinfo.text = PUSH;
                expression(new_level);
                *++cinfo.text = XOR;
                expr_type = INT;

				op_assign_end(op_assign, tmp);
            }
            else if (token == And) {
                // bitwise and
                match(And);

				new_level = Eq;
				op_assign = op_assign_begin(&new_level);

                *++cinfo.text = PUSH;
                expression(new_level);
                *++cinfo.text = AND;
                expr_type = INT;

				op_assign_end(op_assign, tmp);
            }
            else if (token == Eq) {
                // equal ==
                match(Eq);
                *++cinfo.text = PUSH;
                expression(Ne);
                *++cinfo.text = EQ;
                expr_type = INT;
            }
            else if (token == Ne) {
                // not equal !=
                match(Ne);
                *++cinfo.text = PUSH;
                expression(Lt);
                *++cinfo.text = NE;
                expr_type = INT;
            }
            else if (token == Lt) {
                // less than
                match(Lt);
                *++cinfo.text = PUSH;
                expression(Shl);
                *++cinfo.text = LT;
                expr_type = INT;
            }
            else if (token == Gt) {
                // greater than
                match(Gt);
                *++cinfo.text = PUSH;
                expression(Shl);
                *++cinfo.text = GT;
                expr_type = INT;
            }
            else if (token == Le) {
                // less than or equal to
                match(Le);
                *++cinfo.text = PUSH;
                expression(Shl);
                *++cinfo.text = LE;
                expr_type = INT;
            }
            else if (token == Ge) {
                // greater than or equal to
                match(Ge);
                *++cinfo.text = PUSH;
                expression(Shl);
                *++cinfo.text = GE;
                expr_type = INT;
            }
            else if (token == Shl) {
                // shift left
                match(Shl);

				new_level = Add;
				op_assign = op_assign_begin(&new_level);

                *++cinfo.text = PUSH;
                expression(new_level);
                *++cinfo.text = SHL;
                expr_type = INT;

				op_assign_end(op_assign, tmp);
            }
            else if (token == Shr) {
                // shift right
                match(Shr);

				new_level = Add;
				op_assign = op_assign_begin(&new_level);

                *++cinfo.text = PUSH;
                expression(new_level);
                *++cinfo.text = SHR;
                expr_type = INT;

				op_assign_end(op_assign, tmp);
            }
            else if (token == Add) {
                // add
                match(Add);
				
				new_level = Mul;
				op_assign = op_assign_begin(&new_level);

                *++cinfo.text = PUSH;
                expression(new_level);

                expr_type = tmp;
                if (expr_type > PTR) {
                    // pointer type, and not `char *`
                    *++cinfo.text = PUSH;
                    *++cinfo.text = IMM;
                    *++cinfo.text = sizeof(int);
                    *++cinfo.text = MUL;
                }
                *++cinfo.text = ADD;

				op_assign_end(op_assign, tmp);
            }
            else if (token == Sub) {
                // sub
                match(Sub);

				new_level = Mul;
				op_assign = op_assign_begin(&new_level);

                *++cinfo.text = PUSH;
                expression(new_level);
                if (tmp > PTR && tmp == expr_type) {
                    // pointer subtraction
                    *++cinfo.text = SUB;
                    *++cinfo.text = PUSH;
                    *++cinfo.text = IMM;
                    *++cinfo.text = sizeof(int);
                    *++cinfo.text = DIV;
                    expr_type = INT;
                } else if (tmp > PTR) {
                    // pointer movement
                    *++cinfo.text = PUSH;
                    *++cinfo.text = IMM;
                    *++cinfo.text = sizeof(int);
                    *++cinfo.text = MUL;
                    *++cinfo.text = SUB;
                    expr_type = tmp;
                } else {
                    // numeral subtraction
                    *++cinfo.text = SUB;
                    expr_type = tmp;
                }
				op_assign_end(op_assign, tmp);
            }
            else if (token == Mul) {
                // multiply
                match(Mul);

				new_level = Inc;
				op_assign = op_assign_begin(&new_level);

                *++cinfo.text = PUSH;
                expression(new_level);
                *++cinfo.text = MUL;
                expr_type = tmp;

				op_assign_end(op_assign, tmp);
            }
            else if (token == Div) {
                // divide
                match(Div);

				new_level = Inc;
				op_assign = op_assign_begin(&new_level);

                *++cinfo.text = PUSH;
                expression(new_level);
                *++cinfo.text = DIV;
                expr_type = tmp;

				op_assign_end(op_assign, tmp);
            }
            else if (token == Mod) {
                // Modulo
                match(Mod);

				new_level = Inc;
				op_assign = op_assign_begin(&new_level);

                *++cinfo.text = PUSH;
                expression(new_level);
                *++cinfo.text = MOD;
                expr_type = tmp;

				op_assign_end(op_assign, tmp);
            }
            else if (token == Inc || token == Dec) {
                // postfix inc(++) and dec(--)
                // we will increase the value to the variable and decrease it
                // on `ax` to get its original value.
                if (*cinfo.text == LI) {
                    *cinfo.text = PUSH;
                    *++cinfo.text = LI;
                }
                else if (*cinfo.text == LC) {
                    *cinfo.text = PUSH;
                    *++cinfo.text = LC;
                }
                else {
                    printf("%d: bad value in increment\n", line);
                    exit(-1);
                }

                *++cinfo.text = PUSH;
                *++cinfo.text = IMM;
                *++cinfo.text = (expr_type > PTR) ? sizeof(int) : sizeof(char);
                *++cinfo.text = (token == Inc) ? ADD : SUB;
                *++cinfo.text = (expr_type == CHAR) ? SC : SI;
                *++cinfo.text = PUSH;
                *++cinfo.text = IMM;
                *++cinfo.text = (expr_type > PTR) ? sizeof(int) : sizeof(char);
                *++cinfo.text = (token == Inc) ? SUB : ADD;
                match(token);
            }
            else if (token == Brak) {
                // array access var[xx]
                match(Brak);
                *++cinfo.text = PUSH;

				struct_tmp = last_struct;
                expression(Assign);
                match(']');
				last_struct = struct_tmp;
				expr_type = tmp - PTR;

				if (expr_type > CHAR) {
					// pointer, `not char *`
					*++cinfo.text = PUSH;
					*++cinfo.text = IMM;
					*++cinfo.text = get_size(expr_type, last_struct);
					*++cinfo.text = MUL;
				}
				else if (expr_type < CHAR) {
					printf("%d: pointer or array type expected\n", line);
					exit(-1);
				}

				*++cinfo.text = ADD;
				*++cinfo.text = (expr_type == CHAR) ? LC : LI;
            }
			else if (token == Point || token == Member) {
				// struct member access var1.var2
				if (token == Member) {
					tmp -= PTR;
					*++cinfo.text = LI; // this instruction will be overwritten
				}

				if (tmp != STRUCT || last_struct == -1) {
					printf("%d: struct type expected\n", line);
					exit(-1);
				}
				struct_item = last_struct;
				match(token);

				member = (struct StructMember*)find_struct_member(struct_item, symbol_id);
				match(Id);

				expr_type = member->type;
				last_struct = find_struct(member->structId);

				if (*cinfo.text == LI) {
					*cinfo.text = PUSH;
				} else {
					printf("%d: bad lvalue in assignment\n", line);
					exit(-1);
				}
				*++cinfo.text = IMM;
				*++cinfo.text = member->addr;
				*++cinfo.text = ADD;

				if (member->count == 0)
				{
					*++cinfo.text = (expr_type == CHAR) ? LC : LI;
				}
				else {
					expr_type += PTR;
				}
			}
            else {
                printf("%d: compiler error, token = %d\n", line, token);
                exit(-1);
            }
        }
    }
	return expr_type;
}

void statement() {
    // there are 8 kinds of statements here:
    // 1. if (...) <statement> [else <statement>]
    // 2. while (...) <statement>
    // 3. { <statement> }
    // 4. return xxx;
    // 5. <empty statement>;
    // 6. expression; (expression end with semicolon)

    int *a, *b; // bess for branch control

	last_struct = 0;

    if (token == If) {
        // if (...) <statement> [else <statement>]
        //
        //   if (...)           <cond>
        //                      JZ a
        //     <statement>      <statement>
        //   else:              JMP b
        // a:
        //     <statement>      <statement>
        // b:                   b:
        //
        //
        match(If);
        match('(');
        expression(Assign);  // parse condition
        match(')');

        // emit code for if
        *++cinfo.text = JZ;
        b = ++cinfo.text;

        statement();         // parse statement
        if (token == Else) { // parse else
            match(Else);

            // emit code for JMP B
            *b = (int)(cinfo.text + 3);
            *++cinfo.text = JMP;
            b = ++cinfo.text;

            statement();
        }

        *b = (int)(cinfo.text + 1);
    }
    else if (token == While) {
        //
        // a:                     a:
        //    while (<cond>)        <cond>
        //                          JZ b
        //     <statement>          <statement>
        //                          JMP a
        // b:                     b:
        match(While);

        a = cinfo.text + 1;

        match('(');
        expression(Assign);
        match(')');

        *++cinfo.text = JZ;
        b = ++cinfo.text;

        statement();

        *++cinfo.text = JMP;
        *++cinfo.text = (int)a;
        *b = (int)(cinfo.text + 1);
    }
    else if (token == '{') {
        // { <statement> ... }
        match('{');

        while (token != '}') {
            statement();
        }

        match('}');
    }
    else if (token == Return) {
        // return [expression];
        match(Return);

        if (token != ';') {
            expression(Assign);
        }

        match(';');

        // emit code for return
        *++cinfo.text = LEV;
    }
    else if (token == ';') {
        // empty statement
        match(';');
    }
	else if (token == Int || token == Char || token == Struct) {
		// define local variables
		local_variable();
		match(';');
	}
    else {
        // a = b; or function_call();
        expression(Assign);
        match(';');
    }
}

void enum_declaration() {
    // parse enum [id] { a = 1, b = 3, ...}
    int i = 0;

    while (token != '}') {
        if (token != Id) {
            printf("%d: bad enum identifier %d\n", line, token);
            exit(-1);
        }
        next();
        if (token == Assign) {
            // like {a=10}
            next();
            if (token != Num) {
                printf("%d: bad enum initializer\n", line);
                exit(-1);
            }
            i = token_val;
            next();
        }

        symbols_table[symbol_id].item->category = C_Num;
        symbols_table[symbol_id].item->type = INT;
        symbols_table[symbol_id].item->value = i++;
		symbols_table[symbol_id].item->count = 0;

        if (token == ',') {
            next();
        }
    }
}

void struct_declaration(int id) {
	// parse struct id {int a, char b, int* c, ...}
	int basetype, struct_item = -1, struct_symbol_id = -1, count = 0, type;
	struct StructMember *head = 0, *last = 0, *member = 0;

	structs_table[struct_id].id = id;
	structs_table[struct_id].count = id;
	structs_table[struct_id].size = 0;

	while (token != '}') {
		// parse type information
		basetype = INT;
		if (token == Char) basetype = CHAR;
		if (token == Struct) basetype = STRUCT;
		match(token);

		if (basetype == STRUCT) {
			struct_symbol_id = symbol_id;
			struct_item = find_struct(symbol_id);
			match(Id);
		}

		// parse the comma seperated variable declaration.
		while (token != ';') {
			type = basetype;
			// parse pointer type, note that there may exist `int ****x;`
			while (token == Mul) {
				match(Mul);
				type = type + PTR;
			}

			if (token != Id) {
				// invalid declaration
				printf("%d: bad struct member declaration\n", line);
				exit(-1);
			}

			if (type % PTR == STRUCT) {
				if (struct_item == -1 || (struct_symbol_id == id && type == STRUCT)) {
					printf("%d: unrecognized struct type\n", line);
					exit(-1);
				}
			}

			// struct member declaration

			member = malloc(sizeof(struct StructMember));
			memset(member, 0, sizeof(struct StructMember));

			if (head == 0) {
				head = member;
			}

			if (last != 0) {
				last->next = member;
			}
			last = member;

			member->id = symbol_id;
			member->type = type;
			member->addr = structs_table[struct_id].size; 
			member->structId = struct_symbol_id;

			count = 1;

			match(Id);

			if (token == Brak) {
				// array declaration
				match(Brak);
				if (token == Num && token_val > 0) {
					count = token_val;
					member->count = token_val;
				}
				else {
					printf("%d: bad array declaration\n", line);
					exit(-1);
				}
				match(Num);
				match(']');
			}

			structs_table[struct_id].size += get_size(type, struct_item) * count; 
			structs_table[struct_id].count++;

			if (token == ',') {
				match(',');
			}
		}
		match(';');
	}
	structs_table[struct_id].item = head;
	++struct_id;
}

void function_parameter() {
    int basetype, type, params = 0, struct_item, id;

    while (token != ')') {
        // int name, ...
		basetype = INT;
		if (token == Char) basetype = CHAR;
		if (token == Struct) basetype = STRUCT;
		match(token);

		if (basetype == STRUCT) {
			id = symbol_id;
			struct_item = find_struct(symbol_id);
			match(Id);

			if (token != Mul) {
				printf("%d: bad parameter declaration\n", line);
				exit(-1);
			}
		}

        // pointer type
		type = basetype;
        while (token == Mul) {
            match(Mul);
            type = type + PTR;
        }

        // parameter name
        if (token != Id) {
            printf("%d: bad parameter declaration\n", line);
            exit(-1);
        }
        if (symbols_table[symbol_id].item->category == C_Loc) {
            printf("%d: duplicate parameter declaration\n", line);
            exit(-1);
        }

        // store the local variable
		push_symbol_property(&symbols_table[symbol_id]);
        symbols_table[symbol_id].item->category  = C_Loc;
        symbols_table[symbol_id].item->type   = type;
        symbols_table[symbol_id].item->value  = params++;   // index of current parameter
		symbols_table[symbol_id].item->count = 0;
		symbols_table[symbol_id].item->structId = 0;

		if (basetype == STRUCT && type >= PTR) {
			symbols_table[symbol_id].item->structId = (int)id;
		}

		match(Id);

        if (token == ',') {
            match(',');
        }
    }
    index_of_bp = params + 1;
}

void local_variable() {
	int basetype, old_pos, type, id, struct_item = -1;

	// local variable declaration, just like global ones.
	basetype = INT;
	if (token == Char) basetype = CHAR;
	if (token == Struct) basetype = STRUCT;
	match(token);

	if (basetype == STRUCT) {
		id = symbol_id;
		struct_item = find_struct(symbol_id);
		match(Id);
	}

	while (token != ';') {
		// struct TestStruct a, ...;
		type = basetype;
		while (token == Mul) {
			match(Mul);
			type = type + PTR;
		}

		if (token != Id) {
			// invalid declaration
			printf("%d: bad local declaration\n", line);
			exit(-1);
		}

		if (symbols_table[symbol_id].item->category == C_Loc) {
			// identifier exists
			printf("%d: duplicate local declaration\n", line);
			exit(-1);
		}

		old_pos = pos_local;
		// store the local variable
		push_symbol_property(&symbols_table[symbol_id]);
		symbols_table[symbol_id].item->category  = C_Loc;
		symbols_table[symbol_id].item->type   = type;
		symbols_table[symbol_id].item->value  = ++pos_local;   // index of current parameter
		symbols_table[symbol_id].item->count = 0;
		symbols_table[symbol_id].item->structId = 0;

		if (basetype == STRUCT) {
			symbols_table[symbol_id].item->structId = id;
			pos_local = old_pos + align_to_int(get_size(type, struct_item));
			symbols_table[symbol_id].item->value = pos_local;
		}

		match(Id);

		if (token == Brak) {
			// array declaration
			match(Brak);
			if (token == Num && token_val > 0) {
				symbols_table[symbol_id].item->count = token_val;

				// allocate memory & align to 4 byte
				pos_local = old_pos + align_to_int(get_size(type, (int)struct_item) * (token_val));
				symbols_table[symbol_id].item->value = pos_local;
			}
			else {
				printf("%d: bad array declaration\n", line);
				exit(-1);
			}
			match(Num);
			match(']');
		}

		if (token == Assign) {
			*++cinfo.text = LEA;
			*++cinfo.text = index_of_bp - symbols_table[symbol_id].item->value;
			*++cinfo.text = LI; // this instruction will be overwritten
			expr_type = type;

			expression(Assign);
		}

		if (token == ',') {
			match(',');
		}
	}
}

void function_body() {
    // type func_name (...) {...}
    //                   -->|   |<--

    // ... {
    // 2. statements
    // }
	int *variable_num;
    pos_local = index_of_bp;

    // save the cinfo.stack size for local variables
    *++cinfo.text = ENT;
	variable_num = ++cinfo.text;

    // statements
    while (token != '}') {
        statement();
    }

	 *variable_num = pos_local - index_of_bp;

    // emit code for leaving the sub function
    *++cinfo.text = LEV;
}

void function_declaration() {
    // type func_name (...)
	int id = symbol_id, *tmp;

    match('(');
    function_parameter();
    match(')');

	if (token == ';') {
		// declaration	
		*++cinfo.text = JMP;
		*++cinfo.text = 0;
		// store jmp target address
		symbols_table[id].funcAddr = cinfo.text;
		//match(';');
	}
	else {
		// define
		match('{');

		// set jmp target address
		if (symbols_table[id].funcAddr != 0) {
			tmp = symbols_table[id].funcAddr;
			*tmp = (int)(cinfo.text + 1);
			symbols_table[id].funcAddr = 0;
		}

		function_body();
		//match('}');
	}
	// unwind local variable declarations for all local variables.
	symbol_id = 0;
	while (symbols_table[symbol_id].token) {
		if (symbols_table[symbol_id].item->category == C_Loc) {
			pop_symbol_property(&symbols_table[symbol_id]);
		}
		++symbol_id;
	}
}

void global_declaration() {
    // int [*]id [; | (...) {...}]
    int basetype, type, id = 0, struct_item = -1; // tmp, actual type for variable

    basetype = INT;

    // parse enum, this should be treated alone.
    if (token == Enum) {
        // enum [id] { a = 10, b = 20, ... }
        match(Enum);
        if (token != '{') {
            match(Id); // skip the [id] part
        }
        if (token == '{') {
            // parse the assign part
            match('{');
            enum_declaration();
            match('}');
        }

        match(';');
        return;
    }

	// parse struct, this should be treated alone
	if (token == Struct) {
		match(Struct);
		
		id = symbol_id;
		struct_item = find_struct(id);
		basetype = STRUCT;

		match(Id);

		// struct declaration
		if (token == '{') {
			match('{');
			struct_declaration(id);
			match('}');
			match(';');
			return;
		}

		if (struct_item == -1) {
			printf("%d: undefined struct type\n", line);
			exit(-1);
		}
	}

    // parse type information
	if (basetype == STRUCT) {
		// struct variables would be handled below
	}
    else if (token == Int) {
        match(Int);
    }
    else if (token == Char) {
        match(Char);
        basetype = CHAR;
    }

    // parse the comma seperated variable declaration.
    while (token != ';' && token != '}') {
        type = basetype;
        // parse pointer type, note that there may exist `int ****x;`
        while (token == Mul) {
            match(Mul);
            type += PTR;
        }

        if (token != Id) {
            // invalid declaration
            printf("%d: bad global declaration\n", line);
            exit(-1);
        }
		// symbols_new[cid].funcAddr != 0 means function declaration has been processed
        if (symbols_table[symbol_id].item->category && symbols_table[symbol_id].funcAddr == 0) {
            // identifier exists
            printf("%d: duplicate global declaration\n", line);
            exit(-1);
        }

		symbols_table[symbol_id].item->type = type;
		symbols_table[symbol_id].item->structId = (int)id;
		id = symbol_id;

		match(Id); 

        if (token == '(') {
            symbols_table[id].item->category = C_Fun;
            symbols_table[id].item->value = (int)(cinfo.text + 1); // the memory address of function
			symbols_table[id].item->count = 0;
            function_declaration();
        } 
		else if (token == Brak) {
			// array declaration
			match(Brak);
			if (token == Num && token_val >	0) {
				symbols_table[id].item->category = C_Glo; // global variable
				symbols_table[id].item->value = (int)cinfo.data; // assign memory address
				symbols_table[id].item->count = token_val;
				
				// allocate memory & align to 4 byte
				cinfo.data += get_size(type, struct_item) * (token_val);
				cinfo.data = (char*)(align_to_int((int)cinfo.data) * sizeof(int));
			}
			else {
				printf("%d: bad array declaration\n", line);
				exit(-1);
			}
			match(Num);
			match(']');
		}
		else if (token == ',' || token == ';') {
            // variable declaration
            symbols_table[id].item->category = C_Glo; // global variable
            symbols_table[id].item->value = (int)cinfo.data; // assign memory address
			symbols_table[id].item->count = 0;
			cinfo.data += get_size(type, struct_item);
			cinfo.data = (char*)(align_to_int((int)cinfo.data) * sizeof(int));
        }

        if (token == ',') {
            match(',');
        }
    }
    next();
}

void program() {
    // get next token
    next();
    while (token > 0) {
        global_declaration();
    }
}

int eval(struct VirtualMachineState *vm) {
    int *pc = vm->pc, *bp = vm->bp, *sp = vm->sp;
	int ax = 0, cycle = 0;
	int op, *tmp;

    while (1) {
        cycle ++;
        op = *pc++; // get next operation code

        // print debug info
        if (debug) {
            printf("%d> %.4s", cycle,
                   & "LEA ,IMM ,JMP ,CALL,JZ  ,JNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PUSH,"
                   "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
                   "OPEN,READ,CLOS,PRTF,MALC,MSET,MCMP,MCPY,EXIT"[op * 5]);
            if (op <= ADJ)
                printf(" %d\n", *pc);
            else
                printf("\n");
			printf("pc: %d ax:%d cinfo.stack:%d\n", (int)pc, ax, *sp);

			if (pc - cinfo.dinfo.text_head < 64 * 1024) {
				printf("line: %d\n", cinfo.dinfo.debug_line[pc - cinfo.dinfo.text_head]);
			}
        }
        if (op == IMM)       {ax = *pc++;}                                     // load immediate value to ax
        else if (op == LC)   {ax = *(char *)ax;}                               // load character to ax, address in ax
        else if (op == LI)   {ax = *(int *)ax;}                                // load integer to ax, address in ax
        else if (op == SC)   {*(char *)*sp++ = ax;}                       // save character to address, value in ax, address on cinfo.stack
        else if (op == SI)   {*(int *)*sp++ = ax;}                             // save integer to address, value in ax, address on cinfo.stack
        else if (op == PUSH) {*--sp = ax;}                                     // push the value of ax onto the cinfo.stack
        else if (op == JMP)  {pc = (int *)*pc;}                                // jump to the address
        else if (op == JZ)   {pc = ax ? pc + 1 : (int *)*pc;}                   // jump if ax is zero
        else if (op == JNZ)  {pc = ax ? (int *)*pc : pc + 1;}                   // jump if ax is zero
        else if (op == CALL) {*--sp = (int)(pc+1); pc = (int *)*pc;}           // call subroutine
        //else if (op == RET)  {pc = (int *)*sp++;}                              // return from subroutine;
        else if (op == ENT)  {*--sp = (int)bp; bp = sp; sp = sp - *pc++;}      // make new cinfo.stack frame
        else if (op == ADJ)  {sp = sp + *pc++;}                                // add esp, <size>
        else if (op == LEV)  {sp = bp; bp = (int *)*sp++; pc = (int *)*sp++;}  // restore call frame and PC
        else if (op == LEA)  {ax = (int)(bp + *pc++);}                         // load address for arguments.

        else if (op == OR)  ax = *sp++ | ax;
        else if (op == XOR) ax = *sp++ ^ ax;
        else if (op == AND) ax = *sp++ & ax;
        else if (op == EQ)  ax = *sp++ == ax;
        else if (op == NE)  ax = *sp++ != ax;
        else if (op == LT)  ax = *sp++ < ax;
        else if (op == LE)  ax = *sp++ <= ax;
        else if (op == GT)  ax = *sp++ >  ax;
        else if (op == GE)  ax = *sp++ >= ax;
        else if (op == SHL) ax = *sp++ << ax;
        else if (op == SHR) ax = *sp++ >> ax;
        else if (op == ADD) ax = *sp++ + ax;
        else if (op == SUB) ax = *sp++ - ax;
        else if (op == MUL) ax = *sp++ * ax;
        else if (op == DIV) ax = *sp++ / ax;
        else if (op == MOD) ax = *sp++ % ax;

        else if (op == EXIT) { printf("exit(%d)", *sp); return *sp;}
        else if (op == OPEN) { ax = open((char *)sp[1], sp[0]); }
        else if (op == CLOS) { ax = close(*sp);}
        else if (op == READ) { ax = read(sp[2], (char *)sp[1], *sp); }
        else if (op == PRTF) { tmp = sp + pc[1]; ax = printf((char *)tmp[-1], tmp[-2], tmp[-3], tmp[-4], tmp[-5], tmp[-6]); }
        else if (op == MALC) { ax = (int)malloc(*sp);}
        else if (op == MSET) { ax = (int)memset((char *)sp[2], sp[1], *sp);}
        else if (op == MCMP) { ax = memcmp((char *)sp[2], (char *)sp[1], *sp);}
		else if (op == MCPY) { ax = (int)memcpy((char *)sp[2], (char *)sp[1], *sp);}
		else if (op == FREE) { free((void*)*sp);}
        else {
            printf("unknown instruction:%d\n", op);
			printf("line: %d\n", cinfo.dinfo.debug_line[pc - cinfo.dinfo.text_head]);
            return -1;
        }
    }
}

int main(int argc, char **argv)
{
    int i, fd, *tmp, idmain, poolsize;
	struct VirtualMachineState vm;

    argc--;
    argv++;

    // parse arguments
    if (argc > 0 && **argv == '-' && (*argv)[1] == 's') {
        assembly = 1;
        --argc;
        ++argv;
    }
    if (argc > 0 && **argv == '-' && (*argv)[1] == 'd') {
        debug = 1;
        --argc;
        ++argv;
    }
    if (argc < 1) {
        printf("usage: xc [-s] [-d] file ...\n");
        return -1;
    }
	//assembly = 1;
	//debug = 1;

    if ((fd = open(*argv, 0)) < 0) {
        printf("could not open(%s)\n", *argv);
        return -1;
    }

    poolsize = 256 * 1024; // arbitrary size
    line = 1;

    // allocate memory
    if (!(cinfo.text = malloc(poolsize))) {
        printf("could not malloc(%d) for cinfo.text area\n", poolsize);
        return -1;
    }
    if (!(cinfo.data = malloc(poolsize))) {
        printf("could not malloc(%d) for cinfo.data area\n", poolsize);
        return -1;
    }
    if (!(cinfo.stack = malloc(poolsize))) {
        printf("could not malloc(%d) for cinfo.stack area\n", poolsize);
        return -1;
    }
	if (!(cinfo.dinfo.debug_line = malloc(poolsize))) {
		printf("could not malloc(%d) for debug line\n", poolsize);
		return -1;
	}

    memset(cinfo.text, 0, poolsize);
    memset(cinfo.data, 0, poolsize);
    memset(cinfo.stack, 0, poolsize);
	memset(cinfo.dinfo.debug_line, 0, poolsize);
	memset(symbols_table, 0, sizeof(struct Symbol) * 1000);
	memset(structs_table, 0, sizeof(struct StructHeader) * 64);
	
	cinfo.dinfo.text_head = cinfo.text;
	cinfo.dinfo.last_text = 0;

	struct_id = 0;

    cinfo.dinfo.old_text = cinfo.text;

    src = "char else enum if int return sizeof struct while "
          "open read close printf malloc memset memcmp memcpy free exit void main";

     // add keywords to symbol table
    i = Char;
    while (i <= While) {
        next();
        symbols_table[symbol_id].token = i++;
    }

    // add library to symbol table
    i = OPEN;
    while (i <= EXIT) {
        next();
        symbols_table[symbol_id].item->category = C_Sys;
        symbols_table[symbol_id].item->type = INT;
        symbols_table[symbol_id].item->value = i++;
		symbols_table[symbol_id].item->count = 0;
    }

    next(); symbols_table[symbol_id].token = Char; // handle void type
    next(); idmain = symbol_id; // keep track of main

    if (!(src = old_src = malloc(poolsize))) {
        printf("could not malloc(%d) for source area\n", poolsize);
        return -1;
    }
    // read the source file
    if ((i = read(fd, src, poolsize-1)) <= 0) {
        printf("read() returned %d\n", i);
        return -1;
    }
    src[i] = 0; // add EOF character
    close(fd);

    program();

    if (!(vm.pc = (int *)symbols_table[idmain].item->value)) {
        printf("main() not defined\n");
        return -1;
    }

    // dump_text();
    if (assembly) {
        // only for compile
        return 0;
    }

    // setup cinfo.stack
    vm.sp = (int *)((int)cinfo.stack + poolsize);
    *--vm.sp = EXIT; // call exit if main returns
    *--vm.sp = PUSH; tmp = vm.sp;
    *--vm.sp = argc;
    *--vm.sp = (int)argv;
    *--vm.sp = (int)tmp;

    return eval(&vm);
}