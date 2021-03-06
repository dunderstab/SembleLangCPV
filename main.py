import ply.lex as lex
import ply.yacc as yacc
import os
import warning

OUTPUT_FILE = "main"
Lerrors = False

warnings.simplefilter("ignore")

# VERSIONS

currentVersion = "1.1.2"
Beta = "beta"
Author = "Riley"
Name = "SembleLang"
#Description = " A Langauge designed for speed and simplicity. With accuracy and flexibility in mind I hand-crafted this language from the ground up to provide an easy way to get into high-speed coding."
Description = "\n"

# CODE

reserved = {
    "return": "RETURN",
    "def": "DEF",
    "if": "IF",
    "else": "ELSE",
    "while": "WHILE",
    "for": "FOR",
    "break": "BREAK",
    "var": "VAR",
    "const": "CONST",
    "is_arr": "ISARR",
    "len_arr":"ARRLEN",
}

tokens = [
    "PLUS",
    "MINUS",
    "MUL",
    "DIV",
    "EQUALS",
    "NAME",
    "INT",
    "SEMICOLON",
    "LBRAC",
    "RBRAC",
    "LPAREN",
    "RPAREN",
    "COMMA",
    "STRING",
    "NOT",
    "AND",
    "AND2",
    "OR2",
    "DPLUS",
    "DMINUS",
    "OR",
    "DNE",
    "EQ",
    "GRT",
    "GRTE",
    "LT",
    "LTE",
    "LBRACE",
    "RBRACE",
    "COLON"
,] + list(reserved.values())

t_PLUS = r"\+"
t_MINUS = r"\-"
t_DPLUS = r"\+\+"
t_DMINUS = r"\-\-"
t_MUL = r"\*"
t_DIV = r"\/"
t_EQUALS = r"\="
t_SEMICOLON = r"\;"
t_LBRAC = r"\{"
t_RBRAC = r"\}"
t_LPAREN = r"\("
t_RPAREN = r"\)"
t_LBRACE = r"\["
t_RBRACE = r"\]"
t_COLON = r"\:"
t_COMMA = r"\,"

t_AND2 = r"\&\&"
t_AND = r'\&'

t_OR2 = r"\|\|"
t_OR = r'\|'

t_NOT = r"\!"

t_EQ = r"\=\="
t_GRT = r"\>"

GRTE1 = r"\>\="
GRTE2 = r'\=\>'
t_GRTE = r'(' + GRTE1 + r'|' + GRTE2 + r')'

t_LT = r"\<"

LTE1 = r"\<\="
LTE2 = r'\=\<'
t_LTE = r'(' + LTE1 + r'|' + LTE2 + r')'

t_DNE = r"\!\="

t_ignore = r" "

precedence = (
    ("left", "AND2", "OR2"),
    ("left", "NOT"),
    ("left", "GRT", "EQ", "GRTE", "LT", "LTE", "DNE"),
    ("left", "AND", "OR"),
    ("left", "PLUS", "MINUS", "DPLUS", "DMINUS"),
    ("left", "MUL", "DIV")
)

class SEMBLE_ARRAY:
    def __init__(self, len, vals):
        self.len = len
        self.vals = vals
    def __repr__(self):
        return "SEMBLE_ARRAY(len={}, vals={});".format(self.len, self.vals)
    def __str__(self):
        return "SEMBLE_ARRAY(len={}, vals={});".format(self.len, self.vals)


def t_STRING(t):
    r'\"[^\"]*\"'
    t.value = str(t.value[1:][:-1])
    return t

def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'NAME')    # Check for reserved words
    return t

def t_INT(t):
    r'\d+'
    t.value = int(t.value)
    return t


def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

lex = lex.lex()

def p_code(p):
    '''
    code : function
         | code code
    '''
    eval(p[1])

def p_functionnoargs(p):
    '''
    function : DEF NAME LPAREN RPAREN statement
             | DEF NAME LPAREN RPAREN statementc
    '''
    p[0] = ("function", p[5], p[2], [], False)

def p_functiononearg(p):
    '''
    function : DEF NAME LPAREN NAME RPAREN statement
             | DEF NAME LPAREN NAME RPAREN statementc
    '''
    p[0] = ("function", p[6], p[2], [p[4]], True)

def p_functionmanyargs(p):
    '''
    function : DEF NAME LPAREN args RPAREN statement
             | DEF NAME LPAREN args RPAREN statementc
    '''
    p[0] = ("function", p[6], p[2], p[4], False)

def p_statement_if(p):
    '''
    statement : IF LPAREN expression RPAREN statementc
              | IF LPAREN expression RPAREN statement
    '''
    p[0] = ("if", p[3], p[5])

def p_statement_ifelse(p):
    '''
    statement : IF LPAREN expression RPAREN statementc ELSE statementc
                | IF LPAREN expression RPAREN statement ELSE statement
                | IF LPAREN expression RPAREN statementc ELSE statement
                | IF LPAREN expression RPAREN statement ELSE statementc
    '''
    p[0] = ("ifelse", p[3], p[5], p[7])

def p_statement_for(p):
  '''
  statement : FOR LPAREN statement expression SEMICOLON statement RPAREN statementc
            | FOR LPAREN statement expression SEMICOLON statement RPAREN statement
  '''
  p[0] = ("for", p[4], p[3], p[6], p[8])
  


def p_statement_while(p):
  '''
  statement : WHILE LPAREN expression RPAREN statementc
            | WHILE LPAREN expression RPAREN statement
  '''
  p[0] = ("while", p[3], p[5])



def p_statement_dplus(p):
    '''
    statement : NAME DPLUS SEMICOLON
                | NAME DMINUS SEMICOLON
    '''
    #print(p)
    p[0] = (p[2] + "s", p[1])


def p_statement_pdplus(p):
  '''
  statement : DPLUS NAME SEMICOLON
            | DMINUS NAME SEMICOLON
  '''
  p[0] = (p[1] + "s", p[2])


def p_expression_dplus(p):
  '''
  expression : NAME DPLUS
             | NAME DMINUS
  '''
  p[0] = (p[2], p[1])

def p_expression_dplusp(p):
  '''
  expression : DPLUS NAME
             | DMINUS NAME
  '''
  p[0] = (p[1] + "p", p[2])

def unpack(l):
    x = l[0]
    y = l[1]
    if type(x) == list:
        print(unpack(x) + [y])
        return unpack(x) + [y]
    else:
        return l


def p_args(p):
  '''
  args : args COMMA NAME
  '''
  #print("!!! {} ".format(p[1]))
  p[0] = unpack(p[1]) + [p[3]]

def p_argso(p):
  '''
  args : NAME COMMA NAME
  '''
  p[0] = [p[1], p[3]]

def p_argse(p):
  '''
  argse : argse COMMA expression
  '''
  #print("!!! {} ".format(p[1]))
  p[0] = unpack(p[1]) + [p[3]]

def p_statement_break(p):
  '''
  statement : BREAK SEMICOLON
  '''
  
  p[0] = ("break", p[1])

def p_argseo(p):
  '''
  argse : expression COMMA expression
  '''
  p[0] = [p[1], p[3]]


def p_st_fcallna(p):
    '''
    statement : NAME LPAREN RPAREN SEMICOLON
    '''
    p[0] = ("fcall", p[1], False)

def p_st_fcall(p):
    '''
    statement : NAME LPAREN argse RPAREN SEMICOLON
    '''
    p[0] = ("fcall", p[1], p[3])

def p_st_fcalla(p):
    '''
    statement : NAME LPAREN expression RPAREN SEMICOLON
    '''
    p[0] = ("fcall", p[1], [p[3]])


def p_statements(p):
    '''
    statements : statement statements
               | statement statement
    '''
    p[0] = ("statements", p[1], p[2])

def p_statementc(p):
    '''
    statementc : LBRAC statement RBRAC
               | LBRAC statements RBRAC
               | LBRAC empty RBRAC
    '''
    p[0] = ("statementc", p[2])

def p_expression_arr(p):
    '''
    expression : expression COLON LBRACE argse RBRACE
    '''
    p[0] = SEMBLE_ARRAY(p[1], p[4])

def p_expression_arrone(p):
    '''
    expression : expression COLON LBRACE expression RBRACE
    '''
    p[0] = SEMBLE_ARRAY(p[1], [p[4]])

def p_expression_arrno(p):
    '''
    expression : expression COLON LBRACE RBRACE
    '''
    p[0] = SEMBLE_ARRAY(p[1], None)

def p_expression_arra(p):
    '''
    expression : LBRACE argse RBRACE
    '''
    p[0] = SEMBLE_ARRAY(len(p[2]), p[2])

def p_expression_arronea(p):
    '''
    expression : LBRACE expression RBRACE
    '''
    p[0] = SEMBLE_ARRAY(1, [p[2]])

def p_expression_arrnoa(p):
    '''
    expression : LBRACE RBRACE
    '''
    p[0] = SEMBLE_ARRAY(1, None)


def p_statementvardef(p):
    '''
    statement : VAR NAME EQUALS expression SEMICOLON
    '''
    p[0] = ("def", p[2], p[4])

def p_statementvup(p):
    '''
    statement : NAME EQUALS expression SEMICOLON
    '''
    p[0] = ("vup", p[1], p[3])

def p_statement_indexassign(p):
  '''
  statement : expression LBRACE expression RBRACE EQUALS expression SEMICOLON
  '''
  p[0] = ("indexingassign", p[1], p[3], p[6])



def p_statementreturn(p):
    '''
    statement : RETURN expression SEMICOLON
    '''
    p[0] = ("return", p[2])

def p_expression_basic(p):
    '''
    expression : INT
               | NAME
    '''
    p[0] = p[1]

def p_expression_indexing(p):
    '''
    expression : expression LBRACE expression RBRACE
    '''
    p[0] = ("indexing", p[1], p[3])

def p_expression_str(p):
  '''
  expression : STRING
  '''
  p[0] = Str(p[1])

def p_expression_unary(p):
    '''
    expression : NOT expression
    '''
    p[0] = (p[1], p[2])

def p_expression_carr(p):
    '''
    expression : ISARR LPAREN expression RPAREN
    '''
    p[0] = ("isarr", p[3])

def p_expression_larr(p):
    '''
    expression : ARRLEN LPAREN expression RPAREN
    '''
    p[0] = ("arrlen", p[3])



def p_e_fcallna(p):
    '''
    expression : NAME LPAREN RPAREN 
    '''
    p[0] = ("efcall", p[1], False)

def p_e_fcall(p):
    '''
    expression : NAME LPAREN argse RPAREN
    '''
    p[0] = ("efcall", p[1], p[3])

def p_e_fcalla(p):
    '''
    expression : NAME LPAREN expression RPAREN
    '''
    p[0] = ("efcall", p[1], [p[3]])







def p_expression_primitive(p):
    '''
    expression : expression MUL expression
               | expression DIV expression
               | expression PLUS expression
               | expression MINUS expression
               | expression AND expression
               | expression AND2 expression
               | expression OR2 expression
               | expression OR expression
               | expression DNE expression
               | expression EQ expression
               | expression GRT expression
               | expression GRTE expression
               | expression LT expression
               | expression LTE expression
    '''
    p[0] = (p[2], p[1], p[3])





def p_empty(p):
    '''
    empty :
    '''
    p[0] = None

def p_error(p):
    if p is None:
        print("Semble: Parsing Error: Syntax error at End of Line")
        e = yacc.YaccSymbol()
        e.type = 'error'
        e.value = None
        yacc.errok()
        exit(1)
    elif p.type == 'error':
        print(p)
        warnings.warn("Semble: Parsing Error: YACC Parser failed")
        yacc.errok()
        return
    elif hasattr(p, 'value'):
        if hasattr(p, 'lineno'):
            print("Semble: Parsing Error: Syntax error at '{}'. Line {}".format(p.value, p.lineno))
        else:
            print("Semble: Parsing Error: Syntax error at '%s'" % p.value)
        e = yacc.YaccSymbol()
        e.type = 'error'
        e.value = p.value
        yacc.errok()
        exit(1)

class Str:
    def __init__(self, value):
        self.value = value
    def __repr__(self):
        return "<Str = {}>".format(self.value)
    def __str__(self):
        return "<Str = {}>".format(self.value)

parser = yacc.yacc()

currentFunc = "main"

funcs = {
}
funcVars = {}
funcArgs = {}
MPN = 0
data = {}
currentFuncVarVal = 4
funcVarCount = {}

jumpStack = []
globalvars = {}

def grabTup(tup):
  x = []
  for i in tup:
    if type(i) == tuple or type(i) == list:
      x += grabTup(i)
    else:
      x.append(i)
  return x


def Reverse(lst):
    return [ele for ele in reversed(lst)]


def asm(s):
    try:
        funcs[currentFunc].append(s)
    except:
        e_error("Must be inside a function to execute code!")

def e_error(e):
  quit("Semble: EvalError: {}".format(e))

def eval(p):
    global currentFunc, funcVars, funcVarCount, currentFuncVarVal, Lerrors, data, MPN, jumpStack, globalvars, _ADD_START_LINES
    if Lerrors:
        quit("Lexing error!")
    if p == None:
        return

    
    if type(p) != tuple:
        if type(p) == int:
            asm("movl ${}, %ecx\n".format(p))
            return p
        elif type(p) == Str:
            x = p.value
            data["STRlock{}".format(MPN)] = ".asciz \"{}\"".format(x)
            asm("movl ${}, %ecx".format("STRlock{}".format(MPN)))
            MPN += 1
            return 'str'

        elif type(p) == str:
            if p in funcVars:
                asm("movl {}, %ecx".format(funcVars[p]))
            else:
                e_error("Variable '{}' referenced before assignment!".format(p))
            return [p]
        elif type(p) == SEMBLE_ARRAY:
            eval(("efcall", "malloc", [("+", ("*", p.len, 4), 8)]))
            asm("movl %ecx, %edi")
            
            eval(Str("_!_SLA"))
            asm("movl %ecx, {}(%edi)".format(4))
            eval(p.len)
            asm("movl %ecx, {}(%edi) # DJJJJ".format(8))
            if p.vals != None:
                count = 12
                print(p.vals)
                for ele in p.vals:
                    asm("pushl %edi")
                    eval(ele)
                    asm("popl %edi")
                    asm("movl %ecx, {}(%edi)".format(count))
                    count += 4
            asm("movl %edi, %ecx")
            return




    if p[0] == "function":
        funcVars = {}

        currentFuncVarVal = 4
        funcVarCount[p[2]] = 0
        currentFunc = p[2]
        funcs[p[2]] = []
        a = 8
        ini = False
        for arg in grabTup(p[3]):
          ini = True
          funcVars[arg] = "{}(%ebp)".format(a)
          a += 4
        if ini:
          funcArgs[currentFunc] = a - 4
        else:
          funcArgs[currentFunc] = 8          
        eval(p[1])

        

    
    
    elif p[0] == "farri":
        eval(p[1])
        asm("pushl %ecx")
        eval(0)
        asm("popl %edx")
        asm("movl (%edx,%ecx, 4), %ebx")
        asm("movl %ebx, %ecx")


    elif p[0] == "isarr":
        eval(("+", ("efcall", "strcmp", [Str("_!_SLA"), ("farri", p[1])]), 1))
    
    elif p[0] == "arrlen":
        eval(p[1])
        asm("pushl %ecx")
        eval(1)
        asm("popl %edx")
        asm("movl (%edx,%ecx, 4), %ebx")
        asm("movl %ebx, %ecx")


    elif p[0] == "statementc":
        eval(p[1])


    elif p[0] == "statements":
        eval(p[1])
        eval(p[2])

    elif p[0] == "fcall":
        n = 0
        for i in Reverse(p[2]):
            n += 1
            eval(i)
            asm("pushl %ecx")
        #if p[1] in funcs.keys():
        asm("call {}".format(p[1]))
        for i in range(n):
            asm("popl %ebx")
       # else:
         #   e_error("Function Error: Undefined function {}".format(p[1]))
    
    elif p[0] == "return":
        eval(p[1])
        asm("movl %ecx, %eax")
        asm("jmp .l{}".format(currentFunc))

        

    elif p[0] == "ifelse":
        eval(p[1])
        asm("cmpl $1, %ecx")
        v = MPN
        MPN += 1
        asm("jne .ne{}".format(v))
        
        eval(p[2])
        a = MPN
        MPN += 1
        asm("jmp .ne{}".format(a))


        asm(".ne{}:".format(v))
        eval(p[3])
        asm(".ne{}:".format(a))
        

        

        eval(p[1])


    elif p[0] == "break":
        if jumpStack != []:
            x = jumpStack[-1]

            asm("jmp {} # break".format(x))

            del jumpStack[-1]
        else:
            e_error("Break condition in non-loop!")



    elif p[0] == "if":
        eval(p[1])
        asm("cmpl $1, %ecx")
        a = MPN
        MPN += 1
        asm("jne .ne{}".format(a))
        eval(p[2])

        asm(".ne{}:".format(a))

    elif p[0] == "for":
        eval(p[2])
        eval(p[1])
        asm("cmpl $1, %ecx")
        a = MPN
        MPN += 1
        asm("jne .ne{}".format(a))

        d = MPN
        MPN += 1
        asm(".ne{}:".format(d))
        jumpStack.append(".ne{}".format(a))
        
        eval(p[4])


        eval(p[3])
        eval(p[1])
        asm("cmpl $1, %ecx")
        asm("jne .ne{}".format(a))
        asm("jmp .ne{}".format(d))
        asm(".ne{}:".format(a))

    elif p[0] == "while":
        eval(p[1])
        asm("cmpl $1, %ecx")
        a = MPN
        MPN += 1
        asm("jne .ne{}".format(a))

        d = MPN
        MPN += 1
        asm(".ne{}:".format(d))
        jumpStack.append(".ne{}".format(a))
        
        eval(p[2])



        eval(p[1])
        asm("cmpl $1, %ecx")
        asm("jne .ne{}".format(a))
        asm("jmp .ne{}".format(d))
        asm(".ne{}:".format(a))
    elif p[0] == "vup":
        if p[1] in funcVars:
            x = funcVars[p[1]]
            t = eval(p[2])
            asm("movl %ecx, {} # variable {}".format(x, p[1]))
        else:
            e_error("Unknown Variable ?")
    
    elif p[0] == "indexingassign":
      print("pp {}".format(p))
      eval(p[1])
      asm("pushl %ecx")
      eval(p[2])
      asm("pushl %ecx")
      eval(p[3])
      asm("movl %ecx, %esi")
      asm("popl %ecx")
      asm("addl $2, %ecx")
      asm("popl %edx")
      asm("movl %esi, (%edx,%ecx, 4)")

    elif p[0] == "indexing":
        print(p[1])
        print(p[2])
        eval(p[1])
        asm("pushl %ecx")
        eval(p[2])
        asm("popl %edx")
        asm("addl $2, %ecx")
        asm("movl (%edx,%ecx, 4), %ebx")
        asm("movl %ebx, %ecx")


    elif p[0] == "def":
        if p[1] in funcVars:
            e_error("Variable ? already defined!")
        else:
            t = eval(p[2])
            x = "-{}(%ebp)".format(currentFuncVarVal)
            asm("movl %ecx, {} # variable {}".format(x, p[1]))
            funcVars[p[1]] = x
            funcVarCount[currentFunc] += 4
            currentFuncVarVal += 4
    elif p[0] == "+":
        if type(p[1]) == int and type(p[2]) == int:
            asm("movl ${}, %ecx".format(p[1] + p[2])) 
        else:
            eval(p[1])
            asm("movl %ecx, %edx")
            asm("pushl %edx")
            eval(p[2])
            asm("popl %edx")
            asm("addl %ecx, %edx")
            asm("movl %edx, %ecx")
    elif p[0] == "-":
        if type(p[1]) == int and type(p[2]) == int:
            asm("movl ${}, %ecx".format(p[1] - p[2]))
        else:
            eval(p[1])
            asm("movl %ecx, %edx")
            asm("pushl %edx")
            eval(p[2])
            asm("popl %edx")
            asm("subl %ecx, %edx")
            asm("movl %edx, %ecx")
    elif p[0] == "*":
        if type(p[1]) == int and type(p[2]) == int:
            asm("movl ${}, %ecx".format(p[1] * p[2]))
        else:
            eval(p[1])
            asm("movl %ecx, %edx")
            asm("pushl %edx")
            eval(p[2])
            asm("popl %edx")
            asm("imull %ecx, %edx")
            asm("movl %edx, %ecx")
    elif p[0] == "&":
        if type(p[1]) == int and type(p[2]) == int:
            asm("movl ${}, %ecx".format(p[1]))
            asm("andl ${}, %ecx".format(p[2]))
        else:
            eval(p[1])
            asm("movl %ecx, %edx")
            asm("pushl %edx")
            eval(p[2])
            asm("popl %edx")
            asm("andl %ecx, %edx")
            asm("movl %edx, %ecx")

    elif p[0] == "++":
        if p[1] in funcVars:
            asm("movl {}, %ecx".format(funcVars[p[1]]))
            asm("incl {}".format(funcVars[p[1]]))
        else:
            e_error("Variable '{}' referenced before assignment!".format(p[1]))
    
    elif p[0] == "--":
        if p[1] in funcVars:
            asm("movl {}, %ecx".format(funcVars[p[1]]))
            asm("decl {}".format(funcVars[p[1]]))
        else:
            e_error("Variable '{}' referenced before assignment!".format(p[1]))

    elif p[0] == "++s":
        if p[1] in funcVars:
            asm("incl {}".format(funcVars[p[1]]))
        else:
            e_error("Variable '{}' referenced before assignment!".format(p[1]))

    elif p[0] == "--s":
        if p[1] in funcVars:
            asm("decl {}".format(funcVars[p[1]]))
        else:
            e_error("Variable '{}' referenced before assignment!".format(p[1]))

    elif p[0] == "++p":
        if p[1] in funcVars:
            asm("incl {}".format(funcVars[p[1]]))
            asm("movl {}, %ecx".format(funcVars[p[1]]))
        else:
            e_error("Variable '{}' referenced before assignment!".format(p[1]))

    elif p[0] == "--p":
        if p[1] in funcVars:
            asm("decl {}".format(funcVars[p[1]]))
            asm("movl {}, %ecx".format(funcVars[p[1]]))
        else:
            e_error("Variable '{}' referenced before assignment!".format(p[1]))

    elif p[0] == "&&":
        #print(p)
        eval(p[1])
        asm("movl %ecx, %edx")
        asm("pushl %edx")
        eval(p[2])
        asm("popl %edx")
        asm("cmpl $1, %edx")
        v = MPN
        MPN += 1
        asm("jne .ne{}".format(v))
        asm("cmpl $1, %ecx")
        asm("jne .ne{}".format(v))
        asm("jmp .ne{}".format(MPN))
        asm(".ne{}:".format(v))
        asm("movl $0, %ecx")
        asm(".ne{}:".format(MPN))
        MPN += 1
    elif p[0] == "||":
        #print(p)
        eval(p[1])
        asm("movl %ecx, %edx")
        asm("pushl %edx")
        eval(p[2])
        asm("popl %edx")
        asm("cmpl $1, %edx")
        v = MPN
        MPN += 1
        a = MPN
        MPN += 1
        asm("je .ne{}".format(a))
        asm("cmpl $1, %ecx")
        asm("je .ne{}".format(a))
        asm("movl $0, %ecx")
        asm("jmp .ne{}".format(v))
        asm(".ne{}:".format(a))
        asm("movl $1, %ecx")
        asm(".ne{}:".format(v))
        MPN += 1
    elif p[0] == "|":
        if type(p[1]) == int and type(p[2]) == int:
            asm("movl ${}, %ecx".format(p[1]))
            asm("orl ${}, %ecx".format(p[2]))
        else:
            eval(p[1])
            asm("movl %ecx, %edx")
            asm("pushl %edx")
            eval(p[2])
            asm("popl %edx")
            asm("orl %ecx, %edx")
            asm("movl %edx, %ecx")

    elif p[0] == "==":
        eval(p[1])
        asm("movl %ecx, %edx")
        asm("pushl %edx")
        eval(p[2])
        asm("popl %edx")
        asm("cmpl %ecx, %edx")
        asm("jne .ne{}".format(MPN))
        v = MPN
        MPN += 1
        asm("movl $1, %ecx")
        asm("jmp .ne{}".format(MPN))
        asm(".ne{}:".format(v))
        asm("movl $0, %ecx")
        asm(".ne{}:".format(MPN))
        MPN += 1
    elif p[0] == "!=":
        eval(p[1])
        asm("movl %ecx, %edx")
        asm("pushl %edx")
        eval (p[2])
        asm("popl %edx")
        asm("cmpl %ecx, %edx")
        asm("je .ne{}".format(MPN))
        v = MPN
        MPN += 1
        asm("movl $1, %ecx")
        asm("jmp .ne{}".format(MPN))
        asm(".ne{}:".format(v))
        asm("movl $0, %ecx")
        asm(".ne{}:".format(MPN))
        MPN += 1

    elif p[0] == ">":
        eval(p[1])
        asm("movl %ecx, %edx")
        asm("pushl %edx")
        eval(p[2])
        asm("popl %edx")
        asm("cmpl %ecx, %edx")
        asm("jle .ne{}".format(MPN))
        v = MPN
        MPN += 1
        asm("movl $1, %ecx")
        asm("jmp .ne{}".format(MPN))
        asm(".ne{}:".format(v))
        asm("movl $0, %ecx")
        asm(".ne{}:".format(MPN))
        MPN += 1

    elif p[0] == ">=" or p[0] == "=>": 
        eval(p[1])
        asm("movl %ecx, %edx")
        asm("pushl %edx")
        eval(p[2])
        asm("popl %edx")
        asm("cmpl %ecx, %edx")
        asm("jl .ne{}".format(MPN))
        v = MPN
        MPN += 1
        asm("movl $1, %ecx")
        asm("jmp .ne{}".format(MPN))
        asm(".ne{}:".format(v))
        asm("movl $0, %ecx")
        asm(".ne{}:".format(MPN))
        MPN += 1

    elif p[0] == "<":
        eval(p[1])
        asm("movl %ecx, %edx")
        asm("pushl %edx")
        eval(p[2])
        asm("popl %edx")
        asm("cmpl %ecx, %edx")
        asm("jge .ne{}".format(MPN))
        v = MPN
        MPN += 1
        asm("movl $1, %ecx")
        asm("jmp .ne{}".format(MPN))
        asm(".ne{}:".format(v))
        asm("movl $0, %ecx")
        asm(".ne{}:".format(MPN))
        MPN += 1

    elif p[0] == "<=" or p[0] == "=<": 
        eval(p[1])
        asm("movl %ecx, %edx")
        asm("pushl %edx")
        eval(p[2])
        asm("popl %edx")
        asm("cmpl %ecx, %edx")
        asm("jg .ne{}".format(MPN))
        v = MPN
        MPN += 1
        asm("movl $1, %ecx")
        asm("jmp .ne{}".format(MPN))
        asm(".ne{}:".format(v))
        asm("movl $0, %ecx")
        asm(".ne{}:".format(MPN))
        MPN += 1
    
    elif p[0] == "!":
        eval(p[1])
        asm("notl %ecx")
    
    elif p[0] == "efcall":
        n = 0
        for i in Reverse(p[2]):
            n += 1
            eval(i)
            asm("pushl %ecx")
        #if p[1] in funcs.keys():
        asm("call {}".format(p[1]))
        for i in range(n):
            asm("popl %ebx")
        asm("movl %eax, %ecx")


    
    
imports = []

def line_wrap(str, width=25):
    strs = []
    nstr = ""
    for i in str:
        nstr = nstr + i
        if len(nstr) == width:
            strs.append(nstr)
            nstr = ""
    if nstr != "":
        strs.append(nstr)
    return strs
def code(fi):
    codeStr = ""
    with open(fi, "r") as file:
        
        for line in file:
            if line.startswith("#include"):
                line = line.replace("\n", "")
                l = line.replace("#include ", "")
                if l not in imports:
                    codeStr += " " + code("lib/{}.smb".format(l))
                    imports.append(l)
            line = line.strip().replace("\t", "") + "\n"
            codeStr += line + " "
    return codeStr


codeStr = code("main.smb").replace("\n", "")
parser.parse(codeStr)

lex.input(codeStr)
while True:
  tok = lex.token()
  if not tok:
    break
  else:
    with open("lex.out", "a") as lexout:
      lexout.write(str(tok) + "\n")

def addListStr(list, delimiter=""):
    x = ""
    for i in list:
        x += i + delimiter
    return x

def cmpf():
    global currentVersion, Beta, Author, Name, Description, data, _ADD_START_LINES
    _startInclude = True
    if "main" not in funcs.keys():
        _startInclude = False
    with open("main.s", "w") as file:
        file.write("\n.section .data\n\n\n")
        for i in data:
            file.write('\n{}:'.format(i))
            file.write("\n\t{}".format(data[i]))
        #COMMENTS
        file.write("# FILE GENERATED BY SEMBLELANG" + "\n")
        file.write("#                             " + "\n")
        file.write("# {} version {}{}".format(Name, Beta, currentVersion) + "\n")
        file.write("# By {}".format(Author) + "\n")
        for str in line_wrap(Description, width=41):
            file.write("# " + str + "\n")
        


        file.write("\n\n\n.section .text\n\n\n")
        #for i in data:
        #    file.write('\n{}:'.format(i))
        #    file.write("\n\t{}".format(data[i]))
        #    file.write("\n.section .text\n\n\n")
        #    if '_start' not in funcs.keys():
        #        file.write("\n.globl _start\n\n_start:\n\tcall main\n\tmovl %eax, %ebx\n\tmovl $1, %eax\n\tint $0x80")
        #
        #    file.write("\n\n\n")
    
        if '_start' not in funcs.keys() and _startInclude:
            file.write("\n.globl _start\n\n_start:\n\t\n\tcall main\n\tmovl %eax, %ebx\n\tmovl $1, %eax\n\tint $0x80")
        for func in funcs:
            if func not in ['_start']:
                file.write("\n.type {}, @function".format(func))
            file.write("\n" + func + ":")
            if func not in ['_start']:
                file.write("\n\tpushl %ebp")
                file.write("\n\tmovl %esp, %ebp")
                if funcVarCount[func] != 0:
                    file.write("\n\tsubl ${}, %esp".format(funcVarCount[func]))
            for line in funcs[func]:
                file.write("\n\t" + line)
            if func not in ['_start']:
                file.write("\n\t.l{}:".format(func))
                file.write("\n\tmovl %ebp, %esp\n\tpopl %ebp") 
                file.write("\n\tret")
            file.write("\n")
    os.system("as --32 main.s -o main.o")
    os.system("ld -m elf_i386 -dynamic-linker /lib/ld-linux.so.2 main.o -o {} -lc && rm main.o".format(OUTPUT_FILE))

cmpf()

    