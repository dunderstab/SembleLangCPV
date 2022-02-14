import ply.lex as lex
import ply.yacc as yacc
import os
import warnings

OUTPUT_FILE = "main"
Lerrors = False

warnings.simplefilter("ignore")

# VERSIONS

currentVersion = "1.1.2"
Beta = "beta"
Author = "Riley"
Name = "SembleLang"
Description = " A Langauge designed for speed and simplicity. With accuracy and flexibility in mind I hand-crafted this language from the ground up to provide an easy way to get into high-speed coding."

# CODE

reserved = {
    "return": "RETURN",
    "def": "DEF"
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
    "COMMA"
] + list(reserved.values())

t_PLUS = r"\+"
t_MINUS = r"\-"
t_MUL = r"\*"
t_DIV = r"\/"
t_EQUALS = r"\="
t_SEMICOLON = r"\;"
t_LBRAC = r"\{"
t_RBRAC = r"\}"
t_LPAREN = r"\("
t_RPAREN = r"\)"
t_COMMA = r"\,"

t_ignore = r" "

precedence = (
    ("left", "PLUS", "MINUS"),
    ("left", "MUL", "DIV")
)

def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'NAME')    # Check for reserved words
    return t

def t_INT(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_COMMENT(t):
    r'\/\*.*\*\/'
    pass

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
    p[0] = ("function", p[5], p[2], None)

def p_functiononearg(p):
    '''
    function : DEF NAME LPAREN expression RPAREN statement
             | DEF NAME LPAREN expression RPAREN statementc
    '''
    p[0] = ("function", p[6], p[2], [p[4]])

def p_functionmanyargs(p):
    '''
    function : DEF NAME LPAREN explist RPAREN statement
             | DEF NAME LPAREN explist RPAREN statementc
    '''
    p[0] = ("function", p[6], p[2], p[4])

def p_explist(p):
    '''
    explist : expression COMMA expression
            | explist COMMA expression
    '''

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


def p_statementvardef(p):
    '''
    statement : NAME EQUALS expression SEMICOLON
    '''
    p[0] = ("def", p[1], p[3])
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

def p_expression_primitive(p):
    '''
    expression : expression MUL expression
               | expression DIV expression
               | expression PLUS expression
               | expression MINUS expression
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

parser = yacc.yacc()

currentFunc = "main"

funcs = {
}
funcVars = {}
MPN = 0

currentFuncVarVal = 4
funcVarCount = {"main": 0}


def asm(s):
    try:
        funcs[currentFunc].append(s)
    except:
        e_error("Must be inside a function to execute code!")

def e_error(e):
  print("Eval error: {}".format(e))

def eval(p):
    global currentFunc, funcVars, funcVarCount, currentFuncVarVal, Lerrors
    if Lerrors:
        quit("Lexing error!")
    
    if type(p) != tuple:
        if type(p) == int:
            asm("movl ${}, %ecx\n".format(p))
            return p
        return p

    if p[0] == "function":
        funcVars = {}

        currentFuncVarVal = 4
        funcVarCount = {p[2]: 0}
        currentFunc = p[2]
        funcs[p[2]] = []
        eval(p[1])
    elif p[0] == "statementc":
        eval(p[1])

    elif p[0] == "statements":
        eval(p[1])
        eval(p[2])
    
    elif p[0] == "return":
        eval(p[1])
        asm("movl %ecx, %eax")
        asm("jmp .l{}".format(currentFunc))


    elif p[0] == "def":
        if p[1] in funcVars:
            x = funcVars[p[1]]
            t = eval(p[2])
            asm("movl %ecx, {} # variable {}".format(x, p[1]))
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

    elif type(p) == int:
        asm("movl ${}, %ecx".format(p))

    elif type(p) == str:
        if p in funcVars:
            asm("movl {}, %ecx".format(funcVars[p]))
        else:
            e_error("Variable '{}' referenced before assignment!".format(p))
        return [p]

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

def cmpf():
    global currentVersion, Beta, Author, Name, Description
    _startInclude = True
    if "main" not in funcs.keys():
        _startInclude = False
    with open("main.s", "w") as file:
        #COMMENTS
        file.write("# FILE GENERATED BY SEMBLELANG" + "\n")
        file.write("#                             " + "\n")
        file.write("# {} version {}{}".format(Name, Beta, currentVersion) + "\n")
        file.write("# By {}".format(Author) + "\n")
        for str in line_wrap(Description, width=41):
            file.write("# " + str + "\n")
        


        file.write("\n\n\n.section .data\n\n\n")
        #for i in data:
        #    file.write('\n{}:'.format(i))
        #    file.write("\n\t{}".format(data[i]))
        #    file.write("\n.section .text\n\n\n")
        #    if '_start' not in funcs.keys():
        #        file.write("\n.globl _start\n\n_start:\n\tcall main\n\tmovl %eax, %ebx\n\tmovl $1, %eax\n\tint $0x80")
        #
        #    file.write("\n\n\n")
    
        if '_start' not in funcs.keys() and _startInclude:
            file.write("\n.globl _start\n\n_start:\n\tcall main\n\tmovl %eax, %ebx\n\tmovl $1, %eax\n\tint $0x80")
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
                if line.startswith("movl $0, "):
                    line = "xorl " + line.replace("movl $0, ", "") + ", " + line.replace("movl $0, ", "")
                file.write("\n\t" + line)
            if func not in ['_start']:
                file.write("\n\t.l{}:".format(func))
                file.write("\n\tmovl %ebp, %esp\n\tpopl %ebp") 
                file.write("\n\tret")
            file.write("\n")
    os.system("as --32 main.s -o main.o")
    os.system("ld -m elf_i386 -dynamic-linker /lib/ld-linux.so.2 main.o -o {} -lc && rm main.o".format(OUTPUT_FILE))

cmpf()

    