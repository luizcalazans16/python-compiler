import ply.yacc as yacc
import ply.lex as lex
import random

reserved = {
    'if': 'IF',
    'ifelse': 'IFELSE',
    'while': 'WHILE',
    'true': 'TRUE',
    'false': 'FALSE',
    'var': 'VAR',
    'print': 'PRINT',
    'to': 'TO',
    'end': 'END',
    'fo': 'FORWARD',
    'forward': 'FORWARD',
    'bk': 'BACKWARD',
    'back': 'BACKWARD',
    'rt': 'RIGHT',
    'right': 'RIGHT',
    'lt': 'LEFT',
    'left': 'LEFT',
    'pu': 'PENUP',
    'penup': 'PENUP',
    'pd': 'PENDOWN',
    'pendown': 'PENDOWN',
    'heading': 'HEADING',
    'setxy': 'SETXY',
    'home': 'HOME',
    'wipeclean': 'WIPECLEAN',
    'wc': 'WIPECLEAN',
    'cs': 'RESET',
    'clearscreen': 'RESET',
    'random': 'RANDOM',
    'xcor': 'XCOR',
    'ycor': 'YCOR',
    'typein': 'TYPEIN'
}

tokens = [
    'STRING', 'FLOAT', 'INT',
    'LBR', 'RBR', 'LPAR', 'RPAR',
    'QUOTE', 'COLON', 'COMMA',
    'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'POWER',
    'EQUALS', 'GTE', 'LTE', 'GT', 'LT', 'NE'
] + list(set(reserved.values()))


# Tokens

t_LBR = r'\['
t_RBR = r'\]'
t_LPAR = r'\('
t_RPAR = r'\)'
t_QUOTE = r'"'
t_COLON = r':'
t_COMMA = r','
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_POWER = r'\^'
t_EQUALS = r'='
t_GTE = r'>='
t_LTE = r'<='
t_GT = r'>'
t_LT = r'<'
t_NE = r'!='

@lex.TOKEN(r'[a-zA-Z_][a-zA-Z0-9_]*')
def t_STRING(t):
    #r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value, 'STRING')
    return t

@lex.TOKEN(r'\d*\.\d+')
def t_FLOAT(t):
    #r'\d*\.\d+'
    t.value = float(t.value)
    return t

@lex.TOKEN(r'\d+')
def t_INT(t):
    #r'\d+'
    t.value = int(t.value)
    return t

t_ignore = " \t"

@lex.TOKEN(r'\n+')
def t_newline(t):
    #r'\n+'
    t.lexer.lineno += len(t.value)


def t_error(t):
    print(f"Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


precedence = (
    ('right', 'RANDOM'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE'),
    ('right', 'UMINUS'),
    ('left', 'POWER'),
)


def p_statement_list(p):
    '''
    statement_list : statement_list statement
                   | statement
    '''
    if len(p) == 3:
        p[0] = p[1] + p[2]
    else:
        p[0] = p[1]


def p_statement(p):
    '''
    statement : turtle_instruction
              | if_statement
              | ifelse_statement
              | while_statement
              | variable_declaration
              | procedure_definition
              | procedure_call
              | print_statement
    '''
    p[0] = [p[1]]


def p_turtle_instruction(p):
    '''
    turtle_instruction : SETXY LBR expression COMMA expression RBR
                       | FORWARD expression
                       | BACKWARD expression
                       | RIGHT expression
                       | LEFT expression
                       | HEADING expression
                       | PENUP
                       | PENDOWN
                       | HOME
                       | WIPECLEAN
                       | RESET
                       | XCOR
                       | YCOR
                       | TYPEIN expression
    '''
    function = reserved[p[1]]
    if len(p) == 7:
        p[0] = (function, p[3], p[5])
    elif len(p) == 3:
        p[0] = (function, p[2])
    else:
        p[0] = (function, None)


def p_if_statement(p):
    '''
    if_statement : IF condition LBR statement_list RBR
    '''
    p[0] = (reserved[p[1]], p[2], p[4])


def p_ifelse_statement(p):
    '''
    ifelse_statement : IFELSE condition LBR statement_list RBR LBR statement_list RBR
    '''
    p[0] = (reserved[p[1]], p[2], p[4], p[7])


def p_while_statement(p):
    '''
    while_statement : WHILE condition LBR statement_list RBR
    '''
    p[0] = (reserved[p[1]], p[2], p[4])


def p_variable_declaration(p):
    '''
    variable_declaration : VAR word expression
    '''
    p[0] = (reserved[p[1]], p[2], p[3])


def p_procedure_definition(p):
    '''
    procedure_definition : TO STRING LBR parameter_list RBR statement_list END
    '''
    args = [a for a in p[4] if a is not None]
    p[0] = ('DEF', p[2], args, p[6])


def p_parameter_list(p):
    '''
    parameter_list : parameter_list COMMA parameter
                   | parameter
    '''
    if len(p) == 4:
        p[0] = p[1] + p[3]
    else:
        p[0] = p[1]


def p_parameter(p):
    '''
    parameter : name
              | empty
    '''
    p[0] = [p[1]]


def p_procedure_call(p):
    '''
    procedure_call : STRING LBR expression_list RBR
    '''
    args = [a for a in p[3] if a is not None]
    p[0] = ('CALL', p[1], args)


def p_expression_list(p):
    '''
    expression_list : expression_list COMMA expression
                    | expression
                    | empty
    '''
    if len(p) == 4:
        p[0] = p[1] + [p[3]]
    else:
        p[0] = [p[1]]


def p_print_statement_word(p):
    '''
    print_statement : PRINT word
    '''
    p[0] = ('PRINT_WORD', p[2])


def p_print_statement_expression(p):
    '''
    print_statement : PRINT expression
    '''
    p[0] = ('PRINT_EXPR', p[2])


def p_expression_int_float(p):
    '''
    expression : INT
               | FLOAT
    '''
    p[0] = p[1]


def p_expression_var(p):
    '''
    expression : name
    '''
    p[0] = ('VAR', p[1])


def p_expression_group(p):
    '''
    expression : LPAR expression RPAR
    '''
    p[0] = p[2]


def p_expression_uminus(p):
    '''
    expression : MINUS expression %prec UMINUS
    '''
    p[0] = ('UMINUS', p[2])


def p_expression_random(p):
    '''
    expression : RANDOM expression
    '''
    p[0] = (reserved[p[1]], p[2])


def p_expression(p):
    '''
    expression : expression TIMES expression
               | expression DIVIDE expression
               | expression PLUS expression
               | expression MINUS expression
               | expression POWER expression
    '''
    p[0] = (p[2], p[1], p[3])


def p_condition_true_false(p):
    '''
    condition : TRUE
              | FALSE
    '''
    p[0] = (p[1] == 'true')


def p_condition(p):
    '''
    condition : expression GT expression
              | expression LT expression
              | expression GTE expression
              | expression LTE expression
              | expression EQUALS expression
              | expression NE expression
    '''
    p[0] = (p[2], p[1], p[3])


def p_word(p):
    '''
    word : QUOTE STRING
    '''
    p[0] = p[2]


def p_name(p):
    '''
    name : COLON STRING
    '''
    p[0] = p[2]


def p_empty(p):
    '''
    empty :
    '''
    p[0] = None


def p_error(p):
    if p:
        print("Syntax error at token", p.type)
        parser.errok()
    else:
        print("Syntax error at EOF")


# Interpretador
symbol_table = {}


def run(program):
    for statement in program:
        execute(statement)


def execute(s):
    global symbol_table
    function = s[0]
    arg1 = s[1]
    arg2 = s[2] if len(s) > 2 else None
    arg3 = s[3] if len(s) > 3 else None

    # Funções da tartaruga
    if function == 'SETXY':
        return calc(arg1), calc(arg2)
    elif function == 'FORWARD':
        return calc(arg1)
    elif function == 'BACKWARD':
        return calc(arg1)
    elif function == 'RIGHT':
        return calc(arg1)
    elif function == 'LEFT':
        return calc(arg1)
    elif function == 'HEADING':
        print('HEADING')
    elif function == 'PENUP':
        print('PENUP')
    elif function == 'PENDOWN':
        print('PENDOWN')
    elif function == 'HOME':
        print('HOME')
    elif function == 'WIPECLEAN':
        print('WIPECLEAN')
    elif function == 'RESET':
        print('RESET')
    elif function == 'XCOR':
        print('XCOR')
    elif function == 'YCOR':
        print('YCOR')
    elif function == 'TYPEIN':
        print('TYPEIN')
        return calc(arg1)

    # Estruturas de controle
    elif function == 'IF':
        if eval(arg1):
            run(arg2)
    elif function == 'IFELSE':
        if eval(arg1):
            run(arg2)
        else:
            run(arg3)
    elif function == 'WHILE':
        while eval(arg1):
            run(arg2)
    # Declaração de variáveis
    elif function == 'VAR':
        symbol_table[arg1] = calc(arg2)
    # Declaração de funções
    elif function == 'DEF':
        symbol_table[arg1] = {
            'args': [a for a in arg2 if a is not None],
            'body': arg3
        }
    elif function == 'CALL':
        try:
            procedure = symbol_table[arg1]
            arg2 = [a for a in arg2 if a is not None]
            if type(procedure) != dict:
                print(f"{arg1} is not a procedure")
                return
            if len(arg2) != len(procedure['args']):
                print(f"expected {len(procedure['args'])} arguments, but got {len(arg2)}")
                return
            old_symbol_table = symbol_table.copy()
            for arg, arg_expr in zip(procedure['args'], arg2):
                symbol_table[arg] = calc(arg_expr)
            run(procedure['body'])
            symbol_table = old_symbol_table.copy()
        except LookupError:
            print(f"Undeclared procedure {arg1}")
    # Funções de print
    elif function == 'PRINT_WORD':
        print(arg1)
    elif function == 'PRINT_EXPR':
        print(calc(arg1))


def calc(e):
    global symbol_table
    if type(e) == tuple:
        id = e[0]
        arg1 = e[1]
        arg2 = e[2] if len(e) > 2 else None
        if id == '+':
            return calc(arg1) + calc(arg2)
        elif id == '-':
            return calc(arg1) - calc(arg2)
        elif id == '*':
            return calc(arg1) * calc(arg2)
        elif id == '/':
            return calc(arg1) / calc(arg2)
        elif id == '^':
            return calc(arg1) ** calc(arg2)
        elif id == 'VAR':
            try:
                return symbol_table[arg1]
            except LookupError:
                print(f"Undefined variable {arg1}")
                return
        elif id == 'UMINUS':
            return (-1) * calc(arg1)
        elif id == 'RANDOM':
            return random.randrange(calc(arg1))
    else:
        return e


def eval(c):
    global symbol_table
    if type(c) != tuple:
        return c
    else:
        op = c[0]
        arg1 = calc(c[1])
        arg2 = calc(c[2])
        if op == '>':
            return arg1 > arg2
        elif op == '<':
            return arg1 < arg2
        elif op == '>=':
            return arg1 >= arg2
        elif op == '<=':
            return arg1 <= arg2
        elif op == '=':
            return arg1 == arg2
        elif op == '!=':
            return arg1 != arg2


lexer = lex.lex()
parser = yacc.yacc()


def main():
    while True:
        try:
            cmd = input(f'input > ')
            if cmd.strip() == 'exit':
                break
        except EOFError:
            break
        lines = [cmd]
        while True:
            line = input(f'input > ')
            if line:
                lines.append(line)
            else:
                break
        s = '\n'.join(lines)

        p = parser.parse(s)

        if p is None:
            pass
        else:
            try:
                print('AST             : ', p)
                # run(p)
            except Exception as e:
                print(f'Erro no interpretador do Python: {e}')
                lexer.input(s)
                tokens = [(tok.type, tok.value) for tok in lexer]
                print('tokens          : ', tokens)
                print('AST             : ', p)
                print('Symbol table    : ', symbol_table)
                continue


if __name__ == '__main__':
    main()
