import ply.yacc as yacc
import ply.lex as lex

reserved = {
    'if': 'IF',
    'else': 'ELSE',
    'while': 'WHILE',
    'true': 'TRUE',
    'false': 'FALSE',
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
    'typein': 'TYPEIN',
    'sqrt': 'SQRT',
    'and': 'AND',
    'or': 'OR',
    'not': 'NOT',
    'then': 'THEN'
}

tokens = [
    'ID', 'FLOAT', 'INT', 'STRING',
    'LPAR', 'RPAR', 'LK', 'RK',
    'COLON', 'COMMA',
    'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'POWER', 'ASSIGN',
    'EQ', 'GTE', 'LTE', 'GT', 'LT', 'NE'
] + list(set(reserved.values()))


t_LPAR = r'\('
t_LK = r'{'
t_RK = r'}'
t_RPAR = r'\)'
t_COLON = r':'
t_COMMA = r','
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_POWER = r'\^'
t_EQ = r'=='
t_ASSIGN = r'='
t_GTE = r'>='
t_LTE = r'<='
t_GT = r'>'
t_LT = r'<'
t_NE = r'!='


@lex.TOKEN(r'[a-zA-Z_][a-zA-Z0-9_]*')
def t_ID(t):
    t.type = reserved.get(t.value, 'ID')
    return t


@lex.TOKEN(r'\".*\"')
def t_STRING(t):
    t.type = reserved.get(t.value, 'STRING')
    return t


@lex.TOKEN(r'\d*\.\d+')
def t_FLOAT(t):
    r'\d*\.\d+'
    t.value = float(t.value)
    return t


@lex.TOKEN(r'\d+')
def t_INT(t):
    t.value = int(t.value)
    return t


t_ignore = " \t"


@lex.TOKEN(r'\n+')
def t_newline(t):
    t.lexer.lineno += len(t.value)


def t_error(t):
    print(f"Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


# Precedência do Parser

precedence = (
    ('right', 'RANDOM'),
    ('right', 'SQRT'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE'),
    ('right', 'UMINUS'),
    ('left', 'POWER'),
)

# Gramática


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
              | while_statement
              | variable_declaration
              | procedure_definition
              | procedure_call
              | print_statement
    '''
    p[0] = [p[1]]


def p_turtle_instruction(p):
    '''
    turtle_instruction : SETXY LK expression COMMA expression RK
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
    if_statement : IF LPAR condition RPAR THEN statement_list END
                 | IF LPAR condition RPAR THEN statement_list ELSE statement_list END
    '''
    if len(p) == 8:
        p[0] = (reserved[p[1]], p[3], p[6])
    else:
        p[0] = (reserved[p[1]], p[3], p[6], reserved[p[7]], p[8])


def p_while_statement(p):
    '''
    while_statement : WHILE LPAR condition RPAR statement_list END
    '''
    p[0] = (reserved[p[1]], p[3], p[5])


def p_variable_declaration(p):
    '''
    variable_declaration : ID ASSIGN expression
    '''
    p[0] = ('VAR', p[1], p[3])
    symbol_table[p[1]] = p[3]


def p_procedure_definition(p):
    '''
    procedure_definition : TO ID parameter_list statement_list END
    '''
    args = [a for a in p[3] if a is not None]
    p[0] = ('DEF', p[2], args, p[4])
    symbol_table[p[2]] = {
        'args': [a for a in args if a is not None],
        'body': p[4]
    }


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
    procedure_call : ID expression_list
    '''
    args = [a for a in p[2] if a is not None]
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


def p_print_statement(p):
    '''
    print_statement : PRINT word
                    | PRINT expression
    '''
    p[0] = (reserved[p[1]], p[2])


def p_expression_int_float(p):
    '''
    expression : INT
               | FLOAT
    '''
    p[0] = p[1]


def p_expression_binary(p):
    '''
    expression : expression TIMES expression
               | expression DIVIDE expression
               | expression PLUS expression
               | expression MINUS expression
               | expression POWER expression
    '''
    p[0] = (p[2], p[1], p[3])


def p_expression_math(p):
    '''
    expression : SQRT expression
    '''
    p[0] = ('SQRT', p[2])


def p_expression_others(p):
    '''
    expression : MINUS expression %prec UMINUS
               | LPAR expression RPAR
               | RANDOM expression
    '''
    if len(p) == 5:
        p[0] = ('UMINUS', p[2])
    if len(p) == 4:
        p[0] = p[2]
    if len(p) == 3:
        p[0] = (reserved[p[1]], p[2])


def p_expression_name(p):
    '''
    expression : name
    '''
    p[0] = ('VAR', p[1])


def p_condition(p):
    '''
    condition : bool_expression
              | bool_expression OR condition
              | bool_expression AND condition
              | NOT condition
    '''
    if len(p) == 2:
        p[0] = p[1]
    if len(p) == 3:
        p[0] = (reserved[p[1]], p[2])
    if len(p) == 4:
        p[0] = (reserved[p[2]], p[1], p[3])


def p_condition_true_false(p):
    '''
    condition : TRUE
              | FALSE
    '''
    p[0] = (p[1] == 'true')


def p_bool_expression(p):
    '''
    bool_expression : expression GT expression
                    | expression LT expression
                    | expression GTE expression
                    | expression LTE expression
                    | expression EQ expression
                    | expression NE expression
    '''
    p[0] = (p[2], p[1], p[3])


def p_word(p):
    '''
    word : STRING
    '''
    p[0] = p[1].replace('"', '')


def p_name(p):
    '''
    name : COLON ID
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


symbol_table = {}
lexer = lex.lex()
parser = yacc.yacc()


def main():
    global symbol_table
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
            print('AST             :', p)
            continue


if __name__ == '__main__':
    main()
