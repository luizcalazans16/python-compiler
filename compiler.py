# Imports
import ply.yacc as yacc
import ply.lex as lex
from collections.abc import Iterable

program = []
# Palavras reservadas
reserved = {
    'if': 'IF',
    'else': 'ELSE',
    'while': 'WHILE',
    'true': 'TRUE',
    'false': 'FALSE',
    'write': 'WRITE',
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

# Tokens
tokens = [
    'ID', 'FLOAT', 'INT', 'STRING',
    'LPAR', 'RPAR', 'LK', 'RK',
    'COLON', 'COMMA',
    'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'POWER', 'ASSIGN',
    'EQ', 'GTE', 'LTE', 'GT', 'LT', 'NE'
] + list(set(reserved.values()))


# Caractéres respectivos aos Tokens

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
    ('left', 'PLUS', 'MINUS'),
    ('right', 'SQRT'),
    ('left', 'TIMES', 'DIVIDE'),
    ('right', 'UMINUS'),
    ('left', 'POWER'),
)

# Gramática


def p_program(p):
    '''
    program : statement statement_list
    '''
    statement = p[1]
    statements = p[2]
    if statements is None:
        statements = [statement]
    else:
        statements.insert(0, statement)
    p[0] = statements
    vm_program = []
    for statement_body in statements:
        vm_program.extend(statement_body)
    global program
    program = list(flatten(vm_program))


def flatten(lis):
    for item in lis:
        if isinstance(item, Iterable) and not isinstance(item, str):
            for x in flatten(item):
                yield x
        else:
            yield item


def p_statement_list(p):
    '''
    statement_list : statement statement_list
                   | empty
    '''
    if len(p) == 2:
        p[0] = None
        return
    statement = p[1]
    statements = p[2]
    if statements is None:
        statements = [statement]
    else:
        statements.insert(0, statement)
    p[0] = statements


def p_statement(p):
    '''
    statement : turtle_instruction
              | if_statement
              | while_statement
              | variable_declaration
              | procedure_definition
              | procedure_call
              | write_statement
    '''
    p[0] = p[1]


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
        p[0] = [p[3], p[5], 'CALL ' + function]
    elif len(p) == 3:
        p[0] = [p[2], 'CALL ' + function]
    else:
        p[0] = ['CALL ' + function]
    program.extend(p[0])


def p_if_statement(p):
    '''
    if_statement : IF LPAR condition RPAR THEN statement_list END
                 | IF LPAR condition RPAR THEN statement_list ELSE statement_list END
    '''
    if len(p) == 8:
        p[0] = [p[3], 'CALL IF CONDITION', p[6], 'CALL IF EXECUTION']
    else:
        p[0] = [
            p[3],
            'CALL IF CONDITION',
            p[6],
            'CALL IF EXECUTION',
            p[8],
            'CALL ELSE EXECUTION']
    program.extend(p[0])


def p_while_statement(p):
    '''
    while_statement : WHILE LPAR condition RPAR statement_list END
    '''
    p[0] = [p[3], p[5], 'CALL ' + reserved[p[1]]]
    program.extend(p[0])


def p_variable_declaration(p):
    '''
    variable_declaration : ID ASSIGN expression
    '''
    p[0] = [p[3], 'STORE ' + p[1]]
    #symbol_table[p[1]] = p[3]
    program.extend(p[0])


def p_procedure_definition(p):
    '''
    procedure_definition : TO ID parameter_list statement_list END
    '''
    args = [a for a in p[3] if a is not None]
    p[0] = ['DEF ' + p[2], args, p[4]]
    symbol_table[p[2]] = {
        'args': [a for a in args if a is not None],
        'body': p[4]
    }
    program.extend(p[0])


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
    p[0] = ['CALL ' + p[1], args]
    program.extend(p[0])


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


def p_write_statement(p):
    '''
    write_statement : WRITE word
                    | WRITE expression
                    | WRITE expression word
                    | WRITE word expression
    '''
    if len(p) == 3:
        p[0] = [p[2], 'CALL ' + reserved[p[1]]]
    else:
        p[0] = [p[2], p[3], 'CALL ' + reserved[p[1]]]
    program.extend(p[0])


def p_expression_int_float(p):
    '''
    expression : INT
               | FLOAT
    '''
    p[0] = [f'PUSH {p[1]}']
    program.extend(p[0])


def p_expression_binary(p):
    '''
    expression : expression TIMES expression
               | expression DIVIDE expression
               | expression PLUS expression
               | expression MINUS expression
               | expression POWER expression
    '''
    operators = {
        '+': 'ADD',
        '-': 'SUB',
        '*': 'MUL',
        '/': 'DIV',
        '^': 'POW'
    }

    p[0] = [p[1], p[3], operators[p[2]]]
    program.extend(p[0])


def p_expression_math(p):
    '''
    expression : SQRT LPAR expression RPAR
    '''
    p[0] = [f'CALL {reserved[p[1]]}', p[3]]
    program.extend(p[0])


def p_expression_uminus(p):
    '''
    expression : MINUS expression %prec UMINUS
    '''
    p[0] = [p[2], 'UMINUS']
    program.extend(p[0])


def p_expression_group(p):
    '''
    expression : LPAR expression RPAR
    '''
    p[0] = [p[2]]
    program.extend(p[0])


def p_expression_random(p):
    '''
    expression : RANDOM expression
    '''
    p[0] = [p[2], 'CALL RANDOM']
    program.extend(p[0])


def p_expression_name(p):
    '''
    expression : name
    '''
    p[0] = p[1]


def p_condition(p):
    '''
    condition : bool_expression
              | bool_expression OR condition
              | bool_expression AND condition
              | NOT condition
    '''
    if len(p) == 2:
        p[0] = [p[1]]
    if len(p) == 3:
        p[0] = [p[2], reserved[p[1]]]
    if len(p) == 4:
        p[0] = [p[1], p[3], reserved[p[2]]]

    program.extend(p[0])


def p_condition_true_false(p):
    '''
    condition : TRUE
              | FALSE
    '''
    p[0] = ['PUSH ' + p[1]]
    program.extend(p[0])


def p_bool_expression(p):
    '''
    bool_expression : expression GT expression
                    | expression LT expression
                    | expression GTE expression
                    | expression LTE expression
                    | expression EQ expression
                    | expression NE expression
    '''
    operators = {
        '>': 'GT',
        '<': 'LT',
        '>=': 'GTE',
        '<=': 'LTE',
        '==': 'EQ',
        '!=': 'NEQ'
    }

    p[0] = [p[1], p[3], operators[p[2]]]
    program.extend(p[0])


def p_word(p):
    '''
    word : STRING
    '''
    p[0] = 'PUSH ' + p[1]


def p_name(p):
    '''
    name : COLON ID
    '''
    p[0] = ['LOAD ' + p[2]]
    program.extend(p[0])


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


# Tabela de símbolos
symbol_table = {}


# Declaração do lexer e parser
lexer = lex.lex()
parser = yacc.yacc()


def main():
    global symbol_table
    lines = ''
    try:
        with open("program.logo", "r") as read_file:
            for line in read_file.readlines():
                lines += line
        print("Program:\n\n", lines, "\n\n")
    except EOFError:
        print('EOF Error')
    p = parser.parse(lines.strip())

    if p is None:
        pass
    else:
        with open("new_program.logo", "w") as write_file:
            for line in program:
                print(line)
                write_file.write(line)
                write_file.write("\n")


if __name__ == '__main__':
    main()
