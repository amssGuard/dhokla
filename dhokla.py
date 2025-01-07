################
#IMPORTS
################
import string

################
#DIGITS
################
DIGITS = '0123456789'
LETTERS = string.ascii_letters
LETTERS_DIGITS = LETTERS+DIGITS

################
#ERROR
################
class Error:
    def __init__(self,posStart,posEnd,errorName,detail):
        self.posStart = posStart
        self.posEnd = posEnd
        self.errorName = errorName
        self.detail = detail

    def as_string(self):
        result = f'{self.errorName}: {self.detail}'
        result += f'\nat:[{self.posStart.idx} to {self.posEnd.idx}]'
        return result
    
class IllegalCharacter(Error):
    def __init__(self, posStart, posEnd, detail):
        super().__init__(posStart, posEnd, 'Illegal Character', detail)

class InvalidSyntax(Error):
    def __init__(self, posStart, posEnd,  detail):
        super().__init__(posStart, posEnd, 'invalid syntax', detail)

class ExpectedCharErr(Error):
    def __init__(self, posStart, posEnd, detail):
        super().__init__(posStart, posEnd, "Expected char", detail)

class RTError(Error):
    def __init__(self, posStart, posEnd,  detail,context):
        super().__init__(posStart, posEnd, 'Runtime Error', detail)
        self.context = context

    def as_string(self):
        result = self.generate_traceback()
        result += f'{self.errorName}: {self.detail}'
        result += f'\nat:[{self.posStart.idx} to {self.posEnd.idx}]'
        return result

    def generate_traceback(self):
        result = ''
        pos = self.posStart
        ctx = self.context

        while ctx:
            result = f'{ctx.display_name}\n'+result
            pos = ctx.parent_entry_pos
            ctx = ctx.parent

        return 'Traceback(most recent call last):\n' + result


################
#POSITION
################
class Position:
    def __init__(self,idx,ftxt):
        self.idx = idx
        self.ftxt = ftxt
    
    def advance(self):
        self.idx += 1
        
        return self
    
    def copy(self):
        return Position(self.idx,self.ftxt)



###############
#TOKENS
###############
TT_INT          = 'INT'
TT_FLOAT        = 'FLOAT'
TT_PLUS         = 'PLUS'
TT_MINUS        = 'MINUS'
TT_MUL          = 'MUL'
TT_DIV          = 'DIV'
TT_LPAREN       = 'LPAREN'
TT_RPAREN       = 'RPAREN'
TT_POW          = 'POW'
TT_EOF          = 'EOF'
TT_IDENTIFIER   = 'IDENTIFIER'
TT_EQ           = 'EQ'
TT_KEYWORD      = 'KEYWORD'
TT_EE           = 'EE'
TT_NE           = 'NE'
TT_LT           = 'LT'
TT_LTE          = 'LTE'
TT_GT           = 'GT'
TT_GTE          = 'GTE' 

KEYWORDS = ['VAR','AND','OR','NOT',]

class Token:
    def __init__(self,type_,value = None,posStart=None,posEnd=None):
        self.type = type_
        self.value = value
        if posStart:
            self.posStart = posStart.copy()
            self.posEnd = posStart.copy()
            self.posEnd.advance()

        if posEnd:
            self.posEnd = posEnd

    def matches(self,type_,value):
        return self.type == type_ and self.value == value

    def __repr__(self):
        if self.value: return f'{self.type}:{self.value}'
        return f'{self.type}'

##############
#Lexer
##############
class Lexer:
    def __init__(self,text):
        self.text = text
        self.pos = Position(-1,text)
        self.currChar = None
        self.advance()

    def advance(self):
        self.pos.advance()
        self.currChar = self.text[self.pos.idx] if self.pos.idx < len(self.text) else None


    def make_tokens(self):
        tokens = []

        token_map = {
            '+'  : TT_PLUS,
            '-'  : TT_MINUS,
            '*'  : TT_MUL,
            '/'  : TT_DIV,
            '('  : TT_LPAREN,
            ')'  : TT_RPAREN,
            '^'  :TT_POW,
        }

        while self.currChar != None:
            if self.currChar in ' \t':
                self.advance()
                continue
            if self.currChar in token_map:
                token_type = token_map[self.currChar]
                tokens.append(Token(token_type,posStart=self.pos))
                self.advance()
            elif self.currChar == '!':
                tok,error = self.make_not_equals()
                if error: return [],error
                tokens.append(tok)
            elif self.currChar == '=':
                tokens.append(self.make_equals())
            elif self.currChar == '<':
                tokens.append(self.make_less_than())
            elif self.currChar == '>':
                tokens.append(self.make_greater_than())
            elif self.currChar in LETTERS:
                tokens.append(self.make_identifier())
            elif self.currChar in DIGITS:
                tokens.append(self.make_number())
            else:
                posStart = self.pos.copy()
                char = self.currChar
                self.advance()
                return [],IllegalCharacter(posStart,self.pos,"'"+char+"'")
        tokens.append(Token(TT_EOF,posStart=self.pos))
        return tokens,None
        
    def make_number(self):
        posStart = self.pos.copy()
        num_str = ''
        dot_cnt = 0

        while self.currChar!= None and self.currChar in DIGITS + '.':
            if self.currChar == '.':
                if dot_cnt == 1: break
                dot_cnt += 1
                num_str += '.'
            else:
                num_str += self.currChar
            self.advance()
        if dot_cnt == 0:
            return Token(TT_INT,int(num_str),posStart,self.pos)
        else:
            return Token(TT_FLOAT,float(num_str),posStart,self.pos)
        
    def make_identifier(self):
        id_str = ''
        posStart = self.pos.copy()
        
        while self.currChar !=None and self.currChar in LETTERS_DIGITS + '_':
            id_str+=self.currChar
            self.advance()

        tok_type = TT_KEYWORD if id_str in KEYWORDS else TT_IDENTIFIER
        return Token(tok_type,id_str,posStart,self.pos)
    
    def make_not_equals(self):
        posStart = self.pos.copy()
        self.advance()

        if self.currChar == '=':
            print('hi')
            self.advance()
            return Token(TT_NE,posStart=posStart,posEnd=self.pos),None
        
        self.advance()
        return None,ExpectedCharErr(posStart,self.pos," '=' after '!'")
    
    
    def make_equals(self):
        posStart = self.pos.copy()
        tok_type = TT_EQ
        self.advance()

        if self.currChar == '=':
            self.advance()
            tok_type = TT_EE
        return Token(tok_type,posStart=posStart,posEnd=self.pos)
    
    def make_greater_than(self):
        posStart = self.pos.copy()
        tok_type = TT_GT
        self.advance()

        if self.currChar == '=':
            self.advance()
            tok_type = TT_GTE
        return Token(tok_type,posStart=posStart,posEnd=self.pos)
    
    def make_less_than(self):
        posStart = self.pos.copy()
        tok_type = TT_LT
        self.advance()

        if self.currChar == '=':
            self.advance()
            tok_type = TT_LTE
        return Token(tok_type,posStart=posStart,posEnd=self.pos)


##############
#NODES
##############
class NumberNode:
    def __init__(self,tok):
        self.tok = tok
        self.posStart = self.tok.posStart
        self.posEnd = self.tok.posEnd
    def __repr__(self):
        return f'{self.tok}'
    
class VarAccessNode:
    def __init__(self,var_name_tok):
        self.var_name_tok = var_name_tok

        self.posStart = self.var_name_tok.posStart
        self.posEnd = self.var_name_tok.posEnd

    def __repr__(self):
        return f'{self.var_name_tok}'

class VarAssignNode:
    def __init__(self,var_name_tok,value_node):
        self.var_name_tok = var_name_tok
        self.value_node = value_node

        self.posStart = self.var_name_tok.posStart
        self.posEnd = self.value_node.posEnd

    def __repr__(self):
        return f'{self.var_name_tok}'


class BinOpNode:
    def __init__(self,left_node,op_tok,right_node):
        self.left_node = left_node
        self.op_tok = op_tok
        self.right_node = right_node
        self.posStart = self.left_node.posStart
        self.posEnd = self.right_node.posEnd

    def __repr__(self):
        return f'({self.left_node},{self.op_tok},{self.right_node})' 
    
class UnaryOpNode:
    def __init__(self,op_tok,node):
        self.op_tok = op_tok
        self.node = node
        self.posStart = self.op_tok.posStart
        self.posEnd = self.node.posEnd
    
    def __repr__(self):
        return f'({self.op_tok,self.node})'
    
##############
#ParseResult
##############

class ParseResult:
    def __init__(self):
        self.error = None
        self.node = None
        self.advance_count = 0

    def register_advancement(self):
        self.advance_count += 1

    def register(self,res):
        self.advance_count += res.advance_count
        if res.error: self.error = res.error
        return res.node
    
    def success(self,node):
        self.node = node
        return self
    
    def failure(self,error):
        print(self.advance_count)
        if not self.error or self.advance_count == 0:
            self.error = error
        return self


##############
#Parser
##############
class Parser:
    def __init__(self,tokens):
        self.tokens = tokens
        self.tok_idx = -1 
        self.advance()

    def advance(self):
        self.tok_idx += 1
        if self.tok_idx<len(self.tokens):
            self.currTok = self.tokens[self.tok_idx]
        return self.currTok
    
    def bin_op(self,func_a,ops,func_b=None):
        if func_b==None:
            func_b = func_a
        res = ParseResult()
        left = res.register(func_a())
        if res.error: return res
        while self.currTok.type in ops or (self.currTok.type,self.currTok.value) in ops:
            op_tok = self.currTok
            res.register_advancement()
            self.advance()
            right = res.register(func_b())
            if res.error: return res
            left = BinOpNode(left,op_tok,right)

        return res.success(left)

##############################################

    def atom(self):
        print('at')
        res = ParseResult()
        tok = self.currTok
        if tok.type in (TT_INT,TT_FLOAT):
            res.register_advancement()
            self.advance()
            return res.success(NumberNode(tok))
        
        elif tok.type == TT_IDENTIFIER:
            res.register_advancement()
            self.advance()
            return res.success(VarAccessNode(tok))
        
        elif tok.type == TT_LPAREN:
            res.register_advancement()
            self.advance()
            expr = res.register(self.expr())
            if res.error: return res
            if self.currTok.type == TT_RPAREN:
                res.register_advancement()
                self.advance()
                return res.success(expr)
            else: 
                return res.failure(InvalidSyntax(tok.posStart,tok.posEnd,"Expected an ')'"))
        return res.failure(InvalidSyntax(tok.posStart,tok.posEnd,"Expected an int, float, identifier, '+', '-', '(' "))
    
    def power(self):
        print('pow')
        return self.bin_op(self.atom,(TT_POW,),self.factor)

    def factor(self):
        print('fac')
        res = ParseResult()
        tok = self.currTok
        if tok.type in (TT_PLUS,TT_MINUS):
            res.register_advancement()
            self.advance()
            factor = res.register(self.factor())
            if res.error: return res
            return res.success(UnaryOpNode(tok,factor))
        return self.power()
    
    
    def term(self):
        print('ter')
        return self.bin_op(self.factor,(TT_MUL,TT_DIV))
    
    def arith_expr(self):
        return self.bin_op(self.term,(TT_PLUS,TT_MINUS))
    
    def compr_expr(self):
        res = ParseResult()
        if self.currTok.matches(TT_KEYWORD,'NOT'):
            op_tok = self.currTok
            res.register_advancement()
            self.advance()

            node = res.register(self.compr_expr())
            if res.error: return res
            return res.success(UnaryOpNode(op_tok,node))
        node = res.register(self.bin_op(self.arith_expr,(TT_EE,TT_NE,TT_GT,TT_GTE,TT_LT,TT_LTE)))
        if res.error: return res.failure(InvalidSyntax(self.currTok.posStart,self.currTok.posEnd,"Expected an int, float, identifier, '+', '-', '(', 'NOT' "))
        return res.success(node)
    
    def expr(self):
        print('expr')
        res = ParseResult()
        if self.currTok.matches(TT_KEYWORD,'VAR'): 
            res.register_advancement()
            self.advance()
            if self.currTok.type != TT_IDENTIFIER:
                return res.failure(InvalidSyntax(self.currTok.posStart,self.currTok.posEnd,'Expected Identifier'))
            
            var_name = self.currTok
            res.register_advancement()
            self.advance()

            if self.currTok.type!=TT_EQ:
                return res.failure(InvalidSyntax(self.currTok.posStart,self.currTok.posEnd,'Expected ='))
            res.register_advancement()
            self.advance()

            expr = res.register(self.expr())
            if res.error: return res
            return res.success(VarAssignNode(var_name,expr))
        node = res.register(self.bin_op(self.compr_expr,((TT_KEYWORD,"AND"),(TT_KEYWORD,"OR"))))
        if res.error: return res.failure(InvalidSyntax(self.currTok.posStart,self.currTok.posEnd,"Expected 'VAR', int, float, identifier, '+', '-' or '('"))
        return res.success(node)

    def parse(self):
        res = self.expr()
        if not res.error and self.currTok.type!=TT_EOF:
            return res.failure(InvalidSyntax(self.currTok.posStart,self.currTok.posEnd,"Expected '+',  '-', '*' or '/'"))
        return res 
    

##############
#Runtime Res
##############
class RTResult:
    def __init__(self):
        self.value = None 
        self.error = None
    
    def register(self,res):
        if res.error: self.error = res.error
        return res.value
    
    def success(self,value):
        self.value = value
        return self
    
    def failure(self,error):
        self.error = error
        return self


##############
#Value
##############
class Number:
    def __init__(self,value):
        self.value = value
        self.set_pos()
        self.set_context()

    def set_pos(self,posStart = None,posEnd=None):
        self.posStart = posStart
        self.posEnd = posEnd
        return self
    
    def set_context(self,context=None):
        self.context = context
        return self

    def added_to(self,other):
        if isinstance(other,Number):
            return Number(self.value + other.value).set_context(self.context),None
    
    def subbed_to(self,other):
        if isinstance(other,Number):
            return Number(self.value - other.value).set_context(self.context),None
    
    def multed_to(self,other):
        if isinstance(other,Number):
            return Number(self.value * other.value).set_context(self.context),None
        
    def dived_to(self,other):
        if isinstance(other,Number):
            if other.value == 0:
                return None,RTError(other.posStart,other.posEnd,'Division Error',self.context)
            return Number(self.value / other.value).set_context(self.context),None
        
    def powed_to(self,other):
        if isinstance(other,Number):
            return Number(self.value ** other.value).set_context(self.context),None
    
    def get_comparison_eq(self, other):
        if isinstance(other, Number):
            return Number(int(self.value == other.value)).set_context(self.context), None

    def get_comparison_ne(self, other):
        if isinstance(other,Number):
            return Number(int(self.value!=other.value)).set_context(self.context),None
    
    def get_comparison_lt(self,other):
        if isinstance(other,Number):
            return Number(int(self.value<other.value)).set_context(self.context),None

    def get_comparison_gt(self,other):
        if isinstance(other,Number):
            return Number(int(self.value>other.value)).set_context(self.context),None

    def get_comparison_lte(self,other):
        if isinstance(other,Number):
            return Number(int(self.value<=other.value)).set_context(self.context),None
    
    def get_comparison_gte(self,other):
        if isinstance(other,Number):
            return Number(int(self.value>=other.value)).set_context(self.context),None

    def anded_by(self,other):
        if isinstance(other,Number):
            return Number(int(self.value and other.value)).set_context(self.context),None

    def ored_by(self,other):
        if isinstance(other,Number):
            return Number(int(self.value or other.value)).set_context(self.context),None
    
    def notted(self):
        return Number(1 if self.value == 0 else 0).set_context(self.context), None

    def copy(self):
        copy = Number(self.value)
        copy.set_pos(self.posStart,self.posEnd)
        copy.set_context(self.context)
        return copy

    def __repr__(self):
        return str(self.value)
    

##############
#Context
##############
class Context:
    def __init__(self,display_name,parent=None,parent_entry_pos=None):
        self.display_name = display_name
        self.parent = parent
        self.parent_entry_pos = parent_entry_pos
        self.symbol_table = None
    



##############
#SymbolTable
##############
class SymbolTable:
    def __init__(self):
        self.symbols = {}
        self.parent = None

    def get(self,name):
        value = self.symbols.get(name, None)
        if value == None and self.parent:
            return self.parent.get(name)
        return value
    
    def set(self,name,value):
        self.symbols[name] = value

    def remove(self,name):
        del self.symbols[name]

##############
#Interpreter
##############
class Interpreter:
    def visit(self,node,context):
        method_name = f'visit_{type(node).__name__}'
        method = getattr(self,method_name,self.no_visit_method)
        return method(node,context)

    def no_visit_method(self, node,context):
        raise Exception(f'no_visit_{type(node).__name__} method defined ')

    def visit_NumberNode(self,node,context):
        #print('nn')
        return RTResult().success( 
    Number(node.tok.value).set_context(context).set_pos(node.posStart,node.posEnd))

    def visit_VarAccessNode(self,node,context):
        res = RTResult()
        var_name = node.var_name_tok.value
        value = context.symbol_table.get(var_name)

        if value is None:
            return res.failure(RTError(node.posStart,node.posEnd,f"'{var_name}' is not defined",context))
        return res.success(value)

    def visit_VarAssignNode(self,node,context):
        res = RTResult()
        var_name = node.var_name_tok.value
        value = res.register(self.visit(node.value_node,context))
        if res.error: return res

        context.symbol_table.set(var_name,value)
        return res.success(value)

    def visit_BinOpNode(self,node,context):
        #print('bn')
        res = RTResult()
        left  = res.register(self.visit(node.left_node,context))
        if res.error: return res
        right = res.register(self.visit(node.right_node,context))
        if res.error: return res

        if node.op_tok.type == TT_PLUS:
            result,error = left.added_to(right)
        elif node.op_tok.type == TT_MINUS:
            result,error = left.subbed_to(right)
        elif node.op_tok.type == TT_MUL:
            result,error = left.multed_to(right)
        elif node.op_tok.type == TT_DIV:
            result,error = left.dived_to(right)
        elif node.op_tok.type == TT_POW:
            result,error = left.powed_to(right)
        elif node.op_tok.type == TT_EE:
            result, error = left.get_comparison_eq(right)
        elif node.op_tok.type == TT_NE:
            result, error = left.get_comparison_ne(right)
        elif node.op_tok.type == TT_LT:
            result, error = left.get_comparison_lt(right)
        elif node.op_tok.type == TT_GT:
            result, error = left.get_comparison_gt(right)
        elif node.op_tok.type == TT_LTE:
            result, error = left.get_comparison_lte(right)
        elif node.op_tok.type == TT_GTE:
            result, error = left.get_comparison_gte(right)
        elif node.op_tok.matches(TT_KEYWORD, 'AND'):
            result, error = left.anded_by(right)
        elif node.op_tok.matches(TT_KEYWORD, 'OR'):
            result, error = left.ored_by(right)
        if error:
            return res.failure(error)
        else:
            return res.success(result.set_pos(node.posStart,node.posEnd))

    def visit_UnaryOpNode(self,node,context):
        #print('un')
        res = RTResult()
        number = res.register(self.visit(node.node,context))
        if res.error: return res
        error = None
        if node.op_tok.type == TT_MINUS:
            number,error = number.multed_to(Number(-1))
        elif node.op_tok.matches(TT_KEYWORD,'NOT'):
            number, error = number.notted()
        if error:
            return res.failure(error)
        return res.success(number.set_pos(node.posStart,node.posEnd))


##############
#Run
##############

global_symbol_table = SymbolTable()
global_symbol_table.set('NULL',Number(0))
global_symbol_table.set('TRUE',Number(1))
global_symbol_table.set('FALSE',Number(0))

def run(text):
    lexer = Lexer(text)
    tokens,error = lexer.make_tokens()
    print(tokens)
    if error: return None,error

    parser = Parser(tokens)
    ast  = parser.parse()
    print(ast.node)
    if ast.error: return None,ast.error

    #run program
    interpreter = Interpreter()
    context = Context('<brogram>')
    context.symbol_table = global_symbol_table
    result = interpreter.visit(ast.node,context)

    return result.value,result.error
