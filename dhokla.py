####################################
#IMPORTS
####################################
from string_with_arrow import *
import string
####################################
#Constant
####################################
DIGITS = "1234567890"
LETTERS = string.ascii_letters
LETTERS_DIGITS = string.ascii_letters + DIGITS

####################################
#Position
####################################
class Position:
    def __init__(self,idx,lineNo,col,fn,text) -> None:
        self.idx = idx
        self.lineNo = lineNo
        self.col = col
        self.fn = fn
        self.text = text

    def advance(self,currCharacter=None):
        self.idx +=1
        self.col +=1

        if currCharacter == '\n':
            self.col = 0
            self.lineNo += 1
        return self
    
    def copy(self): return Position(self.idx,self.lineNo,self.col,self.fn,self.text)


#####################################
#ERROR
#####################################

class Error:
    def __init__(self,posStart,posEnd,errorName,detail) -> None:
        self.posStart = posStart
        self.posEnd = posEnd
        self.errorName = errorName
        self.detail = detail

    def as_string(self):
        result = f'{self.errorName}:{self.detail}'
        result += f',File{self.posStart.fn},line:{self.posStart.lineNo+1}'
        result += '\n\n'+ string_with_arrows(self.posStart.text,self.posStart,self.posEnd)
        return result

class IllegalCharacterError(Error):
    def __init__(self,posStart,posEnd,detail) -> None:
        super().__init__(posStart,posEnd,"Illegal Character",detail)

class ExpectedCharError(Error):
    def __init__(self, posStart, posEnd, detail) -> None:
        super().__init__(posStart, posEnd, "Expected Character", detail)

class IllegalSyntaxError(Error):
    def __init__(self,posStart,posEnd,detail) -> None:
        super().__init__(posStart,posEnd,"Illegal Syntax",detail)

class RTError(Error):
    def __init__(self, posStart, posEnd, detail,context) -> None:
        super().__init__(posStart, posEnd, 'Runtime Error', detail)
        self.context = context

    def as_string(self):
        result = self.generate_traceback()
        result += f'{self.errorName}:{self.detail}'
        result += '\n\n'+ string_with_arrows(self.posStart.text,self.posStart,self.posEnd)
        return result

    def generate_traceback(self):
        result = ''
        pos = self.posStart
        ctx = self.context

        while ctx:
            result = f' File {pos.fn}, line{str(pos.lineNo + 1)}, in {ctx.display_name}\n' + result
            pos = ctx.parent_entry_pos
            ctx = ctx.parent

        return 'Traceback (most recent call last):\n'+result


####################################
#TOKENS
####################################
TT_INT        = 'INT'
TT_FLOAT      = 'FLOAT'
TT_IDENTIFIER = 'IDENTIFIER'
TT_KEYWORD    = 'KEYWORD'
TT_PLUS       = 'PLUS'
TT_MINUS      = 'MINUS'
TT_MUL        = 'MUL'
TT_DIV        = 'DIV'
TT_POW        = 'POW'
TT_EQ         = 'EQ'
TT_LPAREN     = 'LPAREN'
TT_RPAREN     = 'RPAREN'
TT_EE         = 'EE'
TT_NE         = 'NE'
TT_LT         = 'LT'
TT_GT         = 'GT'
TT_LTE        = 'LTE'
TT_GTE        = 'GTE'
TT_EOF        = 'EOF'

KEYWORDS = ['VAR','AND','OR','NOT']

class Token:
    def __init__(self,type_,value=None,posStart=None,posEnd=None) -> None:
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

    def __repr__(self) -> str:
        if self.value: return f'{self.type}:{self.value}'
        return f'{self.type}'



#####################################
#LEXER
#####################################

class Lexer: 
    def __init__(self,fn,text) -> None:
        self.fn = fn
        self.text = text
        self.position = Position(-1,0,-1,fn,text)
        self.currChar = None
        self.advance()

    def advance(self):
        self.position.advance(self.currChar)
        self.currChar = self.text[self.position.idx] if self.position.idx < len(self.text) else None

    def make_tokens(self):
        tokens = []
        while self.currChar is not None:
            if self.currChar in " \t":
                self.advance()
                continue
            if self.currChar == '+':
                tokens.append(Token(TT_PLUS,posStart=self.position))
                self.advance()
            elif self.currChar == '-':
                tokens.append(Token(TT_MINUS,posStart=self.position))
                self.advance()
            elif self.currChar == '*':
                tokens.append(Token(TT_MUL,posStart=self.position))
                self.advance()
            elif self.currChar == '/':
                tokens.append(Token(TT_DIV,posStart=self.position))
                self.advance()
            elif self.currChar == '^':
                tokens.append(Token(TT_POW,posStart=self.position))
                self.advance()
            elif self.currChar == '(':
                tokens.append(Token(TT_LPAREN,posStart=self.position))
                self.advance()
            elif self.currChar == ')':
                tokens.append(Token(TT_RPAREN,posStart=self.position))
                self.advance()
            elif self.currChar in DIGITS:
                tokens.append(self.make_number())
            elif self.currChar in LETTERS:
                tokens.append(self.make_identifier())
            elif self.currChar == '!':
                tok,error = self.make_not_equals()
                if error: return[],error
                tokens.append(tok)
            elif self.currChar == '=':
                tokens.append(self.make_equals())
            elif self.currChar == '<':
                tokens.append(self.make_less_than())
            elif self.currChar == '>':
                tokens.append(self.make_greater_than())
            else:
                posStart = self.position.copy()
                char = self.currChar
                self.advance()
                return [],IllegalCharacterError(posStart,self.position,char)
        tokens.append(Token(TT_EOF,posStart=self.position))
        return tokens,None

    def make_number(self):
        numstr = ''
        dot_cnt = 0
        posStart = self.position.copy()
        while self.currChar != None and self.currChar in DIGITS + '.':
            if self.currChar == '.':
                if dot_cnt == 1:
                    break
                dot_cnt += 1
                numstr+='.'
            else:
                numstr+=self.currChar
            self.advance()
        if dot_cnt==1:
            return Token(TT_FLOAT,float(numstr),posStart,self.position)
        else:
            return Token(TT_INT,int(numstr),posStart,self.position)

    def make_identifier(self):
        id_str = ''
        posStart = self.position.copy()
        while self.currChar != None and self.currChar in LETTERS_DIGITS:
            id_str += self.currChar
            self.advance()

        tok_type = TT_KEYWORD if id_str in KEYWORDS else TT_IDENTIFIER
        return Token(tok_type, id_str, posStart, self.position)

    def make_not_equals(self):
        posStart = self.position.copy()
        self.advance()
        if self.currChar == '=':
            self.advance()
            return Token(TT_NE,posStart=posStart,posEnd=self.position),None
        self.advance()
        return None,ExpectedCharError(posStart,self.position,"'=' (after '!')")

    def make_equals(self):
        tok_type = TT_EQ
        posStart = self.position.copy()
        self.advance()
        if self.currChar == '=':
            self.advance()
            tok_type =  TT_EE
        return Token(tok_type,posStart=posStart,posEnd=self.position)

    def make_less_than(self):
        tok_type = TT_LT
        posStart = self.position.copy()
        self.advance()
        if self.currChar == '=':
            self.advance()
            tok_type =  TT_LTE
        return Token(tok_type,posStart=posStart,posEnd=self.position)

    def make_greater_than(self):
        tok_type = TT_GT
        posStart = self.position.copy()
        self.advance()
        if self.currChar == '=':
            self.advance()
            tok_type =  TT_GTE
        return Token(tok_type,posStart=posStart,posEnd=self.position)
#####################################


#####################################
#PARSE RESULT
#####################################

class ParseResult:
    def __init__(self) -> None:
        self.error = None
        self.node = None
        self.advance_count = 0

    def register_advancement(self):
        self.advance_count+=1

    def register(self,res):
        self.advance_count += res.advance_count
        if res.error: self.error = res.error
        return res.node

    def success(self,node):
        self.node = node
        return self
    
    def failure(self,error):
        if not self.error or self.advance_count == 0:
            self.error = error
        return self


#####################################
#PARSER
#####################################  

############
# NODES     
############
class NumberNode:
    def __init__(self,tok) -> None:
        self.tok = tok

        self.posStart = self.tok.posStart
        self.posEnd = self.tok.posEnd
    def __repr__(self) -> str:
        return f'{self.tok}'
    
class BinOpNode:
    def __init__(self,left,right,op) -> None:
        self.left = left
        self.right = right
        self.op = op
        
        self.posStart = self.left.posStart
        self.posEnd = self.right.posEnd
    def __repr__(self) -> str:
        return f'({self.left},{self.op},{self.right})'
    
class VarAccessNode:
    def __init__(self,var_name_tok) -> None:
        self.var_name_tok = var_name_tok
        self.posStart = self.var_name_tok.posStart
        self.posEnd = self.var_name_tok.posEnd


class VarAssignNode:
    def __init__(self,var_name_tok,value_node) -> None:
        self.var_name_tok = var_name_tok
        self.value_node = value_node
        self.posStart = self.var_name_tok.posStart
        self.posEnd = self.value_node.posEnd
    
class UnaryOpNode:
    def __init__(self,op,node) -> None:
        self.op = op
        self.node = node

        self.posStart = self.op.posStart
        self.posEnd = self.node.posEnd
    def __repr__(self) -> str:
        return f'({self.op},{self.node})'
############

class Parser:
    def __init__(self,token) -> None:
        self.token = token 
        self.idx = -1
        self.advance()

    def advance(self):
        self.idx += 1
        if self.idx<len(self.token):
            self.currentTok = self.token[self.idx]
        return self.currentTok
    
    def atom(self):
        res = ParseResult()
        tok = self.currentTok

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
            if self.currentTok.type == TT_RPAREN:
                res.register_advancement()
                self.advance()
                return res.success(expr)
            else: return res.failure(IllegalSyntaxError(self.currentTok.posStart,self.currentTok.posEnd,"Expected a ')'"))

        return res.failure(IllegalSyntaxError(tok.posStart,tok.posEnd,"Expected an int,float,identifier, '+','-' or '('"))
    
    def power(self):
        return self.binOp(self.atom,(TT_POW,),self.factor)

    
    def factor(self):
        res = ParseResult()
        tok = self.currentTok

        if tok.type in (TT_PLUS,TT_MINUS):
            res.register_advancement()
            self.advance()
            factor = res.register(self.factor())
            if res.error: return res
            return res.success(UnaryOpNode(tok,factor))

        return self.power()

    def term(self):
        return self.binOp(self.factor,(TT_MUL,TT_DIV))

    def expr(self):
        res = ParseResult()
        if self.currentTok.matches(TT_KEYWORD,'VAR'):
            res.register_advancement()
            self.advance()
            if self.currentTok.type != TT_IDENTIFIER:
                return res.failure(IllegalSyntaxError(self.currentTok.posStart,self.currentTok.posEnd,'Expected Identifier'))

            var_name = self.currentTok
            res.register_advancement()
            self.advance()

            if self.currentTok.type != TT_EQ:
                return res.failure(IllegalSyntaxError(self.currentTok.posStart,self.currentTok.posEnd,"Expected an '='"))
            res.register_advancement()
            self.advance()
            expr = res.register(self.expr())
            if res.error: return res
            return res.success(VarAssignNode(var_name,expr))
        node = res.register(self.binOp(self.term,(TT_PLUS,TT_MINUS)))
        if res.error: 
            return res.failure(IllegalSyntaxError(self.currentTok.posStart,self.currentTok.posEnd,"Expected an 'VAR',int,float,identifier, '+','-' or '()'"))
        return res.success(node)

    def binOp(self,func_a,ops,func_b=None):
        if func_b == None:
            func_b = func_a
        res = ParseResult()
        left = res.register(func_a())
        if res.error: return res
        while self.currentTok is not None and self.currentTok.type in ops:
            op_tok = self.currentTok
            res.register_advancement()
            self.advance()
            right = res.register(func_b())
            if res.error: return res
            left = BinOpNode(left,right,op_tok)
        return res.success(left)

        

    def parse(self):
        res = self.expr()
        if not res.error and self.currentTok.type != TT_EOF:
            return res.failure(IllegalSyntaxError(self.currentTok.posStart,self.currentTok.posEnd,"expected '+','-','*'or'/'"))
        return res
    
#####################################


#####################################
#RUNTIME RESULT
#####################################
class RTResult:
    def __init__(self) -> None:
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


#####################################
#VALUES (mathematical operation)
#####################################

class Number:
    def __init__(self,value) -> None:
        self.value = value
        self.set_pos()
        self.set_context()

    def set_pos(self,posStart=None,posEnd=None):
        self.posStart = posStart
        self.posEnd = posEnd
        return self
    
    def set_context(self,context=None):
        self.context = context
        return self

    def added_to(self, other):
        if isinstance(other,Number):
            return Number(self.value + other.value).set_context(self.context),None
        
    def subbed_to(self, other):
        if isinstance(other,Number):
            return Number(self.value - other.value).set_context(self.context),None
        
    def multed_to(self, other):
        if isinstance(other,Number):
            return Number(self.value * other.value).set_context(self.context),None
        
    def dived_to(self, other):
        if isinstance(other,Number):
            if other.value == 0:
                return None,RTError(other.posStart,other.posEnd,'Division by Zero',self.context)
            return Number(self.value / other.value).set_context(self.context),None
        
    def powed_by(self,other):
        if isinstance(other,Number):
            return Number(self.value ** other.value).set_context(self.context),None
        
    def __repr__(self) -> str:
        return str(self.value)


#####################################
#CONTEXT
#####################################

class Context:
    def __init__(self,display_name,parent=None, parent_entry_pos=None) -> None:
        self.display_name = display_name
        self.parent = parent
        self.parent_entry_pos = parent_entry_pos
        self.symbol_table = None


#####################################
#SYMBOL TABLE
#####################################

class SymbolTable:
    def __init__(self) -> None:
        self.symbols = {}
        self.parent = None

    def get(self,name):
        value = self.symbols.get(name,None)
        if value == None and self.parent:
            return self.parent.get(name)
        return value

    def set(self,name,value):
        self.symbols[name] = value

    def remove(self,name):
        del self.symbols[name]
        

#####################################
#INTERPRETER
#####################################

class Interpreter:
    def visit(self,node,context):
        method_name = f'visit_{type(node).__name__}'
        method = getattr(self,method_name,self.no_visit_method)
        return method(node,context)
    
    def no_visit_method(self,node,context):
        raise Exception(f'No visit_{type(node).__name__} method defined')
    
    ###########

    def visit_NumberNode(self,node,context):
        return RTResult().success(Number(node.tok.value).set_context(context).set_pos(node.posStart,node.posEnd))

    def visit_VarAccessNode(self,node,context):
        res = RTResult()
        var_name = node.var_name_tok.value
        value = context.symbol_table.get(var_name)

        if not value:
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
        res = RTResult()
        leftnode = res.register(self.visit(node.left,context))
        if res.error: return res
        rightnode = res.register(self.visit(node.right,context))
        if res.error: return res

        if leftnode is None:
            print("Error: Left operand is None")
        if rightnode is None:
            print("Error: Right operand is None")

        if node.op.type == TT_PLUS:
            result,error = leftnode.added_to(rightnode)
        elif node.op.type == TT_MINUS:
            result,error = leftnode.subbed_to(rightnode)
        elif node.op.type == TT_MUL:
            result,error = leftnode.multed_to(rightnode)
        elif node.op.type == TT_DIV:
            result,error = leftnode.dived_to(rightnode)
        elif node.op.type == TT_POW:
            result,error = leftnode.powed_by(rightnode)

        if error: return res.failure(error)
        else:
            return res.success(result.set_pos(node.posStart,node.posEnd))
    
    def visit_UnaryOpNode(self,node,context):
        res = RTResult()
        number,error = res.register(self.visit(node.node,context))
        if res.error: return res

        error = None
        if node.op.type == TT_PLUS:
            number = number.multed_to(Number(-1))

        if error:
            return res.failure(error)
        else:
            return res.success(number.set_pos(node.posStart,node.posEnd))

#####################################


#####################################
#RUN
#####################################

global_symbol_table = SymbolTable()
global_symbol_table.set('null',Number(0))

def run(fileName,text):
    lexer = Lexer(fileName,text)
    token,error = lexer.make_tokens()
    print(token)
    if error: return None,error
    parse = Parser(token)
    ast = parse.parse()
    #print(ast.node)
    if ast.error: return None,ast.error

    interpreter = Interpreter()
    context = Context('<dhokla Module>')
    context.symbol_table = global_symbol_table
    result = interpreter.visit(ast.node,context)

    return result.value,result.error
    #return ast.node,ast.error#token,ast.node
    
