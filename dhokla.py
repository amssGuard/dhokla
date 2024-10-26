####################################
#IMPORTS
####################################
from string_with_arrow import *

####################################
#Constant
####################################
DIGITS = "1234567890"

####################################
#Position
####################################
class Position:
    def __init__(self,idx,lineNo,col,text) -> None:
        self.idx = idx
        self.lineNo = lineNo
        self.col = col
        self.text = text

    def advance(self,currCharacter=None):
        self.idx +=1
        self.col +=1

        if currCharacter == '\n':
            self.col = 0
            self.lineNo += 1
        return self
    
    def copy(self): return Position(self.idx,self.lineNo,self.col,self.text)


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
        result += '\n\n'+ string_with_arrows(self.posStart.text,self.posStart,self.posEnd)
        return result

class IllegalCharacterError(Error):
    def __init__(self,posStart,posEnd,detail) -> None:
        super().__init__(posStart,posEnd,"Illegal Character",detail)

class IllegalSyntaxError(Error):
    def __init__(self,posStart,posEnd,detail) -> None:
        super().__init__(posStart,posEnd,"Illegal Syntax",detail)
        


####################################
#TOKENS
####################################
TT_INT    = 'INT'
TT_FLOAT  = 'FLOAT'
TT_PLUS   = 'PLUS'
TT_MINUS  = 'MINUS'
TT_MUL    = 'MUL'
TT_DIV    = 'DIV'
TT_LPAREN = 'LPAREN'
TT_RPAREN = 'RPAREN'
TT_EOF    = 'EOF'

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

    def __repr__(self) -> str:
        if self.value: return f'{self.type}:{self.value}'
        return f'{self.type}'



#####################################
#LEXER
#####################################

class Lexer: 
    def __init__(self,text) -> None:
        self.text = text
        self.position = Position(-1,0,-1,text)
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
            elif self.currChar in DIGITS:
                tokens.append(self.make_number())
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
#####################################


#####################################
#PARSE RESULT
#####################################

class ParseResult:
    def __init__(self) -> None:
        self.error = None
        self.node = None

    def register(self,res):
        if isinstance(res,ParseResult):
            if res.error: self.error = res.error
            return res.node
        return res

    def success(self,node):
        self.node = node
        return self
    
    def failure(self,error):
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
    def __repr__(self) -> str:
        return f'{self.tok}'
    
class BinOpNode:
    def __init__(self,left,right,op) -> None:
        self.left = left
        self.right = right
        self.op = op
    def __repr__(self) -> str:
        return f'({self.left},{self.op},{self.right})'
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
    
    def factor(self):
        res = ParseResult()
        tok = self.currentTok

        if tok.type in (TT_INT,TT_FLOAT):
            res.register(self.advance())
            return res.success(NumberNode(tok))
        return res.failure(IllegalSyntaxError(tok.posStart,tok.posEnd,"Expected an Integer or Float"))

    def term(self):
        return self.binOp(self.factor,(TT_MUL,TT_DIV))

    def expr(self):
        return self.binOp(self.term,(TT_PLUS,TT_MINUS))

    def binOp(self,func,ops):
        res = ParseResult()
        left = res.register(func())
        if res.error: return res
        while self.currentTok is not None and self.currentTok.type in ops:
            op_tok = self.currentTok
            res.register(self.advance())
            right = res.register(func())
            if res.error: return res
            left = BinOpNode(left,right,op_tok)
        return res.success(left)

        

    def parse(self):
        res = self.expr()
        if not res.error and self.currentTok.type != TT_EOF:
            return res.failure(IllegalSyntaxError(self.currentTok.posStart,self.currentTok.posEnd,"expected '+','-','*'or'/'"))
        return res
    
#####################################

def run(text):
    lexer = Lexer(text)
    token,error = lexer.make_tokens()
    if error: return None,error
    parse = Parser(token)
    ast = parse.parse()

    return ast.node,ast.error#token,ast.node
    
