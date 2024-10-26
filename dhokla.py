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

    def advance(self,currCharacter):
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
        return result

class IllegalCharacterError(Error):
    def __init__(self,posStart,posEnd,detail) -> None:
        super().__init__(posStart,posEnd,"Illegal Character",detail)
        


####################################
#TOKENS
####################################
TT_INT = 'INT'
TT_FLOAT = 'FLOAT'
TT_PLUS = 'PLUS'
TT_MINUS = 'MINUS'
TT_MUL = 'MUL'
TT_DIV = 'DIV'
TT_LPAREN = 'LPAREN'
TT_RPAREN = 'RPAREN'

class Token:
    def __init__(self,type_,value=None) -> None:
        self.type = type_
        self.value = value

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
                tokens.append(Token(TT_PLUS))
                self.advance()
            elif self.currChar == '-':
                tokens.append(Token(TT_MINUS))
                self.advance()
            elif self.currChar == '*':
                tokens.append(Token(TT_MUL))
                self.advance()
            elif self.currChar == '/':
                tokens.append(Token(TT_DIV))
                self.advance()
            elif self.currChar in DIGITS:
                tokens.append(self.make_number())
            else:
                posStart = self.position.copy()
                char = self.currChar
                self.advance()
                return [],IllegalCharacterError(posStart,self.position,char)
        return tokens

    def make_number(self):
        numstr = ''
        dot_cnt = 0
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
            return Token(TT_FLOAT,float(numstr))
        else:
            return Token(TT_INT,int(numstr))
#####################################


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
        tok = self.currentTok

        if tok.type in (TT_INT,TT_FLOAT):
            self.advance()
            return NumberNode(tok)

    def term(self):
        return self.binOp(self.factor,(TT_MUL,TT_DIV))

    def expr(self):
        return self.binOp(self.term,(TT_PLUS,TT_MINUS))

    def binOp(self,func,ops):
        left = func()
        while self.currentTok is not None and self.currentTok.type in ops:
            op_tok = self.currentTok
            self.advance()
            right = func()
            left = BinOpNode(left,right,op_tok)
        return left

        

    def parse(self):
        res = self.expr()
        return res
    
#####################################

def run(text):
    lexer = Lexer(text)
    token,error = lexer.make_tokens()
    if error: return None,error
    parse = Parser(token)
    ast = parse.parse()

    return token,ast
    
