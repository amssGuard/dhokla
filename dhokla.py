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
                print("---Error Message-----")
                return []
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


            


def run(text):
    lexer = Lexer(text)
    token = lexer.make_tokens()
    return token
    