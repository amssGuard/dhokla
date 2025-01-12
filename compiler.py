class BytecodeGenerator:
    def generate(self, node):
        bytecode = []

        if isinstance(node, NumberNode):
            bytecode.append(('PUSH', node.tok.value))

        elif isinstance(node, BinOpNode):
            bytecode.extend(self.generate(node.left_node))
            bytecode.extend(self.generate(node.right_node))

            if node.op_tok.type == TT_PLUS:
                bytecode.append(('ADD',))
            elif node.op_tok.type == TT_MINUS:
                bytecode.append(('SUB',))
            elif node.op_tok.type == TT_MUL:
                bytecode.append(('MUL',))
            elif node.op_tok.type == TT_DIV:
                bytecode.append(('DIV',))

        elif isinstance(node, UnaryOpNode):
            bytecode.extend(self.generate(node.node))
            if node.op_tok.type == TT_MINUS:
                bytecode.append(('PUSH', -1))
                bytecode.append(('MUL',))
       

        return bytecode


def run_with_vm(text):
    lexer = Lexer(text)
    tokens, error = lexer.make_tokens()

    if error:
        return None, error

    parser = Parser(tokens)
    ast = parser.parse()

    if ast.error:
        return None, ast.error

    bytecode_generator = BytecodeGenerator()
    bytecode = bytecode_generator.generate(ast.node)

    vm = VM()
    result = vm.execute(bytecode)

    return result, None
