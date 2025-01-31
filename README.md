# Dhokla: Lightweight Interpreter

## **Overview**
Dhokla is a lightweight interpreter designed to parse and execute custom programming instructions. This project explores the fundamental principles of interpreter design, including tokenization, parsing, and evaluation. It serves as a learning experience in understanding how programming languages process and execute code.

---

## **Features**
- **Lexical Analysis:** Efficient tokenization of input code into meaningful symbols.
- **Parsing:** Validation and structuring of code using syntax rules.
- **Evaluation:** Execution of parsed instructions for real-time results.
- **Public Variables:** All variables are accessible to simplify usage and data manipulation.

---

## **Project Structure**
```
├── dhokla
│   ├── shell.py        # entry point
│   ├── dhokla.py       # Token, Lexer, Parser and Interpreter
└── README.md           # Project documentation
```

---

## **Getting Started**

### **Prerequisites**
Ensure you have Python 3 installed.

### **Installation**
Clone the repository:

```bash
git clone https://github.com/amssGuard/dhokla.git
cd dhokla
```

### **Usage**
Run the interpreter by executing:

```bash
python shell.py
```
Input custom code to see how the interpreter evaluates expressions.

#### **Example:**
```plaintext
Dhokla> 5 + 3 * (2 - 1)
Output: 8
```

---

## **How It Works**

### **1. Lexical Analysis**
The lexer breaks down input code into recognizable tokens such as numbers, operators, and parentheses.

### **2. Parsing**
The parser validates syntax and builds a syntax tree for structured code execution.

### **3. Evaluation**
The interpreter evaluates the syntax tree and computes results.

---

## **Challenges and Solutions**
### **Key Challenges:**
- Efficiently managing nested expressions and operator precedence.
- Ensuring robust error handling for invalid syntax.

### **Solutions:**
- Implemented recursive parsing for better handling of expression trees.
- Developed comprehensive error messages to guide debugging.

---

## **Learning Outcomes**
- Deep understanding of lexical analysis, parsing, and evaluation.
- Practical knowledge of building interpreters from scratch.
- Insight into language design choices and their impact on usability.

---

## **Future Enhancements**
- **Virtual Machine:** Extend the project by implementing a bytecode virtual machine.
- **Additional Features:** Support for control flow, functions, and more advanced data types.
- **Optimizations:** Improve performance and scalability.

---

