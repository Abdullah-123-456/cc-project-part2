#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <cctype>
#include <memory>
#include <stack>

using namespace std;

// Parse tree node structure
struct ParseTreeNode {
    string value;
    vector<shared_ptr<ParseTreeNode>> children;

    ParseTreeNode(const string& val) : value(val) {}

    void addChild(shared_ptr<ParseTreeNode> child) {
        children.push_back(child);
    }
};

// Token types
enum TokenType {
    IDENTIFIER, NUMBER,
    TYPE_ADADI, TYPE_ASHRIYA, TYPE_HARF, TYPE_MATN, TYPE_MANTIQI,
    LPAREN, RPAREN, LBRACE, RBRACE,
    COMMA, SEMICOLON_DOUBLE, COLON_EQUALS,
    EQUALS_EQUALS, LESS_THAN, GREATER_THAN, LESS_EQUALS, GREATER_EQUALS, NOT_EQUALS, ANGLE_NOT_EQUALS,
    PLUS, MINUS, MULTIPLY, DIVIDE,
    FOR, WHILE, AGAR, WAGARNA,
    END_OF_FILE, ERROR
};

class Token {
public:
    TokenType type;
    string lexeme;
    int line;
    int column;

    Token(TokenType t = ERROR, string lex = "", int ln = 0, int col = 0)
        : type(t), lexeme(lex), line(ln), column(col) {}
};

class Parser {
private:
    vector<Token> tokens;
    size_t currentTokenIndex = 0;
    bool hasError = false;
    bool panicMode = false;
    shared_ptr<ParseTreeNode> parseTreeRoot;
    stack<shared_ptr<ParseTreeNode>> nodeStack;

    // Helper function to create and push a new node
    void pushNode(const string& name) {
        auto node = make_shared<ParseTreeNode>(name);
        if (!nodeStack.empty()) {
            nodeStack.top()->addChild(node);
        }
        nodeStack.push(node);
    }

    // Helper function to pop the current node
    void popNode() {
        if (!nodeStack.empty()) {
            if (nodeStack.size() == 1) {
                parseTreeRoot = nodeStack.top();
            }
            nodeStack.pop();
        }
    }

    // Helper function to add a terminal node
    void addTerminalNode(const string& value) {
        if (!nodeStack.empty()) {
            nodeStack.top()->addChild(make_shared<ParseTreeNode>(value));
        }
    }

    // Print parse tree recursively
    void printParseTree(const shared_ptr<ParseTreeNode>& node, int depth = 0) {
        if (!node) return;

        for (int i = 0; i < depth; ++i) {
            cout << "|   ";
        }

        cout << "-> " << node->value << endl;

        for (const auto& child : node->children) {
            printParseTree(child, depth + 1);
        }
    }

    Token currentToken() {
        if (currentTokenIndex < tokens.size()) {
            return tokens[currentTokenIndex];
        }
        return Token(END_OF_FILE, "", 0, 0);
    }

    void advance() {
        if (currentTokenIndex < tokens.size()) {
            currentTokenIndex++;
        }
    }

    bool match(TokenType expected) {
        if (currentToken().type == expected) {
            addTerminalNode(currentToken().lexeme);
            advance();
            return true;
        }
        return false;
    }

    void error(const string& message) {
        if (panicMode) return;

        Token t = currentToken();
        cerr << "Parse error at line " << t.line << ", column " << t.column
            << ": " << message << ". Found '" << t.lexeme << "'" << endl;
        hasError = true;

        panicMode = true;
        synchronize();
        panicMode = false;
    }

    void synchronize() {
        while (currentToken().type != END_OF_FILE) {
            if (currentToken().type == SEMICOLON_DOUBLE) {
                advance();
                return;
            }

            switch (currentToken().type) {
            case FOR:
            case WHILE:
            case AGAR:
            case TYPE_ADADI:
            case TYPE_ASHRIYA:
            case TYPE_HARF:
            case TYPE_MATN:
            case TYPE_MANTIQI:
            case LBRACE:
                return;
            default:
                advance();
            }
        }
    }

    bool consume(TokenType expected, const string& errorMsg) {
        if (match(expected)) {
            return true;
        }
        error(errorMsg);
        return false;
    }

public:
    Parser(const vector<Token>& tokenList) : tokens(tokenList) {
        parseTreeRoot = make_shared<ParseTreeNode>("Program");
        nodeStack.push(parseTreeRoot);
    }

    bool parse() {
        Function();
        if (!hasError && currentToken().type != END_OF_FILE) {
            error("Unexpected tokens at end of input");
        }

        if (!hasError) {
            cout << "\nParse Tree:\n";
            cout << "==========\n";
            printParseTree(parseTreeRoot);
        }

        return !hasError;
    }

private:
    // Grammar rule implementations
    void Function() {
        pushNode("Function");

        Type();
        consume(IDENTIFIER, "Expected function name identifier");
        consume(LBRACE, "Expected '{' after function name");
        ArgList();
        consume(RBRACE, "Expected '}' after argument list");
        Stmt();

        popNode();
    }

    void ArgList() {
        pushNode("ArgList");
        if (currentToken().type != RBRACE) {
            Arg();
            ArgListPrime();
        }
        popNode();
    }

    void ArgListPrime() {
        pushNode("ArgList'");
        if (match(COMMA)) {
            Arg();
            ArgListPrime();
        }
        popNode();
    }

    void Arg() {
        pushNode("Arg");
        consume(IDENTIFIER, "Expected parameter name identifier");
        Type();
        popNode();
    }

    void Declaration() {
        pushNode("Declaration");
        IdentList();
        Type();
        consume(SEMICOLON_DOUBLE, "Expected ';;' after declaration");
        popNode();
    }

    void Type() {
        pushNode("Type");
        TokenType type = currentToken().type;
        if (type == TYPE_ADADI || type == TYPE_ASHRIYA || type == TYPE_HARF ||
            type == TYPE_MATN || type == TYPE_MANTIQI) {
            addTerminalNode(currentToken().lexeme);
            advance();
        }
        else {
            error("Expected a type keyword");
        }
        popNode();
    }

    void IdentList() {
        pushNode("IdentList");
        if (!consume(IDENTIFIER, "Expected identifier in identifier list")) {
            popNode();
            return;
        }
        IdentTail();
        popNode();
    }

    void IdentTail() {
        pushNode("IdentTail");
        if (match(COMMA)) {
            IdentList();
        }
        popNode();
    }

    void Stmt() {
        pushNode("Stmt");
        TokenType type = currentToken().type;
        if (type == FOR) {
            ForStmt();
        }
        else if (type == WHILE) {
            WhileStmt();
        }
        else if (type == AGAR) {
            IfStmt();
        }
        else if (type == LBRACE) {
            CompStmt();
        }
        else if (isTypeToken(type)) {
            Declaration();
        }
        else if (match(SEMICOLON_DOUBLE)) {
            // Empty statement
        }
        else {
            Expr();
            consume(SEMICOLON_DOUBLE, "Expected ';;' after expression");
        }
        popNode();
    }

    bool isTypeToken(TokenType type) {
        return type == TYPE_ADADI || type == TYPE_ASHRIYA || type == TYPE_HARF ||
            type == TYPE_MATN || type == TYPE_MANTIQI;
    }

    void ForStmt() {
        pushNode("ForStmt");
        consume(FOR, "Expected 'for'");
        consume(LPAREN, "Expected '(' after 'for'");
        OptExpr();
        consume(SEMICOLON_DOUBLE, "Expected ';;' after for initializer");
        OptExpr();
        consume(SEMICOLON_DOUBLE, "Expected ';;' after for condition");
        Expr();
        consume(RPAREN, "Expected ')' after for clauses");
        Stmt();
        popNode();
    }

    void WhileStmt() {
        pushNode("WhileStmt");
        consume(WHILE, "Expected 'while'");
        consume(LPAREN, "Expected '(' after 'while'");
        OptExpr();
        consume(RPAREN, "Expected ')' after while condition");
        Stmt();
        popNode();
    }

    void IfStmt() {
        pushNode("IfStmt");
        consume(AGAR, "Expected 'agar'");
        consume(LPAREN, "Expected '(' after 'agar'");
        OptExpr();
        consume(RPAREN, "Expected ')' after if condition");
        Stmt();
        ElsePart();
        popNode();
    }

    void ElsePart() {
        pushNode("ElsePart");
        if (match(WAGARNA)) {
            Declaration();
        }
        popNode();
    }

    void CompStmt() {
        pushNode("CompStmt");
        consume(LBRACE, "Expected '{'");
        StmtList();
        consume(RBRACE, "Expected '}'");
        popNode();
    }

    void StmtList() {
        pushNode("StmtList");
        while (currentToken().type != RBRACE && currentToken().type != END_OF_FILE) {
            Stmt();
        }
        popNode();
    }

    void Expr() {
        pushNode("Expr");
        Rvalue();
        AssignTail();
        popNode();
    }

    void AssignTail() {
        pushNode("AssignTail");
        if (match(SEMICOLON_DOUBLE)) {
            // Do nothing, already matched
        }
        else if (match(COLON_EQUALS)) {
            consume(IDENTIFIER, "Expected identifier after ':='");
        }
        popNode();
    }

    void Rvalue() {
        pushNode("Rvalue");
        Mag();
        CompareTail();
        popNode();
    }

    void CompareTail() {
        pushNode("CompareTail");
        if (isCompareOp(currentToken().type)) {
            Compare();
            Mag();
            CompareTail();
        }
        popNode();
    }

    bool isCompareOp(TokenType type) {
        return type == EQUALS_EQUALS || type == LESS_THAN || type == GREATER_THAN ||
            type == LESS_EQUALS || type == GREATER_EQUALS || type == NOT_EQUALS ||
            type == ANGLE_NOT_EQUALS;
    }

    void Compare() {
        pushNode("Compare");
        TokenType type = currentToken().type;
        if (isCompareOp(type)) {
            addTerminalNode(currentToken().lexeme);
            advance();
        }
        else {
            error("Expected comparison operator");
        }
        popNode();
    }

    void Mag() {
        pushNode("Mag");
        Term();
        AddSubTail();
        popNode();
    }

    void AddSubTail() {
        pushNode("AddSubTail");
        if (match(PLUS)) {
            Term();
            AddSubTail();
        }
        else if (match(MINUS)) {
            Term();
            AddSubTail();
        }
        popNode();
    }

    void Term() {
        pushNode("Term");
        Factor();
        MulDivTail();
        popNode();
    }

    void MulDivTail() {
        pushNode("MulDivTail");
        if (match(MULTIPLY)) {
            Term();
        }
        else if (match(DIVIDE)) {
            Factor();
        }
        popNode();
    }

    void Factor() {
        pushNode("Factor");
        if (match(LPAREN)) {
            Rvalue();
            consume(RPAREN, "Expected ')' after expression");
        }
        else if (match(IDENTIFIER) || match(NUMBER)) {
            // Already consumed by match
        }
        else {
            error("Expected identifier, number, or '('");
        }
        popNode();
    }

    void OptExpr() {
        pushNode("OptExpr");
        if (currentToken().type != SEMICOLON_DOUBLE && currentToken().type != RPAREN) {
            Expr();
        }
        popNode();
    }
};

vector<Token> loadTokensFromFile(const string& filename) {
    vector<Token> tokens;
    ifstream file(filename);
    if (!file.is_open()) {
        cerr << "Error: Could not open token file: " << filename << endl;
        return tokens;
    }

    string tokenStr;
    int line = 1;
    while (getline(file, tokenStr)) {
        TokenType type = ERROR;

        if (tokenStr == "(") type = LPAREN;
        else if (tokenStr == ")") type = RPAREN;
        else if (tokenStr == "{") type = LBRACE;
        else if (tokenStr == "}") type = RBRACE;
        else if (tokenStr == ",") type = COMMA;
        else if (tokenStr == ";;") type = SEMICOLON_DOUBLE;
        else if (tokenStr == ":=") type = COLON_EQUALS;
        else if (tokenStr == "==") type = EQUALS_EQUALS;
        else if (tokenStr == "<") type = LESS_THAN;
        else if (tokenStr == ">") type = GREATER_THAN;
        else if (tokenStr == "<=") type = LESS_EQUALS;
        else if (tokenStr == ">=") type = GREATER_EQUALS;
        else if (tokenStr == "!=") type = NOT_EQUALS;
        else if (tokenStr == "<>") type = ANGLE_NOT_EQUALS;
        else if (tokenStr == "+") type = PLUS;
        else if (tokenStr == "-") type = MINUS;
        else if (tokenStr == "*") type = MULTIPLY;
        else if (tokenStr == "/") type = DIVIDE;
        else if (tokenStr == "for") type = FOR;
        else if (tokenStr == "while") type = WHILE;
        else if (tokenStr == "agar") type = AGAR;
        else if (tokenStr == "wagarna") type = WAGARNA;
        else if (tokenStr == "adadi") type = TYPE_ADADI;
        else if (tokenStr == "ashriya") type = TYPE_ASHRIYA;
        else if (tokenStr == "harf") type = TYPE_HARF;
        else if (tokenStr == "matn") type = TYPE_MATN;
        else if (tokenStr == "mantiqi") type = TYPE_MANTIQI;
        else if (isdigit(tokenStr[0])) type = NUMBER;
        else if (isalpha(tokenStr[0]) || tokenStr[0] == '_') type = IDENTIFIER;

        tokens.push_back(Token(type, tokenStr, line, 0));
        line++;
    }

    return tokens;
}

int main() {
    const string tokenFile = "tokens1.txt";
    vector<Token> tokens = loadTokensFromFile(tokenFile);

    if (tokens.empty()) {
        cerr << "No tokens loaded. Exiting." << endl;
        return 1;
    }

    Parser parser(tokens);
    if (parser.parse()) {
        cout << "Parsing completed successfully!" << endl;
    }
    else {
        cout << "Parsing completed with errors." << endl;
    }

    return 0;
}
