#include "main.h"

#include <stdio.h>
#include <ctype.h>
#include <stdarg.h>

#include "c-minux.tab.h"
#include "eprintf.h"
#include "array.h"

void yyerror(const char *msg) {
    weprintf("%s", msg);
}

FILE *finp;

struct {
    int type;
    const char *lex;
} keywords[] = {
    { IF, "if" },
    { ELSE, "else" },
    { ENDIF, "endif" },
    { INT, "int" },
    { VOID, "void" },
    { FOR, "for" },
    { BREAK, "break" },
    { RETURN, "return" },
    {0}
};

Array(LexEnt) lextab;

int CreateLex(const char *lex) {
    for (int i = 0; i < lextab.len; i++) {
        if (strcmp(lextab.arr[i].lex, lex) == 0)
            return i;
    }
    LexEnt ent = { .lex = estrdup(lex), .type = ID };
    Append(&lextab, ent);
    return lextab.len - 1;
}

LexEnt *GetLex(int id) {
    return &lextab.arr[id];
}

void InitLexTab(void) {
    for (int i = 0; keywords[i].lex != NULL; i++) {
        int id = CreateLex(keywords[i].lex);
        GetLex(id)->type = keywords[i].type;
    }
}

int line = 1, column = 1;

int yylex(void) {
    int c;
    do {
        c = fgetc(finp);
        column++;
        if (c == '\n') {
            column = 1;
            line++;
        }
    } while (c != EOF && isspace(c));
    if (c == EOF)
        return YYEOF;
    yylloc.first_line = line;
    yylloc.first_column = column - 1;
    yylloc.last_line = line;
    yylloc.last_column = column;
    if (isdigit(c)) {
        yylval.number = 0;
        while (c != EOF && isdigit(c)) {
            yylval.number *= 10;
            yylval.number += (c - '0');
            c = fgetc(finp);
            column++;
        }
        if (c != EOF)
            ungetc(c, finp);
        column--;
        yylloc.last_column = column;
        return NUMBER;
    }
    if (isalpha(c)) {
        Str lex = {0};
        do {
            Append(&lex, c);
            c = fgetc(finp);
            column++;
        } while (c != EOF && isalnum(c));
        Append(&lex, '\0');
        if (c != EOF)
            ungetc(c, finp);
        column--;
        yylloc.last_column = column;
        yylval.lexid = CreateLex(lex.arr);
        ArrayRelease(&lex);
        return GetLex(yylval.lexid)->type;
    }
    if (c == '=') {
        c = fgetc(finp);
        column++;
        if (c == '=') {
            yylloc.last_column = column;
            return EQUAL;
        }
        if (c != EOF)
            ungetc(c, finp);
        column--;
        yylloc.last_column = column;
        return '=';
    }
    return c;
}

void InitDeclaration(Declaration *decl) {
    decl->type = -1;
    decl->length = 0;
    decl->parity = -1;
    ArrayZero(&decl->args);
    decl->body = NULL;
}

void DestructDeclaration(Declaration *decl) {
    if (decl == NULL)
        return;
    for (int i = 0; i < decl->args.len; i++) {
        DestructDeclaration(&decl->args.arr[i]);
    }
    ArrayRelease(&decl->args);
    DestructStatement(decl->body);
    free(decl->body);
}

void DestructStatement(Statement *stmt) {
    if (stmt == NULL)
        return;
    if (stmt->type == STMT_COMPOUND) {
        DeclarationArray *declarr = &stmt->declarr;
        for (int i = 0; i < declarr->len; i++) {
            DestructDeclaration(&declarr->arr[i]);
        }
        ArrayRelease(declarr);
        StatementArray *stmtarr = &stmt->stmtarr;
        for (int i = 0; i < stmtarr->len; i++) {
            DestructStatement(&stmtarr->arr[i]);
        }
        ArrayRelease(stmtarr);
    } else if (stmt->type == STMT_EXPR || stmt->type == STMT_RETEXPR) {
        DestructExpression(&stmt->expr);
    } else if (stmt->type == STMT_COND) {
        DestructExpression(&stmt->ifcond);
        DestructStatement(stmt->ifbody);
        free(stmt->ifbody);
        DestructStatement(stmt->elsebody);
        free(stmt->elsebody);
    } else if (stmt->type == STMT_ITER) {
        DestructExpression(&stmt->iterinit);
        DestructExpression(&stmt->itercond);
        DestructExpression(&stmt->iterstep);
        DestructStatement(stmt->iterbody);
        free(stmt->iterbody);
    }
}

void InitExpression(Expression *expr) {
    expr->left = expr->right = NULL;
    ArrayZero(&expr->args);
}

void DestructExpression(Expression *expr) {
    if (expr == NULL)
        return;
    DestructExpression(expr->left);
    free(expr->left);
    DestructExpression(expr->right);
    free(expr->right);
    for (int i = 0; i < expr->args.len; i++) {
        DestructExpression(&expr->args.arr[i]);
    }
    ArrayRelease(&expr->args);
}

DeclarationArray program;

void SetProgram(DeclarationArray *p) {
    program = *p;
}

const char *StrType(int type) {
    if (type == INT)
        return "int";
    if (type == VOID)
        return "void";
    return "???";
}

void PrintDeclarationArray(DeclarationArray *specarr);
void PrintStatement(Statement *stmt);
void PrintExpression(Expression *expr);

void PrintDeclaration(Declaration *decl) {
    printf("%s %s", StrType(decl->type), GetLex(decl->lexid)->lex);
    if (decl->length != 0) {
        printf("[%d]", decl->length);
        return;
    }
    if (decl->parity != -1) {
        printf("(");
        for (int j = 0; j < decl->args.len; j++) {
            Declaration *arg = &decl->args.arr[j];
            if (j > 0)
                printf(", ");
            PrintDeclaration(arg);
        }
        printf(")\n");
        PrintStatement(decl->body);
        return;
    }
}

void PrintDeclarationArray(DeclarationArray *declarr) {
    for (int i = 0; i < declarr->len; i++) {
        Declaration *decl = &declarr->arr[i];
        PrintDeclaration(decl);
        printf(";\n");
    }
}

void PrintStatement(Statement *stmt) {
    switch (stmt->type) {
    case STMT_EXPR: {
        PrintExpression(&stmt->expr);
        printf(";");
        break;
    }
    case STMT_COND: {
        printf("if (");
        PrintExpression(&stmt->ifcond);
        printf(")\n");
        PrintStatement(stmt->ifbody);
        if (stmt->elsebody != NULL) {
            printf("\nelse\n");
            PrintStatement(stmt->elsebody);
        }
        break;
    }
    case STMT_ITER: {
        printf("for (");
        PrintExpression(&stmt->iterinit);
        printf("; ");
        PrintExpression(&stmt->itercond);
        printf("; ");
        PrintExpression(&stmt->iterstep);
        printf(")\n");
        PrintStatement(stmt->iterbody);
        break;
    }
    case STMT_BREAK: {
        printf("break;");
        break;
    }
    case STMT_RETVOID: {
        printf("return;");
        break;
    }
    case STMT_RETEXPR: {
        printf("return ");
        PrintExpression(&stmt->expr);
        printf(";");
        break;
    }
    case STMT_COMPOUND: {
        printf("{\n");
        PrintDeclarationArray(&stmt->declarr);
        StatementArray *stmtarr = &stmt->stmtarr;
        for (int i = 0; i < stmtarr->len; i++) {
            Statement *stmt = &stmtarr->arr[i];
            PrintStatement(stmt);
            printf("\n");
        }
        printf("}");
        break;
    }
    case STMT_NOP: {
        printf(";");
        break;
    }
    default:
        printf("???STATEMENT???");
    }
}

void PrintExpression(Expression *expr) {
    switch(expr->type) {
    case EXPR_VAR: {
        printf("%s", GetLex(expr->lexid)->lex);
        break;
    }
    case EXPR_ARRAYCELL: {
        printf("%s[", GetLex(expr->lexid)->lex);
        PrintExpression(expr->left);
        printf("]");
        break;
    }
    case EXPR_VAR_ASSIGN: {
        printf("(%s = ", GetLex(expr->lexid)->lex);
        PrintExpression(expr->right);
        printf(")");
        break;
    }
    case EXPR_ARRAYCELL_ASSIGN: {
        printf("(%s[", GetLex(expr->lexid)->lex);
        PrintExpression(expr->left);
        printf("] = ");
        PrintExpression(expr->right);
        printf(")");
        break;
    }
    case EXPR_MULT: {
        printf("(");
        PrintExpression(expr->left);
        printf(" * ");
        PrintExpression(expr->right);
        printf(")");
        break;
    }
    case EXPR_ADD: {
        printf("(");
        PrintExpression(expr->left);
        printf(" + ");
        PrintExpression(expr->right);
        printf(")");
        break;
    }
    case EXPR_SUB: {
        printf("(");
        PrintExpression(expr->left);
        printf(" - ");
        PrintExpression(expr->right);
        printf(")");
        break;
    }
    case EXPR_EQUAL: {
        printf("(");
        PrintExpression(expr->left);
        printf(" == ");
        PrintExpression(expr->right);
        printf(")");
        break;
    }
    case EXPR_LESS: {
        printf("(");
        PrintExpression(expr->left);
        printf(" < ");
        PrintExpression(expr->right);
        printf(")");
        break;
    }
    case EXPR_CONST: {
        printf("%d", expr->value);
        break;
    }
    case EXPR_CALL: {
        printf("%s(", GetLex(expr->lexid)->lex);
        ExpressionArray *args = &expr->args;
        for (int i = 0; i < args->len; i++) {
            Expression *arg = &args->arr[i];
            if (i > 0)
                printf(", ");
            PrintExpression(arg);
        }
        printf(")");
        break;
    }
    default:
        printf("???EXPR???");
    }
}

void PrintFileAtLoc(YYLTYPE *loc) {
    FILE *finp;
    if ((finp = fopen("input.txt", "r")) == NULL)
        eprintf("fopen(input.txt):");
    int line = 1, column = 1;
    int c;
    while (line < loc->first_line) {
        c = fgetc(finp);
        if (c == EOF)
            eprintf("unexpected EOF");
        column++;
        if (c == '\n') {
            line++;
            column = 1;
        }
    }
    // fprintf(stderr, "%d-%d\n", loc->first_column, loc->last_column);
    Str str = {0};
    while (line == loc->first_line) {
        c = fgetc(finp);
        if (c == EOF || c == '\n')
            break;
        Append(&str, c);
    }
    Append(&str, '\0');
    fprintf(stderr, " %d\t| %s\n", line, str.arr);
    fprintf(stderr, " \t| ");
    for (int i = 1; i < loc->first_column; i++) {
        c = ' ';
        if (str.arr[i - 1] == '\t')
            c = '\t';
        fputc(c, stderr);
    }
    fputc('^', stderr);
    int last_column = loc->first_line == loc->last_line ? loc->last_column : 1 + StrLen(&str);
    for (int i = loc->first_column+1; i < last_column; i++) {
        fputc('~', stderr);
        if (str.arr[i - 1] == '\t')
            fputc('\t', stderr);
    }
    fputc('\n', stderr);
    ArrayRelease(&str);
    fclose(finp);
}

void ReportError(YYLTYPE *loc, const char *fmt, ...) {
    va_list args;
    fprintf(stderr, "%d.%d: syntax error: ", loc->first_line, loc->first_column);
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fprintf(stderr, "\n");
    PrintFileAtLoc(loc);
}

int main() {
    // yydebug = 1;
    InitLexTab();
    if ((finp = fopen("input.txt", "r")) == NULL)
        eprintf("fopen(input.txt):");
    if (yyparse() != 0)
        eprintf("yyparse() failed");
    // PrintDeclarationArray(&program);
    CreateCodegen();
    CodegenProgram(&program);
    OutputCode();
    DisposeCodegen();
    for (int i = 0; i < program.len; i++) {
        Declaration *decl = &program.arr[i];
        DestructDeclaration(decl);
    }
    ArrayRelease(&program);
    return 0;
}

