#include "main.h"
#include <stdio.h>
#include <ctype.h>
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

int yylex(void) {
    int c;
    c = fgetc(finp);
    while (c != EOF && isspace(c)) {
        c = fgetc(finp);
    }
    if (c == EOF)
        return YYEOF;
    if (isdigit(c)) {
        ungetc(c, finp);
        if (fscanf(finp, "%d", &yylval.number) != 1)
            eprintf("scanf number failed:");
        return NUMBER;
    }
    if (isalpha(c)) {
        Str lex = {0};
        do {
            Sprintf(&lex, "%c", c);
            c = fgetc(finp);
        } while (c != EOF && isalnum(c));
        if (c != EOF)
            ungetc(c, finp);
        yylval.lexid = CreateLex(lex.arr);
        ArrayRelease(&lex);
        return GetLex(yylval.lexid)->type;
    }
    if (c == '=') {
        c = fgetc(finp);
        if (c == '=')
            return EQUAL;
        if (c != EOF)
            ungetc(c, finp);
        return '=';
    }
    return c;
}

void InitTypeSpec(TypeSpec *spec) {
    spec->type = -1;
    spec->length = 0;
    spec->parity = -1;
    ArrayZero(&spec->args);
    spec->body = NULL;
}

void DestructTypeSpec(TypeSpec *spec) {
    if (spec == NULL)
        return;
    for (int i = 0; i < spec->args.len; i++) {
        DestructTypeSpec(&spec->args.arr[i]);
    }
    ArrayRelease(&spec->args);
    DestructStatement(spec->body);
    free(spec->body);
}

void DestructStatement(Statement *stmt) {
    if (stmt == NULL)
        return;
    if (stmt->type == STMT_COMPOUND) {
        TypeSpecArray *declarr = &stmt->compound.declarr;
        for (int i = 0; i < declarr->len; i++) {
            DestructTypeSpec(&declarr->arr[i]);
        }
        ArrayRelease(declarr);
        StatementArray *stmtarr = &stmt->compound.stmtarr;
        for (int i = 0; i < stmtarr->len; i++) {
            DestructStatement(&stmtarr->arr[i]);
        }
        ArrayRelease(stmtarr);
        return;
    }
    if (stmt->type == STMT_EXPR || stmt->type == STMT_RETEXPR) {
        DestructExpression(&stmt->expr);
        return;
    }
    if (stmt->type == STMT_COND) {
        DestructExpression(&stmt->ifcond);
        DestructStatement(stmt->ifbody);
        free(stmt->ifbody);
        DestructStatement(stmt->elsebody);
        free(stmt->elsebody);
        return;
    }
    if (stmt->type == STMT_ITER) {
        DestructExpression(&stmt->iterinit);
        DestructExpression(&stmt->itercond);
        DestructExpression(&stmt->iterstep);
        DestructStatement(stmt->iterbody);
        free(stmt->iterbody);
        return;
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

TypeSpecArray program;

void SetProgram(TypeSpecArray *p) {
    program = *p;
}

const char *StrType(int type) {
    if (type == INT)
        return "int";
    if (type == VOID)
        return "void";
    return "???";
}

void PrintTypeSpecArray(TypeSpecArray *specarr);
void PrintStatement(Statement *stmt);
void PrintExpression(Expression *expr);

void PrintTypeSpec(TypeSpec *spec) {
    printf("%s %s", StrType(spec->type), GetLex(spec->lexid)->lex);
    if (spec->length != 0) {
        printf("[%d]", spec->length);
        return;
    }
    if (spec->parity != -1) {
        printf("(");
        for (int j = 0; j < spec->args.len; j++) {
            TypeSpec *arg = &spec->args.arr[j];
            if (j > 0)
                printf(", ");
            PrintTypeSpec(arg);
        }
        printf(")\n");
        PrintStatement(spec->body);
        return;
    }
}

void PrintTypeSpecArray(TypeSpecArray *specarr) {
    for (int i = 0; i < specarr->len; i++) {
        TypeSpec *spec = &specarr->arr[i];
        PrintTypeSpec(spec);
        printf(";\n");
    }
}

void PrintStatement(Statement *stmt) {
    if (stmt->type == STMT_EXPR) {
        PrintExpression(&stmt->expr);
        printf(";");
        return;
    }
    if (stmt->type == STMT_COND) {
        printf("if (");
        PrintExpression(&stmt->ifcond);
        printf(")\n");
        PrintStatement(stmt->ifbody);
        if (stmt->elsebody != NULL) {
            printf("\nelse\n");
            PrintStatement(stmt->elsebody);
        }
        return;
    }
    if (stmt->type == STMT_ITER) {
        printf("for (");
        PrintExpression(&stmt->iterinit);
        printf("; ");
        PrintExpression(&stmt->itercond);
        printf("; ");
        PrintExpression(&stmt->iterstep);
        printf(")\n");
        PrintStatement(stmt->iterbody);
        return;
    }
    if (stmt->type == STMT_BREAK) {
        printf("break;");
        return;
    }
    if (stmt->type == STMT_RETVOID) {
        printf("return;");
        return;
    }
    if (stmt->type == STMT_RETEXPR) {
        printf("return ");
        PrintExpression(&stmt->expr);
        printf(";");
        return;
    }
    if (stmt->type == STMT_COMPOUND) {
        printf("{\n");
        PrintTypeSpecArray(&stmt->compound.declarr);
        StatementArray *stmtarr = &stmt->compound.stmtarr;
        for (int i = 0; i < stmtarr->len; i++) {
            Statement *stmt = &stmtarr->arr[i];
            PrintStatement(stmt);
            printf("\n");
        }
        printf("}");
        return;
    }
    if (stmt->type == STMT_NOP) {
        printf(";");
        return;
    }
    printf("INVALID_STATMENT");
}

void PrintExpression(Expression *expr) {
    if (expr->type == EXPR_VAR) {
        printf("%s", GetLex(expr->lexid)->lex);
        return;
    }
    if (expr->type == EXPR_ARRAYCELL) {
        printf("%s[", GetLex(expr->lexid)->lex);
        PrintExpression(expr->left);
        printf("]");
        return;
    }
    if (expr->type == EXPR_VAR_ASSIGN) {
        printf("(%s = ", GetLex(expr->lexid)->lex);
        PrintExpression(expr->right);
        printf(")");
        return;
    }
    if (expr->type == EXPR_ARRAYCELL_ASSIGN) {
        printf("(%s[", GetLex(expr->lexid)->lex);
        PrintExpression(expr->left);
        printf("] = ");
        PrintExpression(expr->right);
        printf(")");
        return;
    }
    if (expr->type == EXPR_MULT) {
        printf("(");
        PrintExpression(expr->left);
        printf(" * ");
        PrintExpression(expr->right);
        printf(")");
        return;
    }
    if (expr->type == EXPR_ADD) {
        printf("(");
        PrintExpression(expr->left);
        printf(" + ");
        PrintExpression(expr->right);
        printf(")");
        return;
    }
    if (expr->type == EXPR_SUB) {
        printf("(");
        PrintExpression(expr->left);
        printf(" - ");
        PrintExpression(expr->right);
        printf(")");
        return;
    }
    if (expr->type == EXPR_EQUAL) {
        printf("(");
        PrintExpression(expr->left);
        printf(" == ");
        PrintExpression(expr->right);
        printf(")");
        return;
    }
    if (expr->type == EXPR_LESS) {
        printf("(");
        PrintExpression(expr->left);
        printf(" < ");
        PrintExpression(expr->right);
        printf(")");
        return;
    }
    if (expr->type == EXPR_CONST) {
        printf("%d", expr->value);
        return;
    }
    if (expr->type == EXPR_CALL) {
        printf("%s(", GetLex(expr->lexid)->lex);
        ExpressionArray *args = &expr->args;
        for (int i = 0; i < args->len; i++) {
            Expression *arg = &args->arr[i];
            if (i > 0)
                printf(", ");
            PrintExpression(arg);
        }
        printf(")");
        return;
    }
    printf("INVALID_EXPRESSION");
}


int main() {
    // yydebug = 1;
    InitLexTab();
    if ((finp = fopen("input.txt", "r")) == NULL)
        eprintf("fopen(input.txt):");
    if (yyparse() != 0)
        eprintf("yyparse() failed");
    PrintTypeSpecArray(&program);
    for (int i = 0; i < program.len; i++) {
        TypeSpec *decl = &program.arr[i];
        DestructTypeSpec(decl);
    }
    ArrayRelease(&program);
    printf("done!\n");
    return 0;
}

