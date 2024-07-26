#include <stdio.h>

#include "c-minux.tab.h"

static const char *StrType(int type) {
    if (type == INT)
        return "int";
    if (type == VOID)
        return "void";
    return "???";
}

void PrintDeclarationArray(DeclarationArray *specarr);
static void PrintStatement(Statement *stmt);
static void PrintExpression(Expression *expr);

static void PrintDeclaration(Declaration *decl) {
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

static void PrintStatement(Statement *stmt) {
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

static void PrintExpression(Expression *expr) {
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
