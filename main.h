#ifndef MAIN_H
#define MAIN_H

#include "array.h"

typedef struct LexEnt LexEnt;

typedef struct TypeSpec TypeSpec;
typedef Array(TypeSpec) TypeSpecArray;

typedef struct CompoundStatement CompoundStatement;

typedef struct Expression Expression;
typedef Array(Expression) ExpressionArray;

typedef struct Statement Statement;
typedef Array(Statement) StatementArray;

struct LexEnt {
    const char *lex;
    int type;
};

struct TypeSpec {
    int type, length, parity;
    int lexid;
    TypeSpecArray args;
    Statement *body;
};

extern void InitTypeSpec(TypeSpec *);
extern void DestructTypeSpec(TypeSpec *);

struct CompoundStatement {
    TypeSpecArray declarr;
    StatementArray stmtarr;
};

enum {
    EXPR_VAR,
    EXPR_ARRAYCELL,
    EXPR_VAR_ASSIGN,
    EXPR_ARRAYCELL_ASSIGN,
    EXPR_MULT,
    EXPR_ADD,
    EXPR_SUB,
    EXPR_EQUAL,
    EXPR_LESS,
    EXPR_CALL,
    EXPR_CONST
};

struct Expression {
    int type;
    Expression *left, *right;
    int lexid;
    ExpressionArray args;
    int value;
};

extern void InitExpression(Expression *);
extern void DestructExpression(Expression *);

enum {
    STMT_EXPR,
    STMT_COND,
    STMT_ITER,
    STMT_BREAK,
    STMT_RETVOID,
    STMT_RETEXPR,
    STMT_COMPOUND,
    STMT_NOP
};

struct Statement {
    int type;
    union {
        CompoundStatement compound;
        Expression expr;
        struct {
            Expression iterinit, itercond, iterstep;
            Statement *iterbody;
        };
        struct {
            Expression ifcond;
            Statement *ifbody, *elsebody;
        };
    };
};

extern void DestructStatement(Statement *);

extern int CreateLex(const char *);
extern LexEnt *GetLex(int);

extern void SetProgram(TypeSpecArray *);

#endif