#ifndef MAIN_H
#define MAIN_H

#include "array.h"

typedef struct Location Location;

typedef struct LexEnt LexEnt;

typedef struct Declaration Declaration;
typedef Array(Declaration) DeclarationArray;

typedef struct Statement Statement;
typedef Array(Statement) StatementArray;

typedef struct Expression Expression;
typedef Array(Expression) ExpressionArray;

struct Location {
    int first_line, first_column, last_line, last_column;
};

struct LexEnt {
    const char *lex;
    int type;
};

struct Declaration {
    int type, length, parity, lexid;
    Location loc;
    DeclarationArray args;
    Statement *body;
};

extern void InitDeclaration(Declaration *);
extern void DestructDeclaration(Declaration *);

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
    EXPR_CONST,
    EXPR_ERROR
};

struct Expression {
    int type, lexid, value;
    Expression *left, *right;
    Location loc;
    ExpressionArray args;
};

extern void InitExpression(Expression *);
extern void DestructExpression(Expression *);

enum {
    STMT_EXPR,
    STMT_COND,
    STMT_ITER,
    STMT_BREAK,
    STMT_CONTINUE,
    STMT_RETVOID,
    STMT_RETEXPR,
    STMT_COMPOUND,
    STMT_NOP,
    STMT_ERROR,
};

struct Statement {
    int type;
    Location loc;
    union {
        struct {
            DeclarationArray declarr;
            StatementArray stmtarr;
        };
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

extern void SetProgram(DeclarationArray *);

extern void PrintDeclarationArray(DeclarationArray *);

extern void CreateCodegen(void);
extern void OutputCode(void);
extern void DisposeCodegen(void);
extern void CodegenProgram(DeclarationArray *);

extern void PrintFileAtLoc(Location loc);
extern void ReportError(Location loc, const char *fmt, ...);

#endif
