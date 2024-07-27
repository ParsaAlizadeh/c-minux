#ifndef MAIN_H
#define MAIN_H

#include "array.h"

typedef struct Location Location;

typedef struct LexEnt LexEnt;

typedef struct Kind Kind;
typedef Array(Kind) KindArray;

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

enum KindType {
    KIND_VOID,
    KIND_INT,
    KIND_ARRAY,
    KIND_POINTER,
    KIND_RETVOID,
    KIND_RETINT,
};

struct Kind {
    enum KindType type;
    int length;
    KindArray args;
};

extern void InitKind(Kind *);
extern void DestructKind(Kind *);

struct Declaration {
    int lexid;
    Location loc;
    Kind kind;
    DeclarationArray args;
    Statement *body;
};

extern void InitDeclaration(Declaration *);
extern void DestructDeclaration(Declaration *);

enum ExprType {
    EXPR_VAR,
    EXPR_ARRAYCELL,
    EXPR_ASSIGN,
    EXPR_MUL,
    EXPR_ADD,
    EXPR_SUB,
    EXPR_DIV,
    EXPR_REM,
    EXPR_BITAND,
    EXPR_BITOR,
    EXPR_BITXOR,
    EXPR_EQUAL,
    EXPR_LESS,
    EXPR_GREATER,
    EXPR_EQLT,
    EXPR_EQGT,
    EXPR_CALL,
    EXPR_CONST,
    EXPR_ERROR
};

struct Expression {
    enum ExprType type;
    int lexid, value;
    Expression *left, *right;
    Location loc;
    ExpressionArray args;
};

extern void InitExpression(Expression *);
extern void DestructExpression(Expression *);

enum StmtType {
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
    enum StmtType type;
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
