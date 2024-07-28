#include "main.h"
#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include "array.h"
#include "c-minux.tab.h"
#include "eprintf.h"

void yyerror(const char *msg) {
    weprintf("%s", msg);
}

static const char *inputpath;
static FILE *finp;

static struct {
    int type;
    const char *lex;
} keywords[] = {
    { IF, "if" },
    { ELSE, "else" },
    { INT, "int" },
    { VOID, "void" },
    { FOR, "for" },
    { BREAK, "break" },
    { CONTINUE, "continue" },
    { RETURN, "return" },
    {0}
};

static Array(LexEnt) lextab;

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

static void InitLexTab(void) {
    for (int i = 0; keywords[i].lex != NULL; i++) {
        int id = CreateLex(keywords[i].lex);
        GetLex(id)->type = keywords[i].type;
    }
}

static int line = 1, column = 1, last_line = 1, last_column = 0;

static int Fgetc(void) {
    int c = fgetc(finp);
    if (c == EOF)
        return c;
    last_line = line;
    last_column = column;
    if (c == '\n') {
        line++;
        column = 1;
    } else {
        column++;
    }
    return c;
}

static void Fungetc(int c) {
    ungetc(c, finp);
    line = last_line;
    column = last_column;
}

static int eqfollow[] = {
    ['='] = EQUAL,
    ['<'] = EQLT,
    ['>'] = EQGT,
    ['!'] = NOTEQ,
    ['|'] = OREQ,
    ['^'] = XOREQ,
    ['&'] = ANDEQ,
    ['+'] = ADDEQ,
    ['-'] = SUBEQ,
    ['*'] = MULEQ,
    ['/'] = DIVEQ,
    ['%'] = REMEQ,
};

int yylex(void) {
    int c;
    do {
        c = Fgetc();
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
            c = Fgetc();
        }
        if (c != EOF)
            Fungetc(c);
        yylloc.last_column = column;
        return NUMBER;
    }
    if (isalpha(c) || c == '_') {
        Str lex = {0};
        do {
            Append(&lex, c);
            c = Fgetc();
        } while (c != EOF && (isalnum(c) || c == '_'));
        Append(&lex, '\0');
        if (c != EOF)
            Fungetc(c);
        yylloc.last_column = column;
        yylval.lexid = CreateLex(lex.arr);
        ArrayRelease(&lex);
        return GetLex(yylval.lexid)->type;
    }
    /* operators which could follow by equal sign, also && || */
    if (strchr("=<>!|^&+-*/%", c) != NULL) {
        int cnxt = Fgetc();
        if (cnxt == '=') {
            yylloc.last_column = column;
            return eqfollow[c];
        }
        if (c == '&' && cnxt == '&') {
            yylloc.last_column = column;
            return ANDTHEN;
        }
        if (c == '|' && cnxt == '|') {
            yylloc.last_column = column;
            return ORELSE;
        }
        if (cnxt != EOF)
            Fungetc(cnxt);
        return c;
    }
    return c;
}

void InitKind(Kind *kind) {
    kind->type = KIND_VOID;
    kind->length = 0;
    ArrayZero(&kind->args);
}

void DestructKind(Kind *kind) {
    ArrayRelease(&kind->args);
}

void InitDeclaration(Declaration *decl) {
    decl->lexid = -1;
    InitKind(&decl->kind);
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
    DestructKind(&decl->kind);
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
    expr->type = -1;
    expr->assign = 0;
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

static DeclarationArray program;

void SetProgram(DeclarationArray *p) {
    program = *p;
}

void PrintFileAtLoc(Location loc) {
    FILE *finp;
    if ((finp = fopen(inputpath, "r")) == NULL)
        eprintf("fopen(%s):", inputpath);
    int line = 1, column = 1;
    int c;
    while (line < loc.first_line) {
        c = fgetc(finp);
        if (c == EOF)
            eprintf("unexpected EOF");
        column++;
        if (c == '\n') {
            line++;
            column = 1;
        }
    }
    Str str = {0};
    while (line == loc.first_line) {
        c = fgetc(finp);
        if (c == EOF || c == '\n')
            break;
        Append(&str, c);
    }
    Append(&str, '\0');
    fprintf(stderr, " %d\t| %s\n", line, str.arr);
    fprintf(stderr, " \t| ");
    for (int i = 1; i < loc.first_column; i++) {
        c = ' ';
        if (str.arr[i - 1] == '\t')
            c = '\t';
        fputc(c, stderr);
    }
    fputc('^', stderr);
    int last_column = loc.first_line == loc.last_line ? loc.last_column : 1 + StrLen(&str);
    for (int i = loc.first_column+1; i < last_column; i++) {
        fputc('~', stderr);
        if (str.arr[i - 1] == '\t')
            fputc('\t', stderr);
    }
    fputc('\n', stderr);
    ArrayRelease(&str);
    fclose(finp);
}

static void PrintLocation(Location loc) {
    fprintf(stderr, "%d.%d", loc.first_line, loc.first_column);
    if (loc.last_line != loc.first_line) {
        fprintf(stderr, "-%d.%d", loc.last_line, loc.first_column);
    } else if (loc.last_column > loc.first_column + 1) {
        fprintf(stderr, "-%d", loc.last_column);
    }
}

static int errorhappend;

void ReportError(Location loc, const char *fmt, ...) {
    va_list args;
    PrintLocation(loc);
    fprintf(stderr, ": syntax error: ");
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fprintf(stderr, "\n");
    PrintFileAtLoc(loc);
    errorhappend++;
}

int main(int argc, char *argv[]) {
    setprogname(argv[0]);
    if (argc != 2)
        eprintf("usage: %s <input.cmx>", getprogname());
    inputpath = argv[1];
    // yydebug = 1;
    InitLexTab();
    if ((finp = fopen(inputpath, "r")) == NULL)
        eprintf("fopen(%s):", inputpath);
    if (yyparse() != 0)
        eprintf("parse error reported, not checking semantics");
    // PrintDeclarationArray(&program);
    CreateCodegen();
    CodegenProgram(&program);
    if (errorhappend > 0)
        weprintf("%d syntax error(s) reported, not generating code", errorhappend);
    else
        OutputCode();
    DisposeCodegen();
    for (int i = 0; i < program.len; i++) {
        Declaration *decl = &program.arr[i];
        DestructDeclaration(decl);
    }
    ArrayRelease(&program);
    if (errorhappend > 0)
        return EXIT_FAILURE;
    return EXIT_SUCCESS;
}

