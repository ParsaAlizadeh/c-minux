%code requires {
    #include "main.h"
    extern int yylex(void);
    extern void yyerror(const char *);
}

%locations
%define parse.error custom

%code {
    extern void PrintFileAtLoc(YYLTYPE *loc);
    extern void ReportError(YYLTYPE *loc, const char *fmt, ...);
}

%union {
    DeclarationArray declarr;
    Declaration decl;
    int number, lexid;
    StatementArray stmtarr;
    Statement stmt;
    Expression expr;
    ExpressionArray exprarr;
}

%destructor {
    for (int i = 0; i < $$.len; i++)
        DestructDeclaration(&$$.arr[i]);
    ArrayRelease(&$$);
} <declarr>

%destructor {
    for (int i = 0; i < $$.len; i++)
        DestructStatement(&$$.arr[i]);
    ArrayRelease(&$$);
} <stmtarr>

%destructor {
    for (int i = 0; i < $$.len; i++)
        DestructExpression(&$$.arr[i]);
    ArrayRelease(&$$);
} <exprarr>

%destructor {
    DestructDeclaration(&$$);
} <decl>

%destructor {
    DestructStatement(&$$);
} <stmt>

%destructor {
    DestructExpression(&$$);
} <expr>

%token <number> NUMBER 
%token <lexid> ID
%token EQUAL "=="
%token <lexid> IF "if" ELSE "else" ENDIF "endif" FOR "for" BREAK "break" RETURN "return" INT "int" VOID "void"

%nterm <declarr> declarations params param-list
%nterm <decl> declare param
%nterm <lexid> type
%nterm <stmtarr> statements
%nterm <stmt> statement compound-stmt expr-stmt cond-stmt iter-stmt break-stmt ret-stmt
%nterm <expr> expr
%nterm <exprarr> arguments arg-list

%right '='
%nonassoc "==" '<'
%left '+' '-'
%left '*'

%%
start: declarations { SetProgram(&$1); }
declarations: %empty {
    ArrayZero(&$$);
};
declarations: declarations declare {
    $$ = $1;
    Append(&$$, $2);
};
declare: type ID ';' {
    InitDeclaration(&$$);
    $$.type = GetLex($1)->type;
    $$.lexid = $2;
};
type: "int" | "void";
declare: type ID '[' NUMBER ']' ';' {
    InitDeclaration(&$$);
    $$.type = GetLex($1)->type;
    $$.lexid = $2;
    $$.length = $4;
    if ($$.length <= 0) {
        ReportError(&@4, "array length must be positive, got %d", $$.length);
        YYERROR;
    }
};
declare: type ID '(' params ')' compound-stmt {
    InitDeclaration(&$$);
    $$.type = GetLex($1)->type;
    $$.lexid = $2;
    $$.args = $4;
    $$.parity = $$.args.len;
    $$.body = malloc(sizeof(Statement));
    *$$.body = $6;
}
params: "void" {
    ArrayZero(&$$);
} | param-list;
param-list: param {
    ArrayZero(&$$);
    Append(&$$, $1);
};
param-list: param-list ',' param {
    $$ = $1;
    Append(&$$, $3);
};
param: "int" ID {
    InitDeclaration(&$$);
    $$.type = GetLex($1)->type;
    $$.lexid = $2;
};
param: "int" ID '[' ']' {
    InitDeclaration(&$$);
    $$.type = GetLex($1)->type;
    $$.lexid = $2;
    $$.length = -1;
};
compound-stmt: '{' declarations statements '}' {
    $$.type = STMT_COMPOUND;
    $$.declarr = $2;
    $$.stmtarr = $3;
}
| '{' error '}' {
    $$.type = STMT_ERROR;
};
statements: %empty {
    ArrayZero(&$$);
};
statements: statements statement {
    $$ = $1;
    Append(&$$, $2);
};
statement: ';' {
    $$.type = STMT_NOP;
}
| expr-stmt | cond-stmt | iter-stmt | break-stmt | ret-stmt | compound-stmt;
ret-stmt: "return" ';' {
    $$.type = STMT_RETVOID;
}
| "return" expr ';' {
    $$.type = STMT_RETEXPR;
    $$.expr = $2;
};
break-stmt: "break" ';' {
    $$.type = STMT_BREAK;
};
iter-stmt: "for" '(' expr ';' expr ';' expr ')' statement {
    $$.type = STMT_ITER;
    $$.iterinit = $3;
    $$.itercond = $5;
    $$.iterstep = $7;
    $$.iterbody = malloc(sizeof(Statement));
    *$$.iterbody = $9;
};
cond-stmt: "if" '(' expr ')' statement "endif" {
    $$.type = STMT_COND;
    $$.ifcond = $3;
    $$.ifbody = malloc(sizeof(Statement));
    *$$.ifbody = $5;
    $$.elsebody = NULL;
}
| "if" '(' expr ')' statement "else" statement "endif" {
    $$.type = STMT_COND;
    $$.ifcond = $3;
    $$.ifbody = malloc(sizeof(Statement));
    *$$.ifbody = $5;
    $$.elsebody = malloc(sizeof(Statement));
    *$$.elsebody = $7;
};
expr-stmt: expr ';' {
    $$.type = STMT_EXPR;
    $$.expr = $1;
}
expr: ID '(' arguments ')' {
    InitExpression(&$$);
    $$.type = EXPR_CALL;
    $$.lexid = $1;
    $$.args = $3;
}
| ID {
    InitExpression(&$$);
    $$.type = EXPR_VAR;
    $$.lexid = $1;
}
| ID '[' expr ']' {
    InitExpression(&$$);
    $$.type = EXPR_ARRAYCELL;
    $$.lexid = $1;
    $$.left = malloc(sizeof(Expression));
    *$$.left = $3;
}
| ID '=' expr {
    InitExpression(&$$);
    $$.type = EXPR_VAR_ASSIGN;
    $$.lexid = $1;
    $$.right = malloc(sizeof(Expression));
    *$$.right = $3;
}
| ID '[' expr ']' '=' expr {
    InitExpression(&$$);
    $$.type = EXPR_ARRAYCELL_ASSIGN;
    $$.lexid = $1;
    $$.left = malloc(sizeof(Expression));
    *$$.left = $3;
    $$.right = malloc(sizeof(Expression));
    *$$.right = $6;
}
| expr '*' expr {
    InitExpression(&$$);
    $$.type = EXPR_MULT;
    $$.left = malloc(sizeof(Expression));
    *$$.left = $1;
    $$.right = malloc(sizeof(Expression));
    *$$.right = $3;
}
| expr '+' expr {
    InitExpression(&$$);
    $$.type = EXPR_ADD;
    $$.left = malloc(sizeof(Expression));
    *$$.left = $1;
    $$.right = malloc(sizeof(Expression));
    *$$.right = $3;
}
| expr '-' expr {
    InitExpression(&$$);
    $$.type = EXPR_SUB;
    $$.left = malloc(sizeof(Expression));
    *$$.left = $1;
    $$.right = malloc(sizeof(Expression));
    *$$.right = $3;
}
| expr "==" expr {
    InitExpression(&$$);
    $$.type = EXPR_EQUAL;
    $$.left = malloc(sizeof(Expression));
    *$$.left = $1;
    $$.right = malloc(sizeof(Expression));
    *$$.right = $3;
}
| expr '<' expr {
    InitExpression(&$$);
    $$.type = EXPR_LESS;
    $$.left = malloc(sizeof(Expression));
    *$$.left = $1;
    $$.right = malloc(sizeof(Expression));
    *$$.right = $3;
}
| '(' expr ')' {
    $$ = $2;
}
| NUMBER {
    InitExpression(&$$);
    $$.type = EXPR_CONST;
    $$.value = $1;
}
| '(' error ')' {
    InitExpression(&$$);
    $$.type = EXPR_ERROR;
};
arguments: %empty {
    ArrayZero(&$$);
} 
| arg-list;
arg-list: expr {
    ArrayZero(&$$);
    Append(&$$, $1);
};
arg-list: arg-list ',' expr {
    $$ = $1;
    Append(&$$, $3);
};
%%
static int yyreport_syntax_error(const yypcontext_t *ctx) {
    int res = 0;
    YYLOCATION_PRINT(stderr, yypcontext_location(ctx));
    fprintf(stderr, ": syntax error:");
    // Report the tokens expected at this point.
    enum { TOKENMAX = 5 };
    yysymbol_kind_t expected[TOKENMAX];
    int n = yypcontext_expected_tokens(ctx, expected, TOKENMAX);
    yysymbol_kind_t lookahead = yypcontext_token(ctx);
    if (n < 0)
        // Forward errors to yyparse.
        res = n;
    else if (n == 0) {
        fprintf(stderr, " unexpected");
        if (lookahead != YYSYMBOL_YYEMPTY)
            fprintf(stderr, " %s", yysymbol_name(lookahead));
    } else {
        for (int i = 0; i < n; ++i)
            fprintf (stderr, "%s %s", i == 0 ? " expected" : " or", yysymbol_name(expected[i]));
        // Report the unexpected token.
        if (lookahead != YYSYMBOL_YYEMPTY)
            fprintf(stderr, " before %s", yysymbol_name(lookahead));
    }
    fprintf(stderr, "\n");
    PrintFileAtLoc(yypcontext_location(ctx));
    return res;
}