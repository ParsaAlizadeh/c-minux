%code requires {
    #include "main.h"
    extern int yylex(void);
    extern void yyerror(const char *);
}

%locations
%define api.location.type {Location}
%define parse.error custom

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
%token EQUAL "==" EQLT "<=" EQGT ">="
%token <lexid> IF "if" ELSE "else" FOR "for" BREAK "break"
%token <lexid> CONTINUE "continue" RETURN "return" INT "int" VOID "void"

%nterm <declarr> declarations params param-list
%nterm <decl> declare param
%nterm <lexid> type
%nterm <stmtarr> statements
%nterm <stmt> statement compound-stmt expr-stmt cond-stmt iter-stmt break-stmt continue-stmt ret-stmt
%nterm <expr> expr iter-expr lvalue
%nterm <exprarr> arguments arg-list

%precedence THEN
%precedence "else"
%right '='
%left '|'
%left '^'
%left '&'
%nonassoc '<' '>' "==" "<=" ">="
%left '+' '-'
%left '*' '/' '%'

%%
start: declarations { SetProgram(&$1); }
declarations: %empty {
    ArrayZero(&$$);
};
declarations: declarations declare {
    $$ = $1;
    Append(&$$, $2);
};
type: "int" | "void";
declare: type ID ';' {
    InitDeclaration(&$$);
    $$.lexid = $2;
    $$.kind.type = GetLex($1)->type == VOID ? KIND_VOID : KIND_INT;
    $$.loc = @$;
};
declare: type ID '[' NUMBER ']' ';' {
    InitDeclaration(&$$);
    $$.lexid = $2;
    $$.kind.type = GetLex($1)->type == VOID ? KIND_VOID : KIND_ARRAY;
    $$.kind.length = $4;
    if ($$.kind.length <= 0) {
        ReportError(@4, "expected array length to be positive, got %d", $$.kind.length);
        YYERROR;
    }
    $$.loc = @$;
};
declare: type ID '(' params ')' compound-stmt {
    InitDeclaration(&$$);
    $$.lexid = $2;
    $$.args = $4;
    $$.kind.type = GetLex($1)->type == VOID ? KIND_RETVOID : KIND_RETINT;
    ArrayPinchN(&$$.kind.args, $$.args.len);
    for (int i = 0; i < $$.args.len; i++) {
        Append(&$$.kind.args, $$.args.arr[i].kind);
    }
    $$.loc = @$;
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
    $$.kind.type = KIND_INT;
    $$.lexid = $2;
    $$.loc = @$;
};
param: "int" ID '[' ']' {
    InitDeclaration(&$$);
    $$.kind.type = KIND_POINTER;
    $$.lexid = $2;
    $$.loc = @$;
};
compound-stmt: '{' declarations statements '}' {
    $$.type = STMT_COMPOUND;
    $$.loc = @$;
    $$.declarr = $2;
    $$.stmtarr = $3;
}
| '{' error '}' {
    $$.type = STMT_ERROR;
    $$.loc = @$;
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
    $$.loc = @$;
}
| expr-stmt | cond-stmt | iter-stmt | break-stmt | continue-stmt | ret-stmt | compound-stmt;
ret-stmt: "return" ';' {
    $$.type = STMT_RETVOID;
    $$.loc = @$;
}
| "return" expr ';' {
    $$.type = STMT_RETEXPR;
    $$.expr = $2;
    $$.loc = @$;
};
break-stmt: "break" ';' {
    $$.type = STMT_BREAK;
    $$.loc = @$;
};
continue-stmt: "continue" ';' {
    $$.type = STMT_CONTINUE;
    $$.loc = @$;
};
iter-stmt: "for" '(' iter-expr ';' iter-expr ';' iter-expr ')' statement {
    $$.type = STMT_ITER;
    $$.loc = @$;
    $$.iterinit = $3;
    $$.itercond = $5;
    $$.iterstep = $7;
    $$.iterbody = malloc(sizeof(Statement));
    *$$.iterbody = $9;
};
iter-expr: expr | %empty {
    InitExpression(&$$);
    $$.type = EXPR_CONST;
    $$.value = 1;
    $$.loc = @$;
};
cond-stmt: "if" '(' expr ')' %prec THEN statement {
    $$.type = STMT_COND;
    $$.loc = @$;
    $$.ifcond = $3;
    $$.ifbody = malloc(sizeof(Statement));
    *$$.ifbody = $5;
    $$.elsebody = NULL;
}
| "if" '(' expr ')' %prec THEN statement "else" statement {
    $$.type = STMT_COND;
    $$.loc = @$;
    $$.ifcond = $3;
    $$.ifbody = malloc(sizeof(Statement));
    *$$.ifbody = $5;
    $$.elsebody = malloc(sizeof(Statement));
    *$$.elsebody = $7;
};
expr-stmt: expr ';' {
    $$.type = STMT_EXPR;
    $$.loc = @$;
    $$.expr = $1;
};

/* read/write expressions */
lvalue: ID {
    InitExpression(&$$);
    $$.type = EXPR_VAR;
    $$.lexid = $1;
    $$.loc = @$;
}
| ID '[' expr ']' {
    InitExpression(&$$);
    $$.type = EXPR_ARRAYCELL;
    $$.lexid = $1;
    $$.left = malloc(sizeof(Expression));
    *$$.left = $3;
    $$.loc = @$;
};
expr: lvalue 
| lvalue '=' expr {
    InitExpression(&$$);
    $$.type = EXPR_ASSIGN;
    $$.left = malloc(sizeof(Expression));
    *$$.left = $1;
    $$.right = malloc(sizeof(Expression));
    *$$.right = $3;
    $$.loc = @$;
};

/* binary expression */
%code {
    #define INIT_BINARY_EXPR(Type, Dst, Left, Right, Loc) do { \
        InitExpression(&Dst); \
        Dst.type = Type; \
        Dst.left = malloc(sizeof(Expression)); \
        *Dst.left = Left; \
        Dst.right = malloc(sizeof(Expression)); \
        *Dst.right = Right; \
        Dst.loc = Loc; \
    } while (0)
};
expr: expr '*' expr {
    INIT_BINARY_EXPR(EXPR_MUL, $$, $1, $3, @$);
}
| expr '+' expr {
    INIT_BINARY_EXPR(EXPR_ADD, $$, $1, $3, @$);
}
| expr '-' expr {
    INIT_BINARY_EXPR(EXPR_SUB, $$, $1, $3, @$);
}
| expr '/' expr {
    INIT_BINARY_EXPR(EXPR_DIV, $$, $1, $3, @$);
}
| expr '%' expr {
    INIT_BINARY_EXPR(EXPR_REM, $$, $1, $3, @$);
}
| expr '&' expr {
    INIT_BINARY_EXPR(EXPR_BITAND, $$, $1, $3, @$);
}
| expr '^' expr {
    INIT_BINARY_EXPR(EXPR_BITXOR, $$, $1, $3, @$);
}
| expr '|' expr {
    INIT_BINARY_EXPR(EXPR_BITOR, $$, $1, $3, @$);
}
| expr "==" expr {
    INIT_BINARY_EXPR(EXPR_EQUAL, $$, $1, $3, @$);
}
| expr '<' expr {
    INIT_BINARY_EXPR(EXPR_LESS, $$, $1, $3, @$);
}
| expr '>' expr {
    INIT_BINARY_EXPR(EXPR_GREATER, $$, $1, $3, @$);
}
| expr "<=" expr {
    INIT_BINARY_EXPR(EXPR_EQLT, $$, $1, $3, @$);
}
| expr ">=" expr {
    INIT_BINARY_EXPR(EXPR_EQGT, $$, $1, $3, @$);
}

/* unary expressions */
expr: '(' expr ')' {
    $$ = $2;
}
| NUMBER {
    InitExpression(&$$);
    $$.type = EXPR_CONST;
    $$.value = $1;
    $$.loc = @$;
}
| '(' error ')' {
    InitExpression(&$$);
    $$.type = EXPR_ERROR;
    $$.loc = @$;
};

/* function call */
expr: ID '(' arguments ')' {
    InitExpression(&$$);
    $$.type = EXPR_CALL;
    $$.lexid = $1;
    $$.loc = @$;
    $$.args = $3;
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
    Str msg = {0};
    // Report the tokens expected at this point.
    enum { TOKENMAX = 5 };
    yysymbol_kind_t expected[TOKENMAX];
    int n = yypcontext_expected_tokens(ctx, expected, TOKENMAX);
    yysymbol_kind_t lookahead = yypcontext_token(ctx);
    if (n < 0)
        // Forward errors to yyparse.
        res = n;
    else if (n == 0) {
        Sprintf(&msg, "unexpected");
        if (lookahead != YYSYMBOL_YYEMPTY)
            Sprintf(&msg, " %s", yysymbol_name(lookahead));
    } else {
        for (int i = 0; i < n; ++i)
            Sprintf(&msg, "%s %s", i == 0 ? "expected" : " or", yysymbol_name(expected[i]));
        // Report the unexpected token.
        if (lookahead != YYSYMBOL_YYEMPTY)
            Sprintf(&msg, " before %s", yysymbol_name(lookahead));
    }
    ReportError(*yypcontext_location(ctx), "%s", msg.arr);
    ArrayRelease(&msg);
    return res;
}