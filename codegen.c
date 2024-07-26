#include <stdio.h>

#include "array.h"
#include "eprintf.h"
#include "c-minux.tab.h"
#include "main.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

static LLVMContextRef context;
static LLVMBuilderRef builder;
static LLVMModuleRef module;

static LLVMTypeRef intTy, voidTy, ptrTy;

void CreateCodegen(void) {
    context = LLVMContextCreate();
    builder = LLVMCreateBuilderInContext(context);
    module = LLVMModuleCreateWithNameInContext("main_module", context);

    intTy = LLVMInt32TypeInContext(context);
    voidTy = LLVMVoidTypeInContext(context);
    ptrTy = LLVMPointerTypeInContext(context, 0);
}

void OutputCode(void) {
    char *error = NULL;
    LLVMPrintModuleToFile(module, "output.ll", &error);
    if (error != NULL)
        eprintf("llvm error: %s", error);
}

void DisposeCodegen(void) {
    LLVMDisposeModule(module);
    LLVMDisposeBuilder(builder);
    LLVMContextDispose(context);
}

enum {
    TY_INT,
    TY_ARRAY,
    TY_POINTER,
    TY_RETVOID,
    TY_RETINT,
};

typedef struct CType CType;
struct CType {
    int kind;
    LLVMTypeRef typeref;
    Array(CType) args;
};

void InitCType(CType *cty) {
    ArrayZero(&cty->args);
    cty->typeref = NULL;
}

void DisposeCType(CType *cty) {
    ArrayRelease(&cty->args);
}

typedef struct SymEnt SymEnt;
struct SymEnt {
    int lexid, scope;
    LLVMValueRef llvmval;
    CType cty;
};

static Array(SymEnt) symtab;
static Array(int) scopest;
static int scopeptr;

static void PrintSymtab(void) {
    printf("**** Symbol Table ****\n");
    for (int i = 0; i < symtab.len; i++) {
        SymEnt *ent = &symtab.arr[i];
        printf("%3d %s\n", ent->scope, GetLex(ent->lexid)->lex);
    }
    printf("****\n");
}

static int StartScope(void) {
    int sc = scopeptr++;
    Append(&scopest, sc);
    // printf(">> Start scope %d\n", sc);
    // PrintSymtab();
    return sc;
}

static void EndScope(void) {
    int sc = scopest.arr[--scopest.len];
    // printf(">> End scope %d\n", sc);
    // PrintSymtab();
    int i;
    for (i = symtab.len; i > 0 && symtab.arr[i - 1].scope == sc; i--) {
        DisposeCType(&symtab.arr[i - 1].cty);
    }
    symtab.len = i;
}

static int SearchSymbol(int lexid) {
    for (int i = symtab.len - 1; i >= 0; i--) {
        SymEnt *ent = &symtab.arr[i];
        if (ent->lexid == lexid) {
            return i;
        }
    }
    return -1;
}

static int GetScope(void) {
    return ArrayLast(&scopest);
}

static int GetFunction(void) {
    for (int i = symtab.len - 1; i >= 0; i--) {
        SymEnt *ent = &symtab.arr[i];
        if (ent->cty.kind == TY_RETINT || ent->cty.kind == TY_RETVOID)
            return i;
    }
    return -1;
}

static void CodegenGlobalVariable(Declaration *decl) {
    const char *lex = GetLex(decl->lexid)->lex;
    if (decl->type == VOID) {
        ReportError(decl->loc, "unexpected type void for global variable \"%s\"", lex);
        return;
    }
    SymEnt ent = { 
        .lexid = decl->lexid, 
        .scope = 0,
    };
    InitCType(&ent.cty);
    if (SearchSymbol(decl->lexid) != -1) {
        ReportError(decl->loc, "unexpected redeclaration of \"%s\" in the global scope", lex);
        return;
    }
    if (decl->length == 0) {
        ent.cty.typeref = intTy;
        ent.llvmval = LLVMAddGlobal(module, intTy, lex);
        LLVMSetInitializer(ent.llvmval, LLVMConstNull(intTy));
        ent.cty.kind = TY_INT;
        Append(&symtab, ent);
        return;
    }
    ent.cty.typeref = LLVMArrayType2(intTy, decl->length);
    ent.llvmval = LLVMAddGlobal(module, ent.cty.typeref, lex);
    LLVMSetInitializer(ent.llvmval, LLVMConstNull(ent.cty.typeref));
    ent.cty.kind = TY_ARRAY;
    Append(&symtab, ent);
}

static void CodegenLocalVariable(Declaration *decl) {
    const char *lex = GetLex(decl->lexid)->lex;
    if (decl->parity != -1) {
        ReportError(decl->loc, "unexpected nested function \"%s\"", lex);
        return;
    }
    if (decl->type == VOID) {
        ReportError(decl->loc, "unexpected type void for local variable \"%s\"", lex);
        return;
    }
    SymEnt ent = {
        .lexid = decl->lexid,
        .scope = GetScope()
    };
    InitCType(&ent.cty);
    if (decl->length == 0) {
        ent.cty.kind = TY_INT;
        ent.cty.typeref = intTy;
        ent.llvmval = LLVMBuildAlloca(builder, intTy, lex);
    } else {
        ent.cty.kind = TY_ARRAY;
        ent.cty.typeref = LLVMArrayType2(intTy, decl->length);
        ent.llvmval = LLVMBuildAlloca(builder, ent.cty.typeref, lex);
    }
    Append(&symtab, ent);
}

static LLVMValueRef CodegenExpression(Expression *);

static int IsPointerKind(int kind) {
    return kind == TY_ARRAY || kind == TY_POINTER;
}

static int IsFunctionKind(int kind) {
    return kind == TY_RETINT || kind == TY_RETVOID;
}

static LLVMValueRef CodegenExpressionRW(Expression *expr) {
    const char *lex = GetLex(expr->lexid)->lex;
    int sym = SearchSymbol(expr->lexid);
    if (sym == -1) {
        ReportError(expr->loc, "undefined variable \"%s\"", lex);
        return NULL;
    }
    SymEnt *ent = &symtab.arr[sym];
    LLVMValueRef ptr;
    switch (expr->type) {
    case EXPR_VAR: {
        if (IsPointerKind(ent->cty.kind))
            return ent->llvmval;
        if (ent->cty.kind != TY_INT) {
            ReportError(expr->loc, "expected variable \"%s\" to be integer", lex);
            return NULL;
        }
        ptr = ent->llvmval;
        break;
    }
    case EXPR_ARRAYCELL: {
        if (!IsPointerKind(ent->cty.kind)) {
            ReportError(expr->loc, "expected variable \"%s\" to be array or pointer");
            return NULL;
        }
        LLVMValueRef ind = CodegenExpression(expr->left);
        if (ind == NULL) {
            ReportError(expr->loc, "expected array index to be integer, got nothing");
            return NULL;
        }
        ptr = LLVMBuildGEP2(builder, intTy, ent->llvmval, &ind, 1, lex);
        break;
    }
    case EXPR_VAR_ASSIGN: {
        if (ent->cty.kind != TY_INT) {
            ReportError(expr->loc, "expected variable \"%s\" to be integer", lex);
            return NULL;
        }
        LLVMValueRef val = CodegenExpression(expr->right);
        if (val == NULL) {
            ReportError(expr->loc, "expected right hand side to be integer");
            return NULL;
        }
        LLVMBuildStore(builder, val, ent->llvmval);
        ptr = ent->llvmval;
        break;
    }
    case EXPR_ARRAYCELL_ASSIGN: {
        if (!IsPointerKind(ent->cty.kind)) {
            ReportError(expr->loc, "expected variable \"%s\" to be array or pointer", lex);
            return NULL;
        }
        LLVMValueRef ind = CodegenExpression(expr->left);
        if (ind == NULL) {
            ReportError(expr->loc, "expected array index to be integer, got nothing");
            return NULL;
        }
        ptr = LLVMBuildGEP2(builder, intTy, ent->llvmval, &ind, 1, lex);
        LLVMValueRef val = CodegenExpression(expr->right);
        LLVMBuildStore(builder, val, ptr);
        break;
    }
    default:
        eprintf("unexpected r/w expression type");
    }
    return LLVMBuildLoad2(builder, intTy, ptr, lex);
}

static LLVMValueRef CodegenExpressionBinary(Expression *expr) {
    LLVMValueRef left = CodegenExpression(expr->left);
    LLVMValueRef right = CodegenExpression(expr->right);
    if (left == NULL) {
        ReportError(expr->loc, "expected left hand side of binary operator to be integer");
        return NULL;
    }
    if (right == NULL) {
        ReportError(expr->loc, "expected right hand side of binary operator to be integer");
        return NULL;
    }
    switch (expr->type) {
    case EXPR_MULT:
        return LLVMBuildMul(builder, left, right, "mul");
    case EXPR_ADD:
        return LLVMBuildAdd(builder, left, right, "add"); 
    case EXPR_SUB:
        return LLVMBuildSub(builder, left, right, "sub");
    case EXPR_EQUAL: {
        LLVMValueRef cmp = LLVMBuildICmp(builder, LLVMIntEQ, left, right, "eq");
        return LLVMBuildZExt(builder, cmp, intTy, "zext");
    }
    case EXPR_LESS: {
        LLVMValueRef cmp = LLVMBuildICmp(builder, LLVMIntSLT, left, right, "lt");
        return LLVMBuildZExt(builder, cmp, intTy, "zext");
    }
    default:
        eprintf("unexpected binary expression type");
    }
    return NULL;
}

static LLVMValueRef CodegenExpressionCall(Expression *expr) {
    const char *lex = GetLex(expr->lexid)->lex;
    int sym = SearchSymbol(expr->lexid);
    if (sym == -1) {
        ReportError(expr->loc, "undefined function \"%s\"", lex);
        return NULL;
    }
    SymEnt *ent = &symtab.arr[sym];
    if (!IsFunctionKind(ent->cty.kind)) {
        ReportError(expr->loc, "expected variable \"%s\" to be function", lex);
        return NULL;
    }
    if (ent->cty.args.len != expr->args.len) {
        ReportError(expr->loc, "expected %d argument(s), passed %d", ent->cty.args.len, expr->args.len);
        return NULL;
    }
    int argc = ent->cty.args.len;
    Array(LLVMValueRef) args = {0};
    for (int i = 0; i < argc; i++) {
        LLVMValueRef arg = CodegenExpression(&expr->args.arr[i]);
        if (arg == NULL) {
            ReportError(expr->loc, "expected argument %d of \"%s\" to be integer, array, or pointer", i + 1, lex);
            return NULL;
        }
        Append(&args, arg);
    }
    LLVMValueRef ret = LLVMBuildCall2(builder, ent->cty.typeref, ent->llvmval, args.arr, args.len, ent->cty.kind == TY_RETVOID ? "" : "call");
    ArrayRelease(&args);
    return ent->cty.kind == TY_RETVOID ? NULL : ret;
}

static LLVMValueRef CodegenExpression(Expression *expr) {
    switch (expr->type) {
    case EXPR_VAR:
    case EXPR_ARRAYCELL:
    case EXPR_VAR_ASSIGN:
    case EXPR_ARRAYCELL_ASSIGN:
        return CodegenExpressionRW(expr);
    case EXPR_MULT:
    case EXPR_ADD:
    case EXPR_SUB:
    case EXPR_EQUAL:
    case EXPR_LESS:
        return CodegenExpressionBinary(expr);
    case EXPR_CALL: 
        return CodegenExpressionCall(expr);
    case EXPR_CONST:
        return LLVMConstInt(intTy, expr->value, 1);
    case EXPR_ERROR:
        return NULL;
    default:
        eprintf("expr not implemented");
    }
    return NULL;
}

static void CodegenStatement(Statement *stmt);

static void CodegenCompund(Statement *comp) {
    StartScope();
    for (int i = 0; i < comp->declarr.len; i++) {
        Declaration *decl = &comp->declarr.arr[i];
        CodegenLocalVariable(decl);
    }
    for (int i = 0; i < comp->stmtarr.len; i++) {
        Statement *stmt = &comp->stmtarr.arr[i];
        CodegenStatement(stmt);
    }
    EndScope();
}

static void CodegenConditional(Statement *cond) {
    SymEnt *fnent = &symtab.arr[GetFunction()];
    LLVMBasicBlockRef bodyblock, elseblock, endblock;
    bodyblock = LLVMAppendBasicBlockInContext(context, fnent->llvmval, "ifbody");
    if (cond->elsebody != NULL)
        elseblock = LLVMAppendBasicBlockInContext(context, fnent->llvmval, "elsebody");
    endblock = LLVMAppendBasicBlockInContext(context, fnent->llvmval, "endif");
    LLVMValueRef condval = CodegenExpression(&cond->ifcond);
    if (condval == NULL) {
        ReportError(cond->loc, "expected condition of if-statement to be integer");
        condval = LLVMConstInt(intTy, 0, 0);
    }
    LLVMValueRef predval = LLVMBuildICmp(builder, LLVMIntNE, condval, LLVMConstInt(intTy, 0, 0), "pred");
    LLVMBuildCondBr(builder, predval, bodyblock, cond->elsebody == NULL ? endblock : elseblock);
    LLVMPositionBuilderAtEnd(builder, bodyblock);
    CodegenStatement(cond->ifbody);
    LLVMBuildBr(builder, endblock);
    if (cond->elsebody != NULL) {
        LLVMPositionBuilderAtEnd(builder, elseblock);
        CodegenStatement(cond->elsebody);
        LLVMBuildBr(builder, endblock);
    }
    LLVMPositionBuilderAtEnd(builder, endblock);
}

typedef struct IterationBlocks IterationBlocks;
struct IterationBlocks {
    LLVMBasicBlockRef stepblock, endblock;
};

Array(IterationBlocks) itertab;

static void CodegenIteration(Statement *iter) {
    SymEnt *fnent = &symtab.arr[GetFunction()];
    LLVMBasicBlockRef condblock, bodyblock, stepblock, endblock;
    condblock = LLVMAppendBasicBlockInContext(context, fnent->llvmval, "itercond");
    bodyblock = LLVMAppendBasicBlockInContext(context, fnent->llvmval, "iterbody");
    stepblock = LLVMAppendBasicBlockInContext(context, fnent->llvmval, "iterstep");
    endblock = LLVMAppendBasicBlockInContext(context, fnent->llvmval, "enditer");
    /* entry block */
    CodegenExpression(&iter->iterinit);
    LLVMBuildBr(builder, condblock);
    /* condition block */
    LLVMPositionBuilderAtEnd(builder, condblock);
    LLVMValueRef condval = CodegenExpression(&iter->itercond);
    if (condval == NULL) {
        ReportError(iter->loc, "expected condition of for-statement to be integer");
        condval = LLVMConstInt(intTy, 0, 0);
    }
    LLVMValueRef predval = LLVMBuildICmp(builder, LLVMIntNE, condval, LLVMConstInt(intTy, 0, 0), "pred");
    LLVMBuildCondBr(builder, predval, bodyblock, endblock);
    /* body block */
    LLVMPositionBuilderAtEnd(builder, bodyblock);
    IterationBlocks ibs = { .stepblock = stepblock, .endblock = endblock };
    Append(&itertab, ibs);
    CodegenStatement(iter->iterbody);
    itertab.len--;
    LLVMBuildBr(builder, stepblock);
    /* step block */
    LLVMPositionBuilderAtEnd(builder, stepblock);
    CodegenExpression(&iter->iterstep);
    LLVMBuildBr(builder, condblock);
    /* exit block */
    LLVMPositionBuilderAtEnd(builder, endblock);
}

static void CodegenStatement(Statement *stmt) {
    switch (stmt->type) {
    case STMT_EXPR:
        CodegenExpression(&stmt->expr);
        break;
    case STMT_COND:
        CodegenConditional(stmt);
        break;
    case STMT_ITER:
        CodegenIteration(stmt);
        break;
    case STMT_BREAK: {
        if (itertab.len == 0) {
            ReportError(stmt->loc, "unexpected break-statement, no matching for-statment");
            return;
        }
        LLVMBuildBr(builder, ArrayLast(&itertab).endblock);
        break;
    }
    case STMT_CONTINUE: {
        if (itertab.len == 0) {
            ReportError(stmt->loc, "unexpected continue-statement, no matching for statement");
            return;
        }
        LLVMBuildBr(builder, ArrayLast(&itertab).stepblock);
        break;
    }
    case STMT_RETVOID: {
        SymEnt *ent = &symtab.arr[GetFunction()];
        if (ent->cty.kind != TY_RETVOID) {
            ReportError(stmt->loc, "unexpected return void in integer function");
            return;
        }
        LLVMBuildRetVoid(builder);
        break;
    }
    case STMT_RETEXPR: {
        SymEnt *ent = &symtab.arr[GetFunction()];
        if (ent->cty.kind != TY_RETINT) {
            ReportError(stmt->loc, "unexpected return integer in void function");
            return;
        }
        LLVMValueRef ret = CodegenExpression(&stmt->expr);
        if (ret == NULL) {
            ReportError(stmt->loc, "expected return value to be integer");
            return;
        }
        LLVMBuildRet(builder, ret);
        break;
    }
    case STMT_COMPOUND:
        CodegenCompund(stmt);
        break;
    case STMT_NOP:
    case STMT_ERROR:
        break;
    default:
        eprintf("statement type not implemented");
    }
}

static void CodegenParameters(LLVMValueRef fn, DeclarationArray *params) {
    for (int i = 0; i < params->len; i++) {
        Declaration *param = &params->arr[i];
        const char *lex =  GetLex(param->lexid)->lex;
        SymEnt ent = {
            .lexid = param->lexid,
            .scope = GetScope()
        };
        InitCType(&ent.cty);
        LLVMValueRef llvmparam = LLVMGetParam(fn, i);
        LLVMSetValueName2(llvmparam, lex, strlen(lex));
        if (param->length < 0) {
            ent.cty.typeref = ptrTy;
            ent.llvmval = llvmparam;
        } else {
            ent.cty.typeref = intTy;
            ent.llvmval = LLVMBuildAlloca(builder, intTy, lex);
            LLVMBuildStore(builder, llvmparam, ent.llvmval);
        }
        Append(&symtab, ent);
    }
}

static void CodegenFunction(Declaration *decl) {
    ArrayPinch(&symtab);
    int sym = symtab.len++;
    SymEnt *ent = &symtab.arr[sym];
    ent->lexid = decl->lexid;
    ent->scope = 0;
    InitCType(&ent->cty);
    StartScope();
    ent->cty.kind = decl->type == VOID ? TY_RETVOID : TY_RETINT;
    const char *lex = GetLex(decl->lexid)->lex;
    Array(LLVMTypeRef) llvmargs = {0};
    int parity = decl->parity;
    ArrayPinchN(&llvmargs, parity);
    ArrayPinchN(&ent->cty.args, parity);
    llvmargs.len = parity;
    ent->cty.args.len = parity;
    for (int i = 0; i < decl->parity; i++) {
        Declaration *argdecl = &decl->args.arr[i];
        CType *cty = &ent->cty.args.arr[i];
        InitCType(cty);
        if (argdecl->length == 0) {
            cty->typeref = intTy;
            cty->kind = TY_INT;
        } else {
            cty->typeref = ptrTy;
            cty->kind = TY_POINTER;
        }
        llvmargs.arr[i] = cty->typeref;
    }
    LLVMTypeRef retTy = decl->type == VOID ? voidTy : intTy;
    ent->cty.typeref = LLVMFunctionType(retTy, llvmargs.arr, decl->parity, 0);
    ArrayRelease(&llvmargs);
    ent->llvmval = LLVMAddFunction(module, lex, ent->cty.typeref);
    LLVMBasicBlockRef entry = LLVMAppendBasicBlockInContext(context, ent->llvmval, "entry");
    LLVMPositionBuilderAtEnd(builder, entry);
    CodegenParameters(ent->llvmval, &decl->args);
    CodegenCompund(decl->body);
    /* ent is now invalid, need to read it again */
    LLVMBasicBlockRef endblock = LLVMGetInsertBlock(builder);
    LLVMValueRef lastInst = LLVMGetLastInstruction(endblock);
    if (lastInst == NULL || !LLVMIsATerminatorInst(lastInst)) {
        if (decl->type == INT)
            ReportError(decl->loc, "expected return statement at the end of integer function \"%s\"", lex);
        else
            LLVMBuildRetVoid(builder);
    }
    EndScope();
}

static void CodegenPrint(void) {
    LLVMTypeRef printfTy = LLVMFunctionType(intTy, &ptrTy, 1, 1);
    LLVMValueRef printfn = LLVMAddFunction(module, "printf", printfTy);
    LLVMTypeRef myprintTy = LLVMFunctionType(voidTy, &intTy, 1, 0);
    LLVMValueRef myprint = LLVMAddFunction(module, "print", myprintTy);
    LLVMBasicBlockRef entry = LLVMAppendBasicBlockInContext(context, myprint, "entry");
    LLVMPositionBuilderAtEnd(builder, entry);
    LLVMValueRef printfmt = LLVMBuildGlobalStringPtr(builder, "%d\n", "printfmt");
    LLVMValueRef printfargs[] = {printfmt, LLVMGetParam(myprint, 0)};
    LLVMBuildCall2(builder, printfTy, printfn, printfargs, 2, "call");
    LLVMBuildRetVoid(builder);

    int lexid = CreateLex("print");
    SymEnt ent = {
        .lexid = lexid,
        .scope = 0,
        .llvmval = myprint
    };
    InitCType(&ent.cty);
    ent.cty.kind = TY_RETVOID;
    ent.cty.typeref = myprintTy;
    CType arg;
    InitCType(&arg);
    arg.kind = TY_INT;
    arg.typeref = intTy;
    Append(&ent.cty.args, arg);
    Append(&symtab, ent);
}

static void CodegenInput(void) {
    LLVMTypeRef scanfTy = LLVMFunctionType(intTy, &ptrTy, 1, 1);
    LLVMValueRef scanfn = LLVMAddFunction(module, "scanf", scanfTy);
    LLVMTypeRef myinputTy = LLVMFunctionType(intTy, NULL, 0, 0);
    LLVMValueRef myinput = LLVMAddFunction(module, "input", myinputTy);
    LLVMBasicBlockRef entry = LLVMAppendBasicBlockInContext(context, myinput, "entry");
    LLVMPositionBuilderAtEnd(builder, entry);
    LLVMValueRef scanfmt = LLVMBuildGlobalStringPtr(builder, " %d", "scanfmt");
    LLVMValueRef inputptr = LLVMBuildAlloca(builder, intTy, "inputptr");
    LLVMValueRef scanfargs[] = {scanfmt, inputptr};
    LLVMBuildCall2(builder, scanfTy, scanfn, scanfargs, 2, "call");
    LLVMValueRef inputval = LLVMBuildLoad2(builder, intTy, inputptr, "inputval");
    LLVMBuildRet(builder, inputval);

    int lexid = CreateLex("input");
    SymEnt ent = {
        .lexid = lexid,
        .scope = 0,
        .llvmval = myinput
    };
    InitCType(&ent.cty);
    ent.cty.kind = TY_RETINT;
    ent.cty.typeref = myinputTy;
    Append(&symtab, ent);
}

void CodegenProgram(DeclarationArray *prog) {
    StartScope();
    CodegenPrint();
    CodegenInput();
    for (int i = 0; i < prog->len; i++) {
        Declaration *decl = &prog->arr[i];
        if (decl->parity == -1)
            CodegenGlobalVariable(decl);
        else
            CodegenFunction(decl);
    }
    EndScope();
}
