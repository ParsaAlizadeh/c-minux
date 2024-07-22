#include <stdio.h>

#include "array.h"
#include "eprintf.h"
#include "main.h"
#include "c-minux.tab.h"

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
    if (decl->type == VOID)
        eprintf("void type");
    if (decl->length < 0)
        eprintf("pointer type");
    SymEnt ent = { 
        .lexid = decl->lexid, 
        .scope = 0,
    };
    InitCType(&ent.cty);
    const char *lex = GetLex(decl->lexid)->lex;
    if (SearchSymbol(decl->lexid) != -1) {
        weprintf("already defined %s", lex);
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
    if (decl->parity != -1)
        eprintf("nested function");
    if (decl->type == VOID)
        eprintf("void type");
    if (decl->length < 0)
        eprintf("array type");
    SymEnt ent = {
        .lexid = decl->lexid,
        .scope = GetScope()
    };
    InitCType(&ent.cty);
    const char *lex = GetLex(decl->lexid)->lex;
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

static LLVMValueRef CodegenExpression(Expression *expr) {
    switch (expr->type) {
    case EXPR_VAR: {
        int sym = SearchSymbol(expr->lexid);
        if (sym == -1)
            eprintf("var not found");
        SymEnt *ent = &symtab.arr[sym];
        if (ent->cty.kind == TY_ARRAY || ent->cty.kind == TY_POINTER)
            return ent->llvmval;
        if (ent->cty.kind != TY_INT)
            eprintf("var not int");
        return LLVMBuildLoad2(builder, intTy, ent->llvmval, "var");
    }
    case EXPR_ARRAYCELL: {
        int sym = SearchSymbol(expr->lexid);
        if (sym == -1)
            eprintf("var not found");
        SymEnt *ent = &symtab.arr[sym];
        if (ent->cty.kind != TY_ARRAY && ent->cty.kind != TY_POINTER)
            eprintf("var not pointer");
        LLVMValueRef ind = CodegenExpression(expr->left);
        if (ind == NULL)
            eprintf("index is null");
        LLVMValueRef ptr = LLVMBuildGEP2(builder, intTy, ent->llvmval, &ind, 1, "cellptr");
        return LLVMBuildLoad2(builder, intTy, ptr, "cell");
    }
    case EXPR_VAR_ASSIGN: {
        int sym = SearchSymbol(expr->lexid);
        if (sym == -1)
            eprintf("var not found");
        SymEnt *ent = &symtab.arr[sym];
        if (ent->cty.kind != TY_INT)
            eprintf("var not int");
        LLVMValueRef val = CodegenExpression(expr->right);
        if (val == NULL)
            eprintf("assign value is null");
        LLVMBuildStore(builder, val, ent->llvmval);
        return LLVMBuildLoad2(builder, intTy, ent->llvmval, "var");
    }
    case EXPR_ARRAYCELL_ASSIGN: {
        int sym = SearchSymbol(expr->lexid);
        if (sym == -1)
            eprintf("var not found");
        SymEnt *ent = &symtab.arr[sym];
        if (ent->cty.kind != TY_ARRAY && ent->cty.kind != TY_POINTER)
            eprintf("var not pointer");
        LLVMValueRef ind = CodegenExpression(expr->left);
        if (ind == NULL)
            eprintf("index is null");
        LLVMValueRef ptr = LLVMBuildGEP2(builder, intTy, ent->llvmval, &ind, 1, "cellptr");
        LLVMValueRef val = CodegenExpression(expr->right);
        LLVMBuildStore(builder, val, ptr);
        return LLVMBuildLoad2(builder, intTy, ptr, "cell");
    
    }
    case EXPR_MULT: {
        LLVMValueRef left = CodegenExpression(expr->left);
        LLVMValueRef right = CodegenExpression(expr->right);
        if (left == NULL || right == NULL)
            eprintf("one side is null");
        return LLVMBuildMul(builder, left, right, "mul");
    }
    case EXPR_ADD: {
        LLVMValueRef left = CodegenExpression(expr->left);
        LLVMValueRef right = CodegenExpression(expr->right);
        if (left == NULL || right == NULL)
            eprintf("one side is null");
        return LLVMBuildAdd(builder, left, right, "add");
    }
    case EXPR_SUB: {
        LLVMValueRef left = CodegenExpression(expr->left);
        LLVMValueRef right = CodegenExpression(expr->right);
        if (left == NULL || right == NULL)
            eprintf("one side is null");
        return LLVMBuildSub(builder, left, right, "sub");
    }
    case EXPR_EQUAL: {
        LLVMValueRef left = CodegenExpression(expr->left);
        LLVMValueRef right = CodegenExpression(expr->right);
        if (left == NULL || right == NULL)
            eprintf("one side is null");
        LLVMValueRef cmp = LLVMBuildICmp(builder, LLVMIntEQ, left, right, "eq");
        return LLVMBuildZExt(builder, cmp, intTy, "zext");
    }
    case EXPR_LESS: {
        LLVMValueRef left = CodegenExpression(expr->left);
        LLVMValueRef right = CodegenExpression(expr->right);
        if (left == NULL || right == NULL)
            eprintf("one side is null");
        LLVMValueRef cmp = LLVMBuildICmp(builder, LLVMIntSLT, left, right, "lt");
        return LLVMBuildZExt(builder, cmp, intTy, "zext");
    }
    case EXPR_CALL: {
        int sym = SearchSymbol(expr->lexid);
        if (sym == -1)
            eprintf("var not found");
        SymEnt *ent = &symtab.arr[sym];
        if (ent->cty.kind != TY_RETINT && ent->cty.kind != TY_RETVOID)
            eprintf("var not function");
        if (ent->cty.args.len != expr->args.len)
            eprintf("argument count dont match");
        int argc = ent->cty.args.len;
        Array(LLVMValueRef) args = {0};
        for (int i = 0; i < argc; i++) {
            LLVMValueRef arg = CodegenExpression(&expr->args.arr[i]);
            if (arg == NULL)
                eprintf("arg is null");
            Append(&args, arg);
        }
        LLVMValueRef ret = LLVMBuildCall2(builder, ent->cty.typeref, ent->llvmval, args.arr, args.len, ent->cty.kind == TY_RETVOID ? "" : "call");
        ArrayRelease(&args);
        return ent->cty.kind == TY_RETVOID ? NULL : ret;
    }
    case EXPR_CONST: {
        return LLVMConstInt(intTy, expr->value, 1);
    }
    case EXPR_ERROR:
        return NULL;
    default:
        eprintf("expr not implemented");
    }
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

Array(LLVMBasicBlockRef) itertab;

static void CodegenIteration(Statement *iter) {
    SymEnt *fnent = &symtab.arr[GetFunction()];
    LLVMBasicBlockRef condblock, bodyblock, endblock;
    condblock = LLVMAppendBasicBlockInContext(context, fnent->llvmval, "itercond");
    bodyblock = LLVMAppendBasicBlockInContext(context, fnent->llvmval, "iterbody");
    endblock = LLVMAppendBasicBlockInContext(context, fnent->llvmval, "enditer");
    CodegenExpression(&iter->iterinit);
    LLVMBuildBr(builder, condblock);
    LLVMPositionBuilderAtEnd(builder, condblock);
    LLVMValueRef condval = CodegenExpression(&iter->itercond);
    if (condval == NULL)
        eprintf("condition value is null");
    LLVMValueRef predval = LLVMBuildICmp(builder, LLVMIntNE, condval, LLVMConstInt(intTy, 0, 0), "pred");
    LLVMBuildCondBr(builder, predval, bodyblock, endblock);
    LLVMPositionBuilderAtEnd(builder, bodyblock);
    Append(&itertab, endblock);
    CodegenStatement(iter->iterbody);
    itertab.len--;
    CodegenExpression(&iter->iterstep);
    LLVMBuildBr(builder, condblock);
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
        if (itertab.len == 0)
            eprintf("break with no loop");
        LLVMBuildBr(builder, ArrayLast(&itertab));
        break;
    }
    case STMT_RETVOID: {
        SymEnt *ent = &symtab.arr[GetFunction()];
        if (ent->cty.kind != TY_RETVOID)
            eprintf("return void in integer function");
        LLVMBuildRetVoid(builder);
        break;
    }
    case STMT_RETEXPR: {
        SymEnt *ent = &symtab.arr[GetFunction()];
        if (ent->cty.kind != TY_RETINT)
            eprintf("return void in integer function");
        LLVMValueRef ret = CodegenExpression(&stmt->expr);
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
        weprintf("implement this statement");
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
    SymEnt *ent = &symtab.arr[symtab.len++];
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
    /* ent is now invalid */
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
}
