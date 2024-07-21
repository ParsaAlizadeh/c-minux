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
    LLVMDumpModule(module);
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
    Array(CType) args;
};

void InitCType(CType *cty) {
    ArrayZero(&cty->args);
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
    printf(">> Start scope %d\n", sc);
    PrintSymtab();
    return sc;
}

static void EndScope(void) {
    int sc = scopest.arr[--scopest.len];
    printf(">> End scope %d\n", sc);
    PrintSymtab();
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
        ent.llvmval = LLVMAddGlobal(module, intTy, lex);
        LLVMSetInitializer(ent.llvmval, LLVMConstNull(intTy));
        ent.cty.kind = TY_INT;
        Append(&symtab, ent);
        return;
    }
    LLVMTypeRef arrayTy = LLVMArrayType2(intTy, decl->length);
    ent.llvmval = LLVMAddGlobal(module, arrayTy, lex);
    LLVMSetInitializer(ent.llvmval, LLVMConstNull(arrayTy));
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
        ent.llvmval = LLVMBuildAlloca(builder, intTy, lex);
    } else {
        ent.cty.kind = TY_ARRAY;
        LLVMTypeRef arrayTy = LLVMArrayType2(intTy, decl->length);
        ent.llvmval = LLVMBuildAlloca(builder, arrayTy, lex);
    }
    Append(&symtab, ent);
}

static void CodegenCompund(Statement *stmt) {
    StartScope();
    for (int i = 0; i < stmt->declarr.len; i++) {
        Declaration *decl = &stmt->declarr.arr[i];
        CodegenLocalVariable(decl);
    }
    EndScope();
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
            ent.llvmval = llvmparam;
        } else {
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
            llvmargs.arr[i] = intTy;
            cty->kind = TY_INT;
        } else {
            llvmargs.arr[i] = ptrTy;
            cty->kind = TY_POINTER;
        }
    }
    LLVMTypeRef retTy = decl->type == VOID ? voidTy : intTy;
    LLVMTypeRef funcTy = LLVMFunctionType(retTy, llvmargs.arr, decl->parity, 0);
    ArrayRelease(&llvmargs);
    ent->llvmval = LLVMAddFunction(module, lex, funcTy);
    LLVMBasicBlockRef entry = LLVMAppendBasicBlock(ent->llvmval, "entry");
    LLVMPositionBuilderAtEnd(builder, entry);
    CodegenParameters(ent->llvmval, &decl->args);
    CodegenCompund(decl->body);
    /* ent is now invalid */
    if (decl->type == VOID) {
        LLVMBuildRetVoid(builder);
    } else {
        LLVMBuildRet(builder, LLVMConstInt(intTy, 0, 0));
    }
    EndScope();
}

void CodegenProgram(DeclarationArray *prog) {
    StartScope();
    for (int i = 0; i < prog->len; i++) {
        Declaration *decl = &prog->arr[i];
        if (decl->parity == -1)
            CodegenGlobalVariable(decl);
        else
            CodegenFunction(decl);
    }
}
