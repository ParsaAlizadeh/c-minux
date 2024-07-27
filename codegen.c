#include <stdio.h>

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

#include "array.h"
#include "c-minux.tab.h"
#include "eprintf.h"
#include "main.h"

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

enum ValueType {
    LVALUE,
    RVALUE
};

typedef struct Value Value;
struct Value {
    enum ValueType type;
    LLVMValueRef llvm;
    Kind kind;
};

static void InitValue(Value *value) {
    InitKind(&value->kind);
    value->type = RVALUE;
    value->llvm = NULL;
}

typedef struct SymEnt SymEnt;
struct SymEnt {
    int lexid, scope;
    Value value;
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
    for (i = symtab.len; i > 0 && symtab.arr[i - 1].scope == sc; i--)
        ;
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

static int IsFunctionKind(Kind *kind) {
    return kind->type == KIND_RETVOID || kind->type == KIND_RETINT;
}

static int GetFunction(void) {
    for (int i = symtab.len - 1; i >= 0; i--) {
        SymEnt *ent = &symtab.arr[i];
        if (IsFunctionKind(&ent->value.kind))
            return i;
    }
    return -1;
}

static LLVMTypeRef GetTypeRefRValue(Kind *kind) {
    switch (kind->type) {
    case KIND_VOID:
        return voidTy;
    case KIND_INT:
        return intTy;
    case KIND_POINTER:
        return ptrTy;
    case KIND_ARRAY:
        return LLVMArrayType2(intTy, kind->length);
    case KIND_RETINT:
    case KIND_RETVOID: {
        LLVMTypeRef retTy = kind->type == KIND_RETINT ? intTy : voidTy;
        Array(LLVMTypeRef) args = {0};
        ArrayPinchN(&args, kind->args.len);
        for (int i = 0; i < kind->args.len; i++) {
            Append(&args, GetTypeRefRValue(&kind->args.arr[i]));
        }
        LLVMTypeRef fnTy = LLVMFunctionType(retTy, args.arr, args.len, 0);
        ArrayRelease(&args);
        return fnTy;
    }
    default:
        eprintf("unknown kind type");
    }
}

static void CodegenGlobalVariable(Declaration *decl) {
    const char *lex = GetLex(decl->lexid)->lex;
    if (decl->kind.type == KIND_VOID) {
        ReportError(decl->loc, "unexpected type void for variable \"%s\"", lex);
        return;
    }
    SymEnt ent = { 
        .lexid = decl->lexid, 
        .scope = 0,
    };
    Value *value = &ent.value;
    InitValue(value);
    value->type = LVALUE;
    value->kind = decl->kind;
    if (SearchSymbol(decl->lexid) != -1) {
        ReportError(decl->loc, "unexpected redeclaration of \"%s\" in the global scope", lex);
        return;
    }
    LLVMTypeRef ty = GetTypeRefRValue(&value->kind);
    value->llvm = LLVMAddGlobal(module, ty, lex);
    LLVMSetInitializer(value->llvm, LLVMConstNull(ty));
    Append(&symtab, ent);
}

static void CodegenLocalVariable(Declaration *decl) {
    const char *lex = GetLex(decl->lexid)->lex;
    if (decl->kind.type == KIND_VOID) {
        ReportError(decl->loc, "unexpected type void for variable \"%s\"", lex);
        return;
    }
    if (decl->kind.type == KIND_RETVOID || decl->kind.type == KIND_RETINT) {
        ReportError(decl->loc, "unexpected nested function \"%s\"", lex);
        return;
    }
    SymEnt ent = {
        .lexid = decl->lexid,
        .scope = GetScope()
    };
    Value *value = &ent.value;
    InitValue(value);
    value->type = LVALUE;
    value->kind = decl->kind;
    LLVMTypeRef ty = GetTypeRefRValue(&value->kind);
    value->llvm = LLVMBuildAlloca(builder, ty, lex);
    Append(&symtab, ent);
}

static int IsPointerKind(Kind *kind) {
    return kind->type == KIND_ARRAY || kind->type == KIND_POINTER;
}

const Value NullValue = {
    .type = RVALUE,
    .kind.type = KIND_VOID,
    .llvm = NULL
};

static Value GetRValue(Value lvalue) {
    if (lvalue.kind.type != KIND_INT)
        return NullValue;
    if (lvalue.type == RVALUE)
        return lvalue;
    Value rvalue = lvalue;
    rvalue.type = RVALUE;
    LLVMTypeRef ty = GetTypeRefRValue(&rvalue.kind);
    rvalue.llvm = LLVMBuildLoad2(builder, ty, lvalue.llvm, "load");
    return rvalue;
}

static Value CodegenExpression(Expression *);

static Value CodegenExpressionLValue(Expression *expr) {
    const char *lex = GetLex(expr->lexid)->lex;
    int sym = SearchSymbol(expr->lexid);
    if (sym == -1) {
        ReportError(expr->loc, "undefined variable \"%s\"", lex);
        return NullValue;
    }
    SymEnt *ent = &symtab.arr[sym];
    LLVMValueRef ptr;
    switch (expr->type) {
    case EXPR_VAR: 
        return ent->value;
    case EXPR_ARRAYCELL: {
        if (!IsPointerKind(&ent->value.kind)) {
            ReportError(expr->loc, "expected variable \"%s\" to be array or pointer", lex);
            return NullValue;
        }
        Value ind = GetRValue(CodegenExpression(expr->left));
        if (ind.kind.type != KIND_INT) {
            ReportError(expr->loc, "expected array index to be int");
            return NullValue;
        }
        Value ptr = {
            .type = LVALUE,
            .kind = KIND_INT,
            .llvm = LLVMBuildGEP2(builder, intTy, ent->value.llvm, &ind.llvm, 1, lex)
        };
        return ptr;
    }
    default:
        eprintf("unexpected r/w expression type");
    }
    return NullValue;
}

static Value CodegenExpressionAssign(Expression *expr) {
    Value left = CodegenExpression(expr->left);
    Value right = GetRValue(CodegenExpression(expr->right));
    if (left.type != LVALUE) {
        ReportError(expr->loc, "unexpected assignment to rvalue");
        return NullValue;
    }
    if (left.kind.type != right.kind.type) {
        ReportError(expr->loc, "expected both sides of assignment to have equal types");
        return NullValue;
    }
    if (left.kind.type != KIND_INT) {
        ReportError(expr->loc, "expected assignment to int");
        return NullValue;
    }
    LLVMBuildStore(builder, right.llvm, left.llvm);
    return left;
}

static LLVMValueRef (*binoptab[])(LLVMBuilderRef, LLVMValueRef, LLVMValueRef, const char *) = {
    [EXPR_MUL] = LLVMBuildMul,
    [EXPR_ADD] = LLVMBuildAdd,
    [EXPR_SUB] = LLVMBuildSub,
    [EXPR_DIV] = LLVMBuildSDiv,
    [EXPR_REM] = LLVMBuildSRem,
    [EXPR_BITAND] = LLVMBuildAnd,
    [EXPR_BITOR] = LLVMBuildOr,
    [EXPR_BITXOR] = LLVMBuildXor,
};

static Value CodegenExpressionBinaryOp(Expression *expr) {
    Value left = GetRValue(CodegenExpression(expr->left));
    Value right = GetRValue(CodegenExpression(expr->right));
    if (left.kind.type != KIND_INT) {
        ReportError(expr->loc, "expected left hand side of binary operator to be int");
        return NullValue;
    }
    if (right.kind.type != KIND_INT) {
        ReportError(expr->loc, "expected right hand side of binary operator to be int");
        return NullValue;
    }
    Value ret;
    InitValue(&ret);
    ret.type = RVALUE;
    ret.kind.type = KIND_INT;
    ret.llvm = binoptab[expr->type](builder, left.llvm, right.llvm, "op");
    return ret;
}

static LLVMIntPredicate cmpoptab[] = {
    [EXPR_EQUAL] = LLVMIntEQ,
    [EXPR_LESS] = LLVMIntSLT,
    [EXPR_GREATER] = LLVMIntSGT,
    [EXPR_EQLT] = LLVMIntSLE,
    [EXPR_EQGT] = LLVMIntSGE
};

static Value CodegenExpressionCompareOp(Expression *expr) {
    Value left = GetRValue(CodegenExpression(expr->left));
    Value right = GetRValue(CodegenExpression(expr->right));
    if (left.kind.type != KIND_INT) {
        ReportError(expr->loc, "expected left hand side of compare operator to be int");
        return NullValue;
    }
    if (right.kind.type != KIND_INT) {
        ReportError(expr->loc, "expected right hand side of compare operator to be int");
        return NullValue;
    }
    Value ret;
    InitValue(&ret);
    ret.type = RVALUE;
    ret.kind.type = KIND_INT;
    LLVMIntPredicate pred = cmpoptab[expr->type];
    LLVMValueRef cmp = LLVMBuildICmp(builder, pred, left.llvm, right.llvm, "cmp");
    ret.llvm = LLVMBuildZExt(builder, cmp, intTy, "zext");
    return ret;
}

static Value CodegenExpressionCall(Expression *expr) {
    const char *lex = GetLex(expr->lexid)->lex;
    int sym = SearchSymbol(expr->lexid);
    if (sym == -1) {
        ReportError(expr->loc, "undefined function \"%s\"", lex);
        return NullValue;
    }
    SymEnt *ent = &symtab.arr[sym];
    Value *fnval = &ent->value;
    Kind *fnkind = &fnval->kind;
    if (!IsFunctionKind(fnkind)) {
        ReportError(expr->loc, "expected variable \"%s\" to be function", lex);
        return NullValue;
    }
    if (fnkind->args.len != expr->args.len) {
        ReportError(expr->loc, "expected %d argument(s), passed %d", fnkind->args.len, expr->args.len);
        return NullValue;
    }
    int argc = fnkind->args.len;
    Array(LLVMValueRef) args = {0};
    for (int i = 0; i < argc; i++) {
        Expression *argexpr = &expr->args.arr[i];
        Value arg = CodegenExpression(argexpr);
        Kind *paramkind = &fnkind->args.arr[i];
        switch (arg.kind.type) {
        case KIND_VOID:
            ReportError(argexpr->loc, "unexpected argument %d of type void for \"%s\"", i + 1, lex);
            break;
        case KIND_INT:
            if (paramkind->type != KIND_INT) {
                ReportError(argexpr->loc, "unexpected passing int to argument %d for \"%s\"", i + 1, lex);
            } else {
                Append(&args, GetRValue(arg).llvm);
            }
            break;
        case KIND_ARRAY:
        case KIND_POINTER:
            if (paramkind->type != KIND_POINTER) {
                ReportError(argexpr->loc, "unexpected passing array or pointer to argument %d for \"%s\"", i + 1, lex);
            } else {
                Append(&args, arg.llvm);
            }
            break;
        default:
            ReportError(argexpr->loc, "unexpected type for argument %d for \"%s\"", i + 1, lex);
        }
    }
    if (args.len != argc) {
        ReportError(expr->loc, "invalid argument(s) for \"%s\"", lex);
        ArrayRelease(&args);
        return NullValue;
    }
    Value ret;
    InitValue(&ret);
    ret.type = RVALUE;
    ret.kind.type = fnkind->type == KIND_RETVOID ? KIND_VOID : KIND_INT;
    LLVMTypeRef fnTy = GetTypeRefRValue(fnkind);
    ret.llvm = LLVMBuildCall2(builder, fnTy, fnval->llvm, args.arr, args.len, fnkind->type == KIND_RETVOID ? "" : "call");
    ArrayRelease(&args);
    return ret;
}

static Value GetConstantValue(int N) {
    Value c;
    InitValue(&c);
    c.type = RVALUE;
    c.kind.type = KIND_INT;
    c.llvm = LLVMConstInt(intTy, N, 1);
    return c;
}

static Value CodegenExpression(Expression *expr) {
    switch (expr->type) {
    case EXPR_VAR:
    case EXPR_ARRAYCELL:
        return CodegenExpressionLValue(expr);
    case EXPR_ASSIGN:
        return CodegenExpressionAssign(expr);
    case EXPR_MUL:
    case EXPR_ADD:
    case EXPR_SUB:
    case EXPR_DIV:
    case EXPR_REM:
    case EXPR_BITAND:
    case EXPR_BITOR:
    case EXPR_BITXOR:
        return CodegenExpressionBinaryOp(expr);
    case EXPR_EQUAL:
    case EXPR_LESS:
    case EXPR_GREATER:
    case EXPR_EQLT:
    case EXPR_EQGT:
        return CodegenExpressionCompareOp(expr);
    case EXPR_CALL: 
        return CodegenExpressionCall(expr);
    case EXPR_CONST:
        return GetConstantValue(expr->value);
    case EXPR_ERROR:
        return NullValue;
    default:
        eprintf("expr not implemented");
    }
    return NullValue;
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
    LLVMValueRef fnvalref = fnent->value.llvm;
    LLVMBasicBlockRef bodyblock, elseblock, endblock;
    bodyblock = LLVMAppendBasicBlockInContext(context, fnvalref, "ifbody");
    if (cond->elsebody != NULL)
        elseblock = LLVMAppendBasicBlockInContext(context, fnvalref, "elsebody");
    endblock = LLVMAppendBasicBlockInContext(context, fnvalref, "endif");
    Value condval = GetRValue(CodegenExpression(&cond->ifcond));
    if (condval.kind.type != KIND_INT) {
        ReportError(cond->loc, "expected condition of if-statement to be int");
        condval = GetConstantValue(0);
    }
    LLVMValueRef predval = LLVMBuildICmp(builder, LLVMIntNE, condval.llvm, LLVMConstInt(intTy, 0, 0), "pred");
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
    LLVMValueRef fnvalref = fnent->value.llvm;
    LLVMBasicBlockRef condblock, bodyblock, stepblock, endblock;
    condblock = LLVMAppendBasicBlockInContext(context, fnvalref, "itercond");
    bodyblock = LLVMAppendBasicBlockInContext(context, fnvalref, "iterbody");
    stepblock = LLVMAppendBasicBlockInContext(context, fnvalref, "iterstep");
    endblock = LLVMAppendBasicBlockInContext(context, fnvalref, "enditer");
    /* entry block */
    CodegenExpression(&iter->iterinit);
    LLVMBuildBr(builder, condblock);
    /* condition block */
    LLVMPositionBuilderAtEnd(builder, condblock);
    Value condval = GetRValue(CodegenExpression(&iter->itercond));
    if (condval.kind.type != KIND_INT) {
        ReportError(iter->loc, "expected condition of for-statement to be integer");
        condval = GetConstantValue(0);
    }
    LLVMValueRef predval = LLVMBuildICmp(builder, LLVMIntNE, condval.llvm, LLVMConstInt(intTy, 0, 0), "pred");
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
        if (ent->value.kind.type != KIND_RETVOID) {
            ReportError(stmt->loc, "unexpected return void in integer function");
            return;
        }
        LLVMBuildRetVoid(builder);
        break;
    }
    case STMT_RETEXPR: {
        SymEnt *ent = &symtab.arr[GetFunction()];
        if (ent->value.kind.type != KIND_RETINT) {
            ReportError(stmt->loc, "unexpected return integer in void function");
            return;
        }
        Value ret = GetRValue(CodegenExpression(&stmt->expr));
        if (ret.kind.type != KIND_INT) {
            ReportError(stmt->loc, "expected return value to be int");
            return;
        }
        LLVMBuildRet(builder, ret.llvm);
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
        const char *lex = GetLex(param->lexid)->lex;
        SymEnt ent = {
            .lexid = param->lexid,
            .scope = GetScope()
        };
        Value *value = &ent.value;
        InitValue(value);
        value->kind = param->kind;
        value->type = value->kind.type == KIND_POINTER ? RVALUE : LVALUE;
        LLVMValueRef llvmparam = LLVMGetParam(fn, i);
        LLVMSetValueName2(llvmparam, lex, strlen(lex));
        if (value->type == RVALUE) {
            value->llvm = llvmparam;
        } else {
            LLVMTypeRef ty = GetTypeRefRValue(&value->kind);
            value->llvm = LLVMBuildAlloca(builder, ty, lex);
            LLVMBuildStore(builder, llvmparam, value->llvm);
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
    Value *fnval = &ent->value;
    InitValue(fnval);
    fnval->type = RVALUE;
    fnval->kind = decl->kind;
    StartScope();
    const char *lex = GetLex(decl->lexid)->lex;
    int parity = decl->args.len;
    LLVMTypeRef fnty = GetTypeRefRValue(&fnval->kind);
    fnval->llvm = LLVMAddFunction(module, lex, fnty);
    LLVMBasicBlockRef entry = LLVMAppendBasicBlockInContext(context, fnval->llvm, "entry");
    LLVMPositionBuilderAtEnd(builder, entry);
    CodegenParameters(fnval->llvm, &decl->args);
    CodegenCompund(decl->body);
    /* ent is now invalid, need to read it again */
    LLVMBasicBlockRef endblock = LLVMGetInsertBlock(builder);
    LLVMValueRef lastInst = LLVMGetLastInstruction(endblock);
    if (lastInst == NULL || !LLVMIsATerminatorInst(lastInst)) {
        if (decl->kind.type == KIND_RETINT)
            ReportError(decl->loc, "expected return statement at the end of integer function \"%s\"", lex);
        else
            LLVMBuildRetVoid(builder);
    }
    EndScope();
}

static const Kind IntKind = {
    .type = KIND_INT,
};

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
    };
    Value *value = &ent.value;
    InitValue(value);
    value->type = RVALUE;
    value->kind.type = KIND_RETVOID;
    value->kind.args.arr = (Kind *)&IntKind;
    value->kind.args.len = 1;
    value->llvm = myprint;
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
        .scope = 0
    };
    Value *value = &ent.value;
    InitValue(value);
    value->type = RVALUE;
    value->kind.type = KIND_RETINT;
    value->llvm = myinput;
    Append(&symtab, ent);
}

void CodegenProgram(DeclarationArray *prog) {
    StartScope();
    CodegenPrint();
    CodegenInput();
    for (int i = 0; i < prog->len; i++) {
        Declaration *decl = &prog->arr[i];
        if (decl->kind.type == KIND_RETVOID || decl->kind.type == KIND_RETINT)
            CodegenFunction(decl);
        else
            CodegenGlobalVariable(decl);
    }
    EndScope();
}
