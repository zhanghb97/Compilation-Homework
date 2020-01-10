/**
 * Clang interpreter
 */

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/EvaluatedExprVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Tooling/Tooling.h"

using namespace clang;

#include "Environment.h"

class InterpreterVisitor : 
  public EvaluatedExprVisitor<InterpreterVisitor> {
public:
  explicit InterpreterVisitor(const ASTContext &context, Environment * env)
  : EvaluatedExprVisitor(context), mEnv(env) {}
  virtual ~InterpreterVisitor() {}

  virtual void VisitBinaryOperator (BinaryOperator * bop) {
    //llvm::errs() << "visit binary op\n";
    VisitStmt(bop);
    mEnv->binop(bop);
  }
  virtual void VisitUnaryOperator (UnaryOperator * uop) {
    VisitStmt(uop);
    mEnv->unaop(uop);
  }
  virtual void VisitDeclRefExpr(DeclRefExpr * expr) {
    //llvm::errs() << "visit declrefexpr\n";
    VisitStmt(expr);
    mEnv->declref(expr);
  }
  virtual void VisitCastExpr(CastExpr * expr) {
    //llvm::errs() << "visit castexpr\n";
    VisitStmt(expr);
    mEnv->cast(expr);
  }
  virtual void VisitParenExpr(ParenExpr * pexpr) {
    VisitStmt(pexpr);
    mEnv->parenexpr(pexpr);
  }
  virtual void VisitArraySubscriptExpr(ArraySubscriptExpr * expr) {
    VisitStmt(expr);
    mEnv->array(expr);
  }
  virtual void VisitCallExpr(CallExpr * call) {
    VisitStmt(call);
    if (mEnv->call(call)) {
      // 新栈帧入栈，存入函数参数
      if (call->getDirectCallee()->getBody())
        VisitStmt(call->getDirectCallee()->getBody());
      // 获取返回值，栈帧出栈
      mEnv->callfinished(call);
    }
  }
  virtual void VisitReturnStmt(ReturnStmt * retstmt) {
    VisitStmt(retstmt);
    mEnv->retstmt(retstmt);
  }
  virtual void VisitDeclStmt(DeclStmt * declstmt) {
    mEnv->decl(declstmt);
  }
  virtual void VisitCharacterLiteral(CharacterLiteral * charliteral) {
    mEnv->charliteral(charliteral);
  }
  virtual void VisitIntegerLiteral(IntegerLiteral * intliteral) {
    mEnv->intliteral(intliteral);
  }
  virtual void VisitUnaryExprOrTypeTraitExpr(UnaryExprOrTypeTraitExpr * sizeoftype) {
    mEnv->sizeoftype(sizeoftype);
  }
  virtual void VisitIfStmt(IfStmt * ifstmt) {
    Visit(ifstmt->getCond());
    // if循环条件判断
    if (mEnv->ifstmt(ifstmt)) {
      if (ifstmt->getThen())
        Visit(ifstmt->getThen());
    } 
    // else情况
    else {
      if (ifstmt->getElse())
        Visit(ifstmt->getElse());  
    }         
  }
  virtual void VisitWhileStmt(WhileStmt * wstmt) {
    Visit(wstmt->getCond());
    // while循环条件判断
    while (mEnv->whilestmt(wstmt)){
      // while循环body
      if (wstmt->getBody())
        VisitStmt(wstmt->getBody());
      Visit(wstmt->getCond());
    }
  }
  virtual void VisitForStmt(ForStmt * fstmt) {
    if (fstmt->getInit()) 
      Visit(fstmt->getInit());
    Visit(fstmt->getCond());
    // for循环条件判断
    while (mEnv->forstmt(fstmt)) {
      // for循环body
      if (fstmt->getBody())
        VisitStmt(fstmt->getBody());
      // for循环增量
      if (fstmt->getInc())
        Visit(fstmt->getInc());
      Visit(fstmt->getCond());
    }
  }
private:
  Environment * mEnv;
};

class InterpreterConsumer : public ASTConsumer {
public:
  explicit InterpreterConsumer(const ASTContext& context) : mEnv(),
    mVisitor(context, &mEnv) {
  }
  virtual ~InterpreterConsumer() {}

  virtual void HandleTranslationUnit(clang::ASTContext &Context) {
    TranslationUnitDecl * decl = Context.getTranslationUnitDecl();
    mEnv.init(decl);

    FunctionDecl * entry = mEnv.getEntry();
    mVisitor.VisitStmt(entry->getBody());
  }
private:
  Environment mEnv;
  InterpreterVisitor mVisitor;
};

class InterpreterClassAction : public ASTFrontendAction {
public: 
  virtual std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(
          clang::CompilerInstance &Compiler, llvm::StringRef InFile) {
    return std::unique_ptr<clang::ASTConsumer>(
      new InterpreterConsumer(Compiler.getASTContext()));
  }
};

int main (int argc, char ** argv) {
  if (argc > 1) {
    clang::tooling::runToolOnCode(new InterpreterClassAction, argv[1], "xxx.c");
  } 
}