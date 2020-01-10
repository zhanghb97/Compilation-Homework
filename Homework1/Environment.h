/**
 * Clang interpreter
 */

#include <stdio.h>
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/Expr.h"
#include "clang/AST/Decl.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Tooling/Tooling.h"

using namespace clang;

class StackFrame {
  // Stmt, Decl在栈帧中的语法树节点
  std::map<Decl*, int> mVars;
  std::map<Stmt*, int> mExprs;
  // 表示当前Stmt节点
  Stmt * mPC;
public:
  StackFrame() : mVars(), mExprs(), mPC() {
  }

  // 将抽象语法树Decl节点，与值进行绑定，键值对存入栈帧的mVars中
  void bindDecl(Decl* decl, int val) {
    mVars[decl] = val;
  }

  // 检查Decl节点是否在该栈帧中的mVars（可以看作静态语义检查）
  bool isInVars(Decl * decl) {
    if (mVars.find(decl) != mVars.end())
      return true;
    return false;
  }

  // 在该栈帧中的mVars里，取Decl节点对应的值
  int getDeclVal(Decl * decl) {
    assert (mVars.find(decl) != mVars.end());
    return mVars.find(decl)->second;
  }

  // 将抽象语法树Stmt节点，与值进行绑定，键值对存入栈帧的mExprs中
  void bindStmt(Stmt * stmt, int val) {
    mExprs[stmt] = val;
  }

  // 在该栈帧中的mExprs里，取Stmt节点对应的值
  int getStmtVal(Stmt * stmt) {
    assert (mExprs.find(stmt) != mExprs.end());
    return mExprs[stmt];
  }

  // 将栈帧中mPC设置为stmt
  void setPC(Stmt * stmt) {
    mPC = stmt;
  }

  // 获取栈帧中当前Stmt节点
  Stmt * getPC() {
    return mPC;
  }
};

// Heap类对象用来模拟堆，用来动态分配数据，进行地址和值的映射
class Heap {
  std::map<int, int> mVals;
  // curr为堆中顶部位置的标记，记录当前的位置
  int curr;
public:
  Heap() : mVals(), curr(0) {
  }

  // 用来模拟堆分配内存空间
  int malloc(int len){
    int i = 0;
    for (i = 0; i < len; ++i) {
      mVals[curr + i] = 0;
    }
    int base = curr;
    curr += len;
    return base;
  }

  // 将值存入堆中对应的位置
  void setVal(int address, int val) {
    assert (mVals.find(address) != mVals.end());
    mVals[address] = val;
  }

  // 取给定位置中的值
  int getVal(int address) {
    assert (mVals.find(address) != mVals.end());
    return mVals[address];
  }
};

class Environment {
  // 记录栈帧的vector，用来模拟栈
  std::vector<StackFrame> mStack;
  // 堆
  Heap mHeap; 
  // 预定义函数
  FunctionDecl * mFree;
  FunctionDecl * mMalloc;
  FunctionDecl * mInput;
  FunctionDecl * mOutput;
  // 函数入口
  FunctionDecl * mEntry;
public:
  // 二元运算符、一元运算符数据类型
  typedef BinaryOperatorKind binOpcode;
  typedef UnaryOperatorKind  unaOpcode;
  // Environment构造函数，模拟内存中加载一段程序
  Environment() : mStack(), mHeap(),  mFree(NULL), mMalloc(NULL), mInput(NULL), mOutput(NULL), mEntry(NULL) {
  }

  // 对Environment进行初始化，处理预定义函数、全局变量、全局数组
  void init(TranslationUnitDecl * unit) {
    mStack.push_back(StackFrame());
    for (TranslationUnitDecl::decl_iterator i =unit->decls_begin(), e = unit->decls_end(); i != e; ++ i) {
      // 处理全局变量、数组
      if (VarDecl * vdecl = dyn_cast<VarDecl>(*i)) {
        // 变量、数组元素的缺省值
        int val = 0;
        // 全局数组，在堆中分配空间，获取数组的首地址
        if (const ConstantArrayType * atype = dyn_cast<ConstantArrayType>(vdecl->getType().getTypePtr())) {
          int len = (int)atype->getSize().getLimitedValue();
          int address = mHeap.malloc(len);
          val = address;
        } 
        // 全局变量，获取变量初值
        else if (Expr * expr = vdecl->getInit()) {
          if (IntegerLiteral * iliteral = dyn_cast<IntegerLiteral>(expr)) {
            val = (int)iliteral->getValue().getLimitedValue();  
          }
        }
        // 将vdecl节点，与数组地址／变量初值进行绑定，存入当前栈帧
        mStack.back().bindDecl(vdecl, val);
      }

      // 处理预定义函数
      if (FunctionDecl * fdecl = dyn_cast<FunctionDecl>(*i) ) {
        if (fdecl->getName().equals("FREE")) mFree = fdecl;
        else if (fdecl->getName().equals("MALLOC")) mMalloc = fdecl;
        else if (fdecl->getName().equals("GET")) mInput = fdecl;
        else if (fdecl->getName().equals("PRINT")) mOutput = fdecl;
        else if (fdecl->getName().equals("main")) mEntry = fdecl;
      }
    }
  }

  // 获取程序入口
  FunctionDecl * getEntry() {
    return mEntry;
  }

  /// !TODO Support comparison operation
  // 二元运算
  void binop(BinaryOperator *bop) {
    Expr * binexpr = NULL;
    if (!(binexpr = dyn_cast<Expr>(bop))) {
        // llvm::errs() << "dyn_cast<Expr>(bop) wrong\n";
    }
    // 获取符号左右表达式
    Expr * left = bop->getLHS();
    Expr * right = bop->getRHS();
    // 获取符号
    binOpcode Opc = bop->getOpcode();
    // 获取左右值
    int lval = mStack.back().getStmtVal(left);
    int rval = mStack.back().getStmtVal(right);

    // 等号
    if (bop->isAssignmentOp()) {
      // 将左侧表达式与右侧值进行绑定
      mStack.back().bindStmt(left, rval);
      // 如果左侧的表达式是简单变量，直接进行绑定
      if (DeclRefExpr * declexpr = dyn_cast<DeclRefExpr>(left)) {
        Decl * decl = declexpr->getFoundDecl();
        mStack.back().bindDecl(decl, rval);
      } 
      // 如果左侧表达式是数组变量，从栈中取基地址和偏移量，将值存入堆中相应位置
      else if (ArraySubscriptExpr * arrayexpr = dyn_cast<ArraySubscriptExpr>(left)) {
        // llvm::errs() << "array assign\n";
        int base = mStack.back().getStmtVal(arrayexpr->getBase());
        int idx = mStack.back().getStmtVal(arrayexpr->getIdx());
        mHeap.setVal(base + idx, rval);
      } 
      // 如果左侧表达式是指针变量，从栈中取地址，将值存入堆中相应位置
      else if (UnaryOperator * uop = dyn_cast<UnaryOperator>(left)) {
        // llvm::errs() << "pointer assign\n";
        int address = mStack.back().getStmtVal(uop->getSubExpr());
        mHeap.setVal(address, rval);
      }
    }
    // 算数符号
    else if (Opc == BO_Add) {
      mStack.back().bindStmt(binexpr, lval + rval);
    } 
    else if (Opc == BO_Sub) {
      mStack.back().bindStmt(binexpr, lval - rval);
    } 
    else if (Opc == BO_Mul) {
      mStack.back().bindStmt(binexpr, lval * rval);
    } 
    else if (Opc == BO_Div) {
      mStack.back().bindStmt(binexpr, lval / rval);
    } 
    else if (Opc == BO_LT) {
      int val = (lval < rval) ? 1:0;
      mStack.back().bindStmt(binexpr, val);
    } 
    else if (Opc == BO_GT) {
      int val = (lval > rval) ? 1:0;
      mStack.back().bindStmt(binexpr, val);
    } 
    else if (Opc == BO_EQ) {
      int val = (lval == rval) ? 1:0;
      mStack.back().bindStmt(binexpr, val);
    }
  }

  // 一元运算符
  void unaop(UnaryOperator *uop) {
    // 获取一元运算表达式
    Expr * unaexpr = dyn_cast<Expr>(uop);
    Expr * subexpr = uop->getSubExpr();
    // 获取一元运算符
    unaOpcode Opc = uop->getOpcode();
    // 获取运算表达式的值
    int val = mStack.back().getStmtVal(subexpr);

    // 指针，从堆中取出指针指向的值，与unaexpr绑定，存入栈帧
    if (Opc == UO_Deref) {
      val = mHeap.getVal(val);
      mStack.back().bindStmt(unaexpr, val);
    } 
    // 负值
    else if (Opc == UO_Minus) {
      mStack.back().bindStmt(unaexpr, -val);
    }        
  }

  // 声明
  void decl(DeclStmt * declstmt) {
    // 迭代器，遍历declstmt
    for (DeclStmt::decl_iterator it = declstmt->decl_begin(), ie = declstmt->decl_end();
      it != ie; ++ it) {
      if (VarDecl * vdecl = dyn_cast<VarDecl>(*it)) {
        // llvm::errs() << "var decl: " << vdecl << "\n";
        int val = 0;

        // 定长数组声明
        if (const ConstantArrayType * atype = dyn_cast<ConstantArrayType>(vdecl->getType().getTypePtr())) {
          // llvm::errs() << "array: " << vdecl << "\n";
          int len = (int)atype->getSize().getLimitedValue();
          int address = mHeap.malloc(len);
          val = address;
        }
        // 变量的声明
        else if (Expr * expr = vdecl->getInit()) {
          // llvm::errs() << "init: " << vdecl << "\n";
          if (IntegerLiteral * iliteral = dyn_cast<IntegerLiteral>(expr)) {
            val = (int)iliteral->getValue().getLimitedValue();  
          }
          else if (CharacterLiteral * charliteral = dyn_cast<CharacterLiteral>(expr)) {
            // llvm::errs() << "char: " << vdecl << "\n";
            // TODO: figure out how to use CharacterLiteral in decl.
          }
        }
        // llvm::errs() << "bind: " << vdecl << "\n";
        // 在当前栈帧中，加入声明和初始值的绑定
        mStack.back().bindDecl(vdecl, val);
      }
    }
  }

  // 引用
  void declref(DeclRefExpr * declref) {
    if (declref->getType()->isIntegerType() ||
      declref->getType()->isPointerType() ||
      declref->getType()->isConstantArrayType()) {
      Decl* decl = declref->getFoundDecl();
      // 检查变量是否在局部作用域中
      if (mStack.back().isInVars(decl)) {
        int val = mStack.back().getDeclVal(decl);
        mStack.back().bindStmt(declref, val);
      }
      // 检查变量是否在全局作用域中
      // 全局作用域栈帧在栈的最前方
      else if (mStack.front().isInVars(decl)){
        int val = mStack.front().getDeclVal(decl);
        mStack.back().bindStmt(declref, val);
      }
      else {
        llvm::errs() << "error: unknown declref!\n";
      }
    }
  }

  void cast(CastExpr * castexpr) {
    // llvm::errs() << "cast!\n";
    // mStack.back().setPC(castexpr);
    if (castexpr->getType()->isFunctionPointerType()) {
      return ;
    } 
    // 处理整型
    else if (castexpr->getType()->isIntegerType()) {
      // llvm::errs() << "cast int!\n";
      Expr * expr = castexpr->getSubExpr();
      int val = mStack.back().getStmtVal(expr);
      mStack.back().bindStmt(castexpr, val);
      // llvm::errs() << "    int: " << val << "\n";
    } 
    // 处理指针
    else if (castexpr->getType()->isPointerType()) {
      Expr * expr = castexpr->getSubExpr();
      int val = mStack.back().getStmtVal(expr);
      mStack.back().bindStmt(castexpr, val);
    }
    // 处理字符
    else if (castexpr->getType()->isAnyCharacterType()) {
      // llvm::errs() << "cast char!\n";
      // TODO: figure out how to use isAnyCharacterType.
    }
  }

  // []
  void parenexpr(ParenExpr * pexpr) {
    Expr * expr = pexpr->getSubExpr();
    int val = mStack.back().getStmtVal(expr);
    mStack.back().bindStmt(pexpr, val);
  }

  // 处理数组下标
  void array(ArraySubscriptExpr * arrayexpr) {
    int base = mStack.back().getStmtVal(arrayexpr->getBase());
    int idx = mStack.back().getStmtVal(arrayexpr->getIdx());
    int val = mHeap.getVal(base + idx);
    mStack.back().bindStmt(arrayexpr, val);
  }
    
  /// !TODO Support Function Call
  // 函数调用
  bool call(CallExpr * callexpr) {
    //mStack.back().setPC(callexpr);
    int val = 0;
    // 获取被调函数
    FunctionDecl * callee = callexpr->getDirectCallee();
    // 预定义函数
    if (callee == mInput) {
      llvm::errs() << "Please Input an Integer Value : ";
      scanf("%d", &val);
      mStack.back().bindStmt(callexpr, val);
    } 
    else if (callee == mOutput) {
      Expr * decl = callexpr->getArg(0);
      val = mStack.back().getStmtVal(decl);
      llvm::errs() << val;
    } 
    else if (callee == mMalloc) {
      // llvm::errs() << "malloc\n";
      Expr * decl = callexpr->getArg(0);
      val = mStack.back().getStmtVal(decl);
      int address = mHeap.malloc(val);
      mStack.back().bindStmt(callexpr, address);
    } 
    else if (callee == mFree) {
      // llvm:errs() << "Free(): do nothing\n";
      return false;
    } 
    // 自定义函数
    else { 
      // 为函数创建新的栈帧，并将参数存入其中
      StackFrame * newStackFrame = new StackFrame();
      for (unsigned int i = 0; i < callexpr->getNumArgs(); ++i) {
        Decl * param = callee->getParamDecl(i);
        Expr * decl = callexpr->getArg(i);
        val = mStack.back().getStmtVal(decl);
        newStackFrame->bindDecl(param, val);
      }
      // 记下调用函数的Stmt节点，作为静态链
      newStackFrame->setPC(callexpr);
      // 给返回值赋初值0
      newStackFrame->bindStmt(callexpr, 0);
      // 将新的栈帧存入Environment中的栈
      mStack.push_back(*newStackFrame);
      return true;
    }
    return false;
  }

  // 函数调用结束
  void callfinished(CallExpr * callexpr) {
    // 取返回值
    int val = mStack.back().getStmtVal(callexpr);
    // 在栈中弹出该栈帧
    mStack.pop_back();
    // 将callexpr节点与值进行绑定
    mStack.back().bindStmt(callexpr, val);
  }
    
  // 处理return
  void retstmt(ReturnStmt * retstmt) {
    // 取静态链中的callexpr节点
    Stmt * callexpr = mStack.back().getPC();
    // 取返回值
    int val = mStack.back().getStmtVal(retstmt->getRetValue());
    // 将callexpr节点与值进行绑定
    mStack.back().bindStmt(callexpr, val);      
  }
    
  // 处理整型数据
  void intliteral(IntegerLiteral * intliteral) {
    int val = (int)intliteral->getValue().getLimitedValue();
    mStack.back().bindStmt(dyn_cast<Expr>(intliteral), val);
  }

  // 处理字符数据
  void charliteral(CharacterLiteral * charliteral) {
    // llvm::errs() << "CharacterLiteral\n";
    int val = (char)charliteral->getValue();
    mStack.back().bindStmt(dyn_cast<Expr>(charliteral), val);
  }

  // 处理sizeof
  void sizeoftype(UnaryExprOrTypeTraitExpr * sizeoftype) {
    if (sizeoftype->getTypeOfArgument()->isIntegerType()) {
      mStack.back().bindStmt(sizeoftype, 1);
    }
    if (const auto *T = sizeoftype->getTypeOfArgument()->getAs<PointerType>()) {
      if (T->getPointeeType()->isIntegerType()) {
        mStack.back().bindStmt(sizeoftype, 1);
      }
    }
  }

  // if，返回条件表达式真假
  bool ifstmt(IfStmt * ifstmt) {
    return (mStack.back().getStmtVal(ifstmt->getCond())) ? true:false;     
  }

  // while，返回条件表达式真假
  bool whilestmt(WhileStmt * wstmt) {
    return (mStack.back().getStmtVal(wstmt->getCond())) ? true:false;
  } 

  // for，返回条件表达式真假
  bool forstmt(ForStmt * fstmt) {
    return (mStack.back().getStmtVal(fstmt->getCond())) ? true:false;
  }
  
  // 将Stmt节点存入当前栈帧的mPC，作为静态链
  void setPC(Stmt * stmt) {
    mStack.back().setPC(stmt);
  }

};