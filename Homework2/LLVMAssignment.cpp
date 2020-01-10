/**
 * LLVM-Assignment
 * 直接调用: 打印被调用函数的名称和行号 
 * 函数指针: 确定可能的调用函数，将其替换成直接调用
 */

#include <llvm/Support/CommandLine.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Support/ToolOutputFile.h>
#include <llvm/Transforms/Scalar.h>

// 编写Pass，作用在Function上，并进行输出
#include <llvm/IR/Function.h>
#include <llvm/Pass.h>
#include <llvm/Support/raw_ostream.h>

#if LLVM_VERSION_MAJOR >= 4
#include <llvm/Bitcode/BitcodeReader.h>
#include <llvm/Bitcode/BitcodeWriter.h>

#else
#include <llvm/Bitcode/ReaderWriter.h>
#endif
using namespace llvm;


#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Module.h> 
#include <llvm/IR/User.h>
#include <llvm/Support/FileSystem.h>
#include <list>
#include <llvm/IR/InstIterator.h>
using namespace std;

#if LLVM_VERSION_MAJOR >= 4
static ManagedStatic<LLVMContext> GlobalContext;
static LLVMContext& getGlobalContext() {
  return *GlobalContext;
}
#endif
/* In LLVM 5.0, when  -O0 passed to clang , the functions generated with clang
 * will have optnone attribute which would lead to some transform passes
 * disabled, like mem2reg.
 */
#if LLVM_VERSION_MAJOR == 5
struct EnableFunctionOptPass : public FunctionPass {
  static char ID;
  EnableFunctionOptPass() : FunctionPass(ID) {}
  bool runOnFunction(Function& F) override {
    if (F.hasFnAttribute(Attribute::OptimizeNone)) {
      F.removeFnAttr(Attribute::OptimizeNone);
    }
    return true;
  }
};

char EnableFunctionOptPass::ID = 0;
#endif

// processed by mem2reg before this pass.
struct FuncPtrPass : public ModulePass {
  // ID是pass的标识
  static char ID;  // Pass identification, replacement for typeid
  FuncPtrPass() : ModulePass(ID) {}
  using nameList = list<StringRef>;
  nameList funcList;
  map<int, nameList> funcMap;
  list<Value*> valueList;
  StringRef funcName;

  // 重写runOnModule，pass从此处开始执行
  bool runOnModule(Module& M) override {
    // Function迭代
    for (Function &Func : M) {
      // BasicBlock迭代
      for (BasicBlock &BB : Func) {
        // Instruction迭代
        for (Instruction &I : BB) {
          // 指令类型
          Instruction* inst = dyn_cast<Instruction>(&I);
          // 判断指令是否为CallInst
          if (CallInst* callInst = dyn_cast<CallInst>(inst)) {
            // 返回调用的函数，如果这是间接函数调用，则返回null
            Function* func = callInst->getCalledFunction();
            // 调用函数的行号
            int callLine = callInst->getDebugLoc().getLine();
            // 如果是间接引用
            if (!func) {
              funcList.clear();
              handleIndirectCall(callInst);
              funcMap.insert(pair<int, nameList>(callLine, funcList));
            } 
            // 如果是直接引用
            else {
              // isIntrinsic(): 如果func的值以"llvm."返回TRUE
              if (!func->isIntrinsic()) {
                funcList.clear();
                map<int, nameList>::iterator it = funcMap.begin(),
                                             ie = funcMap.end();
                StringRef name = func->getName();
                // 向List和Map中装入信息
                if (it == ie) {
                  funcList.push_back(name);
                  funcMap.insert(pair<int, nameList>(callLine, funcList));
                }
                for (; it != ie; ++it) {
                  if (it->first == callLine) {
                    it->second.push_back(func->getName());
                  } else {  
                    // 先插入一个空list,再次遍历的时候插入值
                    funcMap.insert(pair<int, nameList>(callLine, funcList));
                  }
                }
              }
            }
          }
        }
      }
    }

    // 输出结果
    for (map<int, nameList>::iterator map_it = funcMap.begin(),
                                      map_ie = funcMap.end();
         map_it != map_ie; ++map_it) {
      errs() << map_it->first << " : ";
      auto it = map_it->second.begin();
      auto ie = map_it->second.end();

      if (it != ie) {
        errs() << *it;
        ++it;
      }
      for (; it != ie; ++it) {
        errs() << ", " << *it;
      }
      errs() << "\n";
    }
    return false;
  }

  // 处理间接调用，判断间接调用的类型
  void handleIndirectCall(CallInst* callInst) {
    Value* value = callInst->getCalledValue();
    // 如果间接调用来自PHI节点
    if (PHINode* pHINode = dyn_cast<PHINode>(value)) {
      handlePHINode(pHINode);
    }
    // 如果间接调用来自参数节点
    else if (Argument* argument = dyn_cast<Argument>(value)) {
      handleArgument(argument);
    }
    // 如果间接调用来自函数调用
    else if (CallInst* callInst = dyn_cast<CallInst>(value)) {
      handleCallInst(callInst);
    }
  }

  void handleCallInst(CallInst* callInst) {
    // 返回调用的函数，如果这是间接函数调用，则返回null
    Function* func = callInst->getCalledFunction();
    // 如果是直接引用
    if (func) {
      for (inst_iterator inst_it = inst_begin(func), inst_ie = inst_end(func);
           inst_it != inst_ie; ++inst_it) {
        // 取返回值
        if (ReturnInst* ret = dyn_cast<ReturnInst>(&*inst_it)) {
          Value* value = ret->getReturnValue();
          // 如果返回值来自参数
          if (Argument* argument = dyn_cast<Argument>(value)) {
            handleArgument(argument);
          }
          // 如果返回值来自PHI节点
          else if (PHINode* pHINode = dyn_cast<PHINode>(value)) {
            handlePHINode(pHINode);
          }
          // 如果返回值来自函数调用
          else if (CallInst* callInst = dyn_cast<CallInst>(value)) {
            handleCallInst(callInst);
          }
        }
      }
    } 
    // 间接引用
    else {
      Value* value = callInst->getCalledValue();
      // 间接引用来自PHI节点
      if (PHINode* pHINode = dyn_cast<PHINode>(value)) {
        // 遍历PHI节点的每一个入口
        for (Value* inComingValue : pHINode->incoming_values()) {
          // 如果PHI节点的入口为函数
          if (Function* func = dyn_cast<Function>(inComingValue)) {
            for (inst_iterator inst_it = inst_begin(func),
                               inst_ie = inst_end(func);
                 inst_it != inst_ie; ++inst_it) {
              // 取返回值
              if (ReturnInst* ret = dyn_cast<ReturnInst>(&*inst_it)) {
                Value* value = ret->getReturnValue();
                // 如果返回值值来自于参数
                if (Argument* argument = dyn_cast<Argument>(value)) {
                  handleArgument(argument);
                }
                // 如果返回值来自PHI节点
                else if (PHINode* pHINode = dyn_cast<PHINode>(value)) {
                  handlePHINode(pHINode);
                }
                // 如果返回值来自函数调用
                else if (CallInst* callInst = dyn_cast<CallInst>(value)) {
                  handleCallInst(callInst);
                }
              }
            }
          }
        }
      }
    }
  }

  // 处理PHI节点
  void handlePHINode(PHINode* pHINode) {
    // 遍历PHI节点的每一个入口
    for (Value* inComingVal : pHINode->incoming_values()) {
      // 如果入口是一个PHI节点
      if (PHINode *pHINode = dyn_cast<PHINode>(inComingVal)) {
        handlePHINode(pHINode);
      }
      // 如果入口是一个参数节点
      else if (Argument* argument = dyn_cast<Argument>(inComingVal)) {
        handleArgument(argument);
      }
      // 如果入口是一个函数节点
      else if (Function* func = dyn_cast<Function>(inComingVal)) {
        pushBackFunc(func->getName());
      }
    }
  }

  // 处理PHI节点引出的PHI节点，针对test14
  void handlePHICallPHINode(PHINode* pHINode, int flag) {
    // 记录PHI节点入口索引
    int index = 0;
    // 遍历PHI节点的每一个入口
    for (Value* inComingVal : pHINode->incoming_values()) {
      // 如果当前入口与输入入口一致，则继续分析
      if (index == flag){
        // 如果入口是一个PHI节点
        if (PHINode *pHINode = dyn_cast<PHINode>(inComingVal)) {
          handlePHINode(pHINode);
        }
        // 如果入口是一个参数节点 
        else if (Argument* argument = dyn_cast<Argument>(inComingVal)) {
          handleArgument(argument);
        }
        // 如果入口是一个函数节点
        else if (Function* func = dyn_cast<Function>(inComingVal)) {
          pushBackFunc(func->getName());
        }
      }
      index++;
    }
  }

  // 处理参数
  void handleArgument(Argument* arg) {
    // 判断arg来自于函数的第几个参数
    int index = arg->getArgNo();
    // 参数的父函数
    Function* parentFunc = arg->getParent();
    // 寻找父函数在哪里被赋值
    for (User* user : parentFunc->users()) {
      // 父函数被函数赋值
      if (CallInst* callInst = dyn_cast<CallInst>(user)) {
        // 获得赋值函数的第index个参数
        Value* value = callInst->getArgOperand(index);
        // 如果参数来自于函数调用
        if (callInst->getCalledFunction() != parentFunc) {
          Function* func = callInst->getCalledFunction();
          for (inst_iterator inst_it = inst_begin(func),
                             inst_ie = inst_end(func);
               inst_it != inst_ie; ++inst_it) {
            // 取返回值
            if (ReturnInst* ret = dyn_cast<ReturnInst>(&*inst_it)) {
              Value* v = ret->getReturnValue();
              // 如果返回值来自于函数调用
              if (CallInst* call_inst = dyn_cast<CallInst>(v)) {
                // 取函数的第index个参数
                Value* value = call_inst->getArgOperand(index);
                // 处理参数
                if (Argument* argument = dyn_cast<Argument>(value)) {
                  handleArgument(argument);
                }
              }
            }
          }
        }
        // 如果参数来自于函数直接调用
        else if (Function* func = dyn_cast<Function>(value)) {
          pushBackFunc(func->getName());
        } 
        // 如果参数来自于PHI节点
        else if (PHINode* pHINode = dyn_cast<PHINode>(value)) {
          handlePHINode(pHINode);
        }
        // 如果参数来自于参数
        else if (Argument* argument = dyn_cast<Argument>(value)) {
          handleArgument(argument);
        }
      } 
      // 父函数被PHI节点赋值
      else if (PHINode* pHINode = dyn_cast<PHINode>(user)) {
        // 记录PHI节点入口索引
        int flag = 0;
        // 遍历PHI节点
        for (User* user : pHINode->users()) {
          // 如果PHI节点为间接调用
          if (CallInst* callInst = dyn_cast<CallInst>(user)) {
            // 取第index个参数
            Value* value = callInst->getOperand(index);
            // 如果参数为直接调用
            if (Function* func = dyn_cast<Function>(value)) {
              pushBackFunc(func->getName());
            }
            // 如果参数为PHI节点
            else if (PHINode* pHINode = dyn_cast<PHINode>(value)) {
              handlePHICallPHINode(pHINode, flag);
            }
            // 如果参数来自于参数
            else if (Argument* argument = dyn_cast<Argument>(value)) {
              handleArgument(argument);
            }
          }
          flag++;
        }
      }
    }
  }

  // 将函数名放入funcList结尾
  void pushBackFunc(StringRef name) {
    if (find(funcList.begin(), funcList.end(), name) == funcList.end()) {
      funcList.push_back(name);
    }
  }  
};

char FuncPtrPass::ID = 0;
static RegisterPass<FuncPtrPass> X("funcptrpass",
                                   "Print function call instruction");

static cl::opt<std::string> InputFilename(cl::Positional,
                                          cl::desc("<filename>.bc"),
                                          cl::init(""));

int main(int argc, char** argv) {
  LLVMContext& Context = getGlobalContext();
  SMDiagnostic Err;
  // Parse the command line to read the Inputfilename
  cl::ParseCommandLineOptions(
      argc, argv, "FuncPtrPass \n My first LLVM too which does not do much.\n");

  // Load the input module
  std::unique_ptr<Module> M = parseIRFile(InputFilename, Err, Context);
  if (!M) {
    Err.print(argv[0], errs());
    return 1;
  }

  llvm::legacy::PassManager Passes;

  /// Remove functions' optnone attribute in LLVM5.0
  #if LLVM_VERSION_MAJOR == 5
    Passes.add(new EnableFunctionOptPass());
  #endif
  /// Transform it to SSA
  Passes.add(llvm::createPromoteMemoryToRegisterPass());

  /// Your pass to print Function and Call Instructions
  Passes.add(new FuncPtrPass());
  Passes.run(*M.get());
}
