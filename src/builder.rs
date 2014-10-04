use std::collections::HashMap;
use libc::{c_char, c_uint};

use rustc::lib::llvm;

use missing_llvm_bindings::*;
use parser::*;

pub struct Context {
    context: llvm::ContextRef,
    module: llvm::ModuleRef,
    builder: llvm::BuilderRef,
    exec_engine: llvm::ExecutionEngineRef,
    function_passmanager: llvm::PassManagerRef,
    named_values: HashMap<String, llvm::ValueRef>
}

pub type IRBuildingResult = Result<(llvm::ValueRef, bool), String>;

pub trait IRBuilder {
    fn codegen(&self, context: &mut Context) -> IRBuildingResult;
}

pub fn dump_value(value: llvm::ValueRef) {
    unsafe {
        llvm::LLVMDumpValue(value);
    }
}

#[no_mangle]
pub extern fn print(x: f64) -> f64 {
    println!("> {} <", x);
    x
}

#[allow(unused_variable)]
pub fn run(value: llvm::ValueRef, context: &Context) -> f64 {
    //hack for make linker not to remove print function
    let x = print;

    unsafe {
        let result = LLVMRunFunction(context.exec_engine,
                                     value,
                                     0,
                                     0 as *const GenericValueRef);
        let ty = llvm::LLVMDoubleTypeInContext(context.context);
        LLVMGenericValueToFloat(ty, result)
    }
}

impl Context {
    pub fn new(module_name : &str) -> Context {
        unsafe {
            LLVMInitializeNativeTarget();

            let context = llvm::LLVMContextCreate();
            let module = llvm::LLVMModuleCreateWithNameInContext(module_name.to_c_str().as_ptr(), context);
            let builder = llvm::LLVMCreateBuilderInContext(context);
            let named_values = HashMap::new();

            let mut exec_engine = 0 as llvm::ExecutionEngineRef;
            let mut error = 0 as *const c_char;
            LLVMCreateExecutionEngineForModule(&mut exec_engine, module, &mut error);
            assert!(exec_engine != 0 as llvm::ExecutionEngineRef);

            let function_passmanager = llvm::LLVMCreateFunctionPassManagerForModule(module);

            let target_data = LLVMGetExecutionEngineTargetData(exec_engine);
            let data_layout = LLVMCopyStringRepOfTargetData(target_data);
            llvm::LLVMSetDataLayout(module, data_layout);
            llvm::LLVMAddTargetData(target_data, function_passmanager);
            LLVMDisposeMessage(data_layout);

            llvm::LLVMAddVerifierPass(function_passmanager);
            llvm::LLVMAddBasicAliasAnalysisPass(function_passmanager);
            llvm::LLVMAddInstructionCombiningPass(function_passmanager);
            llvm::LLVMAddReassociatePass(function_passmanager);
            llvm::LLVMAddGVNPass(function_passmanager);
            llvm::LLVMAddCFGSimplificationPass(function_passmanager);
            llvm::LLVMInitializeFunctionPassManager(function_passmanager);

            Context{context: context,
                    module: module,
                    builder: builder,
                    exec_engine: exec_engine,
                    function_passmanager:function_passmanager,
                    named_values: named_values}
        }
    }

    pub fn dump(&self) {
        unsafe {
            llvm::LLVMDumpModule(self.module);
        }
    }
}

impl Drop for Context {
    fn drop(&mut self) {
        unsafe {
            llvm::LLVMDisposePassManager(self.function_passmanager);
            LLVMDisposeExecutionEngine(self.exec_engine);
            llvm::LLVMDisposeBuilder(self.builder);
            llvm::LLVMContextDispose(self.context);
        }
    }
}

fn error(message : &str) -> IRBuildingResult {
    Err(message.to_string())
}

impl IRBuilder for Expression {
    fn codegen(&self, context: &mut Context) -> IRBuildingResult {
        unsafe {
            match self {
                &Literal(ref value) => {
                    let ty = llvm::LLVMDoubleTypeInContext(context.context);
                    Ok((llvm::LLVMConstReal(ty, *value), false))
                },
                &Variable(ref name) => {
                    match context.named_values.find(name) {
                        Some(value) => Ok((*value, false)),
                        None => error("unknown variable name")
                    }
                },
                &Binary(ref name, ref lhs, ref rhs) => {
                    let (lhs_value, _) = try!(lhs.codegen(context));
                    let (rhs_value, _) = try!(rhs.codegen(context));

                    match name.as_slice() {
                        "+" => Ok((llvm::LLVMBuildFAdd(context.builder,
                                                       lhs_value,
                                                       rhs_value,
                                                       "addtmp".to_c_str().as_ptr()),
                                   false)),
                        "-" => Ok((llvm::LLVMBuildFSub(context.builder,
                                                       lhs_value,
                                                       rhs_value,
                                                       "subtmp".to_c_str().as_ptr()),
                                   false)),
                        "*" => Ok((llvm::LLVMBuildFMul(context.builder,
                                                    lhs_value,
                                                    rhs_value,
                                                    "multmp".to_c_str().as_ptr()),
                                false)),
                        "<" => {
                            let cmp = llvm::LLVMBuildFCmp(context.builder,
                                                          llvm::RealOLT as c_uint,
                                                          lhs_value,
                                                          rhs_value,
                                                          "cmptmp".to_c_str().as_ptr());
                            let ty = llvm::LLVMDoubleTypeInContext(context.context);
                            Ok((llvm::LLVMBuildUIToFP(context.builder,
                                                      cmp,
                                                      ty,
                                                      "booltmp".to_c_str().as_ptr()),
                                false))
                        },
                        _ => error("invalid binary operator")
                    }
                },
                &Call(ref name, ref args) => {
                    let function = llvm::LLVMGetNamedFunction(context.module, name.to_c_str().as_ptr());
                    if function.is_null() {
                        return error("unknown function referenced")
                    }
                    if llvm::LLVMCountParams(function) as uint != args.len() {
                        return error("incorrect number of arguments passed")
                    }
                    let mut args_value = Vec::new();
                    for arg in args.iter() {
                        let (arg_value, _) = try!(arg.codegen(context));
                        args_value.push(arg_value);
                    }
                    Ok((llvm::LLVMBuildCall(context.builder,
                                            function,
                                            args_value.as_ptr(),
                                            args_value.len() as c_uint,
                                            "calltmp".to_c_str().as_ptr()),
                        false))
                },
                &Conditional{ref cond_expr, ref then_expr, ref else_expr} => {
                    let ty = llvm::LLVMDoubleTypeInContext(context.context);

                    let (cond_value, _) = try!(cond_expr.codegen(context));

                    let zero = llvm::LLVMConstReal(ty, 0.0);

                    let ifcond = llvm::LLVMBuildFCmp(context.builder,
                                                     llvm::RealONE as c_uint,
                                                     cond_value,
                                                     zero,
                                                     "ifcond".to_c_str().as_ptr());

                    let block = llvm::LLVMGetInsertBlock(context.builder);
                    let function = llvm::LLVMGetBasicBlockParent(block);
                    let then_block = llvm::LLVMAppendBasicBlockInContext(context.context, function, "then".to_c_str().as_ptr());
                    let else_block = llvm::LLVMAppendBasicBlockInContext(context.context, function, "else".to_c_str().as_ptr());
                    let merge_block = llvm::LLVMAppendBasicBlockInContext(context.context, function, "ifcont".to_c_str().as_ptr());

                    llvm::LLVMBuildCondBr(context.builder, ifcond, then_block, else_block);

                    llvm::LLVMPositionBuilderAtEnd(context.builder, then_block);
                    let (then_value, _) = try!(then_expr.codegen(context));

                    llvm:: LLVMBuildBr(context.builder, merge_block);
                    let then_end_block = llvm::LLVMGetInsertBlock(context.builder);

                    llvm::LLVMPositionBuilderAtEnd(context.builder, else_block);
                    let (else_value, _) = try!(else_expr.codegen(context));
                    llvm:: LLVMBuildBr(context.builder, merge_block);
                    let else_end_block = llvm::LLVMGetInsertBlock(context.builder);

                     llvm::LLVMPositionBuilderAtEnd(context.builder, merge_block);
                    let phi = llvm::LLVMBuildPhi(context.builder, ty, "ifphi".to_c_str().as_ptr());
                    llvm::LLVMAddIncoming(phi, &then_value, &then_end_block, 1);
                    llvm::LLVMAddIncoming(phi, &else_value, &else_end_block, 1);

                    Ok((phi, false))
                }
            }
        }
    }
}

impl IRBuilder for Prototype {
    fn codegen(&self, context: &mut Context) -> IRBuildingResult {
        unsafe {
            let prev_definition = llvm::LLVMGetNamedFunction(context.module, self.name.to_c_str().as_ptr());

            let function =
                if !prev_definition.is_null() {
                    if llvm::LLVMCountParams(prev_definition) as uint != self.args.len() {
                        return error("redefinition of function with different number of args")
                    }
                    if llvm::LLVMCountBasicBlocks(prev_definition) != 0 {
                        return error("redefinition of function");
                    }

                    prev_definition

                } else {
                    let ty = llvm::LLVMDoubleTypeInContext(context.context);
                    let param_types = Vec::from_elem(self.args.len(), ty);
                    let fty = llvm::LLVMFunctionType(ty, param_types.as_ptr(), param_types.len() as c_uint, false as c_uint);

                    llvm::LLVMAddFunction(context.module,
                                          self.name.to_c_str().as_ptr(),
                                          fty)
                };

            let mut param = llvm::LLVMGetFirstParam(function);
            for arg in self.args.iter() {
                llvm::LLVMSetValueName(param, arg.to_c_str().as_ptr());
                param = llvm::LLVMGetNextParam(param);
            }

            Ok((function, false))
        }
    }
}

impl IRBuilder for Function {
    fn codegen(&self, context: &mut Context) -> IRBuildingResult {
        context.named_values.clear();

        let (function, _) = try!(self.prototype.codegen(context));

        unsafe {
            let basic_block = llvm::LLVMAppendBasicBlockInContext(context.context,
                                                                  function,
                                                                  "entry".to_c_str().as_ptr());

            llvm::LLVMPositionBuilderAtEnd(context.builder, basic_block);

            let mut param = llvm::LLVMGetFirstParam(function);
            for arg in self.prototype.args.iter() {
                context.named_values.insert(arg.clone(), param);
                param = llvm::LLVMGetNextParam(param);
            }

            let body = match self.body.codegen(context) {
                Ok((value, _)) => value,
                Err(message) => {
                    llvm:: LLVMDeleteFunction(function);
                    return Err(message);
                }
            };

            llvm::LLVMBuildRet(context.builder, body);
            llvm::LLVMRunFunctionPassManager(context.function_passmanager, function);
        }

        context.named_values.clear();
        Ok((function, self.prototype.name.as_slice() == ""))
    }
}

impl IRBuilder for ASTNode {
    fn codegen(&self, context: &mut Context) -> IRBuildingResult {
        match self {
            &ExternNode(ref prototype) => prototype.codegen(context),
            &FunctionNode(ref function) => function.codegen(context)
        }
    }
}

impl IRBuilder for Vec<ASTNode> {
    fn codegen(&self, context: &mut Context) -> IRBuildingResult {
        let mut result = error("empty AST");
        for node in self.iter() {
            result = Ok(try!(node.codegen(context)));
        }

        result
    }
}

impl IRBuilder for ParsingResult {
    fn codegen(&self, context: &mut Context) -> IRBuildingResult {
        match self {
            &Ok((ref ast, _)) => ast.codegen(context),
            &Err(ref message) => Err(message.clone())
        }
    }
}