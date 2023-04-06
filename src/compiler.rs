use crate::{scope::Scope, translator::Translator};

use cranelift::{
    codegen::verify_function,
    prelude::{settings::FlagsOrIsa, *},
};
use cranelift_module::{/*DataContext, */ Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule, ObjectProduct};

use crate::parser::{self, Expr};

pub struct Compiler {
    builder_context: FunctionBuilderContext,
    ctx: codegen::Context,
    //data_ctx: DataContext,
    module: ObjectModule,
    scope: Scope,
}

impl Default for Compiler {
    fn default() -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();

        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("Host machine is not supported: {msg}");
        });
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();

        let builder =
            ObjectBuilder::new(isa, "milang", cranelift_module::default_libcall_names()).unwrap();

        let module = ObjectModule::new(builder);

        let builder_context = FunctionBuilderContext::new();
        let ctx = module.make_context();
        //let data_ctx = DataContext::new();
        let scope = Scope::create_root();

        Self {
            builder_context,
            ctx,
            //data_ctx,
            module,
            scope,
        }
    }
}

impl Compiler {
    pub fn compile(mut self, input: &str) -> Result<ObjectProduct, String> {
        let (name, params, return_type, stmts) =
            parser::parser::function(input).map_err(|e| e.to_string())?;
        self.translate_function(&name, params, return_type, stmts)?;

        println!("{}", self.ctx.func.display());

        self.module.clear_context(&mut self.ctx);

        let object = self.module.finish();
        Ok(object)
    }

    pub fn translate_function(
        &mut self,
        name: &str,
        params: Vec<(String, String)>,
        return_type: String,
        stmts: Vec<Expr>,
    ) -> Result<(), String> {
        use crate::ltype::LType;

        for (_name, type_name) in &params {
            let ptype =
                LType::parse_basic(type_name).expect("Failed to parse type: '{type_name}'!");
            let ptype = ptype.to_type();

            self.ctx.func.signature.params.push(AbiParam::new(ptype));
        }

        let ret_type =
            LType::parse_basic(&return_type).expect("Failed to parse type: '{type_name}'!");
        let ret_type = ret_type.to_type();
        self.ctx
            .func
            .signature
            .returns
            .push(AbiParam::new(ret_type));

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);

        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let mut trans = Translator {
            builder,
            module: &mut self.module,
            scope: &mut self.scope,
            variable_index: 0,
        };

        for expr in stmts {
            trans.translate_expr(expr);
        }

        /*
        let return_variable = trans
            .scope
            .get_value_from_scope(&return_type.0)
            .unwrap()
            .get_cl_int_var()
            .unwrap();
        let return_value = trans.builder.use_var(return_variable);

        trans.builder.ins().return_(&[return_value]);
        */
        trans.builder.finalize();

        let id = self
            .module
            .declare_function(name, Linkage::Export, &self.ctx.func.signature)
            .map_err(|e| e.to_string())?;

        let flags = settings::Flags::new(settings::builder());
        let flags_or_isa = FlagsOrIsa {
            flags: &flags,
            isa: None,
        };

        verify_function(&self.ctx.func, flags_or_isa).map_err(|e| e.to_string())?;

        self.module
            .define_function(id, &mut self.ctx)
            .map_err(|e| e.to_string())?;

        Ok(())
    }
}
