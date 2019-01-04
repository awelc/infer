open! IStd
open Go_ast_to_json_t
open Go_ast_to_json_j

let line_loc_mk_src source_file l = { Location.line = l; col = -1; file = source_file }

let line_loc_mk_proc proc_desc l = line_loc_mk_src (Procdesc.get_attributes proc_desc).translation_unit l

let create_node proc_desc instructions line = 
  Procdesc.create_node proc_desc (line_loc_mk_proc proc_desc line) (Procdesc.Node.Stmt_node MethodBody) instructions

let rec create_fn_desc (go_cfg : Context.gocfg) = function
  | `FuncDecl (decl) -> trans_func_decl go_cfg decl
  | `FuncDeclRef (ref) -> if (not(Context.FuncDeclsMap.mem ref go_cfg.func_decls)) then raise (Failure "Function definition not found")
  | `GenDecl (decl) -> raise (Failure "Should not happen")

and trans_res_type = function
  | `Field (field) -> trans_type field.t

and trans_param = function
  | `Field (field) ->
    if (List.length field.names > 1) then raise (Failure "Function parameter can have only one name") else (
      (Mangled.from_string (List.nth_exn field.names 0).id,  trans_type field.t)
    )

and trans_exp (context : Context.t) (ex : expr_type) : (Sil.instr list * Exp.t * Typ.t) = 
  let trans_var name line =
    let (pvar, var_type) = Context.LocalsMap.find name context.locals_map in
    let id = Ident.create_fresh Ident.knormal in
    let load_instr = Sil.Load (id, Exp.Lvar pvar, var_type, line_loc_mk_proc context.proc_desc line) in
      [load_instr], Exp.Var id, var_type
  in
    match ex with
      | `Ident (ident) ->       
        (match ident.id with 
          | "nil" -> [], Exp.null, Typ.mk (Typ.Tptr ((Typ.mk (Tint Typ.IInt)), Typ.Pk_pointer))
          | _ -> trans_var ident.id ident.ln)
      | `StarExpr (expr) -> 
        let load_ptr_list, ptr_var, ptr_var_type = trans_exp context expr.x in
        let var_type = Typ.strip_ptr ptr_var_type in
        let id = Ident.create_fresh Ident.knormal in
        let load_instr = Sil.Load (id, ptr_var, var_type, line_loc_mk_proc context.proc_desc expr.ln) in
        load_ptr_list @ [load_instr], Exp.Var id, var_type
      | `UnaryExpr (expr) ->
        (match expr.tok with
          | "&" -> 
            (match expr.x with
              | `Ident (ident) -> (* must be a variable *)
                let (pvar, var_type) = Context.LocalsMap.find ident.id context.locals_map in
                  [], Exp.Lvar pvar, Typ.mk (Typ.Tptr (var_type, Typ.Pk_pointer)) ) )
      | `BasicLit (lit) ->
        (match lit.kind with
          | "INT" -> 
            [], Exp.int (IntLit.of_int (int_of_string lit.value)), Typ.mk (Tint Typ.IInt) )
      | `CallExpr (expr) ->
        let call_proc_desc = 
          (match expr.fn with
            | `Ident (ident) -> 
                match ident.obj with
                  | Some (o) ->
                    let func_uid = 
                      (match o with
                        | `FuncDecl (decl) -> trans_func_decl context.go_cfg decl; decl.uid
                        | `FuncDeclRef (ref) -> ref)
                    in 
                      Context.FuncDeclsMap.find func_uid context.go_cfg.func_decls)
        in
        let instrs, call_args =
          List.fold
            ~f:(fun (instrs_accu, args_accu) expr ->
              let instrs, ex, ex_type = trans_exp context expr in
              (instrs_accu @ instrs, args_accu @ [(ex, ex_type)]) )
            ~init:([], []) expr.args
        in
        let func_name = Exp.Const (Const.Cfun (Procdesc.get_proc_name call_proc_desc)) in
        let ret_type = Procdesc.get_ret_type call_proc_desc in
        let id = Ident.create_fresh Ident.knormal in
        let loc = line_loc_mk_proc context.proc_desc expr.lparen.ln in
        let call_instr = Sil.Call((id, ret_type), func_name, call_args, loc, CallFlags.default) in
          instrs @ [call_instr], Exp.Var id, ret_type


and trans_type = function
  | `Ident (ident) -> (
    match ident.id with 
      | "int" -> Typ.mk (Tint Typ.IInt)
      | "bool" -> Typ.mk (Tint Typ.IBool))
  | `StarExpr (expr) -> 
      Typ.mk (Typ.Tptr (trans_type expr.x, Typ.Pk_pointer))
  | `UnaryExpr (expr) -> raise (Failure "Should not happen")
  | `BasicLit (list) -> raise (Failure "Should not happen")
  | `CallExpr (expr) -> raise (Failure "Should not happen")

and trans_body context body =
  List.map ~f:(fun (stmt) -> trans_stmt context stmt) body.stmts

and trans_decl_stmt context stmt =
  trans_decl context stmt.decl

and trans_return_stmt (context : Context.t) (stmt : return_stmt_type) = 
  if (List.length stmt.results > 1) then raise (Failure "Only one result value supported for now") else (
    let ex = List.nth_exn stmt.results 0 in
    let ret_var = Pvar.get_ret_pvar (Procdesc.get_proc_name context.proc_desc) in
    let ret_type = Procdesc.get_ret_type context.proc_desc in
    let ex_instr, ex_val, _ = trans_exp context ex in
    let return_instr = Sil.Store (Exp.Lvar ret_var, ret_type, ex_val, line_loc_mk_proc context.proc_desc stmt.ln) in
      create_node context.proc_desc (ex_instr @ [return_instr]) stmt.ln
  )

and trans_assign_stmt (context : Context.t) stmt =
  if ((List.length stmt.lhs > 1) || (List.length stmt.rhs > 1)) then raise (Failure "Only single value assignment supported for now") else (
    let lhs = List.nth_exn stmt.lhs 0 in
    let rhs = List.nth_exn stmt.rhs 0 in
      match lhs with
        | `Ident (ident) -> (* must be a variable *)
          let name = ident.id in
          let ex_instr, ex_val, ex_type = trans_exp context rhs in
          let var_lookup = 
            try
              let (pvar, var_type) = Context.LocalsMap.find name context.locals_map in
                if (not (Typ.equal var_type ex_type)) then raise (Failure "Incorrect type assigned") else ();
                pvar
            with Not_found -> 
              let pvar = Pvar.mk (Mangled.from_string name) (Procdesc.get_proc_name context.proc_desc) in
                context.locals_map <-  Context.LocalsMap.add name (pvar, ex_type) context.locals_map;
                pvar               
          in
            let pvar = var_lookup in
            let assign_instr = Sil.Store (Exp.Lvar pvar, ex_type, ex_val, line_loc_mk_proc context.proc_desc stmt.ln) in
              create_node context.proc_desc (ex_instr @ [assign_instr]) stmt.ln

  )

and trans_stmt context = function
  | `DeclStmt (stmt) -> trans_decl_stmt context stmt
  | `ReturnStmt (stmt) -> trans_return_stmt context stmt
  | `AssignStmt (stmt) -> trans_assign_stmt context stmt

and trans_var_spec (context : Context.t) ln (spec : value_spec_type)   =
  if ((List.length spec.names > 1) || (List.length spec.values > 1)) then raise (Failure "Only single variable declaration supported for now") else (
    let name = (List.nth_exn spec.names 0).id in
    let var_name = Mangled.from_string name in
    let t = trans_type spec.t in
    let ex = List.nth_exn spec.values 0 in
    let pvar = Pvar.mk var_name (Procdesc.get_proc_name context.proc_desc) in
    let ex_instr, ex_val, ex_type = trans_exp context ex in
    let decl_instr = Sil.Store (Exp.Lvar pvar, ex_type, ex_val, line_loc_mk_proc context.proc_desc ln) in
    let var : ProcAttributes.var_data = {name = var_name; typ = ex_type; modify_in_block = false; is_constexpr = false} in
      if (not (Typ.equal t ex_type)) then raise (Failure "Incorrect type assigned in declaration") else ();
      context.locals_map <- Context.LocalsMap.add name (pvar, ex_type) context.locals_map;
      context.locals_list <- var :: context.locals_list;
      create_node context.proc_desc (ex_instr @ [decl_instr]) ln
 )

and trans_spec context ln = function
  | `ValueSpec (spec) -> trans_var_spec context ln spec


and trans_gen_decl context decl =
  if (List.length decl.specs > 1) then raise (Failure "Only one declaration specification supported for now") else (
    match decl.tok with
      | "var" -> trans_spec context decl.ln (List.nth_exn decl.specs 0)
  )

and trans_func_decl (go_cfg : Context.gocfg) decl =
  let func_type = decl.func_type in
  if (List.length func_type.results > 1) then raise (Failure "Only one result value supported for now") else (
    let source_file = go_cfg.src_file in
    let body = decl.body in
    let func_name = Typ.Procname.Go (Typ.Procname.Go.make decl.name.id) in
    let loc_start = line_loc_mk_src source_file func_type.ln in
    let loc_exit = line_loc_mk_src source_file body.rbrace.ln in
    let params = List.map ~f:trans_param func_type.params in
    let proc_attributes = { (ProcAttributes.default source_file func_name) with
        formals = params;
        is_defined = true;
        loc = loc_start;
        ret_type = trans_res_type (List.nth_exn func_type.results 0) } in
    let proc_desc = Cfg.create_proc_desc go_cfg.cfg proc_attributes in
    let start_node = Procdesc.create_node proc_desc loc_start Procdesc.Node.Start_node [] in
    let exit_node = Procdesc.create_node proc_desc loc_exit Procdesc.Node.Exit_node [] in
    let exit_nodes = [exit_node] in    
    let context = Context.create_context proc_desc go_cfg in
    let param_to_local = function
      | (n, t) -> 
        let pvar = Pvar.mk n func_name in
        let var : ProcAttributes.var_data = {name = n; typ = t; modify_in_block = false; is_constexpr = false} in
          context.locals_map <- Context.LocalsMap.add (Mangled.to_string n) (pvar, t) context.locals_map
    in
    let rec link_nodes pred_node = function
        | [] -> Procdesc.node_set_succs_exn proc_desc pred_node exit_nodes exit_nodes (* pred_node is last node - needs to be connected to the sink *)
        | next_node :: tail_nodes -> (Procdesc.node_set_succs_exn proc_desc pred_node [next_node] exit_nodes; link_nodes next_node tail_nodes ) in
          if (not(context.go_cfg == go_cfg)) then raise (Failure "CFG's not equal");
          go_cfg.func_decls <- Context.FuncDeclsMap.add decl.uid proc_desc go_cfg.func_decls;
          List.iter params param_to_local;
          Procdesc.set_start_node proc_desc start_node;
          Procdesc.set_exit_node proc_desc exit_node;
          Procdesc.node_set_succs_exn proc_desc start_node exit_nodes exit_nodes;
          link_nodes start_node (trans_body context body);
          Procdesc.append_locals proc_desc (List.rev context.locals_list);
          ()
  )

and trans_decl context = function
  | `FuncDecl (decl) -> raise (Failure "Should not happen")
  | `FuncDeclRef (ref) -> raise (Failure "Should not happen")
  | `GenDecl (decl) ->  trans_gen_decl context decl


let go_to_json = Config.lib_dir ^ "/go/src_to_ast_json.go"

let compute_icfg source_file =
  let stdout = Unix.open_process_in ("go run " ^ go_to_json ^ " -- " ^ (SourceFile.to_string source_file)) in
  let go_file = Atdgen_runtime.Util.Json.from_channel read_file_type stdout in
  let _ = Unix.close_process_in stdout in
(*    print_endline (Pretty.pretty_file go_file); *)
    let go_cfg = Context.create_cfg source_file in
      List.iter ~f:(fun (decl) -> create_fn_desc go_cfg decl) go_file.decls;
      go_cfg.cfg
