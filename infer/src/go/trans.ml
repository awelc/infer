open! IStd
open Go_ast_to_json_t
open Go_ast_to_json_j

let line_loc_mk_src source_file l = { Location.line = l; col = -1; file = source_file }

let line_loc_mk_proc proc_desc l = line_loc_mk_src (Procdesc.get_attributes proc_desc).translation_unit l

let get_binop_kind (typ : Typ.t option) =
  match typ with
    | Some (t) -> Typ.get_ikind_opt t
    | None -> raise (Failure "Requested typed binop does not specify type kind")

let get_binop ?typ (str: string) =
  match str with
    | "==" -> Binop.Eq, true
    | ">=" -> Binop.Ge, true
    | ">"  -> Binop.Gt, true
    | "<=" -> Binop.Le, true
    | "<"  -> Binop.Lt, true
    | "!=" -> Binop.Ne, true
    | "+"  -> Binop.PlusA(get_binop_kind typ), false
    | "-"  -> Binop.MinusA(get_binop_kind typ), false
    | "*"  -> Binop.Mult(get_binop_kind typ), false
    | "/" -> Binop.Div, false 

let typ_desc_to_kind = function
  | Typ.Tint (kind) ->
    match kind with 
      | Typ.IInt -> "INT"

let create_node (context: Context.t) instructions loc kind =
  context.node_count <- context.node_count + 1; Procdesc.create_node context.proc_desc loc kind instructions

let create_node_method (context: Context.t) instructions loc =
  create_node context instructions loc (Procdesc.Node.Stmt_node MethodBody)

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

and trans_lit context kind value =
  (match kind with
    | "INT" -> 
      [], Exp.int (IntLit.of_int (int_of_string value)), Typ.mk (Tint Typ.IInt) )



and trans_var (context: Context.t) name line =
    let (pvar, var_type) = Context.LocalsMap.find name context.locals_map in
    let id = Ident.create_fresh Ident.knormal in
    let load_instr = Sil.Load (id, Exp.Lvar
     pvar, var_type, line_loc_mk_proc context.proc_desc line) in
      pvar, [load_instr], Exp.Var id, var_type

and trans_exp (context : Context.t) (ex : expr_type) : (Sil.instr list * Exp.t * Typ.t) = 
    match ex with
      | `Ident (ident) ->       
        (match ident.id with 
          | "nil" -> [], Exp.null, Typ.mk (Typ.Tptr ((Typ.mk (Tint Typ.IInt)), Typ.Pk_pointer))
          | _ -> let _, instr, value, typ = trans_var context ident.id ident.ln in 
              instr, value, typ)
      | `StarExpr (expr) -> 
        let load_ptr_list, ptr_var, ptr_var_type = trans_exp context expr.x in
        let var_type = Typ.strip_ptr ptr_var_type in
        let id = Ident.create_fresh Ident.knormal in
        let load_instr = Sil.Load (id, ptr_var, var_type, line_loc_mk_proc context.proc_desc expr.ln) in
        load_ptr_list @ [load_instr], Exp.Var id, var_type
      | `UnaryExpr (expr) ->
        (match expr.op with
          | "&" -> 
            (match expr.x with
              | `Ident (ident) -> (* must be a variable *)
                let (pvar, var_type) = Context.LocalsMap.find ident.id context.locals_map in
                  [], Exp.Lvar pvar, Typ.mk (Typ.Tptr (var_type, Typ.Pk_pointer)) ) )
      | `BinaryExpr (expr) ->
        let binop, is_bool = get_binop expr.op in
        let l_inst_list, l_var, _ = trans_exp context expr.x in
        let r_inst_list, r_var, _ = trans_exp context expr.y in
          if (is_bool) then l_inst_list @ r_inst_list, Exp.BinOp (binop, l_var, r_var), Typ.mk (Tint Typ.IBool)
          else (raise (Failure "Should not happen"))

      | `BasicLit (lit) ->
        trans_lit context lit.kind lit.value
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
  | `BinaryExpr (expr) -> raise (Failure "Should not happen")
  | `BasicLit (list) -> raise (Failure "Should not happen")
  | `CallExpr (expr) -> raise (Failure "Should not happen")

and trans_body context body =
  List.fold ~f:(fun nodes stmt -> nodes @ (trans_stmt context stmt)) ~init:([]) body.stmts

and trans_decl_stmt context stmt =
  trans_decl context stmt.decl

and trans_return_stmt (context : Context.t) (stmt : return_stmt_type) = 
  if (List.length stmt.results > 1) then raise (Failure "Only one result value supported for now") else (
    let ex = List.nth_exn stmt.results 0 in
    let ret_var = Pvar.get_ret_pvar (Procdesc.get_proc_name context.proc_desc) in
    let ret_type = Procdesc.get_ret_type context.proc_desc in
    let ex_instr, ex_val, _ = trans_exp context ex in
    let loc = line_loc_mk_proc context.proc_desc stmt.ln in
    let return_instr = Sil.Store (Exp.Lvar ret_var, ret_type, ex_val, loc) in
    let ret_node_id = context.node_count in
      context.goto_branches <- Context.BranchesMap.add ret_node_id Context.Exit context.goto_branches;
      [create_node_method context (ex_instr @ [return_instr]) loc]
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
            let loc = line_loc_mk_proc context.proc_desc stmt.ln in
            let assign_instr = Sil.Store (Exp.Lvar pvar, ex_type, ex_val, loc) in
              [create_node_method context (ex_instr @ [assign_instr]) loc]

  )

(* used for translating both if and for statements *)
and trans_cond context cond body ?post line =
  let cond_instr, cond_val, _ = trans_exp context cond in
  let not_cond_val = Exp.UnOp (Unop.LNot, cond_val, None) in
  let loc = line_loc_mk_proc context.proc_desc line in
  let prune_instr_true = Sil.Prune (cond_val, loc, true, Sil.Ik_if) in
  let prune_instr_false = Sil.Prune (not_cond_val, loc, false, Sil.Ik_if) in
  let node_kind_true = Procdesc.Node.Prune_node (true, Sil.Ik_if, "method_body") in
  let node_kind_false = Procdesc.Node.Prune_node (false, Sil.Ik_if, "method_body") in
  let if_node_id = context.node_count - 1 in (* node preceding prune nudes *)
  let prune_node_true = create_node context (cond_instr @ [prune_instr_true]) loc node_kind_true in
  let main_body_nodes = trans_body context body in
  let post_node = 
    match post with
      | None -> []
      | Some (s) -> trans_stmt context s (* add post iteration statement for for loops if present *)
  in
  let last_body_node_id = context.node_count - 1 in
  let prune_node_false = create_node context (cond_instr @ [prune_instr_false]) loc node_kind_false in
  let prune_node_false_id = context.node_count - 1 in
    if_node_id, prune_node_false_id, last_body_node_id, prune_node_true :: main_body_nodes @ post_node @ [prune_node_false]


and trans_if_stmt (context : Context.t) (stmt: if_stmt_type) =
  let if_node_id, prune_node_false_id, last_body_node_id, body_nodes = 
    trans_cond context stmt.cond stmt.body stmt.ln
  in
  let link_last_branch_node last_cond_node_id =
    (* link the last node of a branch with if statement's continuation unless the this node represents a return *)
    match Context.BranchesMap.find_opt last_cond_node_id context.goto_branches with 
      | None -> context.goto_branches <- Context.BranchesMap.add last_cond_node_id (Context.Index context.node_count) context.goto_branches
      | Some (target) -> match target with | Context.Exit -> () | Context.Index (_) -> raise (Failure "Index jump instead of expected exit jump") in

    (* link previous node with the false prune node - connection with true prune node will be done by falling through *)
    context.if_branches <- Context.BranchesMap.add if_node_id (Context.Index prune_node_false_id) context.if_branches;
    (* link the last node of the true branch with if statement's continuation if necessary *)
    link_last_branch_node last_body_node_id;
    match stmt.el with
      | None ->
        (* return the list of nodes - all connections are already made *)
        body_nodes
      | Some (el) ->
        (* compute else nodes and link the last node of the false branch with if statement's continuation if necessary *)
        let else_nodes = trans_stmt context el in
        let last_else_node_id = context.node_count - 1 in      
        link_last_branch_node last_else_node_id;
        body_nodes @ else_nodes;


and trans_for_stmt context stmt =
  let init_nodes = trans_stmt context stmt.init in
  let join_node_kind = Procdesc.Node.Join_node in
  let loc = line_loc_mk_proc context.proc_desc stmt.ln in
  let join_node = create_node context [] loc join_node_kind in
  let if_node_id, prune_node_false_id, last_body_node_id, body_nodes = 
    trans_cond context stmt.cond stmt.body ~post:stmt.post stmt.ln
  in
    (* link previous node with the false prune node - connection with true prune node will be done by falling through *)
    context.if_branches <- Context.BranchesMap.add if_node_id (Context.Index prune_node_false_id) context.if_branches;
    context.goto_branches <- Context.BranchesMap.add last_body_node_id (Context.Index if_node_id) context.goto_branches;
    init_nodes @ [join_node] @ body_nodes



and trans_inc_dec_stmt (context: Context.t) stmt =
  (* translate into "regular" two-argument assignment similarly to other languges *)
  match stmt.x with
    | `Ident (ident) -> (* must be a variable *)
      let lhs_pvar, lhs_instr, lhs_val, lhs_type = trans_var context ident.id stmt.ln in
      let _, inc_val, _ = trans_lit context (typ_desc_to_kind lhs_type.desc) "1" in
      let loc = line_loc_mk_proc context.proc_desc stmt.ln in
      let binop_op = match stmt.op with | "++" -> "+" | "--" -> "-" in
      let binop, _= get_binop ~typ:lhs_type binop_op in
      let binop_expr = Exp.BinOp (binop, lhs_val, inc_val) in
      let assign_instr = Sil.Store (Exp.Lvar lhs_pvar, lhs_type, binop_expr, loc) in
        [create_node_method context (lhs_instr @ [assign_instr]) loc]


and trans_stmt context = function
  | `DeclStmt (stmt) -> trans_decl_stmt context stmt
  | `ReturnStmt (stmt) -> trans_return_stmt context stmt
  | `AssignStmt (stmt) -> trans_assign_stmt context stmt
  | `BlockStmt (stmt) -> trans_body context stmt
  | `IfStmt (stmt) -> trans_if_stmt context stmt
  | `ForStmt (stmt) -> trans_for_stmt context stmt
  | `IncDecStmt (stmt) -> trans_inc_dec_stmt context stmt

and trans_var_spec (context : Context.t) ln (spec : value_spec_type)   =
  if ((List.length spec.names > 1) || (List.length spec.values > 1)) then raise (Failure "Only single variable declaration supported for now") else (
    let name = (List.nth_exn spec.names 0).id in
    let var_name = Mangled.from_string name in
    let t = trans_type spec.t in
    let ex = List.nth_exn spec.values 0 in
    let pvar = Pvar.mk var_name (Procdesc.get_proc_name context.proc_desc) in
    let ex_instr, ex_val, ex_type = trans_exp context ex in
    let loc = line_loc_mk_proc context.proc_desc ln in
    let decl_instr = Sil.Store (Exp.Lvar pvar, ex_type, ex_val, loc) in
    let var : ProcAttributes.var_data = {name = var_name; typ = ex_type; modify_in_block = false; is_constexpr = false} in
      if (not (Typ.equal t ex_type)) then raise (Failure "Incorrect type assigned in declaration") else ();
      context.locals_map <- Context.LocalsMap.add name (pvar, ex_type) context.locals_map;
      context.locals_list <- var :: context.locals_list;
      [create_node_method context (ex_instr @ [decl_instr]) loc]
 )

and trans_spec context ln = function
  | `ValueSpec (spec) -> trans_var_spec context ln spec


and trans_gen_decl context decl =
  if (List.length decl.specs > 1) then raise (Failure "Only one declaration specification supported for now") else (
    match decl.tok with
      | "var" -> trans_spec context decl.ln (List.nth_exn decl.specs 0)
  )

and add_edges (context: Context.t) func_nodes_array exit_nodes =
  let proc_desc = context.proc_desc in
  let last_node_ind = Array.length func_nodes_array - 1 in
  let link_nodes node_ind node =
    if (node_ind < last_node_ind) then ( (* omit exit node *)
      let current_node = Array.get func_nodes_array node_ind in
      match Context.BranchesMap.find_opt node_ind context.goto_branches with
          (* goto nodes only link with the target node *)
        | Some (target) -> 
          let target_node_id = match target with | Context.Index (id) -> id | Context.Exit -> last_node_ind in
          let target_node = Array.get func_nodes_array target_node_id in
            Procdesc.node_set_succs_exn proc_desc current_node [target_node] exit_nodes
          (* other nodes link with the following node and, in case of if nodes, also with the target node*)
        | None ->
          let next_node = Array.get func_nodes_array (node_ind + 1) in
          match Context.BranchesMap.find_opt node_ind context.if_branches with
            | Some (target) ->
              let target_node_id = match target with | Context.Index (id) -> id | Context.Exit -> last_node_ind in
              let target_node = Array.get func_nodes_array target_node_id in
                Procdesc.node_set_succs_exn proc_desc current_node (next_node :: [target_node]) exit_nodes
            | None ->
              Procdesc.node_set_succs_exn proc_desc current_node [next_node] exit_nodes;


    )
  in
    Array.iteri ~f:link_nodes func_nodes_array

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
    let context = Context.create_context proc_desc go_cfg in
    let param_to_local = function
      | (n, t) -> 
        let pvar = Pvar.mk n func_name in
        let var : ProcAttributes.var_data = {name = n; typ = t; modify_in_block = false; is_constexpr = false} in
          context.locals_map <- Context.LocalsMap.add (Mangled.to_string n) (pvar, t) context.locals_map
    in
    let start_node = create_node context [] loc_start Procdesc.Node.Start_node in
    let func_nodes = List.iter params param_to_local; trans_body context body in
    let exit_node = create_node context [] loc_exit Procdesc.Node.Exit_node in
    let exit_nodes = [exit_node] in    
      go_cfg.func_decls <- Context.FuncDeclsMap.add decl.uid proc_desc go_cfg.func_decls;
      Procdesc.set_start_node proc_desc start_node;
      Procdesc.set_exit_node proc_desc exit_node;
      add_edges context (Array.of_list ((start_node :: func_nodes) @ exit_nodes)) exit_nodes;
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
