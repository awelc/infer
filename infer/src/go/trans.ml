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

let create_node proc_desc instructions loc kind =
  Procdesc.create_node proc_desc loc kind instructions

let create_node_method proc_desc instructions loc =
  create_node proc_desc instructions loc (Procdesc.Node.Stmt_node MethodBody)

let rec create_fn_desc (go_cfg : Context.gocfg) = function
  | `FuncDecl (decl) -> ignore (trans_func_decl go_cfg decl)
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

and get_var_key ident =
  let n = ident.id in
  match ident.obj with
    | None -> raise (Failure "Variable reference must point to a definition")
    | Some (obj) -> (
      match obj with 
        | `Field (o) -> Context.VarKey.mk n o.uid
        | `FieldRef (ref) -> Context.VarKey.mk n ref
        | `ValueSpec (o) -> Context.VarKey.mk n o.uid
        | `ValueSpecRef (ref) -> Context.VarKey.mk n ref
        | `AssignStmt (o) -> Context.VarKey.mk n o.uid
        | `AssignStmtRef (ref) -> Context.VarKey.mk n ref    
    )

and trans_var (context: Context.t) ident =
    let (pvar, var_type) = Context.LocalsMap.find (get_var_key ident) context.locals_map in
    let id = Ident.create_fresh Ident.knormal in
    let load_instr = 
      Sil.Load (id, Exp.Lvar pvar, var_type, line_loc_mk_proc context.proc_desc ident.ln)
    in
      pvar, [load_instr], Exp.Var id, var_type

and trans_exp (context : Context.t) (ex : expr_type) : (Sil.instr list * Exp.t * Typ.t) = 
    match ex with
      | `Ident (ident) ->       
        (match ident.id with 
          | "nil" -> [], Exp.null, Typ.mk (Typ.Tptr ((Typ.mk (Tint Typ.IInt)), Typ.Pk_pointer))
          | _ -> let _, instr, value, typ = trans_var context ident in 
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
                let (pvar, var_type) = Context.LocalsMap.find (get_var_key ident) context.locals_map in
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
        let call_proc_desc = (
          match expr.fn with
            | `Ident (ident) -> 
                match ident.obj with
                  | None -> raise (Failure "Call expression must point to a function declaration")
                  | Some (o) -> (
                      match o with
                        | `FuncDecl (decl) -> trans_func_decl context.go_cfg decl
                        | `FuncDeclRef (ref) -> Context.FuncDeclsMap.find ref context.go_cfg.func_decls
                    )
        )
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

(* connect previous nodes with the next node, unless  this next node is a break/continue node,
   in which case record appropriate meta date to support break/continue;
    return true if connecting with the next node, false otherwise *)
and connect_with_next_node (context: Context.t) nodes_to_next next_node =
  let break_node_name = Typ.Procname.Go (Typ.Procname.Go.make "42_break") in
  let cont_node_name = Typ.Procname.Go (Typ.Procname.Go.make "42_continue") in
  if (Typ.Procname.equal (Procdesc.Node.get_proc_name next_node) break_node_name) then (
      (* if freshly evaluated statement is "break", skip the node and redirect previous nodes to the end of the loop
          by adding the previous nodes to break_nodes list at appropriate level *)
      let current_break_nodes = Context.NestMap.find next_node context.break_jump_targets in
      List.iter ~f:(fun (n) -> Stack.push current_break_nodes n) nodes_to_next; false
  ) else if (Typ.Procname.equal (Procdesc.Node.get_proc_name next_node) cont_node_name) then (
      (* same as above but for "continue" *)
      let current_cont_nodes = Context.NestMap.find next_node context.cont_jump_targets in
      List.iter ~f:(fun (n) -> Stack.push current_cont_nodes n) nodes_to_next; false      
  ) else (
      (* connect all previous nodes with the first node of the freshly evaluated statement *)          
      List.iter ~f:(fun (n) -> Procdesc.node_set_succs_exn context.proc_desc n [next_node] [context.exit_node]) nodes_to_next; true
  )


and trans_body (context: Context.t) body : (Procdesc.Node.t * Procdesc.Node.t * Procdesc.Node.t list) =
  (* translate first statement *)
  let first_node, last_node, nodes_to_next = trans_stmt context (List.hd_exn body.stmts) in
  let rec trans_body_rec prev_last_node prev_nodes_to_next stmts = (
    match stmts with
      | [] -> first_node, prev_last_node, prev_nodes_to_next
      | stmt :: tail -> 
        let new_first_node, new_last_node, new_nodes_to_next = trans_stmt context stmt in
          if (connect_with_next_node context prev_nodes_to_next new_first_node) then (
              (* new_first_node is a "regular" node *)
              trans_body_rec new_last_node new_nodes_to_next tail
          ) else (
              (* new_first_node is a break/contunue node *)
              trans_body_rec new_last_node [] tail
          )
    )
  in
    (* recursively translate all other statements linkig their nodes together *)
    trans_body_rec last_node nodes_to_next (List.tl_exn body.stmts)

and trans_decl_stmt context stmt =
  trans_decl context stmt.decl

and trans_return_stmt (context : Context.t) (stmt : return_stmt_type) = 
  if (List.length stmt.results > 1) then raise (Failure "Only one result value supported for now") else (
    let proc_desc = context.proc_desc in
    let ex = List.nth_exn stmt.results 0 in
    let ret_var = Pvar.get_ret_pvar (Procdesc.get_proc_name proc_desc) in
    let ret_type = Procdesc.get_ret_type proc_desc in
    let ex_instr, ex_val, _ = trans_exp context ex in
    let loc = line_loc_mk_proc proc_desc stmt.ln in
    let return_instr = Sil.Store (Exp.Lvar ret_var, ret_type, ex_val, loc) in
    let n = create_node proc_desc (ex_instr @ [return_instr]) loc (Procdesc.Node.Stmt_node ReturnStmt) in
      Procdesc.node_set_succs_exn proc_desc n [context.exit_node] [context.exit_node];
      n, n, []
  )

and trans_assign_stmt (context : Context.t) stmt =
  if ((List.length stmt.lhs > 1) || (List.length stmt.rhs > 1)) then raise (Failure "Only single value assignment supported for now") else (
    let proc_desc = context.proc_desc in
    let lhs = List.nth_exn stmt.lhs 0 in
    let rhs = List.nth_exn stmt.rhs 0 in
      match lhs with
        | `Ident (ident) -> (* must be a variable *)
          let ex_instr, ex_val, ex_type = trans_exp context rhs in
          let var_key = get_var_key ident in
          let var_lookup = 
            try
              let (pvar, var_type) = Context.LocalsMap.find var_key context.locals_map in
                if (not (Typ.equal var_type ex_type)) then raise (Failure "Incorrect type assigned") else ();
                pvar
            with Not_found ->
              let var_name = Context.VarKey.to_mangled var_key in
              let pvar = Pvar.mk var_name (Procdesc.get_proc_name proc_desc) in
              let var : ProcAttributes.var_data = {name = var_name; typ = ex_type; modify_in_block = false; is_constexpr = false} in
                context.locals_map <-  Context.LocalsMap.add var_key (pvar, ex_type) context.locals_map;
                context.locals_list <- var :: context.locals_list;
                pvar               
          in
            let pvar = var_lookup in
            let loc = line_loc_mk_proc proc_desc stmt.ln in
            let assign_instr = Sil.Store (Exp.Lvar pvar, ex_type, ex_val, loc) in
            let n = create_node_method proc_desc (ex_instr @ [assign_instr]) loc in
              n, n, [n]
  )

(* used for translating both if and for statements *)
and trans_cond context cond body line =
  let cond_instr, cond_val, _ = trans_exp context cond in
  let proc_desc = context.proc_desc in
  let loc = line_loc_mk_proc proc_desc line in
  let split_node = create_node_method proc_desc cond_instr loc in (* true/false branch "split" node *)
  let not_cond_val = Exp.UnOp (Unop.LNot, cond_val, None) in
  let prune_instr_true = Sil.Prune (cond_val, loc, true, Sil.Ik_if) in
  let prune_instr_false = Sil.Prune (not_cond_val, loc, false, Sil.Ik_if) in
  let node_kind_true = Procdesc.Node.Prune_node (true, Sil.Ik_if, "method_body") in
  let node_kind_false = Procdesc.Node.Prune_node (false, Sil.Ik_if, "method_body") in
  let prune_node_true = create_node proc_desc [prune_instr_true] loc node_kind_true in
  let prune_node_false = create_node proc_desc [prune_instr_false] loc node_kind_false in
  (* create nodes in the true_branch *)
  let true_first_node, true_last_node, true_nodes_to_next = trans_body context body in 
    (* connect prune node of the true branch with the first node of this branch *)
    ignore (connect_with_next_node context [prune_node_true] true_first_node); 
    (* link split node with the false and true prune nodes - we know all nodes so regular node_set_succs_exn can be used *)
    Procdesc.node_set_succs_exn proc_desc split_node [prune_node_true; prune_node_false] [context.exit_node];
    split_node, prune_node_false, true_last_node, true_nodes_to_next


and trans_if_stmt (context : Context.t) (stmt: if_stmt_type) =
  let split_node, prune_node_false, _, true_nodes_to_next = 
    trans_cond context stmt.cond stmt.body stmt.ln
  in
  let last_node = Procdesc.Node.dummy (Procdesc.get_proc_name context.proc_desc) in (* if statement does not really has a single last node *)
    match stmt.el with
      | None ->
        split_node, last_node, prune_node_false :: true_nodes_to_next 
      | Some (el) ->
        (* compute else nodes and link the last node of the false branch with if statement's continuation if necessary *)
        let false_first_node, false_last_node, false_nodes_to_next = trans_stmt context el in
          (* connect prune node of the false branch with the first node of this branch *)
          ignore (connect_with_next_node context [prune_node_false] false_first_node);
          split_node, last_node, false_nodes_to_next @ true_nodes_to_next


and trans_for_stmt (context: Context.t) stmt =
  let proc_desc = context.proc_desc in
  let last_node = Procdesc.Node.dummy (Procdesc.get_proc_name proc_desc) in (* for statement does not really has a single last node *)
  let init_first_node, _, init_nodes_to_next = trans_stmt context stmt.init in
  let loc = line_loc_mk_proc context.proc_desc stmt.ln in
  let split_node, prune_node_false, _, true_nodes_to_next =
    (* break_nodes and cont_nodes should only reflect nodes in the body of the current loop *)
    context.break_nodes <- Stack.create () :: context.break_nodes;
    context.cont_nodes <- Stack.create () :: context.cont_nodes;
    context.jump_nest <- context.jump_nest + 1;
    trans_cond context stmt.cond stmt.body stmt.ln
  in
  let post_first_node, post_last_node, post_nodes_to_next = trans_stmt context stmt.post in
  let dummy_node = Procdesc.Node.dummy (Procdesc.get_proc_name proc_desc) in
    (* connect lat node of the for loop's body with the for loop'd post statement *)
    if (Procdesc.Node.equal post_last_node dummy_node) then (
      (* this exception would fire if the last node belongs to if statement, for statement, or break statement,
         all of which should be illegal  *)
      raise (Failure "Converted post statement of the for loop should have a single last valid node")
    ) else (
      ignore (connect_with_next_node context true_nodes_to_next post_first_node);
    );
    (* connect nodes of the for init statement to the node starting the conditional *)
    ignore (connect_with_next_node context init_nodes_to_next split_node);
    (* connect the last node of the loop with the node starting the conditional *)
    ignore (connect_with_next_node context [post_last_node] split_node);
    (* connect continue nodes to the node starting the first node of the for loop's post statement *)
    ignore (connect_with_next_node context (Stack.fold ~f:(fun accu n -> n :: accu) ~init:[] (List.hd_exn context.cont_nodes)) post_first_node);
    (* add all break nodes to a list of nodes to be connected with a statement following the for loop *)
    let new_nodes_to_next = [prune_node_false] @ (Stack.fold ~f:(fun accu n -> n :: accu) ~init:[] (List.hd_exn context.break_nodes)) in
      context.jump_nest <- context.jump_nest - 1;
      context.break_nodes <- List.tl_exn context.break_nodes;
      context.cont_nodes <- List.tl_exn context.cont_nodes;
      init_first_node, last_node, new_nodes_to_next


and trans_inc_dec_stmt (context: Context.t) stmt =
  (* translate into "regular" two-argument assignment similarly to other languges *)
  match stmt.x with
    | `Ident (ident) -> (* must be a variable *)
      let proc_desc = context.proc_desc in
      let lhs_pvar, lhs_instr, lhs_val, lhs_type = trans_var context ident in
      let _, inc_val, _ = trans_lit context (typ_desc_to_kind lhs_type.desc) "1" in
      let loc = line_loc_mk_proc proc_desc stmt.ln in
      let binop_op = match stmt.op with | "++" -> "+" | "--" -> "-" in
      let binop, _= get_binop ~typ:lhs_type binop_op in
      let binop_expr = Exp.BinOp (binop, lhs_val, inc_val) in
      let assign_instr = Sil.Store (Exp.Lvar lhs_pvar, lhs_type, binop_expr, loc) in
      let n = create_node_method proc_desc (lhs_instr @ [assign_instr]) loc in
        n, n, [n]

and trans_branch_stmt (context: Context.t) (stmt: branch_stmt_type) =
  let proc_desc = context.proc_desc in
  let loc = line_loc_mk_proc context.proc_desc stmt.ln in
  let dummy_node = Procdesc.Node.dummy (Procdesc.get_proc_name proc_desc) in
  let jump_node keyword = (
    (* this node is only used as a marker for appropriate node linking;
       using dummy node with a fake "illegal" function name
       is a workaround to avoid this node being inserted into CFG *)
    let node_name = 
      (match keyword with
        | "break" -> 
          Typ.Procname.Go (Typ.Procname.Go.make "42_break")
        | "continue" ->
          Typ.Procname.Go (Typ.Procname.Go.make "42_continue"))
      in
        Procdesc.Node.dummy node_name
    ) in
    match stmt.label with
      | None ->
        let j_node = jump_node stmt.keyword in
          (match stmt.keyword with
            | "break" -> 
              context.break_jump_targets <- Context.NestMap.add j_node (List.hd_exn context.break_nodes) context.break_jump_targets
            | "continue" ->
              context.cont_jump_targets <- Context.NestMap.add j_node (List.hd_exn context.cont_nodes) context.cont_jump_targets);
          j_node, dummy_node, [] 
      | Some (l) -> (
        let label_node =
          match l.obj with
            | None -> raise (Failure "Branch label must point to a statement")
            | Some (o) -> (
              match o with
                | `LabeledStmt (s) -> 
                  let f, l, n = trans_labeled_stmt context s in f
                | `LabeledStmtRef (ref) -> (
                  match Context.LabeledStmtsMap.find_opt ref context.labeled_stmts with 
                    | None -> Context.LabelNodesMap.find ref context.label_nodes
                    | Some (f, l, n) -> f
                )

            )
        in
          let compute_jumps jump_nodes = (
            let j_node = jump_node stmt.keyword in
            let label_nest = Context.NestMap.find label_node context.label_jump_nests in
            let current_jump_nodes = List.nth_exn jump_nodes ((List.length jump_nodes - 1) - label_nest) in
              j_node, current_jump_nodes
          ) in
            match stmt.keyword with
              | "goto" -> 
                (* have previous nodes link with the first node of the labeled statement *)
                label_node, dummy_node, []
              | "break" ->
                (* figure out what is the the nesting level this break node breaks into *)
                let j_node, current_jump_nodes = compute_jumps context.break_nodes in
                  context.break_jump_targets <- Context.NestMap.add j_node current_jump_nodes context.break_jump_targets;
                  j_node, dummy_node, []
              | "continue" -> 
                (* figure out what is the the nesting level this continue node continues to *)
                let j_node, current_jump_nodes = compute_jumps context.cont_nodes in
                  context.cont_jump_targets <- Context.NestMap.add j_node current_jump_nodes context.cont_jump_targets;
                  j_node, dummy_node, []
      )


and trans_labeled_stmt (context: Context.t) (stmt: labeled_stmt_type) =
  let loc = line_loc_mk_proc context.proc_desc stmt.ln in
  let proc_desc = context.proc_desc in
  let skip_node = create_node proc_desc [] loc (Procdesc.Node.Skip_node ("jump: " ^ stmt.label.id)) in
    context.label_jump_nests <- Context.NestMap.add skip_node context.jump_nest context.label_jump_nests;
    context.label_nodes <- Context.LabelNodesMap.add stmt.uid skip_node context.label_nodes;
    let first_node, last_node, nodes_to_next = trans_stmt context stmt.stmt in
      (* connect skip node identifying the label with actual statement node *)
      Procdesc.node_set_succs_exn proc_desc skip_node [first_node] [context.exit_node];
      context.labeled_stmts <- Context.LabeledStmtsMap.add stmt.uid (skip_node, last_node, nodes_to_next) context.labeled_stmts;    
      skip_node, last_node, nodes_to_next

and trans_labeled_stmt_ref (context: Context.t) ref =
  Context.LabeledStmtsMap.find ref context.labeled_stmts

and trans_empty_stmt (context: Context.t) stmt =
  let proc_desc = context.proc_desc in
  let loc = line_loc_mk_proc proc_desc stmt.ln in
  let n = create_node proc_desc [] loc (Procdesc.Node.Skip_node "empty stmt") in
    n, n, [n]


and trans_stmt context = function
  | `DeclStmt (stmt) -> trans_decl_stmt context stmt
  | `ReturnStmt (stmt) -> trans_return_stmt context stmt
  | `AssignStmt (stmt) -> trans_assign_stmt context stmt
  | `BlockStmt (stmt) -> trans_body context stmt
  | `IfStmt (stmt) -> trans_if_stmt context stmt
  | `ForStmt (stmt) -> trans_for_stmt context stmt
  | `IncDecStmt (stmt) -> trans_inc_dec_stmt context stmt
  | `BranchStmt (stmt) -> trans_branch_stmt context stmt
  | `LabeledStmt (stmt) -> trans_labeled_stmt context stmt
  | `LabeledStmtRef (ref) -> trans_labeled_stmt_ref context ref
  | `EmptyStmt (stmt) -> trans_empty_stmt context stmt

and trans_var_spec (context : Context.t) ln (spec : value_spec_type)   =
  if ((List.length spec.names > 1) || (List.length spec.values > 1)) then raise (Failure "Only single variable declaration supported for now") else (
    let proc_desc = context.proc_desc in
    let ident = (List.nth_exn spec.names 0) in
    let var_key = get_var_key ident in
    let var_name = Context.VarKey.to_mangled var_key in
    let t = trans_type spec.t in
    (* TODO GO: handle the case when no value to assign exists *)
    let ex = List.nth_exn spec.values 0 in
    let pvar = Pvar.mk var_name (Procdesc.get_proc_name proc_desc) in
    let ex_instr, ex_val, ex_type = trans_exp context ex in
    let loc = line_loc_mk_proc proc_desc ln in
    let decl_instr = Sil.Store (Exp.Lvar pvar, ex_type, ex_val, loc) in
    let var : ProcAttributes.var_data = {name = var_name; typ = ex_type; modify_in_block = false; is_constexpr = false} in
      if (not (Typ.equal t ex_type)) then raise (Failure "Incorrect type assigned in declaration") else ();
      context.locals_map <- Context.LocalsMap.add var_key (pvar, ex_type) context.locals_map;
      context.locals_list <- var :: context.locals_list;
      let n = create_node_method proc_desc (ex_instr @ [decl_instr]) loc in
        n, n, [n]
 )

and trans_spec context ln = function
  | `ValueSpec (spec) -> trans_var_spec context ln spec


and trans_gen_decl context decl =
  if (List.length decl.specs > 1) then raise (Failure "Only one declaration specification supported for now") else (
    match decl.tok with
      | "var" -> trans_spec context decl.ln (List.nth_exn decl.specs 0)
  )

and trans_func_decl (go_cfg : Context.gocfg) decl : Procdesc.t =
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
    let start_node = create_node proc_desc [] loc_start Procdesc.Node.Start_node in
    let exit_node = create_node proc_desc [] loc_exit Procdesc.Node.Exit_node in
    let context = Context.create_context proc_desc go_cfg exit_node in
    let param_to_local param =
      match param with 
        | `Field (field) ->
          if (List.length field.names > 1) then raise (Failure "Function parameter can have only one name") else (
            let ident = List.nth_exn field.names 0 in
            let n = ident.id in
            let t = trans_type field.t in
            let var_key = get_var_key ident in
            let var_name = Context.VarKey.to_mangled var_key in
            let pvar = Pvar.mk var_name func_name in
              context.locals_map <- Context.LocalsMap.add var_key (pvar, t) context.locals_map
          )
    in
      (* add parameters to the list of local variables *)
      List.iter ~f:param_to_local func_type.params;
      let first_node, _, nodes_to_next = trans_body context body in
      let exit_nodes = [exit_node] in
        (* connect start node to the beginning of the function body *)
        Procdesc.node_set_succs_exn proc_desc start_node [first_node] exit_nodes;
        (* connect nodes of the function body to exit node *)
        List.iter ~f:(fun (n) -> Procdesc.node_set_succs_exn proc_desc n exit_nodes exit_nodes) nodes_to_next;
        go_cfg.func_decls <- Context.FuncDeclsMap.add decl.uid proc_desc go_cfg.func_decls;
        Procdesc.set_start_node proc_desc start_node;
        Procdesc.set_exit_node proc_desc exit_node;
        Procdesc.append_locals proc_desc (List.rev context.locals_list);
        proc_desc
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
