open! IStd
open Go_ast_to_json_t
open Go_ast_to_json_j

type icfg = { cfg: Cfg.t }

let locals_map = String.Table.create () ~size:8

let line_loc_mk_src source_file l = { Location.line = l; col = -1; file = source_file }

let line_loc_mk_proc proc_desc l = line_loc_mk_src (Procdesc.get_attributes proc_desc).translation_unit l

let create_node proc_desc instructions line = 
  Procdesc.create_node proc_desc (line_loc_mk_proc proc_desc line) (Procdesc.Node.Stmt_node MethodBody) instructions

let rec create_fn_desc source_file icfg = function
  | `FuncDecl (decl) -> trans_func_decl source_file icfg decl
  | `GenDecl (decl) -> raise (Failure "Should not happen")

and trans_param field =
  if (List.length field.names > 1) then raise (Failure "Function parameter can have only one name") else (
    (Mangled.from_string (List.nth_exn field.names 0).id,  trans_type field.t)
  )


and trans_exp proc_desc (ex : expr_type) : (Sil.instr list * Exp.t * Typ.t) = 
  let trans_var name line =
    match Hashtbl.find locals_map name with
      | None -> raise Not_found
      | Some (pvar, var_type) ->
        let id = Ident.create_fresh Ident.knormal in
        let load_instr = Sil.Load (id, Exp.Lvar pvar, var_type, line_loc_mk_proc proc_desc line) in
        [load_instr], Exp.Var id, var_type
  in
    match ex with
      | `Ident (ident) -> 
        (match ident.id with 
          | "nil" -> [], Exp.null, Typ.mk (Typ.Tptr ((Typ.mk (Tint Typ.IInt)), Typ.Pk_pointer))
          | _ -> trans_var ident.id ident.ln )
      | `StarExpr (expr) -> 
        let load_ptr_list, ptr_var, ptr_var_type = trans_exp proc_desc expr.x in
        let var_type = Typ.strip_ptr ptr_var_type in
        let id = Ident.create_fresh Ident.knormal in
        let load_instr = Sil.Load (id, ptr_var, var_type, line_loc_mk_proc proc_desc expr.ln) in
        load_ptr_list @ [load_instr], Exp.Var id, var_type
      | `UnaryExpr (expr) ->
        (match expr.tok with
          | "&" -> 
            (match expr.x with
              | `Ident (ident) -> (* must be a variable *)
                match Hashtbl.find locals_map ident.id with
                  | None -> raise Not_found
                  | Some (pvar, var_type) -> [], Exp.Lvar pvar, Typ.mk (Typ.Tptr (var_type, Typ.Pk_pointer)) ) )
        | `BasicLit (lit) ->
          match lit.kind with
            | "INT" -> 
              [], Exp.int (IntLit.of_int (int_of_string lit.value)), Typ.mk (Tint Typ.IInt)



and trans_type = function
  | `Ident (ident) -> (
    match ident.id with 
      | "int" -> Typ.mk (Tint Typ.IInt)
      | "bool" -> Typ.mk (Tint Typ.IBool))
  | `StarExpr (expr) -> 
      Typ.mk (Typ.Tptr (trans_type expr.x, Typ.Pk_pointer))
  | `UnaryExpr (expr) -> raise (Failure "Should not happen")
  | `BasicLit (list) -> raise (Failure "Should not happen")

and trans_body proc_desc locals_set body =
  List.map ~f:(fun (stmt) -> trans_stmt proc_desc locals_set stmt) body.stmts

and trans_decl_stmt proc_desc locals_set stmt =
  trans_decl proc_desc locals_set stmt.decl

and trans_return_stmt proc_desc locals (stmt : return_stmt_type) = 
  if (List.length stmt.results > 1) then raise (Failure "Only one result value supported for now") else (
    let ex = List.nth_exn stmt.results 0 in
    let ret_var = Pvar.get_ret_pvar (Procdesc.get_proc_name proc_desc) in
    let ret_type = Procdesc.get_ret_type proc_desc in
    let ex_instr, ex_val, _ = trans_exp proc_desc ex in
    let return_instr = Sil.Store (Exp.Lvar ret_var, ret_type, ex_val, line_loc_mk_proc proc_desc stmt.ln) in
      create_node proc_desc (ex_instr @ [return_instr]) stmt.ln
  )

and trans_assign_stmt proc_desc locals_set stmt =
  if ((List.length stmt.lhs > 1) || (List.length stmt.rhs > 1)) then raise (Failure "Only single value assignment supported for now") else (
    let lhs = List.nth_exn stmt.lhs 0 in
    let rhs = List.nth_exn stmt.rhs 0 in
      match lhs with
        | `Ident (ident) -> (* must be a variable *)
          let name = ident.id in
          let ex_instr, ex_val, ex_type = trans_exp proc_desc rhs in
          let var_lookup = 
            (match Hashtbl.find locals_map name with
             | None -> 
                 let pvar = Pvar.mk (Mangled.from_string name) (Procdesc.get_proc_name proc_desc) in
                 Hashtbl.replace locals_map ~key:name ~data:(pvar, ex_type);
                 pvar               
             | Some (pvar, var_type) ->
                 if (not (Typ.equal var_type ex_type)) then raise (Failure "Incorrect type assigned") else (); pvar)
          in
            let pvar = var_lookup in
            let assign_instr = Sil.Store (Exp.Lvar pvar, ex_type, ex_val, line_loc_mk_proc proc_desc stmt.ln) in
              create_node proc_desc (ex_instr @ [assign_instr]) stmt.ln

  )

and trans_stmt proc_desc locals_set = function
  | `DeclStmt (stmt) -> trans_decl_stmt proc_desc locals_set stmt
  | `ReturnStmt (stmt) -> trans_return_stmt proc_desc locals_set stmt
  | `AssignStmt (stmt) -> trans_assign_stmt proc_desc locals_set stmt

and trans_var_spec proc_desc locals_set ln (spec : value_spec_type)   =
  if ((List.length spec.names > 1) || (List.length spec.values > 1)) then raise (Failure "Only single variable declaration supported for now") else (
    let name = (List.nth_exn spec.names 0).id in
    let var_name = Mangled.from_string name in
    let t = trans_type spec.t in
    let ex = List.nth_exn spec.values 0 in
    let pvar = Pvar.mk var_name (Procdesc.get_proc_name proc_desc) in
    let ex_instr, ex_val, ex_type = trans_exp proc_desc ex in
    let decl_instr = Sil.Store (Exp.Lvar pvar, ex_type, ex_val, line_loc_mk_proc proc_desc ln) in
    let var : ProcAttributes.var_data = {name = var_name; typ = ex_type; modify_in_block = false; is_constexpr = false} in
      if (not (Typ.equal t ex_type)) then raise (Failure "Incorrect type assigned in declaration") else ();
      Hashtbl.replace locals_map ~key:name ~data:(pvar, ex_type);
      Stack.push locals_set var;
      create_node proc_desc (ex_instr @ [decl_instr]) ln
 )

and trans_spec proc_desc locals_set ln = function
  | `ValueSpec (spec) -> trans_var_spec proc_desc locals_set ln spec


and trans_gen_decl proc_desc locals_set decl =
  if (List.length decl.specs > 1) then raise (Failure "Only one declaration specification supported for now") else (
    match decl.tok with
      | "var" -> trans_spec proc_desc locals_set decl.ln (List.nth_exn decl.specs 0)
  )

and trans_func_decl source_file icfg decl =
  let func_type = decl.func_type in
  if (List.length func_type.results > 1) then raise (Failure "Only one result value supported for now") else (
    let body = decl.body in
    let name = decl.name in
    let loc_start = line_loc_mk_src source_file func_type.ln in
    let loc_exit = line_loc_mk_src source_file body.rbrace.ln in
    let params = func_type.params in
    let proc_attributes = { (ProcAttributes.default source_file (Typ.Procname.Go (Typ.Procname.Go.make name.id))) with
        formals = List.map ~f:trans_param params;
        is_defined = true;
        loc = loc_start;
        ret_type = trans_type (List.nth_exn func_type.results 0).t } in
    let proc_desc = Cfg.create_proc_desc icfg.cfg proc_attributes in
    let start_node = Procdesc.create_node proc_desc loc_start Procdesc.Node.Start_node [] in
    let exit_node = Procdesc.create_node proc_desc loc_exit Procdesc.Node.Exit_node [] in
    let exit_nodes = [exit_node] in
    let locals_set = Stack.create () in
    let rec link_nodes pred_node = function
        | [] -> Procdesc.node_set_succs_exn proc_desc pred_node exit_nodes exit_nodes (* pred_node is last node - needs to be connected to the sink *)
        | next_node :: tail_nodes -> (Procdesc.node_set_succs_exn proc_desc pred_node [next_node] exit_nodes; link_nodes next_node tail_nodes ) in
            Procdesc.set_start_node proc_desc start_node ;
            Procdesc.set_exit_node proc_desc exit_node ;
            Procdesc.node_set_succs_exn proc_desc start_node exit_nodes exit_nodes;
            link_nodes start_node (trans_body proc_desc locals_set body);
            Procdesc.append_locals proc_desc (List.rev (Stack.fold locals_set ~init:[] ~f:(fun a b -> b :: a)));
            ()
  )

and trans_decl proc_desc locals_set = function
  | `FuncDecl (decl) -> raise (Failure "Should not happen")
  | `GenDecl (decl) -> trans_gen_decl proc_desc locals_set decl


let go_to_json = Config.lib_dir ^ "/go/src_to_ast_json.go"

let compute_icfg source_file =
  let stdout = Unix.open_process_in ("go run " ^ go_to_json ^ " -- " ^ (SourceFile.to_string source_file)) in
  let go_file = Atdgen_runtime.Util.Json.from_channel read_file_type stdout in
  let _ = Unix.close_process_in stdout in
(*    print_endline (Pretty.pretty_file go_file); *)
    let icfg = { cfg = Cfg.create () } in
      List.iter ~f:(fun (decl) -> create_fn_desc source_file icfg decl) go_file.decls;
      icfg.cfg
