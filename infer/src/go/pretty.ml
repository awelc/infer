open! IStd
open Go_ast_to_json_t

let func_decls_map = Int.Table.create () ~size:8
let func_types_map = Int.Table.create () ~size:8
let fields_map = Int.Table.create () ~size:8
let labeled_stmts_map = Int.Table.create () ~size:8

let concatmap sep fn l =  String.concat ~sep:(sep) (List.map ~f:(fn) l)

let rec pretty_file go_file =
  List.iter ~f:(get_def) go_file.defs;
  concatmap "\n" pretty_decl go_file.decls

and get_def = function 
  | `FuncType (t) -> Hashtbl.replace func_types_map t.uid t
  | `Field (field) -> Hashtbl.replace fields_map field.uid field

and pretty_decl = function
  | `GenDecl (decl) -> pretty_gen_decl decl
  | `FuncDecl (decl) -> Hashtbl.replace func_decls_map decl.uid decl; pretty_func_decl decl
  | `FuncDeclRef (ref) -> pretty_func_decl (Hashtbl.find_exn func_decls_map ref)
    
and pretty_func_decl fdecl = 
  "func " ^ (pretty_ident fdecl.name) ^ (pretty_func_type (get_func_type fdecl.func_desc)) ^ " {\n" ^ (pretty_stmt_type fdecl.body) ^ "\n}\n"

and pretty_value_spec (vspec : value_spec_type) : string =
  if (List.length vspec.names > 1) then raise (Failure "Only single variable declaration supported for now") else (
    let ts = (
      match vspec.t with
        | None -> ""
        | Some (t) -> " " ^ pretty_expr t
    ) in
    let vs = (
      match vspec.values with
        | None -> ""
        | Some (values) ->
          if (List.length values > 1) then raise (Failure "Only single variable initialization supported for now") else (
            " = " ^ (pretty_expr (List.nth_exn values 0))
          )
    ) in
      pretty_ident (List.nth_exn vspec.names 0) ^ ts ^ vs
  )

and pretty_spec = function
  | `ValueSpec (spec : value_spec_type) -> pretty_value_spec spec

and pretty_gen_decl gdecl =
  if (List.length gdecl.specs > 1) then raise (Failure "Only one declaration specification supported for now") else (
    gdecl.tok ^ " " ^ pretty_spec (List.nth_exn gdecl.specs 0)
  )

and pretty_ident ident =
  match ident.obj with
    | None -> ident.id
    | Some (o) ->
      match o with
        | `FuncDecl (decl) -> 
          Hashtbl.replace func_decls_map decl.uid decl;
          ident.id
        | `LabeledStmt (stmt) -> 
          Hashtbl.replace labeled_stmts_map stmt.uid stmt;
          ident.id
        | _ -> ident.id

and pretty_star_expr (expr : star_expr_type) : string =
  "*" ^ pretty_expr expr.x

and pretty_call_expr expr =
  pretty_expr expr.fn ^ "(" ^ concatmap ", " pretty_expr expr.args ^ ")"

and pretty_expr = function
  | `Ident (ident) -> pretty_ident ident
  | `StarExpr (expr)  -> pretty_star_expr expr
  | `UnaryExpr (expr) -> expr.op ^ pretty_expr expr.x
  | `BinaryExpr (expr) -> pretty_expr expr.x ^ " " ^ expr.op ^ " " ^ pretty_expr expr.y
  | `BasicLit (lit) -> lit.value  
  | `CallExpr (expr) -> pretty_call_expr expr

and get_field = function
    | `FieldRef (ref) -> Hashtbl.find_exn fields_map ref

and pretty_res_typ f =
  let field = get_field f in
    pretty_expr field.t

and pretty_param f =
  let field = get_field f in  
    if (List.length field.names > 1) then raise (Failure "Function parameter can have only one name") else (
      pretty_ident (List.nth_exn field.names 0) ^ " " ^ pretty_expr field.t
    )

and pretty_func_type func_type =
  if (List.length func_type.results > 1) then raise (Failure "Only one result value supported for now") else (
    "(" ^ (concatmap ", " pretty_param func_type.params) ^ ") " ^  (pretty_res_typ (List.nth_exn func_type.results 0))  
  )

and get_func_type = function
  | `FuncTypeRef (ref) ->
    Hashtbl.find_exn func_types_map ref


and pretty_decl_stmt stmt =
  pretty_decl stmt.decl 

and pretty_return_stmt (stmt : return_stmt_type) : string =
  if (List.length stmt.results > 1) then raise (Failure "Only one result value supported for now") else (
    "return " ^ pretty_expr (List.nth_exn stmt.results 0)
  )

and pretty_assign_stmt stmt =
  if ((List.length stmt.lhs > 1) || (List.length stmt.rhs > 1)) then raise (Failure "Only single value assignment supported for now") else (
    let lhs = List.nth_exn stmt.lhs 0 in
    let rhs = List.nth_exn stmt.rhs 0 in
      pretty_expr lhs ^ " := " ^ pretty_expr rhs
  )

and pretty_if_stmt (stmt: if_stmt_type) =
  "if " ^ pretty_expr stmt.cond ^ " {\n" ^ pretty_stmt_type stmt.body ^ "\n}" ^
  match stmt.el with
    | Some (el) -> " else {\n" ^ pretty_stmt el ^ "\n}"
    | None -> ""

and pretty_for_stmt stmt =
  "for " ^ pretty_stmt stmt.init ^ "; " ^ pretty_expr stmt.cond ^ "; " ^ pretty_stmt stmt.post ^ " {\n" ^ pretty_stmt_type stmt.body ^ "\n}" 

and pretty_inc_dec_stmt stmt =
  pretty_expr stmt.x ^ stmt.op

and pretty_branch_stmt stmt =
  stmt.keyword ^ " " ^ 
  match stmt.label with
    | None -> ""
    | Some (l) -> pretty_ident l

and pretty_labeled_stmt stmt =
  pretty_ident stmt.label ^ ":\n" ^ pretty_stmt stmt.stmt

and pretty_labeled_stmt_ref ref =
  pretty_labeled_stmt (Hashtbl.find_exn labeled_stmts_map ref)

  
and pretty_stmt = function
  | `DeclStmt (stmt) -> pretty_decl_stmt stmt
  | `ReturnStmt (stmt) -> pretty_return_stmt stmt
  | `AssignStmt (stmt) -> pretty_assign_stmt stmt
  | `BlockStmt (stmt) -> pretty_stmt_type stmt
  | `IfStmt (stmt) -> pretty_if_stmt stmt
  | `ForStmt (stmt) -> pretty_for_stmt stmt
  | `IncDecStmt (stmt) -> pretty_inc_dec_stmt stmt
  | `BranchStmt (stmt) -> pretty_branch_stmt stmt
  | `LabeledStmt (stmt) -> Hashtbl.replace labeled_stmts_map stmt.uid stmt; pretty_labeled_stmt stmt
  | `LabeledStmtRef (ref) -> pretty_labeled_stmt_ref ref
  | `EmptyStmt (stmt) -> ""

and pretty_stmt_type body =
  concatmap "\n" pretty_stmt body.stmts
