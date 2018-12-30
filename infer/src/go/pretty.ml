open! IStd
open Go_ast_to_json_t

let concatmap sep fn l =  String.concat ~sep:(sep) (List.map ~f:(fn) l)

let rec pretty_file go_file =
  concatmap "\n" pretty_decl go_file.decls

and pretty_decl = function
  | `GenDecl (decl) -> pretty_gen_decl decl
  | `FuncDecl (decl) -> pretty_func_decl decl
    
and pretty_func_decl fdecl = 
  "func " ^ (pretty_ident fdecl.name) ^ (pretty_func_type fdecl.func_type) ^ " {\n" ^ (pretty_body fdecl.body) ^ "\n}\n"

and pretty_value_spec (vspec : value_spec_type) : string =
  if ((List.length vspec.names > 1) || (List.length vspec.values > 1)) then raise (Failure "Only single variable declaration supported for now") else (
    pretty_ident (List.nth_exn vspec.names 0) ^ " " ^ pretty_expr vspec.t ^  
      if (phys_equal (List.length vspec.values) 1) then " = " ^ (pretty_expr (List.nth_exn vspec.values 0)) else ("")
  )

and pretty_spec = function
  | `ValueSpec (vspec) -> pretty_value_spec vspec

and pretty_gen_decl gdecl =
  if (List.length gdecl.specs > 1) then raise (Failure "Only one declaration specification supported for now") else (
    gdecl.tok ^ " " ^ pretty_spec (List.nth_exn gdecl.specs 0)
  )

and pretty_ident ident =
  ident.id

and pretty_star_expr (expr : star_expr_type) : string =
  "*" ^ pretty_expr expr.x


and pretty_expr = function
  | `Ident (ident) -> ident.id
  | `StarExpr (expr)  -> pretty_star_expr expr
  | `UnaryExpr (expr) -> expr.tok ^ pretty_expr expr.x
  | `BasicLit (lit) -> lit.value  

and pretty_res_typ field =
  pretty_expr field.t

and pretty_param field =
  if (List.length field.names > 1) then raise (Failure "Function parameter can have only one name") else (
    pretty_ident (List.nth_exn field.names 0) ^ " " ^ pretty_expr field.t
  )

and pretty_func_type func_type =
  if (List.length func_type.results > 1) then raise (Failure "Only one result value supported for now") else (
    "(" ^ (concatmap ", " pretty_param func_type.params) ^ ") " ^  (pretty_res_typ (List.nth_exn func_type.results 0))  
  )

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

and pretty_stmt = function
  | `DeclStmt (stmt) -> pretty_decl_stmt stmt
  | `ReturnStmt (stmt) -> pretty_return_stmt stmt
  | `AssignStmt (stmt) -> pretty_assign_stmt stmt

and pretty_body body =
  concatmap "\n" pretty_stmt body.stmts
