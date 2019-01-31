(* context stores per-function data during translation *)

module LocalsMap : Caml.Map.S with type key = string

module IntMap : Caml.Map.S with type key = int

module FuncDeclsMap : Caml.Map.S with type key = int

module LabeledStmtsMap : Caml.Map.S with type key = int

type gocfg = 
	{ cfg: Cfg.t
	; src_file: SourceFile.t
	; mutable func_decls: (Procdesc.t) FuncDeclsMap.t }

type t =
	{ proc_desc: Procdesc.t
 	; mutable locals_map : (Pvar.t * Typ.t) LocalsMap.t
	; mutable locals_list : ProcAttributes.var_data list
	; mutable labeled_stmts: (Procdesc.Node.t * Procdesc.Node.t * Procdesc.Node.t list) LabeledStmtsMap.t
	; mutable break_nodes : Procdesc.Node.t list (* nodes representing a break to immediately enclosing statement *)
	; mutable cont_nodes : Procdesc.Node.t list (* nodes representing a break to immediately enclosing statement *)
	; exit_node : Procdesc.Node.t
	; go_cfg : gocfg } 

val create_context : Procdesc.t -> gocfg -> Procdesc.Node.t -> t

val create_cfg : SourceFile.t -> gocfg
