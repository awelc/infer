(* context stores per-function data during translation *)

module LocalsMap : Caml.Map.S with type key = string

module FuncDeclsMap : Caml.Map.S with type key = int

type gocfg = 
	{ cfg: Cfg.t
	; src_file: SourceFile.t
	; mutable func_decls: (Procdesc.t) FuncDeclsMap.t }

type t =
	{ proc_desc: Procdesc.t
 	; mutable locals_map : (Pvar.t * Typ.t) LocalsMap.t
	; mutable locals_list : ProcAttributes.var_data list
	; go_cfg : gocfg } 

val create_context : Procdesc.t -> gocfg -> t

val create_cfg : SourceFile.t -> gocfg
