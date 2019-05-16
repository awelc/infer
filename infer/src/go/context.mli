(* context stores per-function data during translation *)

module VarKey : sig
	type t
	val compare : t -> t -> int
	val mk : string -> int -> t
	val to_string : t -> string
	val to_mangled : t -> Mangled.t
end

module LocalsMap : Caml.Map.S with type key = VarKey.t

module IntMap : Caml.Map.S with type key = int

module FuncDeclsMap : Caml.Map.S with type key = int

module FuncTypesMap : Caml.Map.S with type key = int

module FieldsMap : Caml.Map.S with type key = int

module LabeledStmtsMap : Caml.Map.S with type key = int

module LabelNodesMap : Caml.Map.S with type key = int

module NestMap : Caml.Map.S with type key = Procdesc.Node.t

module FuncSigsMap : Caml.Map.S with type key = string

type gomodule = 
	{ mutable func_sigs : (Typ.t) FuncSigsMap.t
	; mutable files : string list }

type gocfg = 
	{ cfg: Cfg.t
	; src_file: SourceFile.t
	; mutable func_decls: (Procdesc.t) FuncDeclsMap.t
	; mutable func_types : (Go_ast_to_json_t.func_type_type) FuncTypesMap.t
	; mutable fields : (Go_ast_to_json_t.field_type) FieldsMap.t 
	; go_module : gomodule }

type t =
	{ proc_desc: Procdesc.t
 	; mutable locals_map : (Pvar.t * Typ.t) LocalsMap.t
	; mutable locals_list : ProcAttributes.var_data list
	(* map from label stateement unique id-s to result of evaluating thhis statement *)
	; mutable labeled_stmts: (Procdesc.Node.t * Procdesc.Node.t * Procdesc.Node.t list) LabeledStmtsMap.t
	 (* nodes representing a break statement per nesting level (e.g nested for loops);
	 	when evaluating the break statement, 
	 	head of the list represents the (for, switch, etc.) statement immediately enclosing the break  *)
	; mutable break_nodes : (Procdesc.Node.t) Stack.t list
	(* same as above but for continue statements *)
	; mutable cont_nodes : (Procdesc.Node.t) Stack.t list
	(* map from label statements unique id-s to label nodes *)
	; mutable label_nodes : (Procdesc.Node.t) LabelNodesMap.t
	(* level of nesting of statements that can contain break/continue *)
	; mutable jump_nest : int
	(* jump_nest  at a given label node *)
	; mutable label_jump_nests : (int) NestMap.t
	(* nesting target for a given break node *)
	; mutable break_jump_targets : ((Procdesc.Node.t) Stack.t) NestMap.t
	(* nesting target for a given continue node *)
	; mutable cont_jump_targets : ((Procdesc.Node.t) Stack.t) NestMap.t
	; exit_node : Procdesc.Node.t
	; go_cfg : gocfg } 

val create_context : Procdesc.t -> gocfg -> Procdesc.Node.t -> t

val create_cfg : gomodule -> SourceFile.t -> gocfg

val create_module : string list -> gomodule
