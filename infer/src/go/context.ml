open! IStd

module LocalsMap = Caml.Map.Make(struct
  type t = string [@@deriving compare]
end)

module IntMap = Caml.Map.Make(struct
  type t = int [@@deriving compare]
end)

module FuncDeclsMap = IntMap

module LabeledStmtsMap = IntMap

module LabelNodesMap = IntMap

module NestMap = Procdesc.NodeMap

type gocfg = 
	{ cfg: Cfg.t
	; src_file: SourceFile.t
	; mutable func_decls: (Procdesc.t) FuncDeclsMap.t }

type t =
	{ proc_desc: Procdesc.t
	; mutable locals_map : (Pvar.t * Typ.t) LocalsMap.t
	; mutable locals_list : ProcAttributes.var_data list
	; mutable labeled_stmts: (Procdesc.Node.t * Procdesc.Node.t * Procdesc.Node.t list) LabeledStmtsMap.t
	; mutable break_nodes : (Procdesc.Node.t) Stack.t list
	; mutable cont_nodes :  (Procdesc.Node.t) Stack.t list
	; mutable label_nodes : (Procdesc.Node.t) LabelNodesMap.t
	; mutable jump_nest : int
	; mutable label_jump_nests : (int) NestMap.t
	; mutable break_jump_targets : ((Procdesc.Node.t) Stack.t) NestMap.t
	; mutable cont_jump_targets : ((Procdesc.Node.t) Stack.t) NestMap.t
	; exit_node : Procdesc.Node.t
	; go_cfg : gocfg } 


let create_context proc_desc go_cfg exit_node = 
	{ proc_desc
	; locals_map = LocalsMap.empty
	; locals_list = []
	; labeled_stmts = LabeledStmtsMap.empty
	; break_nodes = []
	; cont_nodes = []
	; jump_nest = 0
	; label_jump_nests = NestMap.empty
	; label_nodes = LabelNodesMap.empty
	; break_jump_targets = NestMap.empty
	; cont_jump_targets = NestMap.empty
	; exit_node
	; go_cfg }

let create_cfg file = 
	{ cfg = Cfg.create ()
	; src_file = file
	; func_decls = FuncDeclsMap.empty }
