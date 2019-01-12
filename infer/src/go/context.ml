open! IStd

module LocalsMap = Caml.Map.Make(struct
  type t = string [@@deriving compare]
end)

module IntMap = Caml.Map.Make(struct
  type t = int [@@deriving compare]
end)

module FuncDeclsMap = IntMap

module BranchesMap = IntMap

type jump_kind = Index of int | Exit

type gocfg = 
	{ cfg: Cfg.t
	; src_file: SourceFile.t
	; mutable func_decls: (Procdesc.t) FuncDeclsMap.t }

type t =
	{ proc_desc: Procdesc.t
	; mutable locals_map : (Pvar.t * Typ.t) LocalsMap.t
	; mutable locals_list : ProcAttributes.var_data list
	; mutable if_branches : jump_kind BranchesMap.t
	; mutable goto_branches : jump_kind BranchesMap.t
	; mutable node_count: int
	; go_cfg : gocfg } 


let create_context proc_desc go_cfg = 
	{ proc_desc
	; locals_map = LocalsMap.empty
	; locals_list = []
	; if_branches = BranchesMap.empty
	; goto_branches = BranchesMap.empty
	; node_count = 0
	; go_cfg }

let create_cfg file = 
	{ cfg = Cfg.create ()
	; src_file = file
	; func_decls = FuncDeclsMap.empty }
