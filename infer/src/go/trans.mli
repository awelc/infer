open! IStd

type icfg = { cfg: Cfg.t }

val compute_icfg : SourceFile.t -> Cfg.t
