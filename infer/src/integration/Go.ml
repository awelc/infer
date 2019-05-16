open! IStd
open Filename
open Sys
module L = Logging

type compilation_type = Build | Install | Run [@@deriving compare]

(* TODO GO: environment-related functions borrowed from the Java front-end; needs refining *)

(* load a stored global tenv if the file is found, and create a new one otherwise *)
let load_tenv () =
  match Tenv.load_global () with
  | None ->
      Tenv.create ()
  | Some _ when Config.models_mode ->
      L.(die InternalError)
        "Unexpected global tenv file found in '%s' while generating the models" Config.captured_dir
  | Some tenv ->
      tenv


(** Store the type environment containing all the types required to perform the analysis *)
let save_tenv tenv =
  L.(debug Capture Medium) "writing new global tenv@." ;
  Tenv.store_global tenv

let capture compilation_type ~prog ~args =
  let tenv = load_tenv () in (* TODO GO: at this point this is just to force creation of global environment *)
    Trans.do_files args;
    save_tenv tenv;
    ()

let string_of_type = function
  | Some "build" -> Build
  | Some "install" -> Install
  | Some "run" -> Run
  | _ -> L.die UserError "Unkown Go compilation type@."
 
