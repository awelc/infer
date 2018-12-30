open! IStd
open Filename
open Sys
module L = Logging

type compilation_type = Build | Install | Run [@@deriving compare]

let init_global_state source_file =
  Language.curr_language := Language.Go ;
  DB.Results_dir.init source_file ;
  Ident.NameGenerator.reset ()


let do_file file_path =
  let shell_cmd = "go build " ^ file_path in
  (  match Utils.with_process_in shell_cmd In_channel.input_all with
    | log, Error err -> raise (Failure "Compilation failure")
    | exception exn -> raise exn
    | log, Ok () -> ()
  );
  ( try
   let source_file = SourceFile.create file_path in
   let cfg = Trans.compute_icfg source_file in                             
     init_global_state source_file;
     SourceFiles.add source_file cfg Tenv.Global None;
     if Config.debug_mode || Config.frontend_tests then Dotty.print_icfg_dotty source_file cfg ;
     ()
    with
      | Parsing.Parse_error -> printf "Syntax Error\n" 
  );
  ()

(* TODO: environment-related functions borrowed from the Java front-end; needs refining *)

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
  let tenv = load_tenv () in (* TODO: at this point this is just to force creation of global environment *)
    List.iter ~f:(do_file) args;
    save_tenv tenv;
    ()



let string_of_type = function
  | Some "build" -> Build
  | Some "install" -> Install
  | Some "run" -> Run
  | _ -> L.die UserError "Unkown Go compilation type@."
 
