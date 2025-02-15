open Core_kernel.Std
open Bap.Std
include Self()
open Format


(* TODO mark only defs, not uses. *)
let taint prog = (object(self)
  inherit Term.mapper as super
  method! map_def def =
    let def = super#map_def def in
    match Term.get_attr def Term.origin with
    | None -> def
    | Some tid -> match Program.lookup jmp_t prog tid with
      | None -> def
      | Some jmp -> match Jmp.kind jmp with
        | Call c -> self#process def (Term.tid jmp) c
        | _ -> def

  method private process def taint call = match Call.target call with
    | Indirect _ -> def
    | Direct callee -> match Term.find sub_t prog callee with
      | Some sub when Term.has_attr sub Sub.warn_unused_result ->
        Term.set_attr def Taint.reg taint
      | _ -> def
end)#run prog

let collect_seeds prog = (object
  inherit [Tid.Set.t] Term.visitor
  method! enter_term cls t seeds =
    match Term.get_attr t Taint.reg with
    | None -> seeds
    | Some seed -> Set.add seeds seed
end)#run prog Tid.Set.empty


let solve prog seeds = (object
  inherit [Tid.Set.t] Term.visitor
  method! enter_term cls t seeds =
    if Term.has_attr t Taint.reg then seeds
    else match Term.get_attr t Taint.regs with
      | None ->
        seeds
      | Some taints ->
        Map.fold taints ~init:seeds ~f:(fun ~key:v ~data:taints seeds ->
            Set.diff seeds taints)
end)#run prog seeds

let marker unchecked = object
  inherit Term.mapper
  method! map_term cls t =
    if Set.mem unchecked (Term.tid t)
    then Term.set_attr t Term.dead ()
    else t
end

let printer unchecked = object
  inherit [unit] Term.visitor
  method! enter_jmp jmp () =
    if Set.mem unchecked (Term.tid jmp)
    then printf "%a" Jmp.pp jmp
end

let print unchecked proj =
  let prog = Project.program proj in
  (printer unchecked)#run prog ()

let mark unchecked proj =
  Project.program proj |>
  (marker unchecked)#run |>
  Project.with_program proj

let taint proj =
  Project.program proj |>
  taint |>
  Project.with_program proj

let run pass proj =
  let prog = Project.program proj in
  let seeds = collect_seeds prog in
  let unchecked = solve prog seeds in
  pass unchecked proj

module Cmdline = struct
  open Cmdliner
  let man = [
    `S "SYNOPSIS";
    `Pre "
    $(b,--$mname)
    $(b,--$mname-taint)
    $(b,--$mname-print)
    $(b,--$mname-mark)";
    `S "DESCRIPTION";
    `P "If a subroutine has GNU attribute $(b,warn_unused_result) and
  its result is not used, then print a warning message.";
    `S "PASSES";
    `I begin
      "$(b,--$mname-taint)",
      "Taint all values defined by functions that are marked with
     $(b,warn_unused_result) attribute. Will run $(b,callsites) as a
     dependency."
    end;
    `I begin
      "$(b,--$mname-print)",
      "Print all calls that weren't checked."
    end;
    `I begin
      "$(b,--$mname-mark)",
      "Mark all unchecked calls with $(b,Term.dead) attribute"
    end;
    `I begin
      "$(b,--$mname)",
      "Same as $(b,--$mname-taint --propagate-taint --$mname-print)"
    end
  ]

  let info = Term.info ~version ~man ~doc name
  let passes = [name; "taint"; "mark"; "print"]
  let not_a_pass s = not (List.mem passes s)

  let parse () = match Term.eval ~argv (Term.pure (),info) with
    | `Version | `Help -> exit 0
    | `Error _ -> exit 1
    | `Ok () -> match Array.find argv ~f:not_a_pass with
      | None -> ()
      | Some pass -> eprintf "Unknown pass: %s" pass; exit 1
end

let () =
  Cmdline.parse ();
  Project.register_pass ~deps:["callsites"] ~name:"taint" taint;
  Project.register_pass' ~name:"print" (run print);
  Project.register_pass  ~name:"mark" (run mark);
  Project.register_pass' ignore ~deps:[name^"-taint"; "propagate-taint"; name^"-print"]
