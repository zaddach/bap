open Core_kernel.Std
open Bap_types.Std
open Or_error

open Bap_disasm_arm_types
open Bap_disasm_arm_utils

module Arm = Bap_disasm_arm
module Mem = Bap_disasm_arm_mem
module Env = Bap_disasm_arm_env
module Shift = Bap_disasm_arm_shift

let word = Word.of_int ~width:32


let lift operand ?link ?x:_ ?cond ?thumb addr =
  let pc_offset = match thumb with
    Some true -> Word.(of_int 4 ~width:32)
    | _ -> Word.(of_int 8 ~width:32) in
  let target =
    match operand with
    | Op.Reg r -> Bil.var (Env.of_reg r)
    | Op.Imm offset ->
      let width = Word.bitwidth offset in
      let _1 = Word.one 32 in
      let min_32 = Word.Int_exn.(_1 lsl Word.of_int 31 ~width) in
      let offset = if offset = min_32 then Word.zero 32 else offset in
      let r = Word.Int_exn.(addr + pc_offset + offset) in
      Bil.int r in
  (* TODO detect change to thumb in `x` *)
  let jump_instr = [Bil.jmp target] in
  let instr_size = word 4 in
  let link_instr =
    let next_addr = Word.Int_exn.(addr + instr_size) in
    match link with
    | Some true -> [Bil.move Env.lr Bil.(int next_addr)]
    | _         -> [] in
  let stmts = link_instr @ jump_instr in
  match cond with
  | Some c -> exec stmts c
  | None -> stmts
