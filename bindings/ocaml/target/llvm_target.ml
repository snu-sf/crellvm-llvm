(*===-- llvm_target.ml - LLVM OCaml Interface ------------------*- OCaml -*-===*
 *
 *                     The LLVM Compiler Infrastructure
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See LICENSE.TXT for details.
 *
 *===----------------------------------------------------------------------===*)

module Endian = struct
  type t =
  | Big
  | Little
end

module CodeGenOptLevel = struct
  type t =
  | None
  | Less
  | Default
  | Aggressive
end

module RelocMode = struct
  type t =
  | Default
  | Static
  | PIC
  | DynamicNoPIC
end

module CodeModel = struct
  type t =
  | Default
  | JITDefault
  | Small
  | Kernel
  | Medium
  | Large
end

module CodeGenFileType = struct
  type t =
  | AssemblyFile
  | ObjectFile
end

(* added for vellvm - start *)
module AlignType = struct
  type t =
  | Invalid_align
  | Integer_align
  | Vector_align
  | Float_align
  | Aggregate_align
(*  | Stack_align *)
end                           
(* added for vellvm - end *)                          

exception Error of string

let () = Callback.register_exception "Llvm_target.Error" (Error "")

module DataLayout = struct
  type t

  external of_string : string -> t = "llvm_datalayout_of_string"
  external as_string : t -> string = "llvm_datalayout_as_string"
  external add_to_pass_manager : [<Llvm.PassManager.any]
                                 Llvm.PassManager.t -> t -> unit
                               = "llvm_datalayout_add_to_pass_manager"
  external byte_order : t -> Endian.t = "llvm_datalayout_byte_order"
  external pointer_size : t -> int = "llvm_datalayout_pointer_size"
  external intptr_type : Llvm.llcontext -> t -> Llvm.lltype
                       = "llvm_datalayout_intptr_type"
  external qualified_pointer_size : int -> t -> int
                                  = "llvm_datalayout_qualified_pointer_size"
  external qualified_intptr_type : Llvm.llcontext -> int -> t -> Llvm.lltype
                                 = "llvm_datalayout_qualified_intptr_type"
  external size_in_bits : Llvm.lltype -> t -> Int64.t
                        = "llvm_datalayout_size_in_bits"
  external store_size : Llvm.lltype -> t -> Int64.t
                      = "llvm_datalayout_store_size"
  external abi_size : Llvm.lltype -> t -> Int64.t
                    = "llvm_datalayout_abi_size"
  external abi_align : Llvm.lltype -> t -> int
                     = "llvm_datalayout_abi_align"
  external stack_align : Llvm.lltype -> t -> int
                       = "llvm_datalayout_stack_align"
  external preferred_align : Llvm.lltype -> t -> int
                           = "llvm_datalayout_preferred_align"
  external preferred_align_of_global : Llvm.llvalue -> t -> int
                                   = "llvm_datalayout_preferred_align_of_global"
  external element_at_offset : Llvm.lltype -> Int64.t -> t -> int
                             = "llvm_datalayout_element_at_offset"
  external offset_of_element : Llvm.lltype -> int -> t -> Int64.t
                             = "llvm_datalayout_offset_of_element"
(* added for vellvm - start *)
  external pointer_size_in_bits : t -> int = "llvm_pointer_size_in_bits"
  external pointer_abi_alignment : t -> int 
    = "llvm_pointer_abi_alignment"
  external pointer_pref_alignment : t -> int 
    = "llvm_pointer_pref_alignment"

  external get_num_alignment : t -> int = "llvm_get_num_alignment"            
  external get_align_type_enum : t -> int -> AlignType.t 
    = "llvm_get_align_type_enum"
  external get_abi_align : t -> int -> int  = "llvm_get_abi_align"
  external get_pref_align : t -> int -> int  = "llvm_get_pref_align"
  external get_type_bitwidth : t -> int -> int  = "llvm_get_type_bit_width"
(* added for vellvm - end *)                                 
end

module Target = struct
  type t

  external default_triple : unit -> string = "llvm_target_default_triple"
  external first : unit -> t option = "llvm_target_first"
  external succ : t -> t option = "llvm_target_succ"
  external by_name : string -> t option = "llvm_target_by_name"
  external by_triple : string -> t = "llvm_target_by_triple"
  external name : t -> string = "llvm_target_name"
  external description : t -> string = "llvm_target_description"
  external has_jit : t -> bool = "llvm_target_has_jit"
  external has_target_machine : t -> bool = "llvm_target_has_target_machine"
  external has_asm_backend : t -> bool = "llvm_target_has_asm_backend"

  let all () =
    let rec step elem lst =
      match elem with
      | Some target -> step (succ target) (target :: lst)
      | None        -> lst
    in
    step (first ()) []
end

module TargetMachine = struct
  type t

  external create : triple:string -> ?cpu:string -> ?features:string ->
                    ?level:CodeGenOptLevel.t -> ?reloc_mode:RelocMode.t ->
                    ?code_model:CodeModel.t -> Target.t -> t
                  = "llvm_create_targetmachine_bytecode"
                    "llvm_create_targetmachine_native"
  external target : t -> Target.t
                  = "llvm_targetmachine_target"
  external triple : t -> string
                  = "llvm_targetmachine_triple"
  external cpu : t -> string
               = "llvm_targetmachine_cpu"
  external features : t -> string
                    = "llvm_targetmachine_features"
  external data_layout : t -> DataLayout.t
                       = "llvm_targetmachine_data_layout"
  external add_analysis_passes : [< Llvm.PassManager.any ] Llvm.PassManager.t -> t -> unit
                               = "llvm_targetmachine_add_analysis_passes"
  external set_verbose_asm : bool -> t -> unit
                           = "llvm_targetmachine_set_verbose_asm"
  external emit_to_file : Llvm.llmodule -> CodeGenFileType.t -> string ->
                          t -> unit
                        = "llvm_targetmachine_emit_to_file"
  external emit_to_memory_buffer : Llvm.llmodule -> CodeGenFileType.t ->
                                   t -> Llvm.llmemorybuffer
                                 = "llvm_targetmachine_emit_to_memory_buffer"
end
