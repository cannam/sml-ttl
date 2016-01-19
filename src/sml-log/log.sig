
signature LOG = sig

    datatype level =
             ERROR | WARN | INFO
    
    datatype interpolable =
             I of int | R of real | B of bool | S of string | SL of string list

    type arg = string * interpolable list
    type thunk = unit -> arg

    val set_log_level : level -> unit

    val no_log : arg
    val info : thunk -> unit
    val warn : thunk -> unit
    val error : thunk -> unit

end
