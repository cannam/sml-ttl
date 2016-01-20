
structure Log : LOG = struct

    datatype level =
             ERROR | WARN | INFO

    val level = ref WARN
    val start_time = ref (Time.now ())

    datatype interpolable =
             I of int | R of real | B of bool | S of string | SL of string list

    type arg = string * interpolable list
    type thunk = unit -> arg

    fun interpolate str values =
        let fun int_aux acc chars [] _ = String.implode (rev acc @ chars)
              | int_aux acc [] _ _ =
                (TextIO.output
                     (TextIO.stdErr,
                      "Logger: WARNING: Too many values in string interpolation for \"" ^ str ^ "\"");
                 int_aux acc [] [] false)
              | int_aux acc (first::rest) values escaped =
                if first = #"\\" then
                    int_aux acc rest values (not escaped)
                else if first = #"%" andalso not escaped then
                    let val insertion =
                            case (hd values) of
                                I i => Int.toString i
                              | R r => Real.toString r
                              | B b => if b then "true" else "false"
                              | S s => s
                              | SL sl => String.concatWith "\n" sl
                    in
                        int_aux ((rev (String.explode insertion)) @ acc)
                                rest (tl values) escaped
                    end
                else
                    int_aux (first::acc) rest values false
        in
            int_aux [] (String.explode str) values false
        end

    fun time_string () =
        Time.fmt 6 (Time.- (Time.now (), !start_time))
            
    val no_log = ("", [])

    fun set_log_level l =
        (level := l;
         start_time := Time.now ())
                                       
    fun print string =
        if string <> "" then
            TextIO.output (TextIO.stdErr,
                           (time_string ()) ^ ": " ^ string ^ "\n")
        else ()

    fun log (string, args) =
        print (interpolate string args)
              
    fun info f =
        if !level = INFO then
            log (f ())
        else ()
                 
    fun warn f =
        if !level = INFO orelse !level = WARN then
            log (f ())
        else ()
                 
    fun error f =
        log (f ())

end

