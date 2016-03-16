
structure FileExtension = struct

    fun extension filename =
        case String.tokens (fn x => x = #".") filename of
            [] => ""
          | bits => hd (rev bits)

end
                              
