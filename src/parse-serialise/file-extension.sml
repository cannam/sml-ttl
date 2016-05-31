
structure FileExtension = struct

    fun extension_of filename =
        case String.tokens (fn x => x = #".") filename of
            [] => ""
          | bits => hd (rev bits)

end
                              
