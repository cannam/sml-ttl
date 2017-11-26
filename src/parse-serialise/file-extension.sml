
structure FileExtension = struct

    fun extensionOf filename =
        let val toLower =
                String.implode o (List.map Char.toLower) o String.explode
        in
            case String.tokens (fn x => x = #".") filename of
                [] => ""
              | bits => toLower (hd (rev bits))
        end

end
                              
