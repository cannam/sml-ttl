
structure FileExtension = struct

    fun extension_of filename =
        let val to_lower =
                String.implode o (List.map Char.toLower) o String.explode
        in
            case String.tokens (fn x => x = #".") filename of
                [] => ""
              | bits => to_lower (hd (rev bits))
        end

end
                              
