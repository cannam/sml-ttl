
signature TESTS = sig

    val name : string

    type test = string * (unit -> bool)
                   
    val tests : test list

end
                      
