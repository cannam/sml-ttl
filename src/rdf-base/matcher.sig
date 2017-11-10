
signature MATCHER = sig

    type t
    
    datatype node = datatype RdfNode.node

    type patnode = node option
    type triple = node * node * node
    type pattern = patnode * patnode * patnode
                                           
    val match : t * pattern -> triple list

end
                        
