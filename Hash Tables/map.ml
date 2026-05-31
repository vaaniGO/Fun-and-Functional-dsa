module type Map = sig 

    type ('k, 'v) t

    val insert: 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t 

    val delete: 'k -> ('k, 'v) t -> ('k, 'v) t

    val find: 'k -> ('k, 'v) t -> 'v option

    val empty: ('k ,'v) t 

    val of_list: ('k * 'v) list -> ('k, 'v) t 

    val bindings: ('k, 'v) t -> ('k * 'v) list

end 