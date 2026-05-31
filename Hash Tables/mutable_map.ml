module type DirectAddressMap = sig

  type 'v t 

  val insert : int -> 'v -> 'v t -> unit (* takes inputs of the value to be inserted, the key, and the map *)

  val find : int -> 'v t -> 'v option 

  val remove : int -> 'v t -> unit 

  val create : int -> 'v t 

  val of_list : int -> (int * 'v) list -> 'v t 

  val bindings : 'v t -> (int * 'v) list

end