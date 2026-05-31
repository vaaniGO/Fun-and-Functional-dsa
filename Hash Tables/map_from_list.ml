module ListMap : Map = struct 

  type ('k, 'v) t = ('k) * 'v list 

  let insert k v m = (k, v) :: m

  let find = List.assoc_opt 

  let remove k lst = List.filter (fun (k', _) -> k <> k') lst 

  let empty = []

  let of_lst lst = lst 

  let keys m = m |> List.map fst |> List.sort_uniq Stdlib.compare

  let binding m k = (l, List.assoc k m)

  let bindings m = List.map (binding m) (keys m)

end 