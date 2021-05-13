type pet

type toy

type _ t = Pet : pet t | Toy : toy t

let display : type k. k t -> string = function Pet -> "Pet" | Toy -> "Toy"
