type pet

type toy

type _ t = Pet : pet t | Toy : toy t

val display : 'k t -> string
