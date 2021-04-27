type _ t =
  | Dream : _ t
  | Strength : _ t
  | Pretty : _ t
  | Flight : _ t
  | Intimidation : _ t
  | Claws : Kind.pet t
  | Leap : Kind.pet t
  | HumanBFF : Kind.pet t
  | Voice_module : Kind.toy t
  | Accessories : Kind.toy t
  | Imaginary_friend : Kind.toy t
  | Authority : Kind.toy t
  | Elite_skill : Kind.toy t
  | Construction : Kind.toy t
  | Growth : Kind.toy t
  | Teddy : Kind.toy t
  | Odd_shape : Kind.toy t
  | Gigantism : Kind.toy t
  | Healing : Kind.toy t
  | Smarts : Kind.toy t
  | Licensed : Kind.toy t
  | Motorized : Kind.toy t
  | Multiplicity : Kind.toy t
  | Punching_ball : Kind.toy t
  | Slinky : Kind.toy t
  | Tough : Kind.toy t
  | Transformation : Kind.toy t
  | Malleable : Kind.toy t

val cp : 'k t -> int

val options : 'k Kind.t -> 'k t list

val display : 'k t -> string
