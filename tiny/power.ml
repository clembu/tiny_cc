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

let cp (type k) (p : k t) =
  match p with
  | Pretty -> 2
  | Strength -> 1
  | Dream -> 2
  | Intimidation -> 1
  | Flight -> 2
  | Claws -> 1
  | Leap -> 1
  | HumanBFF -> 2
  | Voice_module -> 1
  | Accessories -> 1
  | Imaginary_friend -> 1
  | Authority -> 2
  | Elite_skill -> 1
  | Construction -> 4
  | Growth -> 2
  | Teddy -> 2
  | Odd_shape -> 1
  | Gigantism -> 2
  | Healing -> 2
  | Smarts -> 1
  | Licensed -> 1
  | Motorized -> 1
  | Multiplicity -> 2
  | Punching_ball -> 1
  | Slinky -> 1
  | Tough -> 2
  | Transformation -> 1
  | Malleable -> 2

let options : type k. k Kind.t -> k t list = function
  | Kind.Pet ->
      [ Dream; Strength; Pretty; Flight; Intimidation; Claws; Leap; HumanBFF ]
  | Kind.Toy ->
      [ Dream
      ; Strength
      ; Pretty
      ; Flight
      ; Intimidation
      ; Voice_module
      ; Accessories
      ; Imaginary_friend
      ; Authority
      ; Elite_skill
      ; Construction
      ; Growth
      ; Teddy
      ; Odd_shape
      ; Gigantism
      ; Healing
      ; Smarts
      ; Licensed
      ; Motorized
      ; Multiplicity
      ; Punching_ball
      ; Slinky
      ; Tough
      ; Transformation
      ; Malleable
      ]

let display : type k. k t -> string = function
  | Dream -> "Dream"
  | Strength -> "Strength"
  | Pretty -> "Pretty"
  | Flight -> "Flight"
  | Intimidation -> "Intimidation"
  | Claws -> "Claws/Fangs"
  | Leap -> "Leap"
  | HumanBFF -> "Human's Best Friend"
  | Voice_module -> "Voice Module"
  | Accessories -> "Accessories"
  | Imaginary_friend -> "Imaginary Friend"
  | Authority -> "Authority"
  | Elite_skill -> "Elite Skill"
  | Construction -> "Construction"
  | Growth -> "Growth"
  | Teddy -> "Teddy"
  | Odd_shape -> "Odd Shape"
  | Gigantism -> "Gigantism"
  | Healing -> "Healing"
  | Smarts -> "Smarts"
  | Licensed -> "Licensed"
  | Motorized -> "Motorized"
  | Multiplicity -> "Multiplicity"
  | Punching_ball -> "Punching Ball"
  | Slinky -> "Slinky"
  | Tough -> "Tough"
  | Transformation -> "Transformation"
  | Malleable -> "Malleable"
