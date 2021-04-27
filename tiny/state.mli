type build_state = [ `Can_add | `Cannot_add | `Added ]

val available_cp : 'k Kind.t -> 'k Build.char_build -> int

val flaw_build_state : 'k Build.char_build -> 'k Flaw.t -> build_state

val power_build_state :
  'k Kind.t -> 'k Build.char_build -> 'k Power.t -> build_state

val skill_build_state :
     'k Kind.t
  -> 'k Build.char_build
  -> 'g Skill.group
  -> 's Skill.ty
  -> ('g, 's) Skill.t
  -> build_state
