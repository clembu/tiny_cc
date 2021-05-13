type build_state = [ `Can_add | `Cannot_add | `Added ]

type build_group_state = [ `Can_change | `Cannot_change ]

val available_cp : 'k Build.char_build -> int

val skills_cost : ?with_bonus:bool -> 'k Build.char_build -> int

val flaws_bonus_cp : 'k Build.char_build -> int

val powers_cost : 'k Build.char_build -> int

val flaw_build_state : 'k Build.char_build -> 'k Flaw.t -> build_state

val power_build_state :
  'k Kind.t -> 'k Build.char_build -> 'k Power.t -> build_state

val skill_group_build_state :
  'k Kind.t -> 'k Build.char_build -> 'g Skill.group -> int -> build_group_state

val skill_build_state :
     'k Kind.t
  -> 'k Build.char_build
  -> 'g Skill.group
  -> 's Skill.ty
  -> ('g, 's) Skill.t
  -> build_state
