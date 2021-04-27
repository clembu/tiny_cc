type 'g skill_group =
  { skill_group_bonus : int
  ; skill_group : 'g Skill.group
  ; skill_gen_skills : ('g, Skill.S.g) Skill.t list
  ; skill_spec_skills : ('g, Skill.S.s) Skill.t list
  }

type char_skills =
  { smart_skills : Skill.G.smart skill_group
  ; agile_skills : Skill.G.agile skill_group
  ; strong_skills : Skill.G.strong skill_group
  ; tough_skills : Skill.G.tough skill_group
  ; perceptive_skills : Skill.G.perceptive skill_group
  ; cute_skills : Skill.G.cute skill_group
  ; fast_skills : Skill.G.fast skill_group
  }

type 'k char_build =
  { kind : 'k Kind.t
  ; powers : 'k Power.t list
  ; flaws : 'k Flaw.t list
  ; skills : char_skills
  }

val new_build : 'k Kind.t -> 'k char_build

type char_kind = Pet of Kind.pet Kind.t | Toy of Kind.toy Kind.t

val pet : char_kind

val toy : char_kind
