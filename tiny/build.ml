type 'g skill_group =
  { skill_group_bonus : int
  ; skill_group : 'g Skill.group
  ; skill_gen_skills : ('g, Skill.S.g) Skill.t list
  ; skill_spec_skills : ('g, Skill.S.s) Skill.t list
  }

let new_group g =
  { skill_group_bonus = 0
  ; skill_group = g
  ; skill_gen_skills = []
  ; skill_spec_skills = []
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

let new_skills () =
  { smart_skills = new_group Skill.Smart
  ; agile_skills = new_group Skill.Agile
  ; strong_skills = new_group Skill.Strong
  ; tough_skills = new_group Skill.Tough
  ; perceptive_skills = new_group Skill.Perceptive
  ; cute_skills = new_group Skill.Cute
  ; fast_skills = new_group Skill.Fast
  }

type 'k char_build =
  { kind : 'k Kind.t
  ; powers : 'k Power.t list
  ; flaws : 'k Flaw.t list
  ; skills : char_skills
  }

let new_build k = { kind = k; powers = []; flaws = []; skills = new_skills () }

type char_kind = Pet of Kind.pet Kind.t | Toy of Kind.toy Kind.t

let pet = Pet Kind.Pet

let toy = Toy Kind.Toy
