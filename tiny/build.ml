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

let group_bonus (type k g) (g : g Skill.group) ({ skills; _ } : k char_build) =
  match g with
  | Skill.Agile -> skills.agile_skills.skill_group_bonus
  | Skill.Tough -> skills.tough_skills.skill_group_bonus
  | Skill.Smart -> skills.smart_skills.skill_group_bonus
  | Skill.Fast -> skills.fast_skills.skill_group_bonus
  | Skill.Cute -> skills.cute_skills.skill_group_bonus
  | Skill.Perceptive -> skills.perceptive_skills.skill_group_bonus
  | Skill.Strong -> skills.strong_skills.skill_group_bonus

let max_group_bonus (type k) (k : k Kind.t) =
  match k with Kind.Pet -> 3 | Kind.Toy -> 2

let max_power_cost (type k) (k : k Kind.t) =
  match k with Kind.Pet -> 3 | Kind.Toy -> 4

let min_skills_investment (type k) (k : k Kind.t) =
  match k with Kind.Pet -> 8 | Kind.Toy -> 10
