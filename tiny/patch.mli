type 'a toggle = Add of 'a | Remove of 'a

type 'g skill_ty =
  | Gen of ('g, Skill.S.g) Skill.t toggle
  | Spec of ('g, Skill.S.s) Skill.t toggle

type 'g group = Set_bonus of int | Type of 'g skill_ty

type 'k flaw = 'k Flaw.t toggle

type 'k power = 'k Power.t toggle

type skills =
  | Agile of Skill.G.agile group
  | Cute of Skill.G.cute group
  | Perceptive of Skill.G.perceptive group
  | Strong of Skill.G.strong group
  | Tough of Skill.G.tough group
  | Fast of Skill.G.fast group
  | Smart of Skill.G.smart group

type 'k t = Flaw of 'k flaw | Power of 'k power | Skills of skills

val apply : 'k Build.char_build -> 'k t -> 'k Build.char_build

val group_patch : 'g Skill.group -> 'g group -> skills
