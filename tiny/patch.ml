type 'a toggle = Add of 'a | Remove of 'a

type 'g skill_ty =
  | Gen of ('g, Skill.S.g) Skill.t toggle
  | Spec of ('g, Skill.S.s) Skill.t toggle

type 'g group = Set_bonus of int | Type of 'g skill_ty

let apply_toggle l = function
  | Add e -> if List.mem e l then l else e :: l
  | Remove e -> List.filter (Fun.negate (( = ) e)) l

let apply_group (type g) (g : g Build.skill_group) = function
  | Set_bonus n -> { g with skill_group_bonus = n }
  | Type (Gen tg) ->
      { g with skill_gen_skills = apply_toggle g.skill_gen_skills tg }
  | Type (Spec tg) ->
      { g with skill_spec_skills = apply_toggle g.skill_spec_skills tg }

type 'k flaw = 'k Flaw.t toggle

let apply_flaw (type k) (b : k Build.char_build) tg =
  { b with flaws = apply_toggle b.flaws tg }

type 'k power = 'k Power.t toggle

let apply_power (type k) (b : k Build.char_build) tg =
  { b with powers = apply_toggle b.powers tg }

type skills =
  | Agile of Skill.G.agile group
  | Cute of Skill.G.cute group
  | Perceptive of Skill.G.perceptive group
  | Strong of Skill.G.strong group
  | Tough of Skill.G.tough group
  | Fast of Skill.G.fast group
  | Smart of Skill.G.smart group

let group_patch : type g. g Skill.group -> g group -> skills = function
  | Skill.Agile -> fun x -> Agile x
  | Skill.Cute -> fun x -> Cute x
  | Skill.Perceptive -> fun x -> Perceptive x
  | Skill.Fast -> fun x -> Fast x
  | Skill.Tough -> fun x -> Tough x
  | Skill.Smart -> fun x -> Smart x
  | Skill.Strong -> fun x -> Strong x

let apply_skills s =
  let open Build in
  function
  | Agile g -> { s with agile_skills = apply_group s.agile_skills g }
  | Cute g -> { s with cute_skills = apply_group s.cute_skills g }
  | Perceptive g ->
      { s with perceptive_skills = apply_group s.perceptive_skills g }
  | Smart g -> { s with smart_skills = apply_group s.smart_skills g }
  | Tough g -> { s with tough_skills = apply_group s.tough_skills g }
  | Strong g -> { s with strong_skills = apply_group s.strong_skills g }
  | Fast g -> { s with fast_skills = apply_group s.fast_skills g }

type 'k t = Flaw of 'k flaw | Power of 'k power | Skills of skills

let apply (type k) (b : k Build.char_build) = function
  | Flaw f -> apply_flaw b f
  | Power p -> apply_power b p
  | Skills s -> { b with skills = apply_skills b.skills s }
