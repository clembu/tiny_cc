type build_state = [ `Can_add | `Cannot_add | `Added ]

let valid_power : type k. k Kind.t -> k Build.char_build -> k Power.t -> bool =
 fun k b p ->
  match k with
  | Kind.Pet ->
      if List.mem Flaw.Harmless b.flaws then p <> Power.Claws else true
  | Kind.Toy -> true

let valid_skill :
    type k g s.
       k Kind.t
    -> k Build.char_build
    -> g Skill.group
    -> s Skill.ty
    -> (g, s) Skill.t
    -> bool =
 fun k b g _t _s ->
  match k with
  | Kind.Toy ->
      if List.mem Flaw.First_age b.flaws then
        match g with Skill.Smart -> false | _ -> true
      else true
  | Kind.Pet -> true

let skills_group_cost :
    type k g. k Kind.t -> k Build.char_build -> g Build.skill_group -> int =
 fun k b g ->
  let gen_cost =
    List.length
      (List.filter (valid_skill k b g.skill_group Skill.Gen) g.skill_gen_skills)
  in
  if gen_cost = 0 then 0
  else
    gen_cost
    + 2
      * List.length
          (List.filter
             (valid_skill k b g.skill_group Skill.Spec)
             g.skill_spec_skills )

let skills_cost : type k. k Kind.t -> k Build.char_build -> int =
 fun k b ->
  skills_group_cost k b b.skills.fast_skills
  + skills_group_cost k b b.skills.cute_skills
  + skills_group_cost k b b.skills.smart_skills
  + skills_group_cost k b b.skills.tough_skills
  + skills_group_cost k b b.skills.strong_skills
  + skills_group_cost k b b.skills.perceptive_skills
  + skills_group_cost k b b.skills.agile_skills

let available_cp : type k. k Kind.t -> k Build.char_build -> int =
 fun k b ->
  let bonus = List.fold_left ( + ) 0 @@ List.map Flaw.cp b.flaws in
  let pool = 20 + bonus in
  let pool =
    List.fold_left ( - ) pool @@ List.map Power.cp
    @@ List.filter (valid_power k b) b.powers
  in
  pool - skills_cost k b

let flaw_build_state : type k. k Build.char_build -> k Flaw.t -> build_state =
 fun b f ->
  if List.mem f b.flaws then `Added
  else
    match b.flaws with
    | [] -> `Can_add
    | _ :: _ :: _ -> `Cannot_add
    | _ -> `Can_add

let power_build_state :
    type k. k Kind.t -> k Build.char_build -> k Power.t -> build_state =
 fun k b p ->
  if List.mem p b.powers then `Added
  else
    let current_power_cost =
      List.fold_left ( + ) 0 @@ List.map Power.cp b.powers
    in
    let av_cp =
      min (available_cp k b)
        ( (match k with Kind.Pet -> 3 | Kind.Toy -> 4)
        - current_power_cost - Power.cp p )
    in
    if av_cp < 0 then `Cannot_add
    else if valid_power k b p then `Can_add
    else `Cannot_add

let skill_build_state :
    type k g s.
       k Kind.t
    -> k Build.char_build
    -> g Skill.group
    -> s Skill.ty
    -> (g, s) Skill.t
    -> build_state =
 fun k b g t s ->
  let open Build in
  let group_has_skill g =
    match t with
    | Skill.Gen -> List.mem s g.skill_gen_skills
    | Skill.Spec -> List.mem s g.skill_spec_skills
  in
  let group_has_gen_skill () =
    match g with
    | Skill.Agile -> List.length b.skills.agile_skills.skill_gen_skills > 0
    | Skill.Perceptive ->
        List.length b.skills.perceptive_skills.skill_gen_skills > 0
    | Skill.Cute -> List.length b.skills.cute_skills.skill_gen_skills > 0
    | Skill.Fast -> List.length b.skills.fast_skills.skill_gen_skills > 0
    | Skill.Tough -> List.length b.skills.tough_skills.skill_gen_skills > 0
    | Skill.Strong -> List.length b.skills.strong_skills.skill_gen_skills > 0
    | Skill.Smart -> List.length b.skills.smart_skills.skill_gen_skills > 0
  in
  let has_skill =
    match g with
    | Skill.Agile -> group_has_skill b.skills.agile_skills
    | Skill.Perceptive -> group_has_skill b.skills.perceptive_skills
    | Skill.Cute -> group_has_skill b.skills.cute_skills
    | Skill.Fast -> group_has_skill b.skills.fast_skills
    | Skill.Tough -> group_has_skill b.skills.tough_skills
    | Skill.Strong -> group_has_skill b.skills.strong_skills
    | Skill.Smart -> group_has_skill b.skills.smart_skills
  in
  if has_skill then `Added
  else if available_cp k b <= 0 then `Cannot_add
  else
    match t with
    | Skill.Gen -> `Can_add
    | Skill.Spec -> if group_has_gen_skill () then `Can_add else `Cannot_add
