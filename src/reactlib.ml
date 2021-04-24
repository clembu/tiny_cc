type 'a setter = ?step:React.Step.t -> 'a -> unit

module S = struct
  type 'a component = 'a React.signal * 'a setter
end

module E = struct
  type 'a component = 'a React.event * 'a setter
end

type 'a init_value = [ `S of 'a S.component | `Default of 'a ]

let init_signal b = match b with `S s -> s | `Default v -> React.S.create v
