type 'a setter = ?step:React.Step.t -> 'a -> unit

module S : sig
  type 'a component = 'a React.signal * 'a setter
end

module E : sig
  type 'a component = 'a React.event * 'a setter
end

type 'a init_value = [ `S of 'a S.component | `Default of 'a ]

val init_signal : 'a init_value -> 'a S.component
