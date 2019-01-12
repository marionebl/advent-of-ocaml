type claim = < 
  id: string;
  x: int;
  y: int;
  width: int;
  height: int;
  contains: int -> int -> bool;
>

val create_claim : string -> int list -> claim

val parse : string -> claim