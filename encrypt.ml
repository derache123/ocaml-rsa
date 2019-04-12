(** [is_prime n] is true if integer [n] is prime, and false otherwise *)
let is_prime (n : int) : bool =
  failwith "Not yet implemented"

(** [gen_prime m n] is a random prime number p where m <= p < n
    Requires: n > m *)
let rec gen_prime (m : int) (n : int) : int =
  let num = m + (Random.int (n - m)) in
  if is_prime num then num
  else gen_prime m n

(** [mod_exp b e m] is the result of the modular exponentiation operation given
    base [b], exponent [e], and modulus [m] *)
let mod_exp (b : int) (e : int) (m : int) : int =
  failwith "Not yet implemented"

let encrypt (m : string) (e : int) : string =
  failwith "Not yet implemented"

let decrypt (c : string) (d : int) : string =
  failwith "Not yet implemented"