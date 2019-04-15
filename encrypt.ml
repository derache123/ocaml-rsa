(** [gcd a b] is the greatest common divisor of integers [a] and [b] using
    Euclid's algorithm *)
let rec gcd (a : int) (b : int) : int =
  if b = 0 then a
  else gcd b (a mod b)

(** [lcm a b] is the least common multiple of integers [a] and [b] *)
let lcm (a : int) (b : int) : int =
  if a = 0 && b = 0 then 0
  else (abs (a * b)) / (gcd a b)

(** [is_prime n] is true if integer [n] is prime, and false otherwise *)
let is_prime (n : int) : bool =
  if n <= 3 then n > 1
  else if n mod 2 = 0 || n mod 3 = 0 then false
  else let rec loop i n =
         if i * i > n then true
         else if n mod i = 0 || n mod (i + 2) = 0 then false
         else loop (i + 6) n
    in loop 5 n

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