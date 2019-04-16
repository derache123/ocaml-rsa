type publickey = {n: int; e: int}

type privatekey = {n: int; d: int}

type keyset = {public_key: publickey; private_key: privatekey}

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
  if m = 1 then 0
  else let c = 1 in
    let rec loop i c b e m =
      if i = e then c
      else loop (i + 1) ((c * b) mod m) b e m
    in loop 0 c b e m

let gen_keys (p : int) (q : int) (e : int) : keyset =
  let n = p * q in
  let euler_totient = (p - 1) * (q - 1) in
  let carmichael_totient = lcm (p - 1) (q - 1) in
  (* calculate the modular multiplicative inverse of e using Euler's theorem *)
  let d = mod_exp e (euler_totient - 1) carmichael_totient in
  {public_key = {n = n; e = e}; private_key = {n = n; d = d}}

(** [explode s] is a list of chars of each character in string [s] *)
let explode s = List.init (String.length s) (String.get s)

let encode (s : string) : string =
  let explosion = explode s in
  let map_function c =
    string_of_int (Char.code c)
  in String.concat "" (List.map map_function explosion)

let decode (s : string) : string =
  failwith "Not yet implemented"

let encrypt (m : string) (k : publickey) : string =
  (* temporary hardcoded value for testing *)
  let plaintext = 65 in
  string_of_int (mod_exp plaintext k.e k.n)

let decrypt (c : string) (k : privatekey) : string =
  let cyphertext = int_of_string c in
  string_of_int (mod_exp cyphertext k.d k.n)
