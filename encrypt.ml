(* open Big_int *)

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

(** Helper type representing local variables in function [mod_inv] *)
type mod_inv_vars = {u1: int; u3: int; v1: int; v3: int; t1: int; t3: int; iter: int}

(** [mod_inv u v] is the modular inverse of [u] mod [v] *)
let mod_inv (u: int) (v: int) : int =
  let vars = {u1 = 1; u3 = u; v1 = 0; v3 = v; t1 = 0; t3 = 0; iter = 1} in
  let rec loop x =
    if x.v3 = 0 then x
    else loop {u1 = x.v1; u3 = x.v3; v1 = x.u1 + ((x.u3 / x.v3) * x.v1); v3 = (x.u3 mod x.v3); t1 = x.u1 + ((x.u3 / x.v3) * x.v1); t3 = (x.u3 mod x.v3); iter = -(x.iter)}
  in let y = loop vars in
  if y.u3 <> 1 then 0
  else if y.iter < 0 then v - y.u1
  else y.u1

let gen_keys (p : int) (q : int) (e : int) : keyset =
  let n = p * q in
  let totient = (p - 1) * (q - 1) in
  (* calculate the modular inverse of e mod totient(n) *)
  let d = mod_inv e totient in
  {public_key = {n = n; e = e}; private_key = {n = n; d = d}}

(** [explode s] is a list of chars of each character in string [s] *)
let explode s = List.init (String.length s) (String.get s)

(** [encode_char c] is an integer encoding of character [c].
    The encoding scheme is the character's ASCII code - 23.
    The space character will be encoded with a padding: 09.
    Requires: [c] has an ASCII code between 32 and 122, inclusive. *)
let encode_char (c : char) : string =
  match c with
  | ' ' -> "09"
  | _ -> string_of_int (Char.code c - 23)

(** [encode s] is an integer encoding of string [s]. *)
(* let encode (s : string) : Big_int.big_int =
   Big_int.big_int_of_string (String.concat "" (List.map encode_char (explode s))) *)
let encode (s : string) : int =
  int_of_string (String.concat "" (List.map encode_char (explode s)))

let decode_char (s : string) : char =
  Char.chr ((int_of_string s) + 23)

(* let decode (n : Big_int.big_int) : string =
   let explosion = explode (Big_int.string_of_big_int n) in
   let rec helper nums chars = 
    if nums = [] then String.concat "" (List.rev chars)
    else match nums with
      | a::b::t -> helper t ((Char.escaped (decode_char  (String.concat "" [(Char.escaped a); (Char.escaped b)])))::chars)
      | _ -> failwith "impossibru"
   in helper explosion [] *)
let decode (n : int) : string =
  let explosion = explode (string_of_int n) in
  let rec helper nums chars = 
    if nums = [] then String.concat "" (List.rev chars)
    else match nums with
      | a::b::t -> helper t ((Char.escaped (decode_char  (String.concat "" [(Char.escaped a); (Char.escaped b)])))::chars)
      | _ -> failwith "impossibru"
  in helper explosion []

let encrypt_int (m : int) (k : publickey) : int =
  mod_exp m k.e k.n

let encrypt (m : string) (k : publickey) : string =
  let plaintext = encode m in
  string_of_int (mod_exp plaintext k.e k.n)

let decrypt_int (c : int) (k : privatekey) : int =
  mod_exp c k.d k.n

let decrypt (c : string) (k : privatekey) : string =
  let cyphertext = int_of_string c in
  decode (mod_exp cyphertext k.d k.n)
