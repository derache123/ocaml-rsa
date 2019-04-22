(* open Big_int *)
#load "nums.cma" ;;

type publickey = {n: Big_int.big_int; e: int}

type privatekey = {n: Big_int.big_int; d: Big_int.big_int}

type keyset = {public_key: publickey; private_key: privatekey}

(** override modulo operator *)
let (mod) x y =
  let result = x mod y in
  if result >= 0 then result
  else result + y

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
let is_prime (n : Big_int.big_int) : bool =
  if n <= (Big_int.big_int_of_int 3) then n > (Big_int.big_int_of_int 1)
  else if (Big_int.mod_big_int n (Big_int.big_int_of_int 2)) = Big_int.zero_big_int || (Big_int.mod_big_int n (Big_int.big_int_of_int 3)) = Big_int.zero_big_int then false
  else let rec loop i n =
         if (Big_int.square_big_int i) > n then true
         else if (Big_int.mod_big_int n i) = Big_int.zero_big_int || (Big_int.mod_big_int n (Big_int.add_int_big_int 2 i)) = Big_int.zero_big_int then false
         else loop (Big_int.add_int_big_int 6 i) n
    in loop (Big_int.big_int_of_int 5) n

(** [gen_prime m n] is a random prime number p where m <= p < n
    Requires: n > m *)
(* let rec gen_prime (m : int) (n : int) : int =
   let num = m + (Random.int (n - m)) in
   if is_prime num then num
   else gen_prime m n *)

(** [mod_exp b e m] is the result of the modular exponentiation operation given
    base [b], exponent [e], and modulus [m] *)
(* let mod_exp (b : int) (e : int) (m : int) : int =
   if m = 1 then 0
   else let c = 1 in
    let rec loop i c b e m =
      if i = e then c
      else loop (i + 1) ((c * b) mod m) b e m
    in loop 0 c b e m *)

(** [mod_exp b e m] is the result of the modular exponentiation operation given
    base [b], exponent [e], and modulus [m] *)
let mod_exp (b : Big_int.big_int) (e : Big_int.big_int) (m : Big_int.big_int) : Big_int.big_int =
  if Big_int.eq_big_int m Big_int.unit_big_int then Big_int.zero_big_int
  else let rec loop i c b e m =
         if Big_int.eq_big_int i (Big_int.pred_big_int e) then c
         else loop (Big_int.succ_big_int i) (Big_int.mod_big_int (Big_int.mult_big_int c b) m) b e m
    in loop Big_int.zero_big_int Big_int.unit_big_int b e m

(** Helper type representing local variables in function [mod_inv] *)
type mod_inv_vars = {u1: Big_int.big_int; u3: Big_int.big_int; v1: Big_int.big_int; v3: Big_int.big_int; t1: Big_int.big_int; t3: Big_int.big_int; iter: int}

(** [mod_inv u v] is the modular inverse of [u] mod [v] *)
let mod_inv (u: Big_int.big_int) (v: Big_int.big_int) : Big_int.big_int =
  let vars = {u1 = Big_int.unit_big_int; u3 = u; v1 = Big_int.zero_big_int; v3 = v; t1 = Big_int.zero_big_int; t3 = Big_int.zero_big_int; iter = 1} in
  let rec loop x =
    if x.v3 = Big_int.zero_big_int then x
    else loop {u1 = x.v1; u3 = x.v3; v1 = Big_int.add_big_int x.u1 (Big_int.mult_big_int (Big_int.div_big_int x.u3 x.v3) x.v1); v3 = Big_int.mod_big_int x.u3 x.v3; t1 = Big_int.add_big_int x.u1 (Big_int.mult_big_int (Big_int.div_big_int x.u3 x.v3) x.v1); t3 = Big_int.mod_big_int x.u3 x.v3; iter = -(x.iter)}
  in let y = loop vars in
  if y.u3 <> Big_int.unit_big_int then Big_int.zero_big_int
  else if y.iter < 0 then Big_int.sub_big_int v y.u1
  else y.u1

(* TODO: randomly generate p and q within this function, changing first two
   parameters to length bounds, also make sure totient is coprime to e or else
   p and q should be regenerated *)
let gen_keys (m : Big_int.big_int) (n : Big_int.big_int) (e : int) : keyset =
  let rec gen_primes (m : Big_int.big_int) (n : Big_int.big_int) e : (Big_int.big_int * Big_int.big_int) =
    let nums = ((Big_int.add_int_big_int (Random.int (Big_int.int_of_big_int (Big_int.sub_big_int n m))) m), (Big_int.add_int_big_int (Random.int (Big_int.int_of_big_int (Big_int.sub_big_int n m))) m)) in
    let totient = Big_int.mult_big_int (Big_int.pred_big_int (fst nums)) (Big_int.pred_big_int(snd nums)) in
    if is_prime (fst nums) && is_prime (snd nums) then
      if (Big_int.gcd_big_int e totient) = Big_int.unit_big_int then nums
      else gen_primes m n e
    else gen_primes m n e
  in let primes = gen_primes m n (Big_int.big_int_of_int e) in
  print_endline ((Big_int.string_of_big_int (fst primes)) ^ " " ^ (Big_int.string_of_big_int (snd primes)));
  let n = Big_int.mult_big_int (fst primes) (snd primes) in
  let totient = Big_int.mult_big_int (Big_int.pred_big_int (fst primes)) (Big_int.pred_big_int (snd primes)) in
  (* calculate the modular inverse of e mod totient(n) *)
  let d = mod_inv (Big_int.big_int_of_int e) totient in
  (* the if statement is for testing *)
  if Big_int.mod_big_int (Big_int.mult_int_big_int e d) totient = Big_int.unit_big_int then
    {public_key = {n = n; e = e}; private_key = {n = n; d = d}}
  else failwith "e * d mod r != 1"

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
let encode (s : string) : Big_int.big_int =
  Big_int.big_int_of_string (String.concat "" (List.map encode_char (explode s)))

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
let decode (n : Big_int.big_int) : string =
  let explosion = explode (Big_int.string_of_big_int n) in
  let rec helper nums chars = 
    if nums = [] then String.concat "" (List.rev chars)
    else match nums with
      | a::b::t -> helper t ((Char.escaped (decode_char  (String.concat "" [(Char.escaped a); (Char.escaped b)])))::chars)
      | _ -> failwith "impossibru"
  in helper explosion []

let encrypt_int (m : Big_int.big_int) (k : publickey) : Big_int.big_int =
  mod_exp m (Big_int.big_int_of_int k.e) k.n

let encrypt (m : string) (k : publickey) : string =
  let plaintext = encode m in
  if String.length (Big_int.string_of_big_int plaintext) > String.length (Big_int.string_of_big_int k.n) then
    failwith "string is too long"
  else
    Big_int.string_of_big_int (mod_exp plaintext (Big_int.big_int_of_int k.e) k.n)

let decrypt_int (c : Big_int.big_int) (k : privatekey) : Big_int.big_int =
  mod_exp c k.d k.n

let decrypt (c : string) (k : privatekey) : string =
  let cyphertext = Big_int.big_int_of_string c in
  decode (mod_exp cyphertext k.d k.n)
