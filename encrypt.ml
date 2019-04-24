type publickey = {n: string; e: string}

type privatekey = {n: string; d: string}

type keyset = {public_key: publickey; private_key: privatekey}

let publickey_from_keyset ks =
  ks.public_key

let privatekey_from_keyset ks =
  ks.private_key

let string_of_publickey (k : publickey) =
  k.n ^ "$" ^ k.e

let publickey_of_string s =
  match String.split_on_char '$' s with
  | a::b::[] -> {n = a; e = b}
  | _ -> failwith "invalid public key format"

let string_of_privatekey (k : privatekey) =
  k.n ^ "%" ^ k.d

let privatekey_of_string s =
  match String.split_on_char '%' s with
  | a::b::[] -> {n = a; d = b}
  | _ -> failwith "invalid private key format"

(* Define some operators and values for big_int.
   All big_int operators will begin with $. *)
let ($+) x y =
  Big_int.add_big_int x y

let ($-) x y =
  Big_int.sub_big_int x y

let ($*) x y =
  Big_int.mult_big_int x y

let ($/) x y =
  Big_int.div_big_int x y

let ($%) x y =
  Big_int.mod_big_int x y

let ($=) x y =
  Big_int.eq_big_int x y

let ($>) x y =
  Big_int.gt_big_int x y

let ($>=) x y =
  Big_int.ge_big_int x y

let ($<) x y =
  Big_int.lt_big_int x y

let ($<=) x y =
  Big_int.le_big_int x y

let ($>>) x y =
  Big_int.shift_right_big_int x y

let ($<<) x y =
  Big_int.shift_left_big_int x y

let zero = Big_int.zero_big_int

let one = Big_int.unit_big_int

let big x = Big_int.big_int_of_int x

let bsucc x = Big_int.succ_big_int x

let bpred x = Big_int.pred_big_int x

let gcd x y = Big_int.gcd_big_int x y

(** [mod_exp b e m] is the result of the modular exponentiation operation given
    base [b], exponent [e], and modulus [m]. *)
let mod_exp b e m =
  if m $= one then zero
  else let rec loop r b e m =
         if e $<= zero then r
         else let exp = e $>> 1 in
           let base = (Big_int.square_big_int b) $% m in
           if one $= (e $% (big 2))
           then loop ((r $* b) $% m) base exp m
           else loop r base exp m
    in loop one (b $% m) e m

(** [is_prime n] is true (most of the time) if integer [n] is prime,
    and false otherwise.
    This is a probabilistic primality test using Fermat's test. *)
let is_prime n =
  if (n $% (big 2)) $= one
  then if (mod_exp (big 2) (bpred n) n) $= one
    then true
    else false
  else false

(** Helper type representing local variables in function [mod_inv]. *)
type mod_inv_vars = {u1: Big_int.big_int; u3: Big_int.big_int;
                     v1: Big_int.big_int; v3: Big_int.big_int;
                     t1: Big_int.big_int; t3: Big_int.big_int; iter: int}

(** [mod_inv u v] is the modular inverse of [u] mod [v]. *)
let mod_inv u v =
  let vars = {u1 = one; u3 = u; v1 = zero;
              v3 = v; t1 = zero; t3 = zero; iter = 1} in
  let rec loop x =
    if x.v3 $= zero then x
    else loop {u1 = x.v1; u3 = x.v3; v1 = x.u1 $+ ((x.u3 $/ x.v3) $* x.v1);
               v3 = x.u3 $% x.v3; t1 = x.u1 $+ ((x.u3 $/ x.v3) $* x.v1);
               t3 = x.u3 $% x.v3; iter = -(x.iter)}
  in let y = loop vars in
  if not (y.u3 $= one) then zero
  else if y.iter < 0 then v $- y.u1
  else y.u1

(** [gen_random_number n] is a random number with n digits. 
    Requires: n >= 1*)
let gen_random_number n =
  let rec helper m str =
    if m = 0 then Big_int.big_int_of_string str
    else helper (m - 1) (str ^ (string_of_int (Random.int 10)))
  in helper (n - 1) (string_of_int ((Random.int 9) + 1))

let gen_keys n e =
  let rec gen_primes (n : int) e : (Big_int.big_int * Big_int.big_int) =
    let nums = ((gen_random_number n), (gen_random_number n)) in
    let totient = (bpred (fst nums)) $* (bpred (snd nums)) in
    if is_prime (fst nums) && is_prime (snd nums) then
      if (gcd e totient) $= one then nums
      else gen_primes n e
    else gen_primes n e
  in let primes = gen_primes n (big e) in
  let n = (fst primes) $* (snd primes) in
  let totient = (bpred (fst primes)) $* (bpred (snd primes)) in
  (* calculate the modular inverse of e mod totient(n) *)
  let d = mod_inv (big e) totient in
  {public_key = {n = (Big_int.string_of_big_int n);
                 e = (string_of_int e)};
   private_key = {n = (Big_int.string_of_big_int n);
                  d = (Big_int.string_of_big_int d)}}

(** [explode s] is a list of chars of each character in string [s]. *)
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
let encode (s : string) : Big_int.big_int =
  Big_int.big_int_of_string
    (String.concat "" (List.map encode_char (explode s)))

(** [decode_char s] is the character represented by the integer contained
    in string [s]. *)
let decode_char (s : string) : char =
  Char.chr ((int_of_string s) + 23)

(** [decode n] is the string represented by the big integer [n]. *)
let decode (n : Big_int.big_int) : string =
  let explosion = explode (Big_int.string_of_big_int n) in
  let rec helper nums chars = 
    if nums = [] then String.concat "" (List.rev chars)
    else match nums with
      | a::b::t -> helper t ((Char.escaped
                                (decode_char (String.concat ""
                                                [(Char.escaped a);
                                                 (Char.escaped b)])))
                             ::chars)
      | _ -> failwith "impossibru"
  in helper explosion []

let encrypt (m : string) (k : string) : string =
  let key = publickey_of_string k in
  let plaintext = encode m in
  if String.length (Big_int.string_of_big_int plaintext) > String.length key.n
  then
    failwith "string is too long"
  else
    Big_int.string_of_big_int (mod_exp plaintext
                                 (Big_int.big_int_of_string key.e)
                                 (Big_int.big_int_of_string key.n))

let decrypt (c : string) (k : string) : string =
  let key = privatekey_of_string k in
  let cyphertext = Big_int.big_int_of_string c in
  decode (mod_exp cyphertext (Big_int.big_int_of_string key.d)
            (Big_int.big_int_of_string key.n))
