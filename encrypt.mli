(** RSA Implementation *)

(** Type representing a public key *)
type publickey

(** Type representing a private key *)
type privatekey

(** Type representing a keyset consisting of a private key and public key. *)
type keyset

(** [gen_keys p q e] is an RSA keyset generated using primes [p] and [q] and
    exponent [e] *)
val gen_keys : int -> int -> int -> keyset

(** [encrypt m k] is the cyphertext of plaintext message [m] encrypted with
    public key [k] *)
val encrypt : string -> publickey -> string

(** [decrypt c k] is the decrypted plaintext message of cyphertext [c]
    decrypted with private key [k] *)
val decrypt : string -> privatekey -> string