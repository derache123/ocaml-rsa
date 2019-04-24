(** RSA Implementation *)

open Big_int

(** Type representing a public key *)
type publickey

(** Type representing a private key *)
type privatekey

(** Type representing a keyset consisting of a private key and public key. *)
type keyset

(** [string_of_publickey k] is a string representation of public key [k].
    The string representation is the concatenation of the n and e portions
    of the key, separated by the '$' character. *)
val string_of_publickey : publickey -> string

(** [publickey_of_string s] is the public key of the string representation [s]
    of some public key.
    Requires: [s] is a valid representation of a public key. *)
val publickey_of_string : string -> publickey

(** [string_of_privatekey k] is a string representation of private key [k].
    The string representation is the concatenation of the n and d portions
    of the key, separated by the '%' character. *)
val string_of_privatekey : privatekey -> string

(** [privatekey_of_string s] is the private key of the string representation [s]
    of some private key.
    Requires: [s] is a valid representation of a private key. *)
val privatekey_of_string : string -> privatekey

(** [gen_keys n e] is an RSA keyset generated using primes of length [n] digits
    and exponent [e]. *)
val gen_keys : int -> int -> int -> keyset

(** [encrypt m k] is the cyphertext of plaintext message [m] encrypted with
    the public key represented by string [k]. *)
val encrypt : string -> string -> string

(** [decrypt c k] is the decrypted plaintext message of cyphertext [c]
    decrypted with the private key represented by string [k]. *)
val decrypt : string -> string -> string