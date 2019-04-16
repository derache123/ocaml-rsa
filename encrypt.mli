(** RSA Implementation *)

(** Type representing a public key *)
type publickey

(** Type representing a private key *)
type privatekey

(** Type representing a keyset consisting of a private key and public key.
    The first element is the public key and the second element is the
    private key. *)
type keyset

(** [encrypt m e] is the cyphertext of plaintext message [m] encrypted with
    public key [e] *)
val encrypt : string -> int -> string

(** [decrypt c d] is the decrypted plaintext message of cyphertext [c]
    decrypted with private key [d] *)
val decrypt : string -> int -> string