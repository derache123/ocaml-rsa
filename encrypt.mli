(** RSA Implementation *)

(** [encrypt m e] is the cyphertext of plaintext message [m] encrypted with
    public key [e] *)
val encrypt : string -> int -> string

(** [decrypt c d] is the decrypted plaintext message of cyphertext [c]
    decrypted with private key [d] *)
val decrypt : string -> int -> string