
(** [gcc args] reads [args] and extracts the input C file (only one is
    expected) and cpp-relevant options -include, -I, -D, and -U. The
    input C file is preprocessed and the result stored under an {e _eba}
    sub-folder in the working directory.

    @return Path to preprocessed file for analysis. *)
val gcc : string list -> string