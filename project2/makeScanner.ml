(*

  SCAN. Token scanner for the Pure Lisp interpreter.

    James Moen
    17 Nov 19

*)

(* TOKEN. A token. *)

type token =
  CloseParenToken |
  EndToken |
  NumberToken of int |
  OpenParenToken |
  SymbolToken of string ;;

(* MAKE SCANNER. Return a function NEXT TOKEN that reads TOKENs from a file
   with pathname PATH. OCaml RAISEs an exception if there's no such file. *)

let makeScanner path =

(* INPUT. Read chars from this channel. *)

  let input = (open_in path)
  in

(* CH. The char most recently read from INPUT. *)

  let ch = ref ' '
  in

(* NEXT CHAR. Advance CH to the next char from INPUT, or to '\000' if we're at
   the end of INPUT. *)

  let nextChar () =
    try ch := input_char input
    with End_of_file -> ch := '\000'
  in

  let nextEndToken () =
    EndToken
  in

  let nextOpenParenToken () =
    nextChar ();
    OpenParenToken
  in

  let nextCloseParenToken () =
    nextChar ();
    CloseParenToken
  in

  let rec nextNumberToken prefix =
    match !ch with
      '\000' -> let n = int_of_string prefix
                  in NumberToken n |
      '\n'   -> let n = int_of_string prefix
                  in NumberToken n |
      ' '    -> let n = int_of_string prefix
                  in NumberToken n |
      '('    -> let n = int_of_string prefix
                  in NumberToken n |
      ')'    -> let n = int_of_string prefix
                  in NumberToken n |
      _      -> let temp_string = Char.escaped !ch
                  in let temp_prefix = prefix ^ temp_string
                    in nextChar ();
                       nextNumberToken temp_prefix
   in

   let rec nextSymbolToken prefix =
     match !ch with
       '\000' -> let s = prefix
                   in SymbolToken s |
       '\n'   -> let s = prefix
                   in SymbolToken s |
       ' '    -> let s = prefix
                   in SymbolToken s |
       '('    -> let s = prefix
                   in SymbolToken s |
       ')'    -> let s = prefix
                   in SymbolToken s |
        _     -> let temp_string = Char.escaped !ch
                   in let temp_prefix = prefix ^ temp_string
                     in nextChar ();
                        nextSymbolToken temp_prefix
    in

    let nextNumberOrSymbolToken () =
      nextChar();
      match !ch with
      '0' -> nextNumberToken "-" |
      '1' -> nextNumberToken "-" |
      '2' -> nextNumberToken "-" |
      '3' -> nextNumberToken "-" |
      '4' -> nextNumberToken "-" |
      '5' -> nextNumberToken "-" |
      '6' -> nextNumberToken "-" |
      '7' -> nextNumberToken "-" |
      '8' -> nextNumberToken "-" |
      '9' -> nextNumberToken "-" |
       _  -> nextSymbolToken "-"
    in

    let rec nextToken () =
      match !ch with
      '\000' -> nextEndToken () |

      ' '    -> nextChar ();
                nextToken () |

      '\n'   -> nextChar ();
                nextToken () |

      '('    -> nextOpenParenToken () |

      ')'    -> nextCloseParenToken () |

      '-'    -> nextNumberOrSymbolToken () |

      '0' -> nextNumberToken "" |
      '1' -> nextNumberToken "" |
      '2' -> nextNumberToken "" |
      '3' -> nextNumberToken "" |
      '4' -> nextNumberToken "" |
      '5' -> nextNumberToken "" |
      '6' -> nextNumberToken "" |
      '7' -> nextNumberToken "" |
      '8' -> nextNumberToken "" |
      '9' -> nextNumberToken "" |

       _  -> nextSymbolToken ""

(* Finally initialize CH, and return NEXT TOKEN as promised. *)

    in nextChar () ;
      nextToken ;;

(* NEXT TOKENS. Test the token scanner by reading tokens from the file whose
   pathname is PATH, and writing one-line descriptions of each token. *)

let nextTokens path =
  let nextToken = makeScanner path
  in let rec nextTokensing token =
       match token
       with CloseParenToken ->
              Printf.printf "CloseParenToken\n" ;
              nextTokensing (nextToken ()) |

            EndToken ->
              Printf.printf "EndToken\n" |

            NumberToken number ->
              Printf.printf "NumberToken %i\n" number ;
              nextTokensing (nextToken ()) |

            OpenParenToken ->
              Printf.printf "OpenParenToken\n" ;
              nextTokensing (nextToken ()) |

            SymbolToken string ->
              Printf.printf "SymbolToken \"%s\"\n" string ;
              nextTokensing (nextToken ())

     in nextTokensing (nextToken ()) ;;

(* Try reading tokens from EXCLAIM. *)

nextTokens "exclaim" ;;

(* You should see the following printed:

   OpenParenToken
   SymbolToken "define"
   SymbolToken "!"
   OpenParenToken
   SymbolToken "lambda"
   OpenParenToken
   SymbolToken "n"
   CloseParenToken
   OpenParenToken
   SymbolToken "if"
   OpenParenToken
   SymbolToken "="
   SymbolToken "n"
   NumberToken 0
   CloseParenToken
   NumberToken 1
   OpenParenToken
   SymbolToken "∗"
   SymbolToken "n"
   OpenParenToken
   SymbolToken "!"
   OpenParenToken
   SymbolToken "−"
   SymbolToken "n"
   NumberToken 1
   CloseParenToken
   CloseParenToken
   CloseParenToken
   CloseParenToken
   CloseParenToken
   CloseParenToken
   EndToken

*)

(* Output from test case:

type token =
  CloseParenToken
  | EndToken
  | NumberToken of int
  | OpenParenToken
  | SymbolToken of string
val makeScanner : string -> unit -> token = <fun>
val nextTokens : string -> unit = <fun>
OpenParenToken
SymbolToken "define"
SymbolToken "!"
OpenParenToken
SymbolToken "lambda"
OpenParenToken
SymbolToken "n"
CloseParenToken
OpenParenToken
SymbolToken "if"
OpenParenToken
SymbolToken "="
SymbolToken "n"
NumberToken 0
CloseParenToken
NumberToken 1
OpenParenToken
SymbolToken "*"
SymbolToken "n"
OpenParenToken
SymbolToken "!"
OpenParenToken
SymbolToken "-"
SymbolToken "n"
NumberToken 1
CloseParenToken
CloseParenToken
CloseParenToken
CloseParenToken
CloseParenToken
CloseParenToken
EndToken
- : unit = ()

*)
