(* And indeed there will be time
 * To wonder, "Do I dare?" and, "Do I dare?"
 * Time to turn back and descend the stair,
 * With a bald spot in the middle of my hair —
 *)

signature PARSER = sig
  eqtype tok
  type 'a result = ('a * tok list) option
  type 'a parser = tok list -> 'a result
  val literally : tok -> tok parser
  val concatenate : 'a parser -> 'b parser -> ('a * 'b) parser
  val either : 'a parser -> 'a parser -> 'a parser
  val maybe : 'a parser -> 'a option parser
  val many : 'a parser -> 'a list parser

  val eof : unit parser
  val void : 'a list parser
  val any : tok parser

  val process : 'a parser -> ('a -> 'b) -> 'b parser
  val many1 : 'a parser -> 'a list parser
  val choose : 'a parser list -> 'a parser
  val chain : 'a parser list -> 'a list parser
end

functor Parser (eqtype token) : PARSER = struct
  type tok = token
  type 'a result = ('a * tok list) option
  type 'a parser = tok list -> 'a result

  fun literally (c : tok) : tok parser =
    fn inp =>
      case inp of
        [] => NONE
      | a::at =>
          if a = c then
            SOME (c, at)
          else
            NONE

  fun concatenate (a : 'a parser) (b : 'b parser) : ('a * 'b) parser =
    fn inp =>
      case a inp of
        NONE => NONE
      | SOME (p, inp') =>
        case b inp' of
          NONE => NONE
        | SOME (p', inp'') => SOME ((p, p'), inp'')

  fun either (a : 'a parser) (b : 'a parser) : 'a parser =
    fn inp =>
      case a inp of
        NONE => b inp
      | x => x

  fun maybe (a : 'a parser) : 'a option parser =
    fn inp =>
      case a inp of
        NONE => SOME (NONE, inp)
      | SOME (p, inp') => SOME (SOME p, inp')

  fun many (a : 'a parser) : 'a list parser =
    fn inp =>
      case a inp of
        NONE => SOME ([], inp)
      | SOME (p, inp') =>
        let val (p', inp'') = valOf (many a inp') in
          SOME (p :: p', inp'')
        end

  val eof : unit parser =
    fn inp =>
      case inp of
        [] => SOME ((), [])
      | x => NONE

  val void : 'a list parser =
    fn inp => SOME ([], inp)

  val any : tok parser =
    fn inp =>
      case inp of
        [] => NONE
      | x::xs => SOME (x, xs)

  fun process (a : 'a parser) (f : 'a -> 'b) : 'b parser =
    fn inp =>
    case a inp of
      NONE => NONE
    | SOME (p, r') => SOME (f p, r')

  fun many1 (a : 'a parser) : 'a list parser =
    process (concatenate a (many a)) (op::)

  fun choose (ls : 'a parser list) : 'a parser =
    case ls of
      [] => (fn inp => NONE)
    | a::[] => a
    | a::(b::[]) => either a b
    | a::rs => either a (choose rs)

  fun chain (ls : 'a parser list) : 'a list parser =
    case ls of
      [] => void
    | a::rs => process (concatenate a (chain rs)) (op::)

end;


structure StringParser = Parser(type token = char)

local
  open StringParser
in
  val $ = fn str =>
    process (chain (map literally (String.explode str))) String.implode

  infixr 6 || fun a || b = either a b
  infix  7 @@ fun a @@ b = process a b
  infixr 8 ~  fun a ~ b = concatenate a b

  val wsc = choose (map $ [" ", "\t", "\n"])
  val ws  = many  wsc @@ (fn _ => ())
  val ws1 = many1 wsc @@ (fn _ => ())

  val digit = choose (map $ ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"])
  val num_int' = many1 digit @@ String.concat
  val num_int = many1 digit @@ String.concat @@ Int.fromString @@ valOf

end;
