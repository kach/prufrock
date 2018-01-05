(* I have measured out my life with coffee spoons... *)

datatype REPLCmd
  = RComment
  | RCheck of Term
  | RShow of Name
  | RAssert of Name * Term
  | RDefine of Name * Term
  | RDump
  | RRand
  | RConflict

local
  open StringParser
in
  val p_namechar = choose
    (map literally
      (String.explode "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"))
  val p_name = many1 p_namechar @@ String.implode

  fun p_term str =
    (  p_prop
    || p_type
    || p_pi
    || p_lambda
    || p_app
    || p_var
    || p_parens
    )  str
  and p_prop str = ($"Prop" @@ (fn _ => TProp)) str
  and p_type str = ($"Type" ~ ws1 ~ num_int
    @@ (fn (_, (_, n)) => TType n)) str
  and p_var str = ($"$" ~ p_name @@ (fn (_, n) => TVar n)) str
  and p_pi str = (
      ( $"pi"
      ~ ws1
      ~ p_name
      ~ ws
      ~ $":"
      ~ ws
      ~ p_term
      ~ ws
      ~ $"=>"
      ~ ws
      ~ p_term
      )
     @@ (fn (_,(_,(n,(_,(_,(_,(t,(_,(_,(_,v)))))))))) => TPi (n, t, v))
  ) str
  and p_lambda str = (
      ( $"lambda"
      ~ ws1
      ~ p_name
      ~ ws
      ~ $":"
      ~ ws
      ~ p_term
      ~ ws
      ~ $"=>"
      ~ ws
      ~ p_term
      )
     @@ (fn (_,(_,(n,(_,(_,(_,(t,(_,(_,(_,v)))))))))) => TLambda (n, t, v))
  ) str
  and p_app str = (
    ( $"<"
    ~ ws
    ~ p_term
    ~ ws1
    ~ p_term
    ~ ws
    ~ $">"
    )
   @@ (fn (_,(_,(f,(_,(a,(_,_)))))) => TApp (f, a))
  ) str
  and p_parens str = (
    ( $"("
    ~ ws
    ~ p_term
    ~ ws
    ~ $")"
    )
   @@ (fn (_,(_,(t,(_,_)))) => t)
  ) str

  val p_cmt = $"#" ~ (many any) @@ (fn _ => ())

  val p_command =
    (  ws ~ $"Check" ~ ws1 ~ p_term ~ ws ~ (maybe p_cmt)
    @@ (fn (_,(_,(_,(t,_)))) => RCheck t)

    ||  ws ~ $"Show" ~ ws1 ~ p_name ~ ws ~ p_cmt
    @@ (fn (_,(_,(_,(n,_)))) => RShow n)

    || ws ~ $"Assert" ~ ws1 ~ p_name ~ ws1 ~ p_term ~ ws ~ (maybe p_cmt)
    @@ (fn (_,(_,(_,(n,(_,(t,_)))))) => RAssert (n, t))

    || ws ~ $"Define" ~ ws1 ~ p_name ~ ws1 ~ p_term ~ ws ~ (maybe p_cmt)
    @@ (fn (_,(_,(_,(n,(_,(t,_)))))) => RDefine (n, t))

    || ws ~ $"Dump" ~ ws ~ (maybe p_cmt)
    @@ (fn _ => RDump)

    || ws ~ $"Rand" ~ ws ~ (maybe p_cmt)
    @@ (fn _ => RRand)

    || ws ~ $"Conflict" ~ ws ~ (maybe p_cmt)
    @@ (fn _ => RConflict)

    || ws ~ (maybe p_cmt)
    @@ (fn _ => RComment)

    )

end;



val urandom = BinIO.openIn("/dev/urandom");


fun do_rand e =
  let
    fun rand_byte () : int =
      Word8.toInt (valOf (BinIO.input1 urandom))
      ;

    fun rand_int () : int =
      (rand_byte ()) +
      (rand_byte () * 0x100) +
      (rand_byte () * 0x10000)

    fun rand_key e =
      let val e' = env.to_raw e in
        #1 (List.nth(e', rand_int () mod length e'))
      end
      ;

    fun try_rand_pair e =
      let val f = rand_key e;
          val a = rand_key e;
          val t = TApp (TVar f, TVar a)
      in
        (SOME (f, a, typecheck t e))
        handle TypeError s => NONE
      end
      ;

    fun get_rand_pair e =
      case try_rand_pair e of
        SOME x => x
      | NONE => get_rand_pair e

  in
    let val (f, a, t) =  get_rand_pair e;
        val n = (f^" ["^a^"]") in
      env.insert e n t
    end
  end


fun do_conflict (e : Env) =
  if List.exists (fn x => x = TVar "conflict")
            (map #2 (env.to_raw e))
  then e else do_conflict (do_rand e)


fun apply_cmd (cmd : REPLCmd) (e : Env) =
  case cmd of
    RComment => e

  | RCheck t => (
      (print ((string_of_term (typecheck t e))^"\n") ; e)
      handle TypeError s => (print ("Type error: "^s^"\n") ; e)
    )

  | RShow n => (
      case env.lookup e n of
        NONE  =>
        (print "Name not found.\n" ; e)
      | SOME t =>
        (print ((string_of_term t)^"\n") ; e)
    )

  | RAssert (n, t) =>
    env.insert e n t

  | RDefine (n, t) => (
      env.insert e n (typecheck t e)
      handle TypeError s => (print ("Type error: "^s^"\n") ; e)
    )
  
  | RDump => (
      print (string_of_env e) ;
      print "\n" ;
      e
    )

  | RRand => do_rand e

  | RConflict => do_conflict e

fun repl e = (
  (if Posix.ProcEnv.isatty Posix.FileSys.stdin then print ">> " else ()) ;
  let val inp = TextIO.inputLine TextIO.stdIn in
    case inp of
      NONE => ()
    | SOME line =>
      case p_command (String.explode line) of
          NONE => (
            print "Syntax error.\n" ;
            repl e
          )
        | SOME (cmd, []) => (
            repl (apply_cmd cmd e)
          )
        | SOME (_, _) => (
            print "Syntax error.\n" ;
            repl e
          )
  end
);

repl env.empty;
