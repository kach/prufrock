(* ...proofs that follow like a tedious argument	
 * Of insidious intent	
 * To lead you to an overwhelming question...
 *)

signature DICT =
  sig
    type (''k, 'v) dict
    exception DictError of string
    val empty : (''k, 'v) dict
    val of_raw : (''k * 'v) list -> (''k, 'v) dict
    val to_raw : (''k, 'v) dict -> (''k * 'v) list 
    val insert : ( 'k, 'v) dict ->  'k -> 'v -> ('k, 'v) dict
    val lookup : (''k, 'v) dict -> ''k -> 'v option
    val search : (''k, 'v) dict -> ''k -> 'v
    val member : (''k, 'v) dict -> ''k -> bool
  end

structure assoc :> DICT =
  struct
    type (''k, 'v) dict = (''k * 'v) list
    exception DictError of string

    val empty = []

    fun insert (d : ('k, 'v) dict) (k : 'k) (v : 'v) : ('k, 'v) dict =
        (k, v) :: d

    fun of_raw (d' : (''k * 'v) list) : (''k, 'v) dict = d'
    fun to_raw (d' : (''k, 'v) dict) : (''k * 'v) list = d'

    fun lookup (d : (''k, 'v) dict) (k : ''k) : 'v option =
        case d
         of [] => NONE
          | ((k', v') :: d') =>
            if k' = k then
              SOME v'
            else
              lookup d' k

    fun search (d : (''k, 'v) dict) (n : ''k) : 'v =
      let val v' = lookup d n in
        case v'
         of NONE => raise DictError "Key not found"
          | SOME v => v
      end

    fun member (d : (''k, 'v) dict) (n : ''k) : bool =
      let val v' = lookup d n in
        case v'
         of NONE   => false
          | SOME v => true
      end

  end




type Name = string

datatype Term
  = TProp | TType of int
  | TVar    of Name
  | TPi     of Name * Term * Term
  | TLambda of Name * Term * Term
  | TApp    of Term * Term

structure env : DICT = assoc
type Env = (Name, Term) env.dict

exception TypeError of string






fun freein (t : Term) (e : Env) : Name list =
  case t
  of TProp   => []
   | TType n => []
   | TVar  n => (case env.lookup e n of NONE => [n] | _ => [])
   | TPi     (n, t', v) => freein v (env.insert e n v)
   | TLambda (n, t', v) => freein v (env.insert e n v)
   | TApp (f, a) => (freein f e) @ (freein a e)

(* ...no doubt, an easy tool,
 * Deferential, glad to be of use... *)
fun string_of_term (t : Term) : string =
  case t
  of TProp => "Prop"
   | TType n =>
     "(Type "
     ^ (Int.toString n)
     ^ ")"
   | TVar n => n
   | TPi (n, t', v)     =>
     if List.exists (fn x => x = n) (freein v env.empty) then
       "(\226\136\128 "
       ^ n
       ^ " : "
       ^ (string_of_term t')
       ^ ", "
       ^ (string_of_term v)
       ^ ")"
     else
       "("
       ^ (string_of_term t')
       ^ " \226\134\146 "
       ^ (string_of_term v)
       ^ ")"
   | TLambda (n, t', v) =>
     "(\206\187 "
     ^ n
     ^ " : "
     ^ (string_of_term t')
     ^ ". "
     ^ (string_of_term v)
     ^ ")"
   | TApp (f, a) =>
     "("
     ^ (string_of_term f)
     ^ " "
     ^ (string_of_term a)
     ^ ")"

fun string_of_env (e : Env) : string =
  String.concatWith "\n"
    (List.map
      (fn (k, v) =>
         k
         ^ "\t\226\159\188  "
         ^ (string_of_term v))
      (env.to_raw e))

(* Substitute like a malamute *)
fun substitute (name : Name) (value : Term) (t : Term) : Term =
  case t
  of TProp   => t
   | TType n => t
   | TVar  n => if n = name then value else t
   | TPi     (n, v, t') =>
     TPi     (n, substitute name value v, if name = n then t'
                                          else substitute name value t')
   | TLambda (n, v, t') =>
     TLambda (n, substitute name value v, if name = n then t'
                                          else substitute name value t')
   | TApp (f, a) => TApp (substitute name value f, substitute name value a)


fun normalize (t : Term) (e : Env) : Term =
  case t
  of TProp   => TProp
   | TType n => TType n
   | TVar n  =>                                             (* Delta, Zeta? *)
     (case env.lookup e n
      of SOME v => normalize v e
       | NONE => TVar n  (* fly! be free! *)
     )
   | TPi (n, t', v) => TPi (n, normalize t' e, normalize v e)
   | TLambda (n, t', v) => TLambda (n, t', normalize v e)
   | TApp (f, a) =>                                         (* Beta *)
     let
       val nf = normalize f e;
       val na = normalize a e
     in
       case nf
       of TLambda (n, t', v) =>
          normalize (substitute n na v) e
        | _ =>
          TApp (nf, na)
     end

fun subtype (s : Term) (t : Term) (e : Env) : bool =
  let
    val s' = normalize s e;
    val t' = normalize t e
  in
    case (s', t')
    of (TProp, TType n) => 1 <= n                           (* Cumulativity *)
     | (TType n, TType m) => n <= m
     | (TPi (n1, v1, b1), TPi (n2, v2, b2)) =>
       (subtype v1 v2 e) andalso
       (subtype (substitute n1 v1 b1) (substitute n2 v2 b2) e)
     | _ => s' = t'
  end

fun typecheck (t : Term) (e : Env) : Term =
  case t
  of TProp   => TType 1                                   (* Ax-Prop *)
   | TType n => TType (n + 1)                             (* Ax-Set, Ax-Type *)
   | TVar  n =>
     (case env.lookup e n                                 (* Var, Const *)
      of SOME v => v
       | NONE   => raise TypeError ("Name " ^ n ^ " not found!"))
   | TPi (n, t', v') =>
     let
       val tt = typecheck t' e;
       val vv = typecheck v' (env.insert e n t')
     in
       case (tt, vv)
       of (_, TProp) => TProp                             (* Prod-Prop impredicativity mode ON *)
        | (TProp,   TType 0) => TType 0                   (* Prod-Set  *)
        | (TType 0, TType 0) => TType 0                   (* Prod-Set  *)
        | (TType n, TType m) => TType (Int.max (n, m))    (* Prod-Type *)
        | _ => raise TypeError "Universe issues :/"
     end
   | TLambda (n, t, v) =>                                 (* Lam *)
     let val tt = typecheck t e in
       TPi (n, t, typecheck v (env.insert e n t))
     end
   | TApp (f, a) =>                                       (* App *)
     let
       val ft = typecheck f e;
       val at = typecheck a e in
       case ft
       of TPi (n, at', vt) =>
          if subtype at at' e then
              substitute n a vt
          else
              raise TypeError (
                "Mismatched argument types -- "
                ^ (string_of_term at)
                ^ " </: "
                ^ (string_of_term at')
              )
        | _ => raise TypeError (
                 (string_of_term f)
                 ^ " : "
                 ^ (string_of_term ft)
                 ^ " is not a function."
               )
     end




(* TODO Inductive macros *)

fun sort_of_arity (t : Term) : Term =
  case t
  of TProp =>   t
   | TType n => t
   | TPi (n, v, b) => sort_of_arity b
   | _ => raise TypeError ("Bad type for inductive: "^(string_of_term t))

fun compile_inductive (n : Name) (t : Term) (c : (Name * Term) list) (e : Env) =
  let
    val s = sort_of_arity t ;

    fun check_constructor n' t' : bool = (* some kind of positivity criterion TODO *)
      case t'
      of TProp   => false
       | TType n => false
       | TVar n'' => n'' = n
       | TPi (n, v, b) => check_constructor n' b
       | TLambda (n, v, b) => false
       | TApp (f, a) => check_constructor n' f ;

    fun new_constructor ((n', t'), e') =
      if not (check_constructor n' t') then
        raise TypeError ("Bad constructor alert!")
      else
        env.insert e n' t'
  in
    List.foldl new_constructor e c
  end




(*

We encode the following definitions and theorems in Prufrock below...

Inductive nat : Type :=
| O : nat
| S : nat -> nat.

Inductive ev : nat -> Prop :=
| ev_O  : ev O
| ev_SS : forall (n:nat), ev n -> ev (S (S n)).

Theorem test1 : ev (S (S O)).
Proof.
  apply (ev_SS O ev_O).
Qed.

*)

(*
val e = env.of_raw [
    ("nat", TType 0),
    ("O", TVar "nat"),
    ("S", TPi ("_", TVar "nat", TVar "nat")),

    ("ev", TPi ("_", TVar "nat", TProp)),
    ("ev_O", TApp (TVar "ev", TVar "O")),
    ("ev_SS",
        TPi ("n", TVar "nat",
            TPi ("_", TApp (TVar "ev", TVar "n"),
                TApp (TVar "ev", TApp (TVar "S", (TApp (TVar "S", TVar "n"))))))),

    ("ev_2", TApp (TVar "ev", TApp (TVar "S", (TApp (TVar "S", TVar "O")))))
]

(* A term, really. *)
val ev_2_pf = TApp (TApp (TVar "ev_SS", TVar "O"), TVar "ev_O")

fun print_type (t : Term) (e : Env) =
  let
    val t' = typecheck t e
  in
    print ((string_of_term t) ^ " : " ^ (string_of_term t') ^ "\n")
  end
  handle TypeError s =>
    print ("Type error: "^s^"\n")

val main = (
  print_type (TVar "ev") e ;
  print_type (TVar "ev_O") e ;
  print_type (TVar "ev_SS") e ;
  print_type ev_2_pf e ;
  print (string_of_env e) ;
  print "\n\n"
)
*)
