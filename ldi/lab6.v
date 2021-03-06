(** * Imp: Simple Imperative Programs *)

(** In this chapter, we'll take a more serious look at how to use Coq
    to study interesting things outside of itself.  Our case study is
    a _simple imperative programming language_ called Imp, embodying a
    tiny core fragment of conventional mainstream languages such as C
    and Java.  Here is a familiar mathematical function written in
    Imp.

       Z ::= X;; 
       Y ::= 1;; 
       WHILE ! (Z = 0) DO
         Y ::= Y * Z;;
         Z ::= Z - 1
       END
*)

(*Add LoadPath "...".*)

Set Warnings "-notation-overridden,-parsing".
Require Import Coq.Bool.Bool.
Require Import Coq.Arith.Arith.
Require Import Coq.Arith.EqNat.
Require Import Coq.omega.Omega.
Require Import Coq.Lists.List.
Require Import Coq.omega.Omega.
Import ListNotations.

Require Import Maps.

(*Load Maps.*)

(** ** Tacticals *)

(** _Tacticals_ is Coq's term for tactics that take other tactics as
    arguments -- "higher-order tactics," if you will.  *)

(* ----------------------------------------------------------------- *)
(** *** The [try] Tactical *)

(** If [T] is a tactic, then [try T] is a tactic that is just like [T]
    except that, if [T] fails, [try T] _successfully_ does nothing at
    all (instead of failing). *)

Theorem silly : forall (P : Prop), P -> P.
Proof.
  intros P HP.
  try reflexivity. (* just [reflexivity] would have failed *)
  apply HP. (* we can still finish the proof in some other way *)
Qed.

(** There is no real reason to use [try] in completely manual
    proofs like these, but it is very useful for doing automated
    proofs in conjunction with the [;] tactical, which we show
    next. *)

(* ----------------------------------------------------------------- *)
(** *** The [;] Tactical (Simple Form) *)

(** In its most common form, the [;] tactical takes two tactics as
    arguments.  The compound tactic [T;T'] first performs [T] and then
    performs [T'] on _each subgoal_ generated by [T]. *)

(** For example, consider the following trivial lemma: *)

Lemma foo : forall n: nat,  leb 0 n = true.
Proof.
  intros.
  destruct n.
    (* Leaves two subgoals, which are discharged identically...  *)
    - (* n=0 *) simpl. reflexivity.
    - (* n=Sn' *) simpl. reflexivity.
Qed.

(** We can simplify this proof using the [;] tactical: *)

Lemma foo' : forall n, leb 0 n = true.
Proof.
  intros.
  (* [destruct] the current goal *)
  destruct n;
  (* then [simpl] each resulting subgoal *)
  simpl;
  (* and do [reflexivity] on each resulting subgoal *)
  reflexivity.
Qed.


(* ----------------------------------------------------------------- *)
(** *** The [repeat] Tactical *)

(** The [repeat] tactical takes another tactic and keeps applying this
    tactic until it fails. Here is an example showing that [10] is in
    a long list using repeat. *)

Theorem In10 : In 10 [1;2;3;4;5;6;7;8;9;10].
Proof.
  repeat (try (left; reflexivity); right).
Qed.

(** The tactic [repeat T] never fails: if the tactic [T] doesn't apply
    to the original goal, then repeat still succeeds without changing
    the original goal (i.e., it repeats zero times). *)

Theorem In10' : In 10 [1;2;3;4;5;6;7;8;9;10].
Proof.
  repeat (left; reflexivity).
  repeat (right; try (left; reflexivity)).
Qed.

(** The tactic [repeat T] also does not have any upper bound on the
    number of times it applies [T].  If [T] is a tactic that always
    succeeds, then repeat [T] will loop forever (e.g., [repeat simpl]
    loops, since [simpl] always succeeds).  While evaluation in Coq's
    term language, Gallina, is guaranteed to terminate, tactic
    evaluation is not!  This does not affect Coq's logical
    consistency, however, since the job of [repeat] and other tactics
    is to guide Coq in constructing proofs; if the construction
    process diverges, this simply means that we have failed to
    construct a proof, not that we have constructed a wrong one. *)

(* ================================================================= *)
(** ** The [omega] Tactic *)

(** The [omega] tactic implements a decision procedure for a subset of
    first-order logic called _Presburger arithmetic_.  It is based on
    the Omega algorithm invented by William Pugh [Pugh 1991].

    If the goal is a universally quantified formula made out of

      - numeric constants, addition ([+] and [S]), subtraction ([-]
        and [pred]), and multiplication by constants (this is what
        makes it Presburger arithmetic),

      - equality ([=] and [<>]) and ordering ([<=]), and

      - the logical connectives [/\], [\/], [~], and [->],

    then invoking [omega] will either solve the goal or fail, meaning
    that the goal is actually false.  (If the goal is _not_ of this
    form, [omega] will also fail.) *)

Example silly_presburger_example : forall m n o p,
  m + n <= n + o /\ o + 3 = p + 3 ->
  m <= p.
Proof.
  intros. omega.
Qed.

(** (Note the [Require Import Coq.omega.Omega.] at the top of
    the file.) *)

(* ================================================================= *)
(** ** A Few More Handy Tactics *)

(** Finally, here are some miscellaneous tactics that you may find
    convenient.

     - [clear H]: Delete hypothesis [H] from the context.

     - [subst x]: Find an assumption [x = e] or [e = x] in the
       context, replace [x] with [e] throughout the context and
       current goal, and clear the assumption.

     - [subst]: Substitute away _all_ assumptions of the form [x = e]
       or [e = x].

     - [rename... into...]: Change the name of a hypothesis in the
       proof context.  For example, if the context includes a variable
       named [x], then [rename x into y] will change all occurrences
       of [x] to [y].

     - [assumption]: Try to find a hypothesis [H] in the context that
       exactly matches the goal; if one is found, behave like [apply
       H].

     - [contradiction]: Try to find a hypothesis [H] in the current
       context that is logically equivalent to [False].  If one is
       found, solve the goal.

     - [constructor]: Try to find a constructor [c] (from some
       [Inductive] definition in the current environment) that can be
       applied to solve the current goal.  If one is found, behave
       like [apply c].

    We'll see examples as we go along. *)

(** In Maps.v :
    Definition total_map (A:Type) := string -> A.
*)

Definition state := total_map nat.

(* ================================================================= *)
(** ** Syntax of arithmetic expressions  *)

Inductive aexp : Type :=
  | ANum : nat -> aexp
  | AId : string -> aexp
  | APlus : aexp -> aexp -> aexp
  | AMinus : aexp -> aexp -> aexp
  | AMult : aexp -> aexp -> aexp.

(** Defining a few variable names as notational shorthands will make
    examples easier to read: *)

Definition W : string := "W".
Definition X : string := "X".
Definition Y : string := "Y".
Definition Z : string := "Z".

Inductive bexp : Type :=
  | BTrue : bexp
  | BFalse : bexp
  | BEq : aexp -> aexp -> bexp
  | BLe : aexp -> aexp -> bexp
  | BNot : bexp -> bexp
  | BAnd : bexp -> bexp -> bexp.

(* ================================================================= *)
(** ** Notations *)
(** To make Imp programs easier to read and write, we introduce some
    notations and implicit coercions.

    You do not need to understand what these declarations do in detail
    to follow this chapter.  Briefly, though, the [Coercion]
    declaration in Coq stipulates that a function (or constructor) can
    be implicitly used by the type system to coerce a value of the
    input type to a value of the output type.  For instance, the
    coercion declaration for [AId] allows us to use plain strings when
    an [aexp] is expected; the string will implicitly be wrapped with
    [AId]. *)

(** The notations below are declared in specific _notation scopes_, in
    order to avoid conflicts with other interpretations of the same
    symbols.  Again, it is not necessary to understand the details. *)

Coercion AId : string >-> aexp.
Coercion ANum : nat >-> aexp.
Definition bool_to_bexp (b: bool) : bexp :=
  if b then BTrue else BFalse.
Coercion bool_to_bexp : bool >-> bexp.

Bind Scope aexp_scope with aexp.
Infix "+" := APlus : aexp_scope.
Infix "-" := AMinus : aexp_scope.
Infix "*" := AMult : aexp_scope.
Bind Scope bexp_scope with bexp.
Infix "<=" := BLe : bexp_scope.
Infix "=" := BEq : bexp_scope.
Infix "&&" := BAnd : bexp_scope.
Notation "'!' b" := (BNot b) (at level 60) : bexp_scope.

(** We can now write [3 + (X * 2)] instead  of [APlus 3 (AMult X 2)], 
    and [true && !(X <= 4)] instead of [BAnd true (BNot (BLe X 4))]. *)

(* ================================================================= *)
(** ** Evaluation *)

(** The arith and boolean evaluators are extended to handle
    variables in the obvious way, taking a state as an extra
    argument: *)

Fixpoint aeval (st : state) (a : aexp) : nat :=
  match a with
  | ANum n => n
  | AId x => st x
  | APlus a1 a2 => (aeval st a1) + (aeval st a2)
  | AMinus a1 a2  => (aeval st a1) - (aeval st a2)
  | AMult a1 a2 => (aeval st a1) * (aeval st a2)
  end.

Fixpoint beval (st : state) (b : bexp) : bool :=
  match b with
  | BTrue       => true
  | BFalse      => false
  | BEq a1 a2   => beq_nat (aeval st a1) (aeval st a2)
  | BLe a1 a2   => leb (aeval st a1) (aeval st a2)
  | BNot b1     => negb (beval st b1)
  | BAnd b1 b2  => andb (beval st b1) (beval st b2)
  end.

(** We specialize our notation for total maps to the specific case of states,
    i.e. using [{ --> 0 }] as empty state. *)

Notation "{ a --> x }" := 
  (t_update { --> 0 } a x) (at level 0).
Notation "{ a --> x ; b --> y }" := 
  (t_update ({ a --> x }) b y) (at level 0).
Notation "{ a --> x ; b --> y ; c --> z }" := 
  (t_update ({ a --> x ; b --> y }) c z) (at level 0). 
Notation "{ a --> x ; b --> y ; c --> z ; d --> t }" := 
    (t_update ({ a --> x ; b --> y ; c --> z }) d t) (at level 0).
Notation "{ a --> x ; b --> y ; c --> z ; d --> t ; e --> u }" :=
  (t_update ({ a --> x ; b --> y ; c --> z ; d --> t }) e u) (at level 0).
Notation "{ a --> x ; b --> y ; c --> z ; d --> t ; e --> u ; f --> v }" :=
  (t_update ({ a --> x ; b --> y ; c --> z ; d --> t ; e --> u }) f v) (at level 0).

Example aexp1 :
  aeval { X --> 5 } (3 + (X * 2))
  = 13.
Proof. reflexivity. Qed.

Example bexp1 :
  beval { X --> 5 } (true && !(X <= 4))
  = true.
Proof. reflexivity. Qed.

(** * Commands *)

Inductive com : Type :=
  | CSkip : com
  | CAss : string -> aexp -> com
  | CSeq : com -> com -> com
  | CIf : bexp -> com -> com -> com
  | CWhile : bexp -> com -> com.

(** As for expressions, we can use a few [Notation] declarations to make 
    reading and writing Imp programs more convenient. *)

Bind Scope com_scope with com.
Notation "'SKIP'" :=
   CSkip : com_scope.
Notation "x '::=' a" :=
  (CAss x a) (at level 60) : com_scope.
Notation "c1 ;; c2" :=
  (CSeq c1 c2) (at level 80, right associativity) : com_scope.
Notation "'WHILE' b 'DO' c 'END'" :=
  (CWhile b c) (at level 80, right associativity) : com_scope.
Notation "'IFB' c1 'THEN' c2 'ELSE' c3 'FI'" :=
  (CIf c1 c2 c3) (at level 80, right associativity) : com_scope.

(** The following declaration is needed to be able to use the
    notations in match patterns. *)
Open Scope com_scope.

(** For example, here is the factorial function , written as a
    formal definition to Coq: *)

Definition FACTORIAL: com :=
  Z ::= X;;
  Y ::= 1;;
  WHILE ! (Z = 0) DO
    Y ::= Y * Z;;
    Z ::= Z - 1 
  END.

(* ----------------------------------------------------------------- *)
(** *** An infinite loop: *)

Definition loop : com :=
  WHILE true DO
    SKIP
  END.

(* ################################################################# *)
(** * Evaluating Commands *)

(* ================================================================= *)
(** ** Evaluation as a Function (Failed Attempt) *)

(** Here's an attempt at defining an evaluation function for commands,
    omitting the [WHILE] case. *)

Fixpoint ceval_fun (st : state) (c : com): state :=
  match c with
    | SKIP =>
        st
    | x ::= a1 =>
        st & { x --> (aeval st a1) }
    | c1 ;; c2 =>
        let st' := ceval_fun st c1 in
        ceval_fun st' c2
    | IFB b THEN c1 ELSE c2 FI =>
        if (beval st b)
          then ceval_fun st c1
          else ceval_fun st c2
    | WHILE b DO c END =>
        st  (* if (beval st b)
                  then ceval_fun st (c; WHILE b DO c END)
                  else st *)
  end.

(* try this one *)
(* Fixpoint loop_false (n : nat) : False := loop_false n. *)

(** ** Evaluation as a relation *)

Reserved Notation "c1 '/' st '\\' st'"
                  (at level 40, st at level 39).

Inductive ceval : com -> state -> state -> Prop :=
  | E_Skip : forall st,
      SKIP / st \\ st
  | E_Ass  : forall st a1 n x,
      aeval st a1 = n ->
      (x ::= a1) / st \\ st & { x --> n }
  | E_Seq : forall c1 c2 st st' st'',
      c1 / st  \\ st' ->
      c2 / st' \\ st'' ->
      (c1 ;; c2) / st \\ st''
  | E_IfTrue : forall st st' b c1 c2,
      beval st b = true ->
      c1 / st \\ st' ->
      (IFB b THEN c1 ELSE c2 FI) / st \\ st'
  | E_IfFalse : forall st st' b c1 c2,
      beval st b = false ->
      c2 / st \\ st' ->
      (IFB b THEN c1 ELSE c2 FI) / st \\ st'
  | E_WhileFalse : forall b st c,
      beval st b = false ->
      (WHILE b DO c END) / st \\ st
  | E_WhileTrue : forall st st' st'' b c,
      beval st b = true ->
      c / st \\ st' ->
      (WHILE b DO c END) / st' \\ st'' ->
      (WHILE b DO c END) / st \\ st''

  where "c1 '/' st '\\' st'" := (ceval c1 st st').

(** The cost of defining evaluation as a relation instead of a
    function is that we now need to construct _proofs_ that some
    program evaluates to some result state, rather than just letting
    Coq's computation mechanism do it for us. *)

Example ceval_example1:
    (X ::= 2;;
     IFB X <= 1
       THEN Y ::= 3
       ELSE Z ::= 4
     FI)
   / { --> 0 } \\ { X --> 2 ; Z --> 4 }.
Proof.
  (* We must supply the intermediate state *)
  apply E_Seq with { X --> 2 }.
  - (* assignment command *)
    apply E_Ass. reflexivity.
  - (* if command *)
    apply E_IfFalse.
    +  reflexivity.
    +  apply E_Ass. reflexivity.
Qed.

Fixpoint factorial n: nat :=
  match n with
  | 0 => 1
  | S n => S n * factorial n
end.

Compute factorial 6.

Lemma Z_is_not_Y: Z <> Y.
Proof.
  unfold Z. unfold Y. unfold not. intros.
  rewrite <- get_correct in H.
  specialize (H 0). inversion H.
Qed.

(*Exercise 1*: factorial 2 *)

(* it might be useful to use the following two facts about maps *)

Check t_update_shadow.
Check t_update_permute.

Theorem ceval_factorial_2:
  FACTORIAL / { X --> 2 } \\ { X --> 2 ; Z --> 0; Y --> factorial 2 }.
Proof.
  apply E_Seq with {X --> 2 ; Z --> 2}.
  - apply E_Ass. reflexivity.
  - apply E_Seq with {X --> 2; Z --> 2 ; Y --> 1}.
    + apply E_Ass. reflexivity.
    + apply E_WhileTrue with {X --> 2; Z --> 1; Y --> 2}.
      * auto.
      * apply E_Seq with {X --> 2; Z --> 2; Y --> 2}.
        ** rewrite <- t_update_shadow with (x := Y) (v1 := 1) (v2 := 2).
           apply E_Ass.
           auto.
        ** rewrite <- t_update_shadow with (x := Z) (v1 := 2) (v2 := 1).
           rewrite -> t_update_permute with (x2 := Z) (x1 := Y) (m := {X --> 2; Z --> 2}).
           apply E_Ass.
           *** auto.
           *** apply Z_is_not_Y.
      * apply E_WhileTrue with {X --> 2; Z --> 1; Y --> 2}.
        ** auto.
        ** apply E_Seq with {X --> 2; Z --> 1; Y --> 2}.
           *** rewrite <- t_update_shadow with (x := Y) (v1 := 2) (v2 := 2).
               apply E_Ass.
Qed.

(*
Theorem ceval_factorial: forall n: nat,
  (X ::= n;; FACTORIAL)
   / { --> 0 } \\ { X --> n ; Y --> factorial n; Z --> 0 }.
Proof.
  induction n.
  - simpl. apply E_Seq with { X --> 0 }.
    * apply E_Ass. reflexivity.
    * unfold FACTORIAL.
      apply E_Seq with { X --> 0; Z --> 0 }.
      + apply E_Ass. reflexivity.
      + apply E_Seq with { X --> 0; Z --> 0; Y --> 1 }.
        { apply E_Ass. reflexivity. }
        { rewrite -> t_update_permute.
          { apply E_WhileFalse. reflexivity. }
          { unfold Z. unfold Y. unfold not. intros.
            rewrite <- get_correct in H.
            specialize (H 0). inversion H. }
        }
  -
*)


(* ================================================================= *)
(** ** Determinism of Evaluation *)

(** Changing from a computational to a relational definition of
    evaluation is a good move because it frees us from the artificial
    requirement that evaluation should be a total function.  But it
    also raises a question: Is the second definition of evaluation
    really a partial function?  Or is it possible that, beginning from
    the same state [st], we could evaluate some command [c] in
    different ways to reach two different output states [st'] and
    [st'']?

    In fact, this cannot happen: [ceval] _is_ a partial function: *)

(* Exercise 2*: determinism of evaluation *)

Theorem ceval_deterministic: forall c st st1 st2,
     c / st \\ st1  ->
     c / st \\ st2 ->
     st1 = st2.
Proof.
  intros c st st1 st2 E1 E2.
  generalize dependent st2.
  induction E1; intros st2 E2; inversion E2; subst.
  (* TO BE FINISHED *)
Admitted.


(** **** Exercise7: 3 stars (stack_compiler)  *)
(** Old HP Calculators, programming languages like Forth and Postscript,
    and abstract machines like the Java Virtual Machine all evaluate
    arithmetic expressions using a _stack_. For instance, the expression

      (2*3)+(3*(4-2))

   would be written as

      2 3 * 3 4 2 - * +

   and evaluated like this (where we show the program being evaluated
   on the right and the contents of the stack on the left):

      [ ]           |    2 3 * 3 4 2 - * +
      [2]           |    3 * 3 4 2 - * +
      [3, 2]        |    * 3 4 2 - * +
      [6]           |    3 4 2 - * +
      [3, 6]        |    4 2 - * +
      [4, 3, 6]     |    2 - * +
      [2, 4, 3, 6]  |    - * +
      [2, 3, 6]     |    * +
      [6, 6]        |    +
      [12]          |

  The goal of this exercise is to write a small compiler that
  translates [aexp]s into stack machine instructions.

  The instruction set for our stack language will consist of the
  following instructions:
     - [SPush n]: Push the number [n] on the stack.
     - [SLoad x]: Load the identifier [x] from the store and push it
                  on the stack
     - [SPlus]:   Pop the two top numbers from the stack, add them, and
                  push the result onto the stack.
     - [SMinus]:  Similar, but subtract.
     - [SMult]:   Similar, but multiply. *)

Inductive sinstr : Type :=
| SPush : nat -> sinstr
| SLoad : string -> sinstr
| SPlus : sinstr
| SMinus : sinstr
| SMult : sinstr.


(** Write a function to evaluate an instruction in the stack language. It
    should take as input a state, a stack represented as a list of
    numbers (top stack item is the head of the list), and an instruction,
    and it should return the stack after executing the instruction. 
    Use option type to represent situations when the stack contains less 
    than two elements and the instruction to execute is [SPlus], [SMinus], 
    or [SMult].

    Write a function to evaluate programs in the stack language. It
    should take as input a state, a stack represented as a list of
    numbers (top stack item is the head of the list), and a program
    represented as a list of instructions, and it should return the
    stack after executing the program. Use option type and the function 
    that evaluates instructions.

    Test your function on the examples below.
 *)

Definition i_execute (st : state) (stack : list nat)
                   (i : sinstr)
                 : option(list nat)
  (* REPLACE THIS LINE WITH ":= _your_definition_ ." *). Admitted.

Fixpoint s_execute (st : state) (stack : list nat)
                   (prog : list sinstr)
                 : option (list nat)
  (* REPLACE THIS LINE WITH ":= _your_definition_ ." *). Admitted.

Example s_execute1 :
     s_execute { --> 0 } []
       [SPush 5; SPush 3; SPush 1; SMinus]
   = Some [2; 5].
(* FILL IN HERE *) Admitted.


Example s_execute2 :
     s_execute { X --> 3 } [3;4]
       [SPush 4; SLoad X; SMult; SPlus]
   = Some [15; 4].
(* FILL IN HERE *) Admitted.


(** Next, write a function that compiles an [aexp] into a stack
    machine program. The effect of running the program should be the
    same as pushing the value of the expression on the stack. *)

Fixpoint s_compile (e : aexp) : list sinstr
  (* REPLACE THIS LINE WITH ":= _your_definition_ ." *). Admitted.

(** After you've defined [s_compile], prove the following to test
    that it works. *)

Example s_compile1 :
  s_compile (X - (2 * Y))
  = [SLoad X; SPush 2; SLoad Y; SMult; SMinus].
(* FILL IN HERE *) Admitted.
(** [] *)

(** **** Exercise8: 3 stars, advanced (stack_compiler_correct)  *)
(** Now we'll prove the correctness of the compiler implemented in the
    previous exercise.

    Prove the following theorem.  You will need to start by lemma stating a
    more general property to get a usable induction hypothesis; the main
    theorem will then be a simple corollary of this lemma.

    You may use Lemmas app_assoc and app_nil_end in your proofs.
*)


Check app_assoc.

Lemma s_compile_correct_aux : forall (st : state) (e : aexp) p stack,
  s_execute st stack (s_compile e ++ p) = s_execute st (aeval st e :: stack) p.
Proof.
  (* FILL IN HERE *) Admitted.


Check app_nil_end.

Theorem s_compile_correct : forall (st : state) (e : aexp),
  s_execute st [] (s_compile e) = Some [ aeval st e ].
Proof.
  (* FILL IN HERE *) Admitted.
(** [] *)
