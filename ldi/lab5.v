Require Import List.
Import ListNotations.

(** * Preliminaries *)

Print list.
Check (cons 1 (cons 2 (cons 3 nil)) = [13;21]).

(** [Check] merely checks the type, not that the thing holds! *)
Check forall A (h:A) (t:list A), h::nil = cons h t.

Section Lists_of_A.
(* We use a section and a [Variable] so that we do not have to worry about [A]
   at each definition / lemma *)
Variable A: Type.

(** ** Definition by pattern matching on lists *)

Definition tail (l: list A): list A :=
  match l with
  | [] => []   (* functions are total in Coq ! *)
  | h::t => t
  end.

(** Proof by computation *)
Lemma tail_1: forall a b c, tail [a;b;c] = [b;c].
  intros.
  simpl.
  reflexivity.
Qed.

(** ** Definition by recursion (and pattern matching) *)

(** The following definition always terminates:
    the recursive call to length is allowed for subterms of [l] only (extracted by [match]) ! *)

Fixpoint length (l: list A): nat :=
  match l with
  | [] => 0
  | _ :: l' => S (length l')
  end.

(** Another definition, by induction on the first argument *)
Fixpoint app (l m : list A): list A :=
  match l with
  | [] => m
  | a :: l1 => a :: app l1 m
  end.

(** Proof by induction on lists *)
Lemma app_length : forall l1 l2 : list A, 
  length l1 + length l2 = length (app l1 l2).
Proof.
  (* similar to (destruct l1),
     but you additionally get an induction hypothesis *)
  induction l1.
  + simpl.
    reflexivity.
  + simpl.
    intro.
    rewrite -> IHl1.  (* [->] can be skipped; try also rewrite <- *)
    reflexivity. (* trivial is also good enough *)
Qed.

(** This lemma is very easy *)
Lemma app_nil_l : forall l, app nil l = l.
Proof.
  simpl.
  trivial.
Qed.

(** What about this one? :) *)
Lemma app_nil_r : forall l, app l nil = l.
Proof.
  induction l.
  * simpl.
    reflexivity.
  * simpl.
    rewrite IHl.
    reflexivity.
Qed.

(** another proof by induction *)
Lemma app_assoc : forall l1 l2 l3 : list A,
  app l1 (app l2 l3) = app (app l1 l2) l3.
Proof.
  intros.
  induction l1.
  * simpl.
    reflexivity.
  * simpl.
    rewrite IHl1.
    reflexivity.
Qed.

(** recursive predicate on lists *)
Fixpoint In (a:A) (l:list A): Prop :=
  match l with
  | [] => False
  | h::t => a = h \/ In a t
  end.
(* Note that we have defined a predicate [In : A -> list A -> Prop] by recursion on lists *)

Compute (forall a b c d: A, In a [b;c;d]).

(** use the predicate by computing it *)
Lemma In_not_empty : forall (a:A) l, In a l -> l <> [].
Proof.
  intros.
  destruct l.
  + simpl in H.
    tauto.
  + congruence. (* handle simple equality or disequality *)
Qed.

Lemma in_app_or : forall a l1 l2, In a (app l1 l2) -> In a l1 \/ In a l2.
Proof.
  intros.
  simpl in H.
Admitted.

(** ** Inductive predicate *)
Inductive contains (a:A) : list A -> Prop :=
| C_here: forall l, contains a (a::l)
| C_there: forall l b, contains a l -> contains a (b::l).

(* Corresponds to a Prolog definition:

contains(A, [[A|L]]).
contains(A, [[B|L]]] :- contains(A,L).

*)

(** proving an inductive predicate *)
Lemma cont_abc : forall a b c, contains b [a;b;c].
Proof.
  intros.
  apply C_there.
  apply C_here.
Qed.

(** using an inductive predicate *)
Lemma cont_nempty: forall a l, contains a l -> l <> [].
Proof.
  intros.
  inversion H.
  + congruence.
  + congruence.
Qed.

(** three lemmas establishing the correspondence between contains and In *)
Lemma cont_In : forall a l, contains a l -> In a l.
Proof.
  intros.
  induction l.
  * simpl.
    inversion H.
  * simpl.
    inversion H.
    + left.
      trivial.
    + right.
      apply IHl.
      assumption.
Qed.

Lemma In_cont : forall a l, In a l -> contains a l.
Proof.
  intros.
  induction l.
  * inversion H.
  * destruct H.
    + rewrite H.
      apply C_here.
    + apply C_there.
      apply IHl. apply H.
Qed.

(* given the previous two lemmas, this one requires no more than three tactics *)
Lemma In_iff_cont : forall a l, In a l <-> contains a l.
Proof.
  intros.
  unfold iff.
  split.
  * apply In_cont.
  * apply cont_In.
Qed.

(** ** Another inductive predicate *)

Inductive prefix: list A -> list A -> Prop :=
| P_nil: forall l2, prefix [] l2
| P_cons: forall a l1 l2, prefix l1 l2 -> prefix (a::l1) (a::l2).

Lemma pref_contains: forall a l, prefix [a] l -> contains a l.
Proof.
  intros.
  inversion H.
  apply C_here.
Qed.

(** a couple of simple lemmas *)
Lemma pref_reflexive: forall l, prefix l l.
Proof.
  intros.
  induction l.
  * apply P_nil.
  * apply P_cons.
    apply IHl.
Qed.

(* Hint: there are two lists to do induction on,
   but only one will work. *)
Lemma pref_app : forall l1 l2, prefix l1 (l1++l2).
Proof.
  intros.
  induction l1.
  * simpl.
    apply P_nil.
  * simpl.
    apply P_cons.
    apply IHl1.
Qed.

Lemma pref_app2 : forall l1 l2 l3,
  prefix l2 l3 -> prefix (l1++l2) (l1++l3).
Proof.
  intros.
  induction l1.
  * simpl.
    apply H.
  * simpl.
    apply P_cons.
    apply IHl1.
Qed.

(* Hint: this one needs induction + inversion *)
Lemma pref_short : forall l1 l2 l3, prefix (l1++l2) (l1++l3) -> prefix l2 l3.
Proof.
  intros.
  induction l1.
  * simpl in H.
    apply H.
  * apply IHl1.
    simpl in H.
    inversion H.
    apply H1.
Qed.

(** example of proof by induction on the evidence for predicate "prefix" *)
Lemma pref_app3 : forall l1 l2 l3, prefix l1 l2 -> prefix l1 (l2++l3).
Proof.
  intros.
  induction H.
  + constructor.
  + simpl.
    constructor.
    trivial.
Qed.

(** Hint: induction on the evidence for the first occurrence of "prefix" *)
Lemma pref_antisymm: forall l1 l2,
  prefix l1 l2 -> prefix l2 l1 -> l1 = l2.
Proof.
  (* exercise *)
Admitted.

(** transitivity of prefix *)
Lemma pref_trans : forall l1 l2 l3, prefix l1 l2 -> prefix l2 l3 -> prefix l1 l3.
Proof.
  intros.
  induction H.
  + constructor.
  + (* IHprefix is useless! There are two reasons:
      1) If prefix (a::l2) l3 than it is impossible that prefix l2 l3
      2) we need to prove prefix (a :: l1) l3,
         but the conclusion of the inductive assumption only says prefix l1 l3 *)
Restart.  (* We need to strengthen our induction hypothesis
             to the more general form forall l3... *)
  intros l1 l2 l3 H.
  revert l3.  (* now our goal is forall l3... ! *)
  (* exercise *)
Admitted.

(** ** Reversing lists for the bored ones only !!! *)

(* this is the definition from the library, with quadratic time complexity *)
Fixpoint rev_slow (l: list A) :=
  match l with
  | [] => []
  | h::t => rev_slow t ++ [h]
  end.

(* reversing in linear time using an accumulator *) 
Fixpoint reva (lr l : list A) := match l with
  | [] => lr
  | h::t => reva (h::lr) t
end.

Definition rev l := reva [] l.

Lemma rev_equiv: forall l: list A, rev l = rev_slow l.
Proof.
  induction l.
  + unfold rev. simpl. trivial.
  + unfold rev in *.
    simpl.
    rewrite <- IHl.
    simpl.
    (* The inducton hypothesis is not strong enough *)
Abort.

(* This is a more general fact about the accumulator *)
(* Hint: do induction on l3,
   but make sure that the hypothesis is as general as possible! *)
Lemma reva_app: forall l1 l2 l3: list A,
  reva (l1 ++ l2) l3 = reva l1 l3 ++ l2.
Proof.
  (*Exercise*)
Admitted.

(* Hint: specialise the previous lemma *)
Lemma reva_singl: forall a l,
  reva [a] l = reva [] l ++ [a].
Proof.
  (* Exercise *)
Admitted.

Lemma rev_equiv: forall l: list A, rev l = rev_slow l.
Proof.
  induction l.
  + unfold rev. simpl. trivial.
  + unfold rev in *.
    simpl.
    rewrite <- IHl.
    simpl. apply reva_singl.
Qed.

(* another property we might want to prove is that reverse is an involution *)
Lemma rev_rev : forall l : list A, rev (rev l) = l.
Proof.
  unfold rev.
  induction l.
  + simpl.
    trivial.
  + simpl.
    (* No way! Induction hypothesis is no good. We need stronger assumptions... *)
Abort.

(* auxilary lemma 1 *)
Lemma reva_reva : forall l1 lr : list A, reva lr l1 = app (reva [] l1) lr.
Proof.
  (* exercise *)
Admitted.

(* auxilary lemma 2 *)
Lemma reva_app : forall l1 l2 lr, reva lr (app l1 l2) = app (reva [] l2) (reva lr l1).
Proof.
  (* exercise *)
Admitted.

(* Now, we can do the proof! *)
Lemma rev_rev : forall l : list A, rev (rev l) = l.
Proof.
  (* exercise *)
Admitted.

End Lists_of_A.