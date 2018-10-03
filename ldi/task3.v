(* Coq assignment - LDI 2018

Prove the lemmas given below (and replace Admitted with Qed).

It is not allowed to: 
1. import other modules than List,
2. define Ltac tactics,
3. erase statements of the lemma (if you fail to prove a lemma leave Admitted).

It is allowed to:

1. introduce your own definitions and auxiliary lemmas,
2. change the order of the lemmas to prove,
3. add comments.

Remember about revert/generalize tactics.
*)



Require Import List.
Set Implicit Arguments.

Section Zal.

Check list.
Print list.

Variable A: Type.

Inductive podciag : list A -> list A -> Prop :=
| PC_Nil : forall l, podciag nil l
| PC_ConsH : forall a l1 l2, podciag l1 l2 -> podciag (cons a l1) (cons a l2)
| PC_ConsT : forall a l1 l2, podciag l1 l2 -> podciag l1 (cons a l2).

Inductive prefix : list A -> list A -> Prop :=
| P_Nil : forall l, prefix nil l
| P_Cons : forall a l1 l2, prefix l1 l2 -> prefix (cons a l1) (cons a l2).

Inductive sufix : list A -> list A -> Prop :=
| S_Nil : forall l, sufix l l
| S_Cons : forall a l1 l2, sufix l1 l2 -> sufix l1 (cons a l2).

Inductive podlista : list A -> list A -> Prop :=
| PL_Base : forall l1 l2, prefix l1 l2 -> podlista l1 l2
| PL_Cons : forall a l1 l2, podlista l1 l2 -> podlista l1 (cons a l2).


Lemma Prefix_Podlista : forall l1 l2, prefix l1 l2 -> podlista l1 l2.
Proof.
  apply PL_Base.
Qed.

Lemma Sufix_Podlista : forall l1 l2, sufix l1 l2 -> podlista l1 l2.
Proof.
  intros.
  induction H.
  + apply PL_Base. induction l. apply P_Nil. apply P_Cons. assumption.
  + apply PL_Cons. assumption.
Qed.

Lemma Prefix_Podciag : forall l1 l2, prefix l1 l2 -> podciag l1 l2.
Proof.
  intros.
  induction H.
  * apply PC_Nil.
  * apply PC_ConsH. trivial.
Qed.

Lemma Podlista_Podciag: forall l1 l2, podlista l1 l2 -> podciag l1 l2.
Proof.
  intros.
  induction H.
  * induction H.
    + apply PC_Nil.
    + apply PC_ConsH. trivial.
  * apply PC_ConsT. auto.
Qed.

Lemma Append_Podciag_Podciag_Podciag: 
      forall p1 l1 p2 l2, podciag p1 l1 -> podciag p2 l2 
       -> podciag (p1 ++ p2) (l1 ++ l2).
Proof.
  intros.
  induction H.
  * simpl. induction l.
    + simpl. trivial.
    + simpl. apply PC_ConsT. auto.
  * simpl. apply PC_ConsH. auto.
  * simpl. apply PC_ConsT. auto.
Qed.

Lemma Append_Eq_Prefix_Prefix:
      forall l p2 l2, prefix p2 l2 -> prefix (l ++ p2)(l ++ l2).
Proof.
  intros.
  induction l.
  * simpl. trivial.
  * simpl. apply P_Cons. auto.
Qed.

Lemma Append_Sufix_Prefix_Podlista:
      forall p1 l1 p2 l2, sufix p1 l1 -> prefix p2 l2
       -> podlista (p1 ++ p2) (l1 ++ l2).
Proof.
  intros.
  induction H.
  * apply PL_Base. induction l.
    + simpl. auto.
    + simpl. apply P_Cons. auto.
  * simpl. apply PL_Cons. auto.
Qed.

Definition sufixD (s l : list A):= exists p, p ++ s = l.

Lemma Sufix_Appending: forall s x, sufix s (x ++ s).
Proof.
  intros. induction x.
  * simpl. apply S_Nil.
  * simpl. apply S_Cons. auto.
Qed.

Lemma Sufix_SufixD: forall s l, sufix s l <-> sufixD s l.
Proof.
  intros. unfold iff. split.
  * intros. induction H.
    + exists nil. simpl. tauto.
    + inversion IHsufix. exists (a :: x). simpl. rewrite H0. tauto.
  * intros. destruct H. rewrite <- H. apply Sufix_Appending.
Qed.

Lemma Sufix_Prefix: forall s l, sufix s l -> exists p, prefix p l /\ p ++ s = l.
Proof.
  intros. induction H.
  + exists nil. simpl. split. apply P_Nil. tauto.
  + destruct IHsufix. destruct H0. exists (a::x). split.
    * apply P_Cons. auto.
    * simpl. rewrite H1. tauto.
Qed.

Lemma Trans_Podciag: forall l1 l2 l3, podciag l1 l2 -> podciag l2 l3 -> podciag l1 l3.
Proof.
  intros.
  revert l1 H.
  induction H0.
  * intros. inversion H. apply PC_Nil.
  * intros. inversion H.
    + apply PC_Nil.
    + apply PC_ConsH. apply IHpodciag. apply H3.
    + apply PC_ConsT. apply IHpodciag. apply H3.
  * intros. apply PC_ConsT. apply IHpodciag. apply H.
Qed.

End Zal.