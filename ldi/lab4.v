
Lemma FirstLemma: forall A B C: Prop, (A -> B -> C) -> (A -> B) -> (A -> C).
(*intros.*)
intros A B C X Y Z.
apply X.
* assumption.
* apply Y.
  assumption.
Qed.

Print FirstLemma.


(* In natural deduction

G, A |- B
-------------------- (-> intro)
G |- A -> B


G |- A -> B      G |- A
-------------------------(-> elim) (application rule)
G |- B

*)

Lemma and_comm: forall P Q, P /\ Q -> Q /\ P.
Proof.
  intros.
  destruct H as [A B].
  split.
  * assumption.
  * assumption.
Qed.

Print and_comm.

Lemma or_comm: forall P Q, P \/ Q -> Q \/ P.
Proof.
intros.
destruct H.
 * right.
   assumption.
 * left.
   assumption.
Qed.

Print False.

Lemma ZFalszu: False -> 2+2=5.
Proof.
intro.
destruct H.
Qed.

Lemma triple_neg: forall (P: Prop), ~~~P <-> ~P.
Proof.
intros P. split; unfold not.
* intros A B. apply A. intros C. apply C. apply B.
* intros A B. apply B in A. apply A.
Qed.

Lemma lemma_exists1: exists x, x+1=2.
Proof.
exists 1.
simpl.
trivial.
Qed.

Lemma lemma_exists2: forall m n,
  (exists x, x+n = m) -> (n=m) \/ (exists k, m = S k).
Proof.
intros.
destruct H.
destruct x.
* left.
  simpl in H.
  assumption.
* right.
  simpl in H.
  exists (x+n).
  symmetry.
  assumption.
Qed.

  Variables P Q R S : Prop.

  Lemma id_P : P -> P.
  Proof.
    intros.
    apply H.
  Qed.

  Print id_P.

  Lemma imp_dist : (P -> Q -> R) -> (P -> Q) -> P -> R.
  Proof.
  intros.
  apply H.
  * apply H1.
  * apply H0. apply H1.
  Qed.

  Print imp_dist.

  Lemma imp_trans : (P -> Q) -> (Q -> R) -> P -> R.
  Proof.
  intros.
  apply H0. apply H. apply H1.
  Qed.

  Lemma diamond : (P -> Q) -> (P -> R) -> (Q -> R -> S) -> P -> S.
  Proof.
  intros.
  apply H1.
  * apply H. apply H2.
  * apply H0. apply H2.
  Qed.

  Lemma weak_peirce : ((((P -> Q) -> P) -> P) -> Q) -> Q.
  Proof.
  intros.
  apply H.
  intros.
  apply H0.
  intros.
  apply H.
  intros.
  apply H1.
  Qed.

  Lemma and_assoc : P /\ (Q /\ R) -> (P /\ Q) /\ R.
  Proof.
  intros.
  destruct H as [A B].
  destruct B as [B C].
  split.
  * split. apply A. apply B.
  * apply C.
  Qed.

  Lemma de_morgan_1 : ~(P \/ Q) <-> ~P /\ ~Q.
  Proof.
  unfold not. unfold iff.
  split.
  * intros. split.
    ** intros. apply H. left. apply H0.
    ** intros. apply H. right. apply H0.
  * intros. destruct H. case H0.
    ** apply H.
    ** apply H1.
  Qed.

  Lemma de_morgan_2 : ~P \/ ~Q -> ~(P /\ Q).
  Proof.
  unfold not. intros. destruct H0. case H.
  * intros. apply H2. apply H0.
  * intros. apply H2. apply H1.
  Qed.

  Definition Double_neg : Prop := forall P:Prop, ~~P -> P.
  Definition Peirce : Prop := forall P Q : Prop, ((P -> Q) -> P) -> P.

  Lemma Peirce_Double_neg : Peirce <-> Double_neg.
  Proof.
  unfold iff. unfold Peirce. unfold Double_neg. unfold not. split.
  * intros. apply H with False. intros. destruct H0. apply H1.
  * intros. apply H0. intros. apply H. intros. apply H. intros.
  Admitted.

  Theorem all_comm (A : Set) (R : A -> A -> Prop) : 
    (forall a b:A, R a b) -> forall a b:A, R b a.
  Proof.
  intros.
  apply H.
  Qed.

Lemma Lemma_frobenius (A : Set) (p : A -> Prop) (q : Prop):
  (exists x, q /\ p x) <-> q /\ exists x, p x.
Proof.
unfold iff.
split.
* intros. destruct H. destruct H. split.
  ** assumption.
  ** exists x. assumption.
* intros. destruct H. destruct H0. exists x. split.
  ** apply H.
  ** apply H0.
Qed.
