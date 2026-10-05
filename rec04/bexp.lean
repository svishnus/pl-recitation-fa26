inductive BExp where
  | leaf (b : Bool)
  | and (l : BExp) (r : BExp)
  | or (l : BExp) (r : BExp)
  | not (b : BExp)

def eval(b : BExp) : Bool :=
  match b with
  | BExp.leaf b => b
  | BExp.and l r => eval l ∧ eval r -- \wedge
  | BExp.or l r => eval l ∨ eval r -- \or
  | BExp.not b => ¬ (eval b) -- \not

def opt(b : BExp) : BExp := b

def exampleExp : BExp := BExp.and (BExp.leaf true) (BExp.not (BExp.leaf false))

#eval eval (opt exampleExp) = eval exampleExp

theorem opt_correct : ∀ (b : BExp), eval (opt b) = eval b := by
  intro b
  unfold opt
  exact Bool.le_antisymm (fun a => a) fun a => a

def someFunc (t : Type) : Type := sorry

structure BExpProof where
  bexp : BExp
  result : Bool
  certificate : (eval bexp = result)

def crazy (xs : Array Int) : (if xs.size > 10 then String else Int) :=
  sorry

def crazier (xs : Array Int) : Unit :=
  if h : xs.size > 10 then
    let result := crazy xs
    sorry
  else
    sorry
