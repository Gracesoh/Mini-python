type vid = string;

type lambda = {
  vid,
  exp,
}

and aexp =
  | Var(vid)
  | App(aexp, aexp)
  | Lam(lambda)
  | Num(int)
  | Add(aexp, aexp)
  | Bracket(exp)

and exp =
  | Lift(aexp)
  | Let(vid, aexp, exp);
