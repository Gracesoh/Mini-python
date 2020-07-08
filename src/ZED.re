/* starting with delta-less ZED to get the semantics right */

type hole = unit;

type vid = string;

type zexp('op) = {
  op: 'op,
  args: list(aexp),
}

and zctxt('op) = {
  op: 'op,
  args: list(aexp),
  values: list(value),
}

and zpreval('op) = {
  op: 'op,
  values: list(value),
}

and lambda = {
  vid,
  exp,
}

and aexp_op =
  | Var(vid)
  | App /* (aexp, aexp) */
  | Lam(lambda)
  | Num(int)
  | Add /* (aexp, aexp) */
  | Bracket(exp)

and aexp = zexp(op)

and exp_op =
  | Lift(aexp)
  | Let(vid, /* aexp, */ exp)

and exp = zexp(op)

and op =
  | Exp(exp_op)
  | AExp(aexp_op)

and value =
  | VNum(int)
  | Clo(lambda, env)

and binding = {
  vid,
  value,
}
and env = list(binding);

type focus =
  /* | AExp(aexp)
     | Exp(exp) */
  | ZExp(zexp(op))
  | ZPreVal(zpreval(op))
  | Value(value);

type ctxts = list(zctxt(op)); /* heterogeneous types are too hard :( */

type zipper = {
  focus,
  ctxts,
};

type frame = {
  ctxts,
  env,
};

type stack = list(frame);

type config = {
  zipper,
  env,
  stack,
};

let rec lookup = (x: vid, env: env): option(value) =>
  switch (env) {
  | [] => None
  | [{vid: y, value: v}, ...env] =>
    if (x == y) {
      Some(v);
    } else {
      lookup(x, env);
    }
  };

/* type option(config) =
| None
| Some(config); */

let step = (c: config): option(config) =>
  switch (c) {
  /* val */
  | {zipper: {focus: ZExp({op: AExp(Var(x)), args: []}), ctxts}, env, stack} =>
    switch (lookup(x, env)) {
    | None => None
    | Some(v) => Some({
                   zipper: {
                     focus: Value(v),
                     ctxts,
                   },
                   env,
                   stack,
                 })
    }
  /* lam */
  | {zipper: {focus: ZExp({op: AExp(Lam(l)), args: []}), ctxts}, env, stack} =>
    Some({
      zipper: {
        focus: Value(Clo(l, env)),
        ctxts,
      },
      env,
      stack,
    })
  /* zipper skip */
  | {zipper: {focus: ZExp({op, args: []}), ctxts}, env, stack} =>
    Some({
      zipper: {
        focus: ZPreVal({op, values: []}),
        ctxts,
      },
      env,
      stack,
    })
  /* zipper begin */
  | {zipper: {focus: ZExp({op, args: [a, ...args]}), ctxts}, env, stack} =>
    Some({
      zipper: {
        focus: ZExp(a),
        ctxts: [{op, args, values: []}, ...ctxts],
      },
      env,
      stack,
    })
  /* zipper continue */
  | {
      zipper: {focus: Value(v), ctxts: [{op, args: [a, ...args], values}, ...ctxts]},
      env,
      stack,
    } =>
    Some({
      zipper: {
        focus: ZExp(a),
        ctxts: [{op, args, values: [v, ...values]}, ...ctxts],
      },
      env,
      stack,
    })
  /* zipper end */
  | {zipper: {focus: Value(v), ctxts: [{op, args: [], values}, ...ctxts]}, env, stack} =>
    Some({
      zipper: {
        focus: ZPreVal({op, values: List.rev([v, ...values])}),
        ctxts,
      },
      env,
      stack,
    })
  /* app enter */
  | {
      zipper: {
        focus: ZPreVal({op: AExp(App), values: [Clo({vid: x, exp: e}, env), v2]}),
        ctxts,
      },
      env: env',
      stack,
    } =>
    Some({
      zipper: {
        focus: ZExp(e),
        ctxts: [],
      },
      env: [{vid: x, value: v2}, ...env],
      stack: [{ctxts, env: env'}, ...stack],
    })
  /* app exit */
  | {zipper: {focus: Value(v), ctxts: []}, env, stack: [{ctxts, env: env'}, ...stack]} =>
    Some({
      zipper: {
        focus: Value(v),
        ctxts,
      },
      env: env',
      stack,
    })
  /* let */
  | {zipper: {focus: ZPreVal({op: Exp(Let(x, e2)), values: [v1]}), ctxts}, env, stack} =>
    Some({
      zipper: {
        focus: ZExp(e2),
        ctxts,
      },
      env: [{vid: x, value: v1}, ...env],
      stack,
    })
  /* num */
  | {zipper: {focus: ZPreVal({op: AExp(Num(n)), values: []}), ctxts}, env, stack} =>
    Some({
      zipper: {
        focus: Value(VNum(n)),
        ctxts,
      },
      env,
      stack,
    })
  /* add */
  | {
      zipper: {focus: ZPreVal({op: AExp(Add), values: [VNum(v1), VNum(v2)]}), ctxts},
      env,
      stack,
    } =>
    let v3 = v1 + v2;
    Some({
      zipper: {
        focus: Value(VNum(v3)),
        ctxts,
      },
      env,
      stack,
    });
  /* bracket */
  | {zipper: {focus: ZPreVal({op: AExp(Bracket(e)), values: []}), ctxts}, env, stack} =>
    Some({
      zipper: {
        focus: ZExp(e),
        ctxts: [],
      },
      env,
      stack: [{ctxts, env}, ...stack],
    })
  /* lift */
  | {zipper: {focus: ZPreVal({op: Exp(Lift(ae)), values: []}), ctxts}, env, stack} =>
    Some({
      zipper: {
        focus: ZExp(ae),
        ctxts,
      },
      env,
      stack,
    })
  | _ => None
  };

let vidFromZEDLang = vid => vid;

let intFromZEDLang = int => int;

let rec lambdaFromZEDLang = ({vid, exp}: ZEDLang.lambda): lambda => {
  vid: vidFromZEDLang(vid),
  exp: expFromZEDLang(exp),
}

and aexpFromZEDLang = (aexp: ZEDLang.aexp): aexp =>
  switch (aexp) {
  | Var(vid) => {op: AExp(Var(vidFromZEDLang(vid))), args: []}
  | App(aexp1, aexp2) => {op: AExp(App), args: List.map(aexpFromZEDLang, [aexp1, aexp2])}
  | Lam(lambda) => {op: AExp(Lam(lambdaFromZEDLang(lambda))), args: []}
  | Num(int) => {op: AExp(Num(intFromZEDLang(int))), args: []}
  | Add(aexp1, aexp2) => {op: AExp(Add), args: List.map(aexpFromZEDLang, [aexp1, aexp2])}
  | Bracket(exp) => {op: AExp(Bracket(expFromZEDLang(exp))), args: []}
  }

and expFromZEDLang = (exp: ZEDLang.exp): exp =>
  switch (exp) {
  | Lift(aexp) => {op: Exp(Lift(aexpFromZEDLang(aexp))), args: []}
  | Let(vid, aexp, exp) => {
      op: Exp(Let(vidFromZEDLang(vid), expFromZEDLang(exp))),
      args: List.map(aexpFromZEDLang, [aexp]),
    }
  };

/* def inject (e : exp) : state := ⟨sum.inl e, ⟨option.none, env.emp⟩, []⟩ */
let inject = (e: exp): config => {
  zipper: {
    focus: ZExp(e),
    ctxts: [],
  },
  env: [],
  stack: [],
};

/* def is_final : state -> Prop
   | ⟨sum.inr v, ⟨option.none, env⟩, []⟩ := true
   | _ := false */

let isFinal = (c: config): bool =>
  switch (c) {
  | {zipper: {focus: Value(_), ctxts: []}, env: _, stack: []} => true
  | _ => false
  };

let rec iterateMaybeAux = (f, x) =>
  switch (x) {
  | None => []
  | Some(x) =>
    let fx = f(x);
    [x, ...iterateMaybeAux(f, fx)];
  };

let advance = step;

let rec takeWhileInclusive = (p, l) =>
  switch (l) {
  | [] => []
  | [x, ...xs] => [
      x,
      ...if (p(x)) {
           takeWhileInclusive(p, xs);
         } else {
           [];
         },
    ]
  };

let iterateMaybe = (f: 'a => option('a), x: 'a): list('a) => iterateMaybeAux(f, Some(x));

let interpretTrace = (p: exp): list(config) => {
  let states = iterateMaybe(step, inject(p));
  takeWhileInclusive(c => !isFinal(c), states);
};