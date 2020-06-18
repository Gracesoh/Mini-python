open Sidewinder.Theia;
// open TheiaExtensions;
open ZED;

/* TODO: remove flow argument */

let vizVid = ((uid, vid): vid) =>
  str(~uid, ~flowTag={flowNodeType: Leaf, uid, rootPath: []}, vid, ());

let vizInt = ((uid, int): int_uid) =>
  str(~uid, ~flowTag={flowNodeType: Leaf, uid, rootPath: []}, string_of_int(int), ());

let rec vizZExp = (vizOp, (uid, {op, args}): zexp('a)) =>
  noop(
    ~uid,
    ~flowTag={flowNodeType: Leaf, uid, rootPath: []},
    vizOp(op, vizAExps(args)),
    [],
    (),
  )

and vizZCtxt = (vizOp, (uid, {op, args, values}): zctxt('a), hole) =>
  noop(
    ~uid,
    ~flowTag={flowNodeType: Leaf, uid, rootPath: []},
    vizOp(op, vizValues(values) @ [hole, ...vizAExps(args)]),
    [],
    (),
  )

and vizZPreVal = (vizOp, (uid, {op, values}): zpreval('a)) =>
  noop(
    ~uid,
    ~flowTag={flowNodeType: Leaf, uid, rootPath: []},
    vizOp(op, vizValues(values)),
    [],
    (),
  )

and vizLambda = ((uid, {vid, exp}): lambda) =>
  hSeq(
    ~uid,
    ~flowTag={flowNodeType: Leaf, uid, rootPath: []},
    [str("\\", ()), vizVid(vid), str(".", ()), vizExp(exp)],
  )

and vizAExpOp = ((uid, aexp_op): aexp_op, inputs: list(Sidewinder.Kernel.node)) =>
  switch (aexp_op, inputs) {
  | (Var(vid), []) =>
    noop(~uid, ~flowTag={flowNodeType: Leaf, uid, rootPath: []}, vizVid(vid), [], ())
  | (Var(_), _) =>
    failwith("op Var expected input arity 0, but got " ++ string_of_int(List.length(inputs)))
  | (App, [f, x]) =>
    hSeq(
      ~uid,
      ~flowTag={flowNodeType: Dummy, uid, rootPath: []},
      ~gap=2.,
      [
        str(~flowTag={flowNodeType: Leaf, uid, rootPath: [0]}, "(", ()),
        f,
        str(~flowTag={flowNodeType: Leaf, uid, rootPath: [2]}, ")", ()),
        str(~flowTag={flowNodeType: Leaf, uid, rootPath: [3]}, "(", ()),
        x,
        str(~flowTag={flowNodeType: Leaf, uid, rootPath: [5]}, ")", ()),
      ],
    )
  | (App, _) =>
    failwith("op App expected input arity 2, but got " ++ string_of_int(List.length(inputs)))
  | (Lam(lambda), []) =>
    noop(~uid, ~flowTag={flowNodeType: Leaf, uid, rootPath: []}, vizLambda(lambda), [], ())
  | (Lam(_), _) =>
    failwith("op Lam expected input arity 0, but got " ++ string_of_int(List.length(inputs)))
  | (Num(int), []) =>
    noop(~uid, ~flowTag={flowNodeType: Leaf, uid, rootPath: []}, vizInt(int), [], ())
  | (Num(_), _) =>
    failwith("op Num expected input arity 0, but got " ++ string_of_int(List.length(inputs)))
  | (Add, [x, y]) =>
    hSeq(
      ~flowTag={flowNodeType: Dummy, uid, rootPath: []},
      ~gap=2.,
      [
        str(~flowTag={flowNodeType: Leaf, uid, rootPath: [0]}, "(", ()),
        x,
        str(~flowTag={flowNodeType: Leaf, uid, rootPath: [2]}, ")", ()),
        str(~flowTag={flowNodeType: Leaf, uid, rootPath: [3]}, "+", ()),
        str(~flowTag={flowNodeType: Leaf, uid, rootPath: [4]}, "(", ()),
        y,
        str(~flowTag={flowNodeType: Leaf, uid, rootPath: [6]}, ")", ()),
      ],
    )
  | (Add, _) =>
    failwith("op Add expected input arity 2, but got " ++ string_of_int(List.length(inputs)))
  | (Bracket(exp), []) =>
    hSeq(
      ~uid,
      ~flowTag={flowNodeType: Leaf, uid, rootPath: []},
      ~gap=2.,
      [str("{", ()), vizExp(exp), str("}", ())],
    )
  | (Bracket(_), _) =>
    failwith(
      "op Bracket expected input arity 0, but got " ++ string_of_int(List.length(inputs)),
    )
  }

and vizAExp = (aexp: aexp) => vizZExp(vizOp, aexp)

/* TODO: what to do with uid? */
and vizAExps = ((uid, aexps): aexps) =>
  switch (aexps) {
  | Empty => []
  | Cons(aexp, aexps) => [vizAExp(aexp), ...vizAExps(aexps)]
  }

and vizExpOp = ((uid, exp_op): exp_op, inputs: list(Sidewinder.Kernel.node)) =>
  switch (exp_op, inputs) {
  | (Lift(aexp), []) =>
    noop(~uid, ~flowTag={flowNodeType: Leaf, uid, rootPath: []}, vizAExp(aexp), [], ())
  | (Lift(_), _) =>
    failwith("op Lift expected input arity 0, but got " ++ string_of_int(List.length(inputs)))
  | (Let(vid, exp), [ae1]) =>
    vSeq(
      ~uid,
      ~flowTag={flowNodeType: Dummy, uid, rootPath: []},
      [
        hSeq(
          ~gap=2.,
          [
            str(~flowTag={flowNodeType: Leaf, uid, rootPath: [0, 0]}, "let", ()),
            vizVid(vid),
            str(~flowTag={flowNodeType: Leaf, uid, rootPath: [0, 2]}, "=", ()),
            ae1,
            str(~flowTag={flowNodeType: Leaf, uid, rootPath: [0, 4]}, "in", ()),
          ],
        ),
        vizExp(exp),
      ],
    )
  | (Let(_), _) =>
    failwith("op Let expected input arity 1, but got " ++ string_of_int(List.length(inputs)))
  }

and vizExp = (exp: exp) => vizZExp(vizOp, exp)

and vizOp = ((uid, op): op, inputs: list(Sidewinder.Kernel.node)) =>
  switch (op) {
  | Exp(exp_op) =>
    noop(
      ~uid,
      ~flowTag={flowNodeType: Leaf, uid, rootPath: []},
      vizExpOp(exp_op, inputs),
      [],
      (),
    )
  | AExp(aexp_op) =>
    noop(
      ~uid,
      ~flowTag={flowNodeType: Leaf, uid, rootPath: []},
      vizAExpOp(aexp_op, inputs),
      [],
      (),
    )
  }

and vizValue = ((uid, value): value) =>
  switch (value) {
  | VNum(int) =>
    TheiaExtensions.value(
      ~uid,
      ~flowTag={flowNodeType: Leaf, uid, rootPath: []},
      "num",
      vizInt(int),
    )
  | Clo(lambda, env) =>
    TheiaExtensions.value(
      ~uid,
      ~flowTag={flowNodeType: Leaf, uid, rootPath: []},
      "closure",
      hSeq([vizLambda(lambda), vizEnv(env)] |> List.map(n => box(n, [], ()))),
    )
  }

/* TODO: what to do with uid? */
and vizValues = (values: values) => {
  /* tail-recursively build list backwards */
  let rec aux = (acc, (uid, values): values) =>
    switch (values) {
    | Empty => acc
    | Cons(value, values) => aux([vizValue(value), ...acc], values)
    };
  aux([], values);
}

and vizBinding = ((uid, {vid, value}): binding) =>
  hSeq(~uid, ~flowTag={flowNodeType: Leaf, uid, rootPath: []}, [vizVid(vid), vizValue(value)])

and vizEnv = ((uid, env): env) =>
  switch (env) {
  | Empty => str(~uid, ~flowTag={flowNodeType: Leaf, uid, rootPath: []}, "env", ())
  | Cons(b, env) =>
    vSeq(~uid, ~flowTag={flowNodeType: Leaf, uid, rootPath: []}, [vizEnv(env), vizBinding(b)])
  }

and vizFocus = ((uid, focus): focus) =>
  switch (focus) {
  | ZExp(zeo) =>
    noop(~uid, ~flowTag={flowNodeType: Leaf, uid, rootPath: []}, vizZExp(vizOp, zeo), [], ())
  | ZPreVal(zpvo) =>
    noop(
      ~uid,
      ~flowTag={flowNodeType: Leaf, uid, rootPath: []},
      vizZPreVal(vizOp, zpvo),
      [],
      (),
    )
  | Value(value) =>
    noop(~uid, ~flowTag={flowNodeType: Leaf, uid, rootPath: []}, vizValue(value), [], ())
  }

/* TODO: what to do with uid? */
/* threads ctxts through */
/* TODO: might be reversed */
and vizCtxts = ((uid, ctxts): ctxts, hole) =>
  switch (ctxts) {
  | Empty => noop(~flowTag={flowNodeType: Leaf, uid, rootPath: []}, hole, [], ())
  | Cons(ctxt, ctxts) =>
    /* TODO: where to place uid? */
    let highlightHole =
      highlight(
        // ~flowTag={flowNodeType: Leaf, uid, rootPath: []},
        ~fill="hsla(240, 100%, 80%, 33%)",
        hole,
        [],
        (),
      );
    /* noop(
         ~flowTag={flowNodeType: Leaf, uid, rootPath: []},
         vizCtxts(ctxts, vizZCtxt(vizOp, ctxt, highlightHole)),
         [],
         (),
       ); */
    vizCtxts(
      ctxts,
      /* noop(
           ~flowTag={flowNodeType: Leaf, uid, rootPath: []},
           vizZCtxt(vizOp, ctxt, highlightHole),
           [],
           (),
         ), */ vizZCtxt(
        vizOp,
        ctxt,
        highlightHole,
      ),
    );
  }

and vizZipper = ((uid, {focus, ctxts}): zipper) =>
  noop(
    ~uid,
    ~flowTag={flowNodeType: Leaf, uid, rootPath: []},
    vizCtxts(ctxts, vizFocus(focus)),
    [],
    (),
  )

and vizFrame = ((uid, {ctxts, env}): frame) =>
  vSeq(
    ~uid,
    ~flowTag={flowNodeType: Leaf, uid, rootPath: []},
    [vizEnv(env), vizCtxts(ctxts, hole)],
  )

and vizStack = ((uid, stack): stack) =>
  switch (stack) {
  | Empty => str(~uid, ~flowTag={flowNodeType: Leaf, uid, rootPath: []}, "stack", ())
  | Cons(frame, stack) =>
    vSeq(
      ~uid,
      ~flowTag={flowNodeType: Leaf, uid, rootPath: []},
      [vizStack(stack), vizFrame(frame)],
    )
  };

let vizConfig =
    (((rule: string, flow: Sidewinder.Flow.t), (uid, {zipper, env, stack}): config)) => {
  vSeq(
    ~gap=30.,
    [
      str("rule: " ++ rule, ()),
      hSeq(
        ~uid,
        ~flowTag={flowNodeType: Leaf, uid, rootPath: []},
        ~gap=20.,
        [vSeq(~gap=5., [vizEnv(env), vizZipper(zipper)]), vizStack(stack)],
      ),
    ],
  );
};