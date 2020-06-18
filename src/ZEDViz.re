open Sidewinder;
// open TheiaExtensions;
open ZED;

/* TODO: remove flow argument */

let vizVid = (vid: vid) =>
  ConfigIR.mk(~name="vid", ~nodes=None, ~render=_ => Theia.str(vid), ());

let vizInt = (int: int) =>
  ConfigIR.mk(~name="int", ~nodes=None, ~render=_ => Theia.str(string_of_int(int)), ());

// let rec vizZExp = ({op, args}: zexp('a)) =>
// ConfigIR.mk(~name="zexp", ~nodes=Some([vizOp(op), vizAExps(args)]), ~render=(Some([op, ...args])) => Theia.noOp(
//     vizOp(op, vizAExps(args)),
//     [],
//   ))

// and vizZCtxt = (vizOp, (uid, {op, args, values}): zctxt('a), hole) =>
//   noop(
//     ~uid,
//     ~flowTag={flowNodeType: Leaf, uid, rootPath: []},
//     vizOp(op, vizValues(values) @ [hole, ...vizAExps(args)]),
//     [],
//     (),
//   )

// and vizZPreVal = (vizOp, (uid, {op, values}): zpreval('a)) =>
//   noop(
//     ~uid,
//     ~flowTag={flowNodeType: Leaf, uid, rootPath: []},
//     vizOp(op, vizValues(values)),
//     [],
//     (),
//   )

// and vizLambda = ((uid, {vid, exp}): lambda) =>
//   hSeq(
//     ~uid,
//     ~flowTag={flowNodeType: Leaf, uid, rootPath: []},
//     [str("\\", ()), vizVid(vid), str(".", ()), vizExp(exp)],
//   )

// and vizAExpOp = ((uid, aexp_op): aexp_op, inputs: list(Sidewinder.Kernel.node)) =>
//   switch (aexp_op, inputs) {
//   | (Var(vid), []) =>
//     noop(~uid, ~flowTag={flowNodeType: Leaf, uid, rootPath: []}, vizVid(vid), [], ())
//   | (Var(_), _) =>
//     failwith("op Var expected input arity 0, but got " ++ string_of_int(List.length(inputs)))
//   | (App, [f, x]) =>
//     hSeq(
//       ~uid,
//       ~flowTag={flowNodeType: Dummy, uid, rootPath: []},
//       ~gap=2.,
//       [
//         str(~flowTag={flowNodeType: Leaf, uid, rootPath: [0]}, "(", ()),
//         f,
//         str(~flowTag={flowNodeType: Leaf, uid, rootPath: [2]}, ")", ()),
//         str(~flowTag={flowNodeType: Leaf, uid, rootPath: [3]}, "(", ()),
//         x,
//         str(~flowTag={flowNodeType: Leaf, uid, rootPath: [5]}, ")", ()),
//       ],
//     )
//   | (App, _) =>
//     failwith("op App expected input arity 2, but got " ++ string_of_int(List.length(inputs)))
//   | (Lam(lambda), []) =>
//     noop(~uid, ~flowTag={flowNodeType: Leaf, uid, rootPath: []}, vizLambda(lambda), [], ())
//   | (Lam(_), _) =>
//     failwith("op Lam expected input arity 0, but got " ++ string_of_int(List.length(inputs)))
//   | (Num(int), []) =>
//     noop(~uid, ~flowTag={flowNodeType: Leaf, uid, rootPath: []}, vizInt(int), [], ())
//   | (Num(_), _) =>
//     failwith("op Num expected input arity 0, but got " ++ string_of_int(List.length(inputs)))
//   | (Add, [x, y]) =>
//     hSeq(
//       ~flowTag={flowNodeType: Dummy, uid, rootPath: []},
//       ~gap=2.,
//       [
//         str(~flowTag={flowNodeType: Leaf, uid, rootPath: [0]}, "(", ()),
//         x,
//         str(~flowTag={flowNodeType: Leaf, uid, rootPath: [2]}, ")", ()),
//         str(~flowTag={flowNodeType: Leaf, uid, rootPath: [3]}, "+", ()),
//         str(~flowTag={flowNodeType: Leaf, uid, rootPath: [4]}, "(", ()),
//         y,
//         str(~flowTag={flowNodeType: Leaf, uid, rootPath: [6]}, ")", ()),
//       ],
//     )
//   | (Add, _) =>
//     failwith("op Add expected input arity 2, but got " ++ string_of_int(List.length(inputs)))
//   | (Bracket(exp), []) =>
//     hSeq(
//       ~uid,
//       ~flowTag={flowNodeType: Leaf, uid, rootPath: []},
//       ~gap=2.,
//       [str("{", ()), vizExp(exp), str("}", ())],
//     )
//   | (Bracket(_), _) =>
//     failwith(
//       "op Bracket expected input arity 0, but got " ++ string_of_int(List.length(inputs)),
//     )
//   }

// and vizAExp = (aexp: aexp) => vizZExp(vizOp, aexp)

// /* TODO: what to do with uid? */
// and vizAExps = (aexps: aexps) =>
//   switch (aexps) {
//   | Empty => []
//   | Cons(aexp, aexps) => [vizAExp(aexp), ...vizAExps(aexps)]
//   }

// and vizExpOp = ((uid, exp_op): exp_op, inputs: list(Sidewinder.Kernel.node)) =>
//   switch (exp_op, inputs) {
//   | (Lift(aexp), []) =>
//     noop(~uid, ~flowTag={flowNodeType: Leaf, uid, rootPath: []}, vizAExp(aexp), [], ())
//   | (Lift(_), _) =>
//     failwith("op Lift expected input arity 0, but got " ++ string_of_int(List.length(inputs)))
//   | (Let(vid, exp), [ae1]) =>
//     vSeq(
//       ~uid,
//       ~flowTag={flowNodeType: Dummy, uid, rootPath: []},
//       [
//         hSeq(
//           ~gap=2.,
//           [
//             str(~flowTag={flowNodeType: Leaf, uid, rootPath: [0, 0]}, "let", ()),
//             vizVid(vid),
//             str(~flowTag={flowNodeType: Leaf, uid, rootPath: [0, 2]}, "=", ()),
//             ae1,
//             str(~flowTag={flowNodeType: Leaf, uid, rootPath: [0, 4]}, "in", ()),
//           ],
//         ),
//         vizExp(exp),
//       ],
//     )
//   | (Let(_), _) =>
//     failwith("op Let expected input arity 1, but got " ++ string_of_int(List.length(inputs)))
//   }

// and vizExp = (exp: exp) => vizZExp(vizOp, exp)

// and vizOp = ((uid, op): op, inputs: list(Sidewinder.Kernel.node)) =>
//   switch (op) {
//   | Exp(exp_op) =>
//     noop(
//       ~uid,
//       ~flowTag={flowNodeType: Leaf, uid, rootPath: []},
//       vizExpOp(exp_op, inputs),
//       [],
//       (),
//     )
//   | AExp(aexp_op) =>
//     noop(
//       ~uid,
//       ~flowTag={flowNodeType: Leaf, uid, rootPath: []},
//       vizAExpOp(aexp_op, inputs),
//       [],
//       (),
//     )
//   }

let rec vizValue = (value: value) =>
  switch (value) {
  | VNum(int) =>
    ConfigIR.mk(
      ~name="vnum",
      ~nodes=Some([vizInt(int)]),
      ~render=(Some([int])) => TheiaExtensions.value("num", int),
      (),
    )
  | Clo(lambda, env) =>
    ConfigIR.mk(~name="clo", ~nodes=None, ~render=_ => Theia.str("TODO"), ())
  /* TheiaExtensions.value(
       ~uid,
       ~flowTag={flowNodeType: Leaf, uid, rootPath: []},
       "closure",
       hSeq([vizLambda(lambda), vizEnv(env)] |> List.map(n => box(n, [], ()))),
     ) */
  }

// /* TODO: what to do with uid? */
// and vizValues = (values: values) => {
//   /* tail-recursively build list backwards */
//   let rec aux = (acc, (uid, values): values) =>
//     switch (values) {
//     | Empty => acc
//     | Cons(value, values) => aux([vizValue(value), ...acc], values)
//     };
//   aux([], values);
// }

and vizBinding = ({vid, value}: binding) =>
  ConfigIR.mk(
    ~name="binding",
    ~nodes=Some([vizVid(vid), vizValue(value)]),
    ~render=(Some([vid, value])) => Theia.hSeq([vid, value]),
    (),
  )

and vizEnv = (env: env) =>
  switch (env) {
  | [] => ConfigIR.mk(~name="env_empty", ~nodes=None, ~render=_ => Theia.str("env"), ())
  | [b, ...env] =>
    ConfigIR.mk(
      ~name="env_bind",
      ~nodes=Some([vizBinding(b), vizEnv(env)]),
      ~render=(Some([b, env])) => Theia.vSeq([env, b]),
      (),
    )
  };

// and vizFocus = ((uid, focus): focus) =>
//   switch (focus) {
//   | ZExp(zeo) =>
//     noop(~uid, ~flowTag={flowNodeType: Leaf, uid, rootPath: []}, vizZExp(vizOp, zeo), [], ())
//   | ZPreVal(zpvo) =>
//     noop(
//       ~uid,
//       ~flowTag={flowNodeType: Leaf, uid, rootPath: []},
//       vizZPreVal(vizOp, zpvo),
//       [],
//       (),
//     )
//   | Value(value) =>
//     noop(~uid, ~flowTag={flowNodeType: Leaf, uid, rootPath: []}, vizValue(value), [], ())
//   }

// /* TODO: what to do with uid? */
// /* threads ctxts through */
// /* TODO: might be reversed */
// and vizCtxts = ((uid, ctxts): ctxts, hole) =>
//   switch (ctxts) {
//   | Empty => noop(~flowTag={flowNodeType: Leaf, uid, rootPath: []}, hole, [], ())
//   | Cons(ctxt, ctxts) =>
//     /* TODO: where to place uid? */
//     let highlightHole =
//       highlight(
//         // ~flowTag={flowNodeType: Leaf, uid, rootPath: []},
//         ~fill="hsla(240, 100%, 80%, 33%)",
//         hole,
//         [],
//         (),
//       );
//     /* noop(
//          ~flowTag={flowNodeType: Leaf, uid, rootPath: []},
//          vizCtxts(ctxts, vizZCtxt(vizOp, ctxt, highlightHole)),
//          [],
//          (),
//        ); */
//     vizCtxts(
//       ctxts,
//       /* noop(
//            ~flowTag={flowNodeType: Leaf, uid, rootPath: []},
//            vizZCtxt(vizOp, ctxt, highlightHole),
//            [],
//            (),
//          ), */ vizZCtxt(
//         vizOp,
//         ctxt,
//         highlightHole,
//       ),
//     );
//   }

// and vizZipper = ((uid, {focus, ctxts}): zipper) =>
//   noop(
//     ~uid,
//     ~flowTag={flowNodeType: Leaf, uid, rootPath: []},
//     vizCtxts(ctxts, vizFocus(focus)),
//     [],
//     (),
//   )

// and vizFrame = ((uid, {ctxts, env}): frame) =>
//   vSeq(
//     ~uid,
//     ~flowTag={flowNodeType: Leaf, uid, rootPath: []},
//     [vizEnv(env), vizCtxts(ctxts, hole)],
//   )

// and vizStack = ((uid, stack): stack) =>
//   switch (stack) {
//   | Empty => str(~uid, ~flowTag={flowNodeType: Leaf, uid, rootPath: []}, "stack", ())
//   | Cons(frame, stack) =>
//     vSeq(
//       ~uid,
//       ~flowTag={flowNodeType: Leaf, uid, rootPath: []},
//       [vizStack(stack), vizFrame(frame)],
//     )
//   };

let vizConfig = ({zipper, env, stack}: config) =>
  ConfigIR.mk(
    ~name="config",
    ~nodes=Some([/* vizZipper(zipper), */ vizEnv(env) /* , vizStack(stack) */]),
    ~render=
      (Some([/* zipper, */ env /* , stack */])) =>
        Theia.hSeq(~gap=20., [Theia.vSeq(~gap=5., [env /* , zipper */]) /* stack */]),
    (),
  );