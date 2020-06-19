open Sidewinder;
open ZED;

let orHole = on =>
  switch (on) {
  | None => Theia.hole()
  | Some(n) => n
  };

let vizVid = (vid: vid) =>
  Some(ConfigIR.mk(~name="vid", ~nodes=[], ~render=_ => Theia.str(vid), ()));

let vizInt = (int: int) =>
  Some(ConfigIR.mk(~name="int", ~nodes=[], ~render=_ => Theia.str(string_of_int(int)), ()));

let rec vizZExp = ({op, args}: zexp('a)) =>
  Some(
    ConfigIR.mk(
      ~name="zexp",
      ~nodes=[vizOp(op), ...List.map(vizAExp, args)],
      ~render=([op, ...args]) => Theia.hSeq([orHole(op), ...List.map(orHole, args)]),
      (),
    ),
  )

and vizZCtxt = ({op, args, values}: zctxt('a)) =>
  Some(
    ConfigIR.mk(
      ~name="zctxt",
      ~nodes=[vizOp(op), ...List.map(vizValue, values)] @ [None, ...List.map(vizAExp, args)],
      ~render=([op, ...ahv]) => Theia.hSeq([orHole(op), ...List.map(orHole, ahv)]),
      (),
    ),
  )

and vizZPreVal = ({op, values}: zpreval('a)) =>
  Some(
    ConfigIR.mk(
      ~name="zpreval",
      ~nodes=[vizOp(op), ...List.map(vizValue, values)],
      ~render=([op, ...values]) => Theia.hSeq([orHole(op), ...List.map(orHole, values)]),
      (),
    ),
  )

and vizLambda = ({vid, exp}: lambda) =>
  Some(
    ConfigIR.mk(
      ~name="lambda",
      ~nodes=[vizVid(vid), vizExp(exp)],
      ~render=
        ([vid, exp]) =>
          Theia.hSeq([Theia.str("\\"), orHole(vid), Theia.str("."), orHole(exp)]),
      (),
    ),
  )

and vizAExpOp = (aexp_op: aexp_op) =>
  switch (aexp_op) {
  | Var(vid) =>
    Some(
      ConfigIR.mk(
        ~name="var",
        ~nodes=[vizVid(vid)],
        ~render=([vid]) => Theia.noOp(orHole(vid), []),
        (),
      ),
    )
  | App =>
    Some(
      ConfigIR.mk(
        ~name="app",
        ~nodes=[None, None],
        ~render=
          ([f, x]) =>
            Theia.hSeq(
              ~gap=2.,
              [
                Theia.str("("),
                orHole(f),
                Theia.str(")"),
                Theia.str("("),
                orHole(x),
                Theia.str(")"),
              ],
            ),
        (),
      ),
    )

  | Lam(lambda) =>
    Some(
      ConfigIR.mk(
        ~name="lam",
        ~nodes=[vizLambda(lambda)],
        ~render=([lambda]) => orHole(lambda),
        (),
      ),
    )
  | Num(int) =>
    Some(ConfigIR.mk(~name="num", ~nodes=[vizInt(int)], ~render=([int]) => orHole(int), ()))
  | Add =>
    Some(
      ConfigIR.mk(
        ~name="add",
        ~nodes=[None, None],
        ~render=
          ([x, y]) =>
            Theia.hSeq(
              ~gap=2.,
              [
                Theia.str("("),
                orHole(x),
                Theia.str(")"),
                Theia.str("+"),
                Theia.str("("),
                orHole(y),
                Theia.str(")"),
              ],
            ),
        (),
      ),
    )
  | Bracket(exp) =>
    Some(
      ConfigIR.mk(
        ~name="bracket",
        ~nodes=[vizExp(exp)],
        ~render=
          ([exp]) => Theia.hSeq(~gap=2., [Theia.str("{"), orHole(exp), Theia.str("}")]),
        (),
      ),
    )
  }

and vizAExp = (aexp: aexp) => vizZExp(aexp)

and vizExpOp = (exp_op: exp_op) =>
  switch (exp_op) {
  | Lift(aexp) =>
    Some(
      ConfigIR.mk(
        ~name="lift",
        ~nodes=[vizAExp(aexp)],
        ~render=([aexp]) => Theia.noOp(orHole(aexp), []),
        (),
      ),
    )
  | Let(vid, exp) =>
    Some(
      ConfigIR.mk(
        ~name="let",
        ~nodes=[vizVid(vid), vizExp(exp), None],
        ~render=
          ([vid, exp, ae1]) =>
            Theia.vSeq([
              Theia.hSeq(
                ~gap=2.,
                [
                  Theia.str("let"),
                  orHole(vid),
                  Theia.str("="),
                  orHole(ae1),
                  Theia.str("in"),
                ],
              ),
              orHole(exp),
            ]),
        (),
      ),
    )
  }

and vizExp = (exp: exp) => vizZExp(exp)

and vizOp = (op: op) =>
  switch (op) {
  | Exp(exp_op) =>
    Some(
      ConfigIR.mk(
        ~name="exp",
        ~nodes=[vizExpOp(exp_op)],
        ~render=([exp_op]) => orHole(exp_op),
        (),
      ),
    )
  | AExp(aexp_op) =>
    Some(
      ConfigIR.mk(
        ~name="aexp",
        ~nodes=[vizAExpOp(aexp_op)],
        ~render=([aexp_op]) => orHole(aexp_op),
        (),
      ),
    )
  }

and vizValue = (value: value) =>
  switch (value) {
  | VNum(int) =>
    Some(
      ConfigIR.mk(
        ~name="vnum",
        ~nodes=[vizInt(int)],
        ~render=([int]) => TheiaExtensions.value("num", orHole(int)),
        (),
      ),
    )
  | Clo(lambda, env) =>
    Some(
      ConfigIR.mk(
        ~name="clo",
        ~nodes=[vizLambda(lambda), vizEnv(env)],
        ~render=
          ([lambda, env]) =>
            Theia.hSeq([orHole(lambda), orHole(env)] |> List.map(n => Theia.box(n, []))),
        (),
      ),
    )
  }

and vizBinding = ({vid, value}: binding) =>
  Some(
    ConfigIR.mk(
      ~name="binding",
      ~nodes=[vizVid(vid), vizValue(value)],
      ~render=([vid, value]) => Theia.hSeq([orHole(vid), orHole(value)]),
      (),
    ),
  )

and vizEnv = (env: env) =>
  switch (env) {
  | [] => Some(ConfigIR.mk(~name="env_empty", ~nodes=[], ~render=_ => Theia.str("env"), ()))
  | [b, ...env] =>
    Some(
      ConfigIR.mk(
        ~name="env_bind",
        ~nodes=[vizBinding(b), vizEnv(env)],
        ~render=([b, env]) => Theia.vSeq([orHole(env), orHole(b)]),
        (),
      ),
    )
  }

and vizFocus = (focus: focus) =>
  switch (focus) {
  | ZExp(zeo) =>
    Some(
      ConfigIR.mk(
        ~name="focus_zexp",
        ~nodes=[vizZExp(zeo)],
        ~render=([zeo]) => orHole(zeo),
        (),
      ),
    )
  | ZPreVal(zpvo) =>
    Some(
      ConfigIR.mk(
        ~name="focus_zpreval",
        ~nodes=[vizZPreVal(zpvo)],
        ~render=([zpvo]) => Theia.noOp(orHole(zpvo), []),
        (),
      ),
    )
  | Value(value) =>
    Some(
      ConfigIR.mk(
        ~name="focus_value",
        ~nodes=[vizValue(value)],
        ~render=([value]) => Theia.noOp(orHole(value), []),
        (),
      ),
    )
  }

and vizZipper = ({focus, ctxts}: zipper) =>
  Some(
    ConfigIR.mk(
      ~name="zipper",
      ~nodes=[vizFocus(focus), ...List.map(vizZCtxt, ctxts)],
      ~render=([focus, ...ctxts]) => Theia.hSeq([orHole(focus), ...List.map(orHole, ctxts)]),
      (),
    ),
  )

and vizFrame = ({ctxts, env}: frame) =>
  Some(
    ConfigIR.mk(
      ~name="frame",
      ~nodes=[vizEnv(env), ...List.map(vizZCtxt, ctxts)],
      ~render=([env, ...ctxts]) => Theia.vSeq([orHole(env), ...List.map(orHole, ctxts)]),
      (),
    ),
  )

and vizStack = (stack: stack) =>
  switch (stack) {
  | [] =>
    Some(ConfigIR.mk(~name="stack_empty", ~nodes=[], ~render=([]) => Theia.str("stack"), ()))
  | [frame, ...stack] =>
    Some(
      ConfigIR.mk(
        ~name="stack_frame",
        ~nodes=[vizFrame(frame), vizStack(stack)],
        ~render=([frame, stack]) => Theia.vSeq([orHole(stack), orHole(frame)]),
        (),
      ),
    )
  };

let vizConfig = ({zipper, env, stack}: config) =>
  ConfigIR.mk(
    ~name="config",
    ~nodes=[vizZipper(zipper), vizEnv(env), vizStack(stack)],
    ~render=
      ([zipper, env, stack]) =>
        Theia.hSeq(
          ~gap=20.,
          [Theia.vSeq(~gap=5., [orHole(env), orHole(zipper)]), orHole(stack)],
        ),
    (),
  );