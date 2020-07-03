open Sidewinder;
open Bobcat;
open ZED;

let vizVid = (vid: vid) =>
  Some(ConfigIR.mk(~name="vid", ~nodes=[], ~render=_ => Theia.str(vid), ()));

let vizInt = (int: int) =>
  Some(ConfigIR.mk(~name="int", ~nodes=[], ~render=_ => Theia.str(string_of_int(int)), ()));

let rec vizZExp = ({op, args}: zexp('a)) =>
  Some(ConfigIR.mk(~name="zexp", ~nodes=[vizOp(op), vizAExps(args)], ~render=Theia.hSeq, ()))

and vizZCtxt = ({op, args, values}: zctxt('a)) =>
  Some(
    ConfigIR.mk(
      ~name="zctxt",
      ~nodes=[vizOp(op), vizValues(values), None, vizAExps(args)],
      ~render=Theia.hSeq,
      (),
    ),
  )

and vizCtxts = (ctxts: ctxts) =>
  switch (ctxts) {
  | [] => Some(ConfigIR.mk(~name="ctxts_empty", ~nodes=[], ~render=_ => Theia.hole(), ()))
  | [zctxt, ...ctxts] =>
    Some(
      ConfigIR.mk(
        ~name="ctxts_cons",
        ~nodes=[vizZCtxt(zctxt), vizCtxts(ctxts)],
        ~render=Theia.vSeq,
        (),
      ),
    )
  }

and vizZPreVal = ({op, values}: zpreval('a)) =>
  Some(
    ConfigIR.mk(
      ~name="zpreval",
      ~nodes=[vizOp(op), vizValues(values)],
      ~render=Theia.hSeq,
      (),
    ),
  )

and vizLambda = ({vid, exp}: lambda) =>
  Some(
    ConfigIR.mk(
      ~name="lambda",
      ~nodes=[vizVid(vid), vizExp(exp)],
      ~render=([vid, exp]) => Theia.hSeq([Theia.str("\\"), vid, Theia.str("."), exp]),
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
        ~render=([vid]) => Theia.noOp(vid, []),
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
              [Theia.str("("), f, Theia.str(")"), Theia.str("("), x, Theia.str(")")],
            ),
        (),
      ),
    )

  | Lam(lambda) =>
    Some(
      ConfigIR.mk(
        ~name="lam",
        ~nodes=[vizLambda(lambda)],
        ~render=([lambda]) => Theia.noOp(lambda, []),
        (),
      ),
    )
  | Num(int) =>
    Some(ConfigIR.mk(~name="num", ~nodes=[vizInt(int)], ~render=([int]) => int, ()))
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
                x,
                Theia.str(")"),
                Theia.str("+"),
                Theia.str("("),
                y,
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
        ~render=([exp]) => Theia.hSeq(~gap=2., [Theia.str("{"), exp, Theia.str("}")]),
        (),
      ),
    )
  }

and vizAExp = (aexp: aexp) => vizZExp(aexp)

and vizAExps = (aexps: list(aexp)) =>
  switch (aexps) {
  | [] => Some(ConfigIR.mk(~name="aexps_empty", ~nodes=[], ~render=_ => Theia.hole(), ()))
  | [aexp, ...aexps] =>
    Some(
      ConfigIR.mk(
        ~name="aexps_cons",
        ~nodes=[vizAExp(aexp), vizAExps(aexps)],
        ~render=Theia.hSeq,
        (),
      ),
    )
  }

and vizExpOp = (exp_op: exp_op) =>
  switch (exp_op) {
  | Lift(aexp) =>
    Some(
      ConfigIR.mk(
        ~name="lift",
        ~nodes=[vizAExp(aexp)],
        ~render=([aexp]) => Theia.noOp(aexp, []),
        (),
      ),
    )
  | Let(vid, exp) =>
    Some(
      ConfigIR.mk(
        ~name="let",
        ~nodes=[vizVid(vid), None, vizExp(exp)],
        ~render=
          ([vid, ae1, exp]) =>
            Theia.vSeq([
              Theia.hSeq(
                ~gap=2.,
                [Theia.str("let"), vid, Theia.str("="), ae1, Theia.str("in")],
              ),
              exp,
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
        ~render=([exp_op]) => Theia.noOp(exp_op, []),
        (),
      ),
    )
  | AExp(aexp_op) =>
    Some(
      ConfigIR.mk(
        ~name="aexp",
        ~nodes=[vizAExpOp(aexp_op)],
        ~render=([aexp_op]) => Theia.noOp(aexp_op, []),
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
        ~render=([int]) => TheiaExtensions.value("num", int),
        (),
      ),
    )
  | Clo(lambda, env) =>
    Some(
      ConfigIR.mk(
        ~name="clo",
        ~nodes=[vizLambda(lambda), vizEnv(env)],
        ~render=
          ([lambda, env]) => Theia.hSeq([lambda, env] |> List.map(n => Theia.box(n, []))),
        (),
      ),
    )
  }

and vizValues = (values: list(value)) =>
  switch (values) {
  | [] => Some(ConfigIR.mk(~name="values_empty", ~nodes=[], ~render=_ => Theia.hole(), ()))
  | [value, ...values] =>
    Some(
      ConfigIR.mk(
        ~name="values_cons",
        ~nodes=[vizValue(value), vizValues(values)],
        ~render=Theia.hSeq,
        (),
      ),
    )
  }

and vizBinding = ({vid, value}: binding) =>
  Some(
    ConfigIR.mk(
      ~name="binding",
      ~nodes=[vizVid(vid), vizValue(value)],
      ~render=([vid, value]) => Theia.hSeq([vid, value]),
      (),
    ),
  )

and vizEnv = (env: list(binding)) =>
  switch (env) {
  | [] => Some(ConfigIR.mk(~name="env_empty", ~nodes=[], ~render=_ => Theia.str("env"), ()))
  | [b, ...env] =>
    Some(
      ConfigIR.mk(
        ~name="env_bind",
        ~nodes=[vizBinding(b), vizEnv(env)],
        ~render=([b, env]) => Theia.vSeq([env, b]),
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
        ~render=([zeo]) => Theia.noOp(zeo, []),
        (),
      ),
    )
  | ZPreVal(zpvo) =>
    Some(
      ConfigIR.mk(
        ~name="focus_zpreval",
        ~nodes=[vizZPreVal(zpvo)],
        ~render=([zpvo]) => Theia.noOp(zpvo, []),
        (),
      ),
    )
  | Value(value) =>
    Some(
      ConfigIR.mk(
        ~name="focus_value",
        ~nodes=[vizValue(value)],
        ~render=([value]) => Theia.noOp(value, []),
        (),
      ),
    )
  }

and vizZipper = ({focus, ctxts}: zipper) =>
  Some(
    ConfigIR.mk(
      ~name="zipper",
      ~nodes=[vizFocus(focus), vizCtxts(ctxts)],
      ~render=Theia.hSeq,
      (),
    ),
  )

and vizFrame = ({ctxts, env}: frame) =>
  Some(
    ConfigIR.mk(~name="frame", ~nodes=[vizEnv(env), vizCtxts(ctxts)], ~render=Theia.vSeq, ()),
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
        ~render=([frame, stack]) => Theia.vSeq([stack, frame]),
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
        Theia.hSeq(~gap=20., [Theia.vSeq(~gap=5., [env, zipper]), stack]),
    (),
  );
