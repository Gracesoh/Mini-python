open Sidewinder;
open ZEDDelta;

let vizVid = ((place, vid): vid) =>
  Some(ConfigIR.mk(~place, ~name="vid", ~nodes=[], ~render=_ => Theia.str(vid), ()));

let vizInt = ((place, int): int_uid) =>
  Some(
    ConfigIR.mk(~place, ~name="int", ~nodes=[], ~render=_ => Theia.str(string_of_int(int)), ()),
  );

let rec vizZExp = ((place, {op, args}): zexp('a)) =>
  Some(
    ConfigIR.mk(
      ~place,
      ~name="zexp",
      ~nodes=[vizOp(op), ...vizAExps(args)],
      ~render=Theia.hSeq,
      (),
    ),
  )

and vizZCtxt = ((place, {op, args, values}): zctxt('a)) =>
  Some(
    ConfigIR.mk(
      ~place,
      ~name="zctxt",
      ~nodes=[vizOp(op), ...vizValues(values)] @ [None, ...vizAExps(args)],
      ~render=Theia.hSeq,
      (),
    ),
  )

/* TODO: what to do with place? */
and vizCtxts = ((place, ctxts): ctxts) =>
  switch (ctxts) {
  | Empty => []
  | Cons(zctxt, ctxts) => [vizZCtxt(zctxt), ...vizCtxts(ctxts)]
  }

and vizZPreVal = ((place, {op, values}): zpreval('a)) =>
  Some(
    ConfigIR.mk(
      ~place,
      ~name="zpreval",
      ~nodes=[vizOp(op), ...vizValues(values)],
      ~render=Theia.hSeq,
      (),
    ),
  )

and vizLambda = ((place, {vid, exp}): lambda) =>
  Some(
    ConfigIR.mk(
      ~place,
      ~name="lambda",
      ~nodes=[vizVid(vid), vizExp(exp)],
      ~render=([vid, exp]) => Theia.hSeq([Theia.str("\\"), vid, Theia.str("."), exp]),
      (),
    ),
  )

and vizAExpOp = ((place, aexp_op): aexp_op) =>
  switch (aexp_op) {
  | Var(vid) =>
    Some(
      ConfigIR.mk(
        ~place,
        ~name="var",
        ~nodes=[vizVid(vid)],
        ~render=([vid]) => Theia.noOp(vid, []),
        (),
      ),
    )
  | App =>
    Some(
      ConfigIR.mk(
        ~place,
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
        ~place,
        ~name="lam",
        ~nodes=[vizLambda(lambda)],
        ~render=([lambda]) => lambda,
        (),
      ),
    )
  | Num(int) =>
    Some(ConfigIR.mk(~place, ~name="num", ~nodes=[vizInt(int)], ~render=([int]) => int, ()))
  | Add =>
    Some(
      ConfigIR.mk(
        ~place,
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
        ~place,
        ~name="bracket",
        ~nodes=[vizExp(exp)],
        ~render=([exp]) => Theia.hSeq(~gap=2., [Theia.str("{"), exp, Theia.str("}")]),
        (),
      ),
    )
  }

and vizAExp = (aexp: aexp) => vizZExp(aexp)

/* TODO: what to do with place? */
and vizAExps = ((place, aexps): aexps) =>
  switch (aexps) {
  | Empty => []
  | Cons(aexp, aexps) => [vizAExp(aexp), ...vizAExps(aexps)]
  }

and vizExpOp = ((place, exp_op): exp_op) =>
  switch (exp_op) {
  | Lift(aexp) =>
    Some(
      ConfigIR.mk(
        ~place,
        ~name="lift",
        ~nodes=[vizAExp(aexp)],
        ~render=([aexp]) => Theia.noOp(aexp, []),
        (),
      ),
    )
  | Let(vid, exp) =>
    Some(
      ConfigIR.mk(
        ~place,
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

and vizOp = ((place, op): op) =>
  switch (op) {
  | Exp(exp_op) =>
    Some(
      ConfigIR.mk(
        ~place,
        ~name="exp",
        ~nodes=[vizExpOp(exp_op)],
        ~render=([exp_op]) => exp_op,
        (),
      ),
    )
  | AExp(aexp_op) =>
    Some(
      ConfigIR.mk(
        ~place,
        ~name="aexp",
        ~nodes=[vizAExpOp(aexp_op)],
        ~render=([aexp_op]) => aexp_op,
        (),
      ),
    )
  }

and vizValue = ((place, value): value) =>
  switch (value) {
  | VNum(int) =>
    Some(
      ConfigIR.mk(
        ~place,
        ~name="vnum",
        ~nodes=[vizInt(int)],
        ~render=([int]) => TheiaExtensions.value("num", int),
        (),
      ),
    )
  | Clo(lambda, env) =>
    Some(
      ConfigIR.mk(
        ~place,
        ~name="clo",
        ~nodes=[vizLambda(lambda), vizEnv(env)],
        ~render=
          ([lambda, env]) => Theia.hSeq([lambda, env] |> List.map(n => Theia.box(n, []))),
        (),
      ),
    )
  }

/* TODO: what to do with place? */
and vizValues = ((place, values): values) =>
  switch (values) {
  | Empty => []
  | Cons(value, values) => [vizValue(value), ...vizValues(values)]
  }

and vizBinding = ((place, {vid, value}): binding) =>
  Some(
    ConfigIR.mk(
      ~place,
      ~name="binding",
      ~nodes=[vizVid(vid), vizValue(value)],
      ~render=([vid, value]) => Theia.hSeq([vid, value]),
      (),
    ),
  )

and vizEnv = ((place, env): env) =>
  switch (env) {
  | Empty =>
    Some(ConfigIR.mk(~place, ~name="env_empty", ~nodes=[], ~render=_ => Theia.str("env"), ()))
  | Cons(b, env) =>
    Some(
      ConfigIR.mk(
        ~place,
        ~name="env_bind",
        ~nodes=[vizBinding(b), vizEnv(env)],
        ~render=([b, env]) => Theia.vSeq([env, b]),
        (),
      ),
    )
  }

and vizFocus = ((place, focus): focus) =>
  switch (focus) {
  | ZExp(zeo) =>
    Some(
      ConfigIR.mk(
        ~place,
        ~name="focus_zexp",
        ~nodes=[vizZExp(zeo)],
        ~render=([zeo]) => zeo,
        (),
      ),
    )
  | ZPreVal(zpvo) =>
    Some(
      ConfigIR.mk(
        ~place,
        ~name="focus_zpreval",
        ~nodes=[vizZPreVal(zpvo)],
        ~render=([zpvo]) => Theia.noOp(zpvo, []),
        (),
      ),
    )
  | Value(value) =>
    Some(
      ConfigIR.mk(
        ~place,
        ~name="focus_value",
        ~nodes=[vizValue(value)],
        ~render=([value]) => Theia.noOp(value, []),
        (),
      ),
    )
  }

and vizZipper = ((place, {focus, ctxts}): zipper) =>
  Some(
    ConfigIR.mk(
      ~place,
      ~name="zipper",
      ~nodes=[vizFocus(focus), ...vizCtxts(ctxts)],
      ~render=Theia.hSeq,
      (),
    ),
  )

and vizFrame = ((place, {ctxts, env}): frame) =>
  Some(
    ConfigIR.mk(
      ~place,
      ~name="frame",
      ~nodes=[vizEnv(env), ...vizCtxts(ctxts)],
      ~render=Theia.vSeq,
      (),
    ),
  )

and vizStack = ((place, stack): stack) =>
  switch (stack) {
  | Empty =>
    Some(
      ConfigIR.mk(
        ~place,
        ~name="stack_empty",
        ~nodes=[],
        ~render=([]) => Theia.str("stack"),
        (),
      ),
    )
  | Cons(frame, stack) =>
    Some(
      ConfigIR.mk(
        ~place,
        ~name="stack_frame",
        ~nodes=[vizFrame(frame), vizStack(stack)],
        ~render=([frame, stack]) => Theia.vSeq([stack, frame]),
        (),
      ),
    )
  };

let vizConfig = ((place, {zipper, env, stack}): config) =>
  Some(
    ConfigIR.mk(
      ~place,
      ~name="config",
      ~nodes=[vizZipper(zipper), vizEnv(env), vizStack(stack)],
      ~render=
        ([zipper, env, stack]) =>
          Theia.hSeq(~gap=20., [Theia.vSeq(~gap=5., [env, zipper]), stack]),
      (),
    ),
  );

let vizRule = rule =>
  Some(ConfigIR.mk(~name="rule", ~nodes=[], ~render=_ => Theia.str("rule: " ++ rule), ()));

let vizState = (rule, config) =>
  ConfigIR.mk(
    ~name="state",
    ~nodes=[vizRule(rule), vizConfig(config)],
    ~render=Theia.vSeq(~gap=30.),
    (),
  );

/* filter out places in `n` except for those in `places` */
let filterPlaces = (places: list(Place.t), n: ConfigIR.node) => {
  let rec filterPlacesOption = (on: option(ConfigIR.node)) =>
    switch (on) {
    | None => None
    | Some(n) =>
      let nodes = List.map(filterPlacesOption, n.nodes);
      switch (n.place) {
      | Some(p) when !List.mem(p, places) => Some({...n, nodes, place: None})
      | _ => Some({...n, nodes})
      };
    };
  filterPlacesOption(Some(n))->Belt.Option.getExn;
};
