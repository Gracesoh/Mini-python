/* Like FFS5, but uses more generic form of zipper */
/* starting with delta-less ZED to get the semantics right */
open UID;

type hole = unit;

type vid_aux = string;

type vid = makeUIDType(vid_aux);

type int_uid = makeUIDType(int);

type zexp_aux('op) = {
  op: 'op,
  args: aexps,
}

and zexp('op) = makeUIDType(zexp_aux('op))

and zctxt_aux('op) = {
  op: 'op,
  args: aexps,
  values,
}

and zctxt('op) = makeUIDType(zctxt_aux('op))

and zpreval_aux('op) = {
  op: 'op,
  values,
}

and zpreval('op) = makeUIDType(zpreval_aux('op))

and lambda_aux = {
  vid,
  exp,
}

and lambda = makeUIDType(lambda_aux)

and aexp_op_aux =
  | Var(vid)
  | App
  | Lam(lambda)
  | Num(int_uid)
  | Add
  | Bracket(exp)

and aexp_op = makeUIDType(aexp_op_aux)

and aexp_aux = zexp_aux(op)

and aexp = makeUIDType(aexp_aux)

and aexps_aux = makeUIDListTypeAux(aexp_aux)
and aexps = makeUIDListType(aexp_aux)

and exp_op_aux =
  | Lift(aexp)
  | Let(vid, exp)

and exp_op = makeUIDType(exp_op_aux)

and exp_aux = zexp_aux(op)

and exp = makeUIDType(exp_aux)

and op_aux =
  | Exp(exp_op)
  | AExp(aexp_op)

and op = makeUIDType(op_aux)

and value_aux =
  | VNum(int_uid)
  | Clo(lambda, env)

and value = makeUIDType(value_aux)

and values_aux = makeUIDListTypeAux(value_aux)
and values = makeUIDListType(value_aux)

and binding_aux = {
  vid,
  value,
}

and binding = makeUIDType(binding_aux)

and env_aux = makeUIDListTypeAux(binding_aux)
and env = makeUIDListType(binding_aux);

type focus_aux =
  | ZExp(zexp(op))
  | ZPreVal(zpreval(op))
  | Value(value);

type focus = makeUIDType(focus_aux);

type ctxts_aux = makeUIDListTypeAux(zctxt_aux(op));
type ctxts = makeUIDListType(zctxt_aux(op));

type zipper_aux = {
  focus,
  ctxts,
};

type zipper = makeUIDType(zipper_aux);

type frame_aux = {
  ctxts,
  env,
};

type frame = makeUIDType(frame_aux);

type stack_aux = makeUIDListTypeAux(frame_aux);
type stack = makeUIDListType(frame_aux);

type config_aux = {
  zipper,
  env,
  stack,
};

type config = makeUIDType(config_aux);

let mkVid = (v: vid_aux) => makeUIDConstructor("vid", v);
let mkInt = (n: int) => makeUIDConstructor("int", n);
let mkZExp = (ze: zexp_aux('a)) => makeUIDConstructor("zexp", ze);
let mkZCtxt = (zc: zctxt_aux('a)) => makeUIDConstructor("zctxt", zc);
let mkZPreVal = (zp: zpreval_aux('a)) => makeUIDConstructor("zpreval", zp);
let mkLambda = (l: lambda_aux) => makeUIDConstructor("lambda", l);
let mkAExpOp = (aeo: aexp_op_aux) => makeUIDConstructor("aexp_op", aeo);
let mkAExp = (ae: aexp_aux) => makeUIDConstructor("aexp", ae);
let mkAExps = (aes: aexps_aux) => makeUIDConstructor("aexps", aes);
let mkExpOp = (eo: exp_op_aux) => makeUIDConstructor("exp_op", eo);
let mkExp = (e: exp_aux) => makeUIDConstructor("exp", e);
let mkOp = (o: op_aux) => makeUIDConstructor("op", o);
let mkValue = (v: value_aux) => makeUIDConstructor("value", v);
let mkValues = (vs: values_aux) => makeUIDConstructor("values", vs);
let mkBinding = (b: binding_aux) => makeUIDConstructor("binding", b);
let mkEnv = (e: env_aux) => makeUIDConstructor("env", e);
let mkFocus = (f: focus_aux) => makeUIDConstructor("focus", f);
let mkCtxts = (cs: ctxts_aux) => makeUIDConstructor("ctxts", cs);
let mkZipper = (z: zipper_aux) => makeUIDConstructor("zipper", z);
let mkFrame = (f: frame_aux) => makeUIDConstructor("frame", f);
let mkStack = (s: stack_aux) => makeUIDConstructor("stack", s);
let mkConfig = (c: config_aux) => makeUIDConstructor("config", c);

/* to uid */
let vidToUID = (v: ZED.vid): vid => mkVid(v);

let intToUID = (n: int): int_uid => mkInt(n);

/* TODO: op should be compiled, too */
let rec zexpToUID = (opToUID, {op, args}: ZED.zexp('a)): zexp('b) =>
  mkZExp({op: opToUID(op), args: aexpsToUID(args)})

and zctxtToUID = (opToUID, {op, args, values}: ZED.zctxt('a)): zctxt('b) =>
  mkZCtxt({op: opToUID(op), args: aexpsToUID(args), values: valuesToUID(values)})

and zprevalToUID = (opToUID, {op, values}: ZED.zpreval('a)): zpreval('b) =>
  mkZPreVal({op: opToUID(op), values: valuesToUID(values)})

and lambdaToUID = ({vid, exp}: ZED.lambda): lambda =>
  mkLambda({vid: vidToUID(vid), exp: expToUID(exp)})

and aexp_opToUID = (aeo: ZED.aexp_op): aexp_op =>
  (
    switch (aeo) {
    | Var(vid) => Var(vidToUID(vid))
    | App => App
    | Lam(lambda) => Lam(lambdaToUID(lambda))
    | Num(int) => Num(intToUID(int))
    | Add => Add
    | Bracket(exp) => Bracket(expToUID(exp))
    }
  )
  |> mkAExpOp

and aexpToUID = (ae: ZED.aexp): aexp => zexpToUID(opToUID, ae)

and aexpsToUID = (aes: list(ZED.aexp)): aexps =>
  (
    switch (aes) {
    | [] => Empty
    | [ae, ...aes] => Cons(aexpToUID(ae), aexpsToUID(aes))
    }
  )
  |> mkAExps

and exp_opToUID = (eo: ZED.exp_op): exp_op =>
  (
    switch (eo) {
    | Lift(aexp) => Lift(aexpToUID(aexp))
    | Let(vid, exp) => Let(vidToUID(vid), expToUID(exp))
    }
  )
  |> mkExpOp

and expToUID = (e: ZED.exp): exp => zexpToUID(opToUID, e)

and opToUID = (o: ZED.op): op =>
  (
    switch (o) {
    | Exp(exp_op) => Exp(exp_opToUID(exp_op))
    | AExp(aexp_op) => AExp(aexp_opToUID(aexp_op))
    }
  )
  |> mkOp

and valueToUID = (v: ZED.value): value =>
  (
    switch (v) {
    | VNum(int) => VNum(intToUID(int))
    | Clo(lambda, env) => Clo(lambdaToUID(lambda), envToUID(env))
    }
  )
  |> mkValue

and valuesToUID = (vs: list(ZED.value)): values =>
  (
    switch (vs) {
    | [] => Empty
    | [v, ...vs] => Cons(valueToUID(v), valuesToUID(vs))
    }
  )
  |> mkValues

and bindingToUID = ({vid, value}: ZED.binding): binding =>
  mkBinding({vid: vidToUID(vid), value: valueToUID(value)})

and envToUID = (e: ZED.env): env =>
  (
    switch (e) {
    | [] => Empty
    | [b, ...e] => Cons(bindingToUID(b), envToUID(e))
    }
  )
  |> mkEnv

and focusToUID = (f: ZED.focus): focus =>
  (
    switch (f) {
    | ZExp(zeo) => ZExp(zexpToUID(opToUID, zeo))
    | ZPreVal(zpvo) => ZPreVal(zprevalToUID(opToUID, zpvo))
    | Value(value) => Value(valueToUID(value))
    }
  )
  |> mkFocus

and ctxtsToUID = (cs: ZED.ctxts): ctxts =>
  (
    switch (cs) {
    | [] => Empty
    | [c, ...cs] => Cons(zctxtToUID(opToUID, c), ctxtsToUID(cs))
    }
  )
  |> mkCtxts

and zipperToUID = ({focus, ctxts}: ZED.zipper): zipper =>
  mkZipper({focus: focusToUID(focus), ctxts: ctxtsToUID(ctxts)})

and frameToUID = ({ctxts, env}: ZED.frame): frame =>
  mkFrame({ctxts: ctxtsToUID(ctxts), env: envToUID(env)})

and stackToUID = (s: ZED.stack): stack =>
  (
    switch (s) {
    | [] => Empty
    | [f, ...s] => Cons(frameToUID(f), stackToUID(s))
    }
  )
  |> mkStack

and configToUID = ({zipper, env, stack}: ZED.config): config =>
  mkConfig({zipper: zipperToUID(zipper), env: envToUID(env), stack: stackToUID(stack)});

/* from uid */
let vidFromUID = ((_, vid): vid): ZED.vid => vid;

let intFromUID = ((_, int): int_uid): int => int;

let rec zexpFromUID = (opFromUID, (_, {op, args}): zexp('a)): ZED.zexp('b) => {
  op: opFromUID(op),
  args: aexpsFromUID(args),
}

and zctxtFromUID = (opFromUID, (_, {op, args, values}): zctxt('a)): ZED.zctxt('b) => {
  op: opFromUID(op),
  args: aexpsFromUID(args),
  values: valuesFromUID(values),
}

and zprevalFromUID = (opFromUID, (_, {op, values}): zpreval('a)): ZED.zpreval('b) => {
  op: opFromUID(op),
  values: valuesFromUID(values),
}

and lambdaFromUID = ((_, {vid, exp}): lambda): ZED.lambda => {
  vid: vidFromUID(vid),
  exp: expFromUID(exp),
}

and aexp_opFromUID = ((_, aeo): aexp_op): ZED.aexp_op =>
  switch (aeo) {
  | Var(vid) => Var(vidFromUID(vid))
  | App => App
  | Lam(lambda) => Lam(lambdaFromUID(lambda))
  | Num(int) => Num(intFromUID(int))
  | Add => Add
  | Bracket(exp) => Bracket(expFromUID(exp))
  }

and aexpFromUID = (aexp: aexp): ZED.aexp => zexpFromUID(opFromUID, aexp)

and aexpsFromUID = ((_, aexps): aexps): list(ZED.aexp) =>
  switch (aexps) {
  | Empty => []
  | Cons(ae, aexps) => [aexpFromUID(ae), ...aexpsFromUID(aexps)]
  }

and exp_opFromUID = ((_, exp_op): exp_op): ZED.exp_op =>
  switch (exp_op) {
  | Lift(aexp) => Lift(aexpFromUID(aexp))
  | Let(vid, exp) => Let(vidFromUID(vid), expFromUID(exp))
  }

and expFromUID = (exp: exp): ZED.exp => zexpFromUID(opFromUID, exp)

and opFromUID = ((_, op): op): ZED.op =>
  switch (op) {
  | Exp(exp_op) => Exp(exp_opFromUID(exp_op))
  | AExp(aexp_op) => AExp(aexp_opFromUID(aexp_op))
  }

and valueFromUID = ((_, value): value): ZED.value =>
  switch (value) {
  | VNum(int) => VNum(intFromUID(int))
  | Clo(lambda, env) => Clo(lambdaFromUID(lambda), envFromUID(env))
  }

and valuesFromUID = ((_, values): values): list(ZED.value) =>
  switch (values) {
  | Empty => []
  | Cons(value, values) => [valueFromUID(value), ...valuesFromUID(values)]
  }

and bindingFromUID = ((_, {vid, value}): binding): ZED.binding => {
  vid: vidFromUID(vid),
  value: valueFromUID(value),
}

and envFromUID = ((_, env): env): ZED.env =>
  switch (env) {
  | Empty => []
  | Cons(binding, env) => [bindingFromUID(binding), ...envFromUID(env)]
  }

and focusFromUID = ((_, focus): focus): ZED.focus =>
  switch (focus) {
  | ZExp(zeo) => ZExp(zexpFromUID(opFromUID, zeo))
  | ZPreVal(zpvo) => ZPreVal(zprevalFromUID(opFromUID, zpvo))
  | Value(value) => Value(valueFromUID(value))
  }

and ctxtsFromUID = ((_, ctxts): ctxts): ZED.ctxts =>
  switch (ctxts) {
  | Empty => []
  | Cons(ctxt, ctxts) => [zctxtFromUID(opFromUID, ctxt), ...ctxtsFromUID(ctxts)]
  }

and zipperFromUID = ((_, {focus, ctxts}): zipper): ZED.zipper => {
  focus: focusFromUID(focus),
  ctxts: ctxtsFromUID(ctxts),
}

and frameFromUID = ((_, {ctxts, env}): frame): ZED.frame => {
  ctxts: ctxtsFromUID(ctxts),
  env: envFromUID(env),
}

and stackFromUID = ((_, stack): stack): ZED.stack =>
  switch (stack) {
  | Empty => []
  | Cons(frame, stack) => [frameFromUID(frame), ...stackFromUID(stack)]
  }

and configFromUID = ((_, {zipper, env, stack}): config): ZED.config => {
  zipper: zipperFromUID(zipper),
  env: envFromUID(env),
  stack: stackFromUID(stack),
};

/* let rec lookup = (x: vid, env: env): option(value) =>
   switch (env) {
   | [] => None
   | [{vid: y, value: v}, ...env] =>
     if (x == y) {
       Some(v);
     } else {
       lookup(x, env);
     }
   }; */
/* TODO: x_uid and y_uid should be involved in animations */
let rec lookup = (x: vid, env: env): option((value, Sidewinder.Flow.linear)) => {
  let (x_uid, x_val) = x;
  let (env_uid, env_val) = env;
  switch (env_val) {
  | Empty => None
  | Cons((_, {vid: y, value: (v_uid, _) as v}), (_, env_val)) =>
    let (y_uid, y_val) = y;
    let (fresh_v_uid, _) as v = v |> valueFromUID |> valueToUID;
    if (x_val == y_val) {
      let fresh = "valLookup_" ++ rauc();
      Some((v, [(v_uid, [fresh_v_uid])]));
      // switch (v_val) {
      // | VNum((_, n)) =>
      //   Some((
      //     (fresh, VNum(("valLookup_int_" ++ rauc(), n))), /* hack special-casing so we get a fresh
      //      uid for num to avoid duplicated uids later. */
      //     [
      //       /* [|(x_uid, [fresh]), (env_uid, [fresh])|] */
      //       (v_uid, [v_uid, fresh]),
      //     ],
      //   ))
      // | _ =>
      //   Some((
      //     (fresh, v_val),
      //     [
      //       /* [|(x_uid, [fresh]), (env_uid, [fresh])|] */
      //       (v_uid, [v_uid, fresh]),
      //     ],
      //   ))
      // };
    } else {
      lookup(x, (env_uid, env_val));
    };
  };
};

/* TODO: improve transition annotations */
let step = ((_, c): config): option((config, (string, Sidewinder.Flow.linearExt))) =>
  switch (c) {
  /* val */
  /* | {zipper: {focus: ZExp({op: AExp(Var(x)), args: []}), ctxts}, env, stack} =>
     switch (lookup(x, env)) {
     | None => None
     | Some(v) => Some({
                    zipper: {
                      focus: Value(v),
                      ctxts,
                    },
                    env,
                    stack,
                  }) */
  | {
      zipper: (
        _,
        {
          focus: (
            _,
            ZExp((_, {op: (_, AExp((_, Var((x_uid, _) as x)))), args: (_, Empty)})),
          ),
          ctxts: (ctxts_uid, _) as ctxts,
        },
      ),
      env: (env_uid, _) as env,
      stack: (stack_uid, _) as stack,
    } =>
    switch (lookup(x, env)) {
    | Some((v, lookup_ribbon)) =>
      Some((
        mkConfig({zipper: mkZipper({focus: mkFocus(Value(v)), ctxts}), env, stack}),
        (
          "var",
          {
            pattern: [
              (x_uid, []),
              (ctxts_uid, [ctxts_uid]),
              (env_uid, [env_uid]),
              (stack_uid, [stack_uid]),
            ],
            extFn: lookup_ribbon,
          },
          /* TODO: add this back when external functions work */
          /* lookup_ribbon, */
        ),
      ))

    | None => None
    }
  /* lam */
  /* | {zipper: {focus: ZExp({op: AExp(Lam(l)), args: []}), ctxts}, env, stack} =>
     Some({
       zipper: {
         focus: Value(Clo(l, env)),
         ctxts,
       },
       env,
       stack,
     }) */
  | {
      zipper: (
        _,
        {
          focus: (
            _,
            ZExp((_, {op: (_, AExp((_, Lam((l_uid, _) as l)))), args: (_, Empty)})),
          ),
          ctxts: (ctxts_uid, _) as ctxts,
        },
      ),
      env: (env_uid, _) as env,
      stack: (stack_uid, _) as stack,
    } =>
    let (env2_uid, _) as env2 = envToUID(envFromUID(env)); /* use `from` and `to` to scrub all copied IDs */
    Some((
      mkConfig({
        zipper: mkZipper({focus: mkFocus(Value(mkValue(Clo(l, env2)))), ctxts}),
        env,
        stack,
      }),
      (
        "lam",
        {
          pattern: [
            (l_uid, [l_uid]),
            (env_uid, [env_uid, env2_uid]),
            (ctxts_uid, [ctxts_uid]),
            (stack_uid, [stack_uid]),
          ],
          extFn: [],
        },
      ),
    ));
  /* zipper skip */
  /* | {zipper: {focus: ZExp({op, args: []}), ctxts}, env, stack} =>
     Some({
       zipper: {
         focus: ZPreVal({op, values: []}),
         ctxts,
       },
       env,
       stack,
     }) */
  | {
      zipper: (
        _,
        {
          focus: (_, ZExp((_, {op: (op_uid, _) as op, args: (_, Empty)}))),
          ctxts: (ctxts_uid, _) as ctxts,
        },
      ),
      env: (env_uid, _) as env,
      stack: (stack_uid, _) as stack,
    } =>
    Some((
      mkConfig({
        zipper:
          mkZipper({
            focus: mkFocus(ZPreVal(mkZPreVal({op, values: mkValues(Empty)}))),
            ctxts,
          }),
        env,
        stack,
      }),
      (
        "zipper skip",
        {
          pattern: [
            (op_uid, [op_uid]),
            (ctxts_uid, [ctxts_uid]),
            (env_uid, [env_uid]),
            (stack_uid, [stack_uid]),
          ],
          extFn: [],
        },
      ),
    ))
  /* zipper begin */
  /* | {zipper: {focus: ZExp({op, args: [a, ...args]}), ctxts}, env, stack} =>
     Some({
       zipper: {
         focus: ZExp(a),
         ctxts: [{op, args, values: []}, ...ctxts],
       },
       env,
       stack,
     }) */
  | {
      zipper: (
        _,
        {
          focus: (
            _,
            ZExp((
              _,
              {
                op: (op_uid, _) as op,
                args: (_, Cons((a_uid, _) as a, (args_uid, _) as args)),
              },
            )),
          ),
          ctxts: (ctxts_uid, _) as ctxts,
        },
      ),
      env: (env_uid, _) as env,
      stack: (stack_uid, _) as stack,
    } =>
    Some((
      mkConfig({
        zipper:
          mkZipper({
            focus: mkFocus(ZExp(a)),
            ctxts: mkCtxts(Cons(mkZCtxt({op, args, values: mkValues(Empty)}), ctxts)),
          }),
        env,
        stack,
      }),
      (
        "zipper begin",
        {
          pattern: [
            (op_uid, [op_uid]),
            (a_uid, [a_uid]),
            (args_uid, [args_uid]),
            (ctxts_uid, [ctxts_uid]),
            (env_uid, [env_uid]),
            (stack_uid, [stack_uid]),
          ],
          extFn: [],
        },
      ),
    ))
  /* zipper continue */
  /*  | {
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
      }) */
  | {
      zipper: (
        _,
        {
          focus: (_, Value((v_uid, _) as v)),
          ctxts: (
            _,
            Cons(
              (
                _,
                {
                  op: (op_uid, _) as op,
                  args: (_, Cons((a_uid, _) as a, (args_uid, _) as args)),
                  values: (values_uid, _) as values,
                },
              ),
              (ctxts_uid, _) as ctxts,
            ),
          ),
        },
      ),
      env: (env_uid, _) as env,
      stack: (stack_uid, _) as stack,
    } =>
    Some((
      mkConfig({
        zipper:
          mkZipper({
            focus: mkFocus(ZExp(a)),
            ctxts:
              mkCtxts(Cons(mkZCtxt({op, args, values: mkValues(Cons(v, values))}), ctxts)),
          }),
        env,
        stack,
      }),
      (
        "zipper continue",
        {
          pattern: [
            (v_uid, [v_uid]),
            (op_uid, [op_uid]),
            (a_uid, [a_uid]),
            (args_uid, [args_uid]),
            (values_uid, [values_uid]),
            (ctxts_uid, [ctxts_uid]),
            (env_uid, [env_uid]),
            (stack_uid, [stack_uid]),
          ],
          extFn: [],
        },
      ),
    ))

  /* zipper end */
  /* NOTE! The delta version doesn't reverse the list, because that would make things harder. */
  /* | {zipper: {focus: Value(v), ctxts: [{op, args: [], values}, ...ctxts]}, env, stack} =>
     Some({
       zipper: {
         focus: ZPreVal({op, values: List.rev(values)}),
         ctxts,
       },
       env,
       stack,
     }) */
  | {
      zipper: (
        _,
        {
          focus: (_, Value((v_uid, _) as v)),
          ctxts: (
            _,
            Cons(
              (
                _,
                {op: (op_uid, _) as op, args: (_, Empty), values: (values_uid, _) as values},
              ),
              (ctxts_uid, _) as ctxts,
            ),
          ),
        },
      ),
      env: (env_uid, _) as env,
      stack: (stack_uid, _) as stack,
    } =>
    Some((
      mkConfig({
        zipper:
          mkZipper({
            focus: mkFocus(ZPreVal(mkZPreVal({op, values: mkValues(Cons(v, values))}))),
            ctxts,
          }),
        env,
        stack,
      }),
      (
        "zipper end",
        {
          pattern: [
            (v_uid, [v_uid]),
            (op_uid, [op_uid]),
            (values_uid, [values_uid]),
            (ctxts_uid, [ctxts_uid]),
            (env_uid, [env_uid]),
            (stack_uid, [stack_uid]),
          ],
          extFn: [],
        },
      ),
    ))
  /* app enter */
  /* | {
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
     }) */
  | {
      zipper: (
        _,
        {
          focus: (
            _,
            ZPreVal((
              _,
              {
                op: (_, AExp((_, App))),
                values: (
                  _,
                  Cons(
                    (v2_uid, _) as v2,
                    (
                      _,
                      Cons(
                        (
                          _,
                          Clo(
                            (_, {vid: (x_uid, _) as x, exp: (e_uid, _) as e}),
                            (env_uid, _) as env,
                          ),
                        ),
                        (_, Empty),
                      ),
                    ),
                  ),
                ),
              },
            )),
          ),
          ctxts: (ctxts_uid, _) as ctxts,
        },
      ),
      env: (env'_uid, _) as env',
      stack: (stack_uid, _) as stack,
    } =>
    Some((
      mkConfig({
        zipper: mkZipper({focus: mkFocus(ZExp(e)), ctxts: mkCtxts(Empty)}),
        env: mkEnv(Cons(mkBinding({vid: x, value: v2}), env)),
        stack: mkStack(Cons(mkFrame({ctxts, env: env'}), stack)),
      }),
      (
        "app enter",
        {
          pattern: [
            (v2_uid, [v2_uid]),
            (x_uid, [x_uid]),
            (e_uid, [e_uid]),
            (env_uid, [env_uid]),
            (ctxts_uid, [ctxts_uid]),
            (env'_uid, [env'_uid]),
            (stack_uid, [stack_uid]),
          ],
          extFn: [],
        },
      ),
    ))
  /* app exit */
  | {
      zipper: (_, {focus: (_, Value((v_uid, _) as v)), ctxts: (_, Empty)}),
      env: (env_uid, _) as env,
      stack: (
        _,
        Cons(
          (_, {ctxts: (ctxts_uid, _) as ctxts, env: (env'_uid, _) as env'}),
          (stack_uid, _) as stack,
        ),
      ),
    } =>
    Some((
      mkConfig({zipper: mkZipper({focus: mkFocus(Value(v)), ctxts}), env: env', stack}),
      (
        "app exit",
        {
          pattern: [
            (v_uid, [v_uid]),
            (env_uid, []),
            (ctxts_uid, [ctxts_uid]),
            (env'_uid, [env'_uid]),
            (stack_uid, [stack_uid]),
          ],
          extFn: [],
        },
      ),
    ))
  /* let */
  | {
      zipper: (
        _,
        {
          focus: (
            _,
            ZPreVal((
              _,
              {
                op: (_, Exp((_, Let((x_uid, _) as x, (e2_uid, _) as e2)))),
                values: (_, Cons((v1_uid, _) as v1, (_, Empty))),
              },
            )),
          ),
          ctxts: (ctxts_uid, _) as ctxts,
        },
      ),
      env: (env_uid, _) as env,
      stack: (stack_uid, _) as stack,
    } =>
    Some((
      mkConfig({
        zipper: mkZipper({focus: mkFocus(ZExp(e2)), ctxts}),
        env: mkEnv(Cons(mkBinding({vid: x, value: v1}), env)),
        stack,
      }),
      (
        "let",
        {
          pattern: [
            (x_uid, [x_uid]),
            (e2_uid, [e2_uid]),
            (v1_uid, [v1_uid]),
            (ctxts_uid, [ctxts_uid]),
            (env_uid, [env_uid]),
            (stack_uid, [stack_uid]),
          ],
          extFn: [],
        },
      ),
    ))
  /* num */
  | {
      zipper: (
        _,
        {
          focus: (
            _,
            ZPreVal((_, {op: (_, AExp((_, Num((n_uid, _) as n)))), values: (_, Empty)})),
          ),
          ctxts: (ctxts_uid, _) as ctxts,
        },
      ),
      env: (env_uid, _) as env,
      stack: (stack_uid, _) as stack,
    } =>
    Some((
      mkConfig({
        zipper: mkZipper({focus: mkFocus(Value(mkValue(VNum(n)))), ctxts}),
        env,
        stack,
      }),
      (
        "num",
        {
          pattern: [
            (n_uid, [n_uid]),
            (ctxts_uid, [ctxts_uid]),
            (env_uid, [env_uid]),
            (stack_uid, [stack_uid]),
          ],
          extFn: [],
        },
      ),
    ))
  /* add */
  | {
      zipper: (
        _,
        {
          focus: (
            _,
            ZPreVal((
              _,
              {
                op: (_, AExp((_, Add))),
                values: (
                  _,
                  Cons(
                    (_, VNum((v2_uid, v2_val))),
                    (_, Cons((_, VNum((v1_uid, v1_val))), (_, Empty))),
                  ),
                ),
              },
            )),
          ),
          ctxts: (ctxts_uid, _) as ctxts,
        },
      ),
      env: (env_uid, _) as env,
      stack: (stack_uid, _) as stack,
    } =>
    let (v3_uid, _) as v3 = mkInt(v1_val + v2_val);
    Some((
      mkConfig({
        zipper: mkZipper({focus: mkFocus(Value(mkValue(VNum(v3)))), ctxts}),
        env,
        stack,
      }),
      (
        "add",
        {
          pattern: [
            (v1_uid, [v3_uid]),
            (v2_uid, [v3_uid]),
            /* TODO: op should also go to v3_uid? */
            (ctxts_uid, [ctxts_uid]),
            (env_uid, [env_uid]),
            (stack_uid, [stack_uid]),
          ],
          extFn: [],
        },
      ),
    ));
  /* bracket */
  | {
      zipper: (
        _,
        {
          focus: (
            _,
            ZPreVal((
              _,
              {op: (_, AExp((_, Bracket((e_uid, _) as e)))), values: (_, Empty)},
            )),
          ),
          ctxts: (ctxts_uid, _) as ctxts,
        },
      ),
      env: (env_uid, _) as env,
      stack: (stack_uid, _) as stack,
    } =>
    let (env2_uid, _) as env2 = envToUID(envFromUID(env)); /* use `from` and `to` to scrub all copied IDs */
    Some((
      mkConfig({
        zipper: mkZipper({focus: mkFocus(ZExp(e)), ctxts: mkCtxts(Empty)}),
        env: env2,
        stack: mkStack(Cons(mkFrame({ctxts, env}), stack)),
      }),
      (
        "bracket",
        {
          pattern: [
            (e_uid, [e_uid]),
            (ctxts_uid, [ctxts_uid]),
            (env_uid, [env_uid, env2_uid]),
            (stack_uid, [stack_uid]),
          ],
          extFn: [],
        },
      ),
    ));
  /* lift */
  | {
      zipper: (
        _,
        {
          focus: (
            _,
            ZPreVal((_, {op: (_, Exp((_, Lift((ae_uid, _) as ae)))), values: (_, Empty)})),
          ),
          ctxts: (ctxts_uid, _) as ctxts,
        },
      ),
      env: (env_uid, _) as env,
      stack: (stack_uid, _) as stack,
    } =>
    Some((
      mkConfig({zipper: mkZipper({focus: mkFocus(ZExp(ae)), ctxts}), env, stack}),
      (
        "lift",
        {
          pattern: [
            (ae_uid, [ae_uid]),
            (ctxts_uid, [ctxts_uid]),
            (env_uid, [env_uid]),
            (stack_uid, [stack_uid]),
          ],
          extFn: [],
        },
      ),
    ))
  | _ => None
  };

/* def inject (e : exp) : state := ⟨sum.inl e, ⟨option.none, env.emp⟩, []⟩ */
let inject = (e: ZED.exp): config =>
  configToUID({
    zipper: {
      focus: ZExp(e),
      ctxts: [],
    },
    env: [],
    stack: [],
  });

/* def is_final : state -> Prop
   | ⟨sum.inr v, ⟨option.none, env⟩, []⟩ := true
   | _ := false */

let isFinal = ((_, c): config): bool =>
  switch (c) {
  | {zipper: (_, {focus: (_, Value(_)), ctxts: (_, Empty)}), env: _, stack: (_, Empty)} =>
    true
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

let rec iterateMaybeSideEffect = (f: 'a => option(('a, 'b)), x: 'a): (list('a), list('b)) =>
  switch (f(x)) {
  | None => ([x], [])
  | Some((a, b)) =>
    let (als, bls) = iterateMaybeSideEffect(f, a);
    ([x, ...als], [b, ...bls]);
  };

let interpretTrace = (p: ZED.exp): (list((string, Sidewinder.Flow.linearExt)), list(config)) => {
  let (states, rules) = iterateMaybeSideEffect(step, inject(p));
  // Js.log2("rules", rules |> Array.of_list);
  let (actualRules, flow) = rules |> List.split;
  // Js.log2("rules", List.combine(actualRules, flow) |> Array.of_list);
  /* List.combine(rules @ [("", [])], takeWhileInclusive(c => !isFinal(c), states)); */
  (rules, takeWhileInclusive(c => !isFinal(c), states));
};

/* let interpret = p => {
     let (_, s) = interpretTrace(p) |> List.rev |> List.hd;
     switch (s) {
     | (_, {zipper: (_, {focus: (_, Value(v)), ctxts: _}), env: _, stack: _}) => valueFromUID(v)
     | _ => raise(failwith("expected a value"))
     };
   };
    */
