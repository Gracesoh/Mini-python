open Sidewinder;
open Bobcat;
open MiniPython;

/* Int(5) */
let vizExp = (e: exp) =>
  switch (e) {
    | ENone => Some(ConfigIR.mk(~name="ENone", ~nodes=[], ~render=_ => Theia.str("ENone"), ()))
    | False => Some(ConfigIR.mk(~name="False", ~nodes=[], ~render=_ => Theia.str("False"), ()))
    | True => failwith("TODO")
    | Int(i) => failwith("TODO")
    | String(s) => failwith("TODO")
  };

let vizValue = (v: value) =>
  switch (v) {
    | VNone => Some(ConfigIR.mk(~name="VNone", ~nodes=[], ~render=_ => Theia.str("VNone"), ())) 
    | VBool(false) => Some(ConfigIR.mk(~name="false", ~nodes=[], ~render=_ => Theia.str("false"), ())) 
    | VBool(b) => failwith("TODO")
    | VInt(i) => failwith("TODO")
    | VString(n, s) => failwith("TODO")
  };

let vizFocus = (f: focus) =>
  switch (f) {
    | Exp(e) => Some(
      ConfigIR.mk(
        ~name="focus_exp",
        ~nodes=[vizExp(e)],
        ~render=([e]) => Theia.noOp(e, []),
        (),
      ),
    )
    | Value(v) => Some(ConfigIR.mk(
        ~name="focus_value",
        ~nodes=[vizValue(v)],
        ~render=([v]) => Theia.noOp(v, []),
        (),
      ),
    )
  };

let vizConfig = ({focus, env, store, glob}: config) =>
  ConfigIR.mk(
    ~name="config",
    ~nodes=[vizFocus(focus)],
    ~render=
      ([focus]) =>
        Theia.noOp(focus, []),
    (),
  );