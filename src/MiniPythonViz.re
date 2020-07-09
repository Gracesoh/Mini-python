open Sidewinder;
open Bobcat;
open MiniPython;

/* Int(5) */
let vizExp = (e: exp) =>
  switch (e) {
    | ENone => Some(ConfigIR.mk(~name="ENone", ~nodes=[], ~render=_ => Theia.str("ENone"), ()))
    | False => Some(ConfigIR.mk(~name="False", ~nodes=[], ~render=_ => Theia.str("False"), ()))
    | True => Some(ConfigIR.mk(~name="True", ~nodes=[], ~render=_ => Theia.str("True"), ()))
    | Int(int) => Some(ConfigIR.mk(~name="Int", ~nodes=[], ~render=_ => Theia.str("Int"), ()))
    | String(string) => Some(ConfigIR.mk(~name="String", ~nodes=[], ~render=_ => Theia.str("String"), ()))
  };

let vizValue = (v: value) =>
  switch (v) {
    | VNone => Some(ConfigIR.mk(~name="VNone", ~nodes=[], ~render=_ => Theia.str("VNone"), ())) 
    | VBool(false) => Some(ConfigIR.mk(~name="false", ~nodes=[], ~render=_ => Theia.str("false"), ())) 
    | VBool(true) => Some(ConfigIR.mk(~name="true", ~nodes=[], ~render=_ => Theia.str("true"), ())) 
    | VInt(int) => Some(ConfigIR.mk(~name="VInt(int)", ~nodes=[], ~render=_ => Theia.str("VInt(int)"), ())) 
    | VString(int,string) => Some(ConfigIR.mk(~name="VString(String.length(string), string)", ~nodes=[], ~render=_ => Theia.str("VString(String.length(string), string"), ())) 
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

