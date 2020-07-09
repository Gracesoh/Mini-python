open Sidewinder;
open Bobcat;
open MiniPython;

let vizExp = (e: exp) =>
  switch (e) {
    | ENone => Some(ConfigIR.mk(~name="ENone", ~nodes=[], ~render=_ => Theia.str("ENone"), ()))
    | False => Some(ConfigIR.mk(~name="False", ~nodes=[], ~render=_ => Theia.str("False"), ()))
    | True => Some(ConfigIR.mk(~name="True", ~nodes=[], ~render=_ => Theia.str("True"), ()))
    | Int(int) => Some(ConfigIR.mk(~name="Int", ~nodes=[], ~render=_ => Theia.str(string_of_int(int)), ()))
    | String(string) => Some(ConfigIR.mk(~name="String", ~nodes=[], ~render=_ => Theia.str(string), ()))
  };

let vizValue = (v: value) =>
  switch (v) {
    | NoneLiteral => Some(ConfigIR.mk(~name="NoneLiteral", ~nodes=[], ~render=_ => Theia.str("NoneLiteral"), ())) 
    | BooleanLiteral(false) => Some(ConfigIR.mk(~name="false", ~nodes=[], ~render=_ => Theia.str("false"), ())) 
    | BooleanLiteral(true) => Some(ConfigIR.mk(~name="true", ~nodes=[], ~render=_ => Theia.str("true"), ())) 
    | IntegerLiteral(int) => Some(ConfigIR.mk(~name="IntegerLiteral(int)", ~nodes=[], ~render=_ => Theia.str(string_of_int(int)), ())) 
    | StringLiteral(int,string) => Some(ConfigIR.mk(~name="StringLiteral(String.length(string), string)", ~nodes=[], ~render=_ => Theia.hSeq([Theia.str(string_of_int(int)), Theia.str(", "), Theia.str(string)]), ()))  
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

