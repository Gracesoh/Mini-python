open Sidewinder;
open Bobcat;
open MiniPython;

let vizExp = (e: exp) =>
  switch (e) {
    | NoneLiteral => Some(ConfigIR.mk(~name="NoneLiteral", ~nodes=[], ~render=_ => Theia.str("NoneLiteral"), ()))
    | BooleanLiteral(false) => Some(ConfigIR.mk(~name="False", ~nodes=[], ~render=_ => Theia.str("False"), ()))
    | BooleanLiteral(true) => Some(ConfigIR.mk(~name="True", ~nodes=[], ~render=_ => Theia.str("True"), ()))
    | IntegerLiteral(int) => Some(ConfigIR.mk(~name="IntegerLiteral", ~nodes=[], ~render=_ => Theia.str(string_of_int(int)), ()))
    | StringLiteral(string) => Some(ConfigIR.mk(~name="StringLiteral", ~nodes=[], ~render=_ => Theia.str(string), ()))
  };

let vizValue = (v: value) =>
  switch (v) {
    | VNone => Some(ConfigIR.mk(~name="VNone", ~nodes=[], ~render=_ => Theia.str("VNone"), ())) 
    | VBool(false) => Some(ConfigIR.mk(~name="false", ~nodes=[], ~render=_ => Theia.str("false"), ())) 
    | VBool(true) => Some(ConfigIR.mk(~name="true", ~nodes=[], ~render=_ => Theia.str("true"), ())) 
    | VInt(int) => Some(ConfigIR.mk(~name="VInt(int)", ~nodes=[], ~render=_ => Theia.str(string_of_int(int)), ())) 
    | VString(int,string) => Some(ConfigIR.mk(~name="VString(String.length(string), string)", ~nodes=[], ~render=_ => Theia.box(Theia.hSeq([Theia.str(string_of_int(int)), Theia.str(", "), Theia.str(string)]) , [] ), ()))
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

let vizZipper = ({focus, ctxts}: zipper) =>
Some(ConfigIR.mk(
  ~name="zipper",
  ~nodes=[vizFocus(focus)],
  ~render=
    ([focus]) =>
      Theia.noOp(focus, []),
  (),)

);

let vizConfig = ({zipper, env, store, glob}: config) =>
  ConfigIR.mk(
    ~name="config",
    ~nodes=[vizZipper(zipper)],
    ~render=
      ([zipper]) =>
        Theia.noOp(zipper, []),
    (),
  );

