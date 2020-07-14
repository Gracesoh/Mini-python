/* type point = {x:int,y: int}

let my_point: point = {x: 5, y: 6};

type state =
| Red
| Green
| Yellow(int);

let my_state: state = Red;
let my_other_state: state = Yellow(5);

type point_tuple = (int, int);

let my_point_tuple: point_tuple = (5, 6); */

type vid = string;
type loc = int;

type env = list((vid, loc));

type value = 
| VNone
| VBool(bool)
| VInt(int)
| VString(int, string);


type store = list((loc, value));
type glob = env;

/* 
input: 1 + 1

{
  "kind" : "Program",
  "location" : [ 1, 1, 1, 6 ],
  "declarations" : [ ],
  "statements" : [ {
    "kind" : "ExprStmt",
    "location" : [ 1, 1, 1, 5 ],
    "expr" : {
      "kind" : "BinaryExpr",
      "location" : [ 1, 1, 1, 5 ],
      "left" : {
        "kind" : "VInt",
        "location" : [ 1, 1, 1, 1 ],
        "value" : 1
      },
      "operator" : "+",
      "right" : {
        "kind" : "VInt",
        "location" : [ 1, 5, 1, 5 ],
        "value" : 1
      }
    }
  } ],
  "errors" : {
    "errors" : [ ],
    "kind" : "Errors",
    "location" : [ 0, 0, 0, 0 ]
  }
}

 */

type binary_op =
| Add;

type exp = 
| NoneLiteral
| BooleanLiteral(bool)
| IntegerLiteral(int)
| StringLiteral(string)
| BinaryExpr(binary_expr)

and binary_expr = {left: exp, binary_op, right: exp};

type binary_ctxt_left = {left: unit, binary_op, right: exp};
type binary_ctxt_right = {left: value, binary_op, right: unit};
/* {focus: IntLiteral(6), ctxts: [{left: Int(5), binary_op: Add, right: ()}]} */

type binary_ctxt =
| BCtxtLeft(binary_ctxt_left) 
| BCtxtRight(binary_ctxt_right);

type binary_preval = {left: value, binary_op, right: value};

type preval =
| PVBinary(binary_preval);

/* exp -> val */
/* None -> VNone*/
/* False -> VBool(false) */

type focus =
| Exp(exp)
| Value(value)
| PreVal(preval);

type ctxts = list(unit /* TODO */);

type zipper = {
  focus,
  ctxts,
};

type config = {
    zipper,
    env,
    store,
    glob,
};


let step = (c: config): option(config) =>
  switch (c) {
    /* zipper begin */
    /* zipper continue */
    /* zipper end */
    /* NONE */
    | {zipper: {focus: Exp(NoneLiteral), ctxts}, env, store, glob} => Some({zipper: {focus: Value(VNone), ctxts}, env, store, glob})
    /* BOOL-TRUE and BOOL-FALSE */
    | {zipper: {focus: Exp(BooleanLiteral(bool)), ctxts}, env, store, glob} => Some({zipper: {focus: Value(VBool(bool)), ctxts},  env, store, glob})
    /* INT */
    | {zipper: {focus: Exp(IntegerLiteral(int)), ctxts}, env, store, glob} => Some({zipper: {focus: Value(VInt(int)), ctxts}, env, store, glob})
    /* STR */
    | {zipper: {focus: Exp(StringLiteral(string)), ctxts}, env, store, glob} => Some({zipper: {focus: Value(VString(String.length(string), string)), ctxts}, env, store, glob})
    | _ => None
  };

let inject = (e: exp): config => {
  zipper: {focus: Exp(e), ctxts: []},
  env: [],
  store: [],
  glob: [],
};

/* def is_final : state -> Prop
   | ⟨sum.inr v, ⟨option.none, env⟩, []⟩ := true
   | _ := false */

/* let isFinal = (c: config): bool =>
  switch (c) {
  | {zipper: {focus: Value(_), ctxts: []}, env: _, stack: []} => true
  | _ => false
  }; */
let isFinal = (c: config): bool => false;

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

let interpretTrace = (p: exp): list(config) => {
  let states = iterateMaybe(step, inject(p));
  takeWhileInclusive(c => !isFinal(c), states);
};