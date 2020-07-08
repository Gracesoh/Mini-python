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
type exp = 
| ENone 
| False 
| True 
| Int(int)
| String(string);

/* exp -> val */
/* None -> VNone */
/* False -> VBool(false) */

type focus =
| Exp(exp)
| Value(value);

type config = {
    focus,
    env,
    store,
    glob,
};

let step = (c: config): option(config) =>
  switch (c) {
    /* NONE */
    | {focus: Exp(ENone), env, store, glob} => Some({focus: Value(VNone), env, store, glob})
    /* BOOL-FALSE */
    | {focus: Exp(False), env, store, glob} => Some({focus: Value(VBool(false)), env, store, glob})
    /* TODO: other literals */
    | _ => None
  };

let inject = (e: exp): config => {
  focus: Exp(e),
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