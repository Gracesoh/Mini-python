type uid = string; /* only unique within a structure, not across them */
let counter = ref(0);

let readAndUpdateCounter = () => {
  counter := counter^ + 1;
  counter^ - 1;
};

let rauc = () => readAndUpdateCounter() |> string_of_int;

type makeUIDType('a) = (uid, 'a);

type makeUIDListTypeAux('a) =
  | Empty
  | Cons(makeUIDType('a), makeUIDListType('a))

and makeUIDListType('a) = (uid, makeUIDListTypeAux('a));

let makeUIDConstructor = (s, x: 'a) => (s ++ "_" ++ rauc(), x);

/* let rec makeUIDListConstructorAux = (aConstructor, s, xs: list('a)) =>
     switch (xs) {
     | [] => Empty
     | [x, ...xs] => Cons(aConstructor(x), makeUIDListConstructor(aConstructor, s, xs))
     }

   and makeUIDListConstructor = (aConstructor, s, xs: list('a)) => (
     s ++ "_" ++ rauc(),
     makeUIDListConstructorAux(aConstructor, s, xs),
   ); */

/* let makeUIDEmpty = s => makeUIDConstructor(s, Empty);
   let makeUIDCons = (s, x, xs) => makeUIDConstructor(s, Cons(x, xs)); */