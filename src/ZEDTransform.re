open Sidewinder.ConfigIR;

let rec mergeNone = (l1, l2) =>
  switch (l1, l2) {
  | (l1, []) => l1
  | (l1, [None, ...l2]) => mergeNone(l1, l2)
  | ([None, ...l1], [Some(x), ...l2]) => [Some(x), ...mergeNone(l1, l2)]
  | ([Some(x), ...l1], l2) => [Some(x), ...mergeNone(l1, l2)]
  | ([], l2) =>
    Js.log(l2 |> Array.of_list);
    failwith("Some nodes remaining!");
  };

let rec transformOpOption = on => {
  switch (on) {
  | None => None
  | Some(n) =>
    let {name, nodes} as n = {...n, nodes: List.map(transformOpOption, n.nodes)};
    if (name == "zexp") {
      /* TODO: zctxt */
      // zexp args
      let [Some({nodes}), ...args] = nodes;
      // exp/aexp op and existing args
      let [Some({nodes} as op)] = nodes;
      Some({...op, nodes: mergeNone(nodes, args)});
    } else if (name == "zpreval") {
      // zpreval values
      let [Some({nodes}), ...values] = nodes;
      // exp/aexp op and existing values
      let [Some({nodes} as op)] = nodes;
      Some({...op, nodes: mergeNone(nodes, values)});
    } else {
      Some(n);
    };
  };
};

let transformOp = n => transformOpOption(Some(n))->Belt.Option.getExn;