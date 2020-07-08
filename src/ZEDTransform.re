open Sidewinder.ConfigGraphIR;

/* TODO: these transformations assume that the sequences of nodes are lists instead of nested somehow. That needs to be fixed!! */

let rec mergeNone =
        (
          l1: list(option(Sidewinder.ConfigGraphIR.node)),
          l2: list(option(Sidewinder.ConfigGraphIR.node)),
        ) =>
  switch (l1, l2) {
  | (l1, []) => l1
  | ([None, ...l1], [x, ...l2]) => [x, ...mergeNone(l1, l2)]
  | ([Some(x), ...l1], l2) => [Some(x), ...mergeNone(l1, l2)]
  | ([], l2) =>
    Js.log(l2 |> Array.of_list);
    failwith("Some nodes remaining!");
  };

let rec aexpsToList = ({name, nodes} as aexps) =>
  if (name == "aexps_empty") {
    [];
  } else if (name == "aexps_cons") {
    let [Some(aexp), Some(aexps)] = nodes;
    [Some(aexp), ...aexpsToList(aexps)];
  } else {
    Js.log2("expected aexps_empty or aexps_cons. found", name);
    assert(false);
  };

let rec valuesToList = ({name, nodes} as values) =>
  if (name == "values_empty") {
    [];
  } else if (name == "values_cons") {
    let [Some(value), Some(values)] = nodes;
    [Some(value), ...valuesToList(values)];
  } else {
    Js.log2("expected values_empty or values_cons. found", name);
    assert(false);
  };

let rec transformZExpOption = on =>
  switch (on) {
  | None => None
  | Some(n) =>
    let {name, nodes} as n = {...n, nodes: List.map(transformZExpOption, n.nodes)};
    if (name == "zexp") {
      let [Some({nodes}), Some(args)] = nodes;
      let [Some({nodes} as op)] = nodes;
      let args = aexpsToList(args);
      Some({...op, nodes: nodes->mergeNone(args)});
    } else {
      Some(n);
    };
  };

let transformZExp = n => transformZExpOption(Some(n))->Belt.Option.getExn;

let rec transformZCtxtOption = on =>
  switch (on) {
  | None => None
  | Some(n) =>
    let {name, nodes} as n = {...n, nodes: List.map(transformZCtxtOption, n.nodes)};
    if (name == "zctxt") {
      let [Some({nodes}), Some(values), None, Some(args)] = nodes;
      let [Some({nodes} as op)] = nodes;
      let values = valuesToList(values);
      let args = aexpsToList(args);
      Some({...op, nodes: nodes->mergeNone(values)->mergeNone([None, ...args])});
    } else {
      Some(n);
    };
  };

let transformZCtxt = n => transformZCtxtOption(Some(n))->Belt.Option.getExn;

let rec transformZPrevalOption = on =>
  switch (on) {
  | None => None
  | Some(n) =>
    let {name, nodes} as n = {...n, nodes: List.map(transformZPrevalOption, n.nodes)};
    if (name == "zpreval") {
      let [Some({nodes}), Some(values)] = nodes;
      let [Some({nodes} as op)] = nodes;
      let values = valuesToList(values) |> List.rev;
      Some({...op, nodes: nodes->mergeNone(values)});
    } else {
      Some(n);
    };
  };

let transformZPreval = n => transformZPrevalOption(Some(n))->Belt.Option.getExn;

let rec ctxtsToList = ({name, nodes} as ctxts) =>
  if (name == "ctxts_empty") {
    [];
  } else if (name == "ctxts_cons") {
    let [Some(ctxt), Some(ctxts)] = nodes;
    [Some(ctxt), ...ctxtsToList(ctxts)];
  } else {
    Js.log2("expected ctxts_empty or ctxts_cons. found", name);
    assert(false);
  };

let rec zipUp =
        (
          f: option(Sidewinder.ConfigGraphIR.node),
          cs: list(option(Sidewinder.ConfigGraphIR.node)),
        ) =>
  switch (cs) {
  | [] => f
  | [c, ...cs] =>
    switch (c) {
    | None => None
    | Some(c) =>
      let place =
        switch (f) {
        | None => {pat: None, extFns: []}
        | Some(f) =>
          switch (f.place.pat) {
          | None => {pat: None, extFns: f.place.extFns}
          /* TODO: need to add into flow */
          | Some(place) => {pat: Some(place ++ ".highlight"), extFns: f.place.extFns}
          }
        };
      let f =
        Some(
          Sidewinder.ConfigGraphIR.mk(
            /* TODO: add to flow!!! */
            /* ~place?, */
            ~name="highlight",
            ~nodes=[f],
            ~render=([f]) => Bobcat.Theia.highlight(~fill="hsla(240, 100%, 80%, 33%)", f, []),
            (),
          ),
        );
      zipUp(Some({...c, nodes: mergeNone(c.nodes, [f])}), cs);
    }
  };

let rec transformZipperOption = on =>
  switch (on) {
  | None => None
  | Some(n) =>
    let {name, nodes} as n = {...n, nodes: List.map(transformZipperOption, n.nodes)};
    if (name == "zipper") {
      let [f, Some(ctxts)] = nodes;
      let ctxts = ctxtsToList(ctxts);
      zipUp(f, ctxts);
    } else {
      Some(n);
    };
  };

let transformZipper = n => transformZipperOption(Some(n))->Belt.Option.getExn;

let rec transformContinuationOption = on =>
  switch (on) {
  | None => None
  | Some(n) =>
    let {name, nodes} as n = {...n, nodes: List.map(transformContinuationOption, n.nodes)};
    if (name == "frame") {
      let [env, Some(ctxts)] = nodes;
      let ctxts = ctxtsToList(ctxts);
      Some({...n, nodes: [env, zipUp(None, ctxts)]});
    } else {
      Some(n);
    };
  };

let transformContinuation = n => transformContinuationOption(Some(n))->Belt.Option.getExn;