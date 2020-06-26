open Sidewinder.ConfigIR;

let rec mergeNone =
        (
          l1: list(option(Sidewinder.ConfigIR.node)),
          l2: list(option(Sidewinder.ConfigIR.node)),
        ) =>
  switch (l1, l2) {
  | (l1, []) => l1
  | ([None, ...l1], [x, ...l2]) => [x, ...mergeNone(l1, l2)]
  | ([Some(x), ...l1], l2) => [Some(x), ...mergeNone(l1, l2)]
  | ([], l2) =>
    Js.log(l2 |> Array.of_list);
    failwith("Some nodes remaining!");
  };

let rec transformOpOption = on =>
  switch (on) {
  | None => None
  | Some(n) =>
    let {name, place, nodes} as n = {...n, nodes: List.map(transformOpOption, n.nodes)};
    if (name == "zexp" || name == "zctxt" || name == "zpreval") {
      let [Some({nodes}), ...inputs] = nodes;
      let [Some({nodes} as op)] = nodes;
      switch (place) {
      | None =>
        Some(
          Sidewinder.ConfigIR.mk(
            ~name,
            ~nodes=[Some({...op, nodes: mergeNone(nodes, inputs)})],
            ~render=([n]) => Sidewinder.Theia.noOp(n, []),
            (),
          ),
        )
      | Some(place) =>
        Some(
          Sidewinder.ConfigIR.mk(
            ~place,
            ~name,
            ~nodes=[Some({...op, nodes: mergeNone(nodes, inputs)})],
            ~render=([n]) => Sidewinder.Theia.noOp(n, []),
            (),
          ),
        )
      };
    } else {
      Some(n);
    };
  };

let transformOp = n => transformOpOption(Some(n))->Belt.Option.getExn;

let rec zipUp =
        (f: option(Sidewinder.ConfigIR.node), cs: list(option(Sidewinder.ConfigIR.node))) =>
  switch (cs) {
  | [] => f
  | [c, ...cs] =>
    switch (c) {
    | None => None
    | Some(c) =>
      let place =
        switch (f) {
        | None => None
        | Some(f) =>
          switch (f.place) {
          | None => None
          /* TODO: need to add into flow */
          | Some(place) => Some(place ++ ".highlight")
          }
        };
      let f =
        Some(
          Sidewinder.ConfigIR.mk(
            ~place?,
            ~name="highlight",
            ~nodes=[f],
            ~render=
              ([f]) => Sidewinder.Theia.highlight(~fill="hsla(240, 100%, 80%, 33%)", f, []),
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
      let [f, ...ctxts] = nodes;
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
      let [env, ...ctxts] = nodes;
      Some({
        ...n,
        nodes: [
          env,
          zipUp(
            Some(
              Sidewinder.ConfigIR.mk(
                ~name="hole",
                ~nodes=[],
                ~render=_ => Sidewinder.Theia.hole(),
                (),
              ),
            ),
            ctxts,
          ),
        ],
      });
    } else {
      Some(n);
    };
  };

let transformContinuation = n => transformContinuationOption(Some(n))->Belt.Option.getExn;
