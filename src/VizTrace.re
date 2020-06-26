let leftButtonStyle = ReactDOMRe.Style.make(~borderRadius="4px 0px 0px 4px", ~width="48px", ());
let rightButtonStyle = ReactDOMRe.Style.make(~borderRadius="0px 4px 4px 0px", ~width="48px", ());

type state = {
  pos: int,
  length: int,
};

type action =
  | Increment
  | Decrement
  | Length(int)
  | Error;

let initialState = {pos: 0, length: 1};

let reducer = (state, action) => {
  switch (action) {
  | Increment => {...state, pos: min(state.length - 1, state.pos + 1)}
  | Decrement => {...state, pos: max(0, state.pos - 1)}
  | Length(length) => {...state, length}
  | Error => state
  };
};

module Animation = Sidewinder.AnimationComponentHelper;
module AnimationProvider = Sidewinder.AnimationComponentProvider;

let toggle = (Animation.{curr: _, next}) =>
  switch (next) {
  | Before => Animation.{curr: next, next: After}
  | After => {curr: next, next: Before}
  };

let transform = n =>
  n
  |> ZEDTransform.transformZExp
  |> ZEDTransform.transformZCtxt
  |> ZEDTransform.transformZPreval
  |> ZEDTransform.transformZipper
  |> ZEDTransform.transformContinuation;

[@react.component]
let make = (~padding=10., ~program) => {
  let liftedProgram = ZED.expFromZEDLang(program);
  let (rules, nodes) = ZEDDelta.interpretTrace(liftedProgram);
  let (ruleNames, flows) = List.split(rules);
  // Js.log2("ruleNames", ruleNames |> Array.of_list);

  let (state, dispatch) = React.useReducer(reducer, initialState);
  let (Animation.{curr, next}, setAnimationState) = React.useState(() => Animation.initValue);

  // Notice that instead of `useEffect`, we have `useEffect0`. See
  // reasonml.github.io/reason-react/docs/en/components#hooks for more info
  React.useEffect0(() => {
    dispatch(Length(List.length(nodes)));

    // Returning None, instead of Some(() => ...), means we don't have any
    // cleanup to do before unmounting. That's not 100% true. We should
    // technically cancel the promise. Unofortunately, there's currently no
    // way to cancel a promise. Promises in general should be way less used
    // for React components; but since folks do use them, we provide such an
    // example here. In reality, this fetch should just be a plain callback,
    // with a cancellation API
    None;
  });

  /* let swTrace =
     trace
     |> List.map((((rule, flow), c)) => {
          let (flow, n) = ZEDViz.vizState(rule, c) |> Sidewinder.Config.propagatePlace(flow);
          (
            flow,
            n
            |> ZEDTransform.transformOp
            |> ZEDTransform.transformZipper
            |> ZEDTransform.transformContinuation,
          );
        })
     |> List.split
     |> (((flows, ns)) => Sidewinder.Config.compile(flows, ns)); */
  let nodes =
    List.combine(ruleNames @ [""], nodes) |> List.map(((r, n)) => ZEDViz.vizState(r, n));
  /* let cap = 15;

     let nodes = nodes->Belt.List.take(cap + 1)->Belt.Option.getExn;
     let flows = flows->Belt.List.take(cap)->Belt.Option.getExn; */
  // Js.log2("nodes", nodes |> Array.of_list);
  let flowSiftedNodes =
    Sidewinder.Fn.mapPairs((n1, n2) => (n1, n2), nodes)
    |> List.combine(flows)
    |> List.map(((f, (n1, n2))) => {
         let (keys, valueList) = List.split(f);
         let values = List.flatten(valueList);
         //  Js.log2("f before propagation", f |> Array.of_list);
         let (f, n1) = ZEDViz.filterPlaces(keys, n1) |> Sidewinder.Config.propagatePlace(f);
         //  Js.log2("f after propagation", f |> Array.of_list);
         let (_, n2) = ZEDViz.filterPlaces(values, n2) |> Sidewinder.Config.propagatePlace(f);
         (n1 |> transform, f, n2 |> transform);
       });
  // Js.log2("sifted", flowSiftedNodes |> Array.of_list);
  let finalNode = flowSiftedNodes |> List.rev |> List.hd |> (((_, _, n)) => n);
  let finalState = Sidewinder.Config.compileTransition(finalNode, [], finalNode);
  let animatedNodes =
    List.map(((n1, f, n2)) => Sidewinder.Config.compileTransition(n1, f, n2), flowSiftedNodes)
    @ [finalState];
  /*  let flowNodePairs =
        trace |> List.map((((rule, flow), n)) => (flow, ZEDViz.vizState(rule, n)));
      let transitionViz = Sidewinder.Fn.mapPairs(((f1, n1), (f2, n2)) => {}, flowNodePairs); */
  let renderedConfig = List.nth(animatedNodes, state.pos);
  let width = 1000.;
  let height = 300.;
  let xOffset = 0.;
  let yOffset = 100.;
  /* let width = renderedConfig.bbox->Sidewinder.Rectangle.width;
     let height = renderedConfig.bbox->Sidewinder.Rectangle.height; */

  /* /* transform is unnecessary b/c top-level always has identity transform b/c parent controls transform */
     let xOffset =
       /* renderedConfig.transform.translate.x +.  */ renderedConfig.bbox->Sidewinder.Rectangle.x1;
     let yOffset = /* renderedConfig.transform.translate.y +. */
     renderedConfig.bbox->Sidewinder.Rectangle.y1; */
  <div>
    <div> {React.string("state: ")} {React.string(string_of_int(state.pos))} </div>
    <button
      style=leftButtonStyle
      onClick={_event => {
        dispatch(Decrement);
        setAnimationState(_ => Animation.initValue);
      }}>
      {React.string("<-")}
    </button>
    <button onClick={_event => setAnimationState(toggle)}>
      {switch (curr) {
       | Before => React.string("To After")
       | After => React.string("To Before")
       }}
    </button>
    <button
      style=rightButtonStyle
      onClick={_event => {
        dispatch(Increment);
        setAnimationState(_ => Animation.initValue);
      }}>
      {React.string("->")}
    </button>
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width={Js.Float.toString(width +. padding *. 2.)}
      height={Js.Float.toString(height +. padding *. 2.)}>
      <g
        transform={
          "translate("
          ++ Js.Float.toString(xOffset +. padding)
          ++ ", "
          ++ Js.Float.toString(yOffset +. padding)
          ++ ")"
        }>
        <AnimationProvider value=Animation.{curr, next}> renderedConfig </AnimationProvider>
      </g>
    </svg>
  </div>;
  /* ->Belt.Array.mapWithIndex((i, dog) => {
              let imageStyle =
                ReactDOMRe.Style.make(
                  ~height="120px",
                  ~width="100%",
                  ~marginRight=i === Js.Array.length(dogs) - 1 ? "0px" : "8px",
                  ~borderRadius="8px",
                  ~boxShadow="0px 4px 16px rgb(200, 200, 200)",
                  ~backgroundSize="cover",
                  ~backgroundImage={j|url($dog)|j},
                  ~backgroundPosition="center",
                  (),
                );
              <div key=dog style=imageStyle />;
            })
          ->React.array
        }}
     */
};
