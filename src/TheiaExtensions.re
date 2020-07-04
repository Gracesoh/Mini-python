open Bobcat.Theia;

let value = (~uid=?, name, node) => box(~uid?, ~dx=5., ~dy=5., node, []);
let cell = (~uid=?, name, node) => box(~uid?, ~dx=5., ~dy=5., node, []);

let empty = (~uid=?, ()) =>
  atom(
    ~uid?,
    <> </>,
    Bobcat.Rectangle.fromCenterPointSize(~cx=0., ~cy=0., ~width=0., ~height=0.),
  );

let hole =
  atom(
    ~links=[],
    <rect fill="none" width="10" height="10" x="5" y="5" />,
    Bobcat.Rectangle.fromPointSize(~x=0., ~y=0., ~width=10., ~height=10.),
  );

