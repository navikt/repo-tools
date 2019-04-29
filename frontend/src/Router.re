[@react.component]
let make = () => {
  let url = ReasonReactRouter.useUrl();

  let nowShowing =
    switch (url.path) {
    | [] => <Frontpage />
    | ["repo", "create"] => <CreateRepo />
    | _ => <NotFound />
    };
  ();
  nowShowing;
};
