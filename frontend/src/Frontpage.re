[@react.component]
let make = () =>
  <div>
    <h1 className="title"> {ReasonReact.string("Welcome to repo tools!")} </h1>
    <div className="columns">
      <div className="column"> <a href="/repo/create"> {ReasonReact.string("Create repo")} </a> </div>
    </div>
  </div>;
