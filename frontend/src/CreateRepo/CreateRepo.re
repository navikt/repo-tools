open State;
open Effects;

let str = ReasonReact.string;

module PromiseCheckbox = {
  [@react.component]
  let make = (~message, ~checked, ~onClick) =>
    <div>
      <label className="checkbox"> <input type_="checkbox" checked onChange=onClick /> {str(" " ++ message)} </label>
    </div>;
};

[@react.component]
let make = () => {
  let (state, dispatch) = React.useReducer(reducer, initialState);

  React.useEffect1(
    () => {
      fetchTeams(dispatch) |> ignore;
      None;
    },
    Array.of_list([]) // make sure we only run this effect once (on component mount)
  );

  let handlePromiseClick = (promise: string, event: ReactEvent.Form.t) => {
    let checked: bool = [%bs.raw {| event.target.checked |}];
    dispatch(ToggleTermAccept(promise, checked));
  };

  let handleSubmit = event => {
    ReactEvent.Form.preventDefault(event);
    createRepo(
      {
        title: state.title,
        owner: Belt.Option.getWithDefault(state.selectedTeam, ""),
        description: state.description,
      },
      dispatch,
    );
  };

  <div>
    <h1 className="title"> {str("Create GitHub repo")} </h1>
    <form onSubmit=handleSubmit>
      <div className="field">
        <label className="label"> {str("Repository Name")} </label>
        <div className="control">
          <input
            className="input"
            type_="text"
            required=true
            placeholder="Repository Name"
            value={state.title}
            onChange={_ev => dispatch(SetTitle(ReactEvent.Form.target(_ev)##value))}
          />
        </div>
      </div>
      <div className="field">
        <span className="label"> {str("Visibility")} </span>
        <div className="control">
          <label className="radio">
            <input
              type_="radio"
              name="visibility"
              value="public"
              onClick={_event => dispatch(SwitchVisibility(PublicRepo))}
            />
            {str(" Public (open source) ")}
          </label>
          <label className="radio">
            <input
              type_="radio"
              name="visibility"
              value="private"
              onClick={_event => dispatch(SwitchVisibility(PrivateRepo))}
            />
            {str(" Private (only NAV employees) ")}
          </label>
        </div>
      </div>
      {if (state.visibility == PublicRepo) {
         <div>
           <label className="label"> {str("The fine print")} </label>
           {List.map(
              p =>
                <PromiseCheckbox
                  message=p
                  key=p
                  checked={Belt.Set.String.has(state.acceptedTerms, p)}
                  onClick={_event => handlePromiseClick(p, _event)}
                />,
              terms,
            )
            |> Array.of_list
            |> React.array}
           {if (isEverythingAccepted(state.acceptedTerms)) {
              <div>
                <hr />
                <div className="field">
                  <label className="label"> {str("Owner team")} </label>
                  <div className="control">
                    <div className="select">
                      <select
                        value={Belt.Option.getWithDefault(state.selectedTeam, "ignore")}
                        required=true
                        onChange={_ev => dispatch(SetSelectedTeam(ReactEvent.Form.target(_ev)##value))}>
                        <option disabled=true value="ignore"> {str("Choose team")} </option>
                        {
                          let teams =
                            switch (state.teams) {
                            | NotLoaded
                            | LoadFailed => []
                            | Loaded(teams) => teams
                            };

                          List.map(
                            team =>
                              <option key={team.slug} value={string_of_int(team.id)}>
                                {str(
                                   team.name
                                   ++ (
                                     switch (team.description) {
                                     | Some(description) when description != "" => " (" ++ description ++ ")"
                                     | Some(desc) => ""
                                     | None => ""
                                     }
                                   ),
                                 )}
                              </option>,
                            teams,
                          )
                          |> Array.of_list
                          |> React.array;
                        }
                      </select>
                    </div>
                  </div>
                </div>
                <div className="field">
                  <label className="label"> {str("Description of the repository")} </label>
                  <div className="control">
                    <textarea
                      required=true
                      className="textarea"
                      placeholder="What is its purpose? Is this a web application or a library? Is it for NAV employees or citizens? Is it an API or with a user interface?"
                      value={state.description}
                      onChange={_ev => dispatch(SetDescription(ReactEvent.Form.target(_ev)##value))}
                    />
                  </div>
                </div>
                <div className="control">
                  {
                    let buttonText =
                      if (state.status === Idle) {
                        "Create repository";
                      } else {
                        "Waiting...";
                      };
                    let buttonDisabled = state.selectedTeam == None || state.status != Idle;
                    <button className="button is-link" disabled=buttonDisabled> {str(buttonText)} </button>;
                  }
                </div>
              </div>;
            } else {
              str("(Please check all the boxes above.)");
            }}
         </div>;
       } else if (state.visibility == PrivateRepo) {
         <div>
           <h2 className="title is-2"> {str("Notice about private GitHub repositories")} </h2>
           <p>
             <a href="https://github.com/navikt/offentlig/blob/master/OpenSource.md">
               {str("NAV strives to make as much as possible of its code open source.")}
             </a>
             {str(
                {js|
                    For this reason, it's not possible to create a new private GitHub repository
                    via the repo-tools app. You must make a security review of your project requirements,
                    detailing the reasons why the repository has to stay private/closed source. After a
                    complete review, contact a GitHub admin in NAV, who will create the repository for you.
                |js},
              )}
           </p>
           <p>
             <a href="https://navno.sharepoint.com/sites/intranett-it/SitePages/Open-Sourcing.aspx?web=1">
               {str("We have a private document, detailing security requirements of open source repositories in NAV.")}
             </a>
           </p>
           <p> {str("Ask us at Slack, #open-source for details about such a security review.")} </p>
         </div>;
       } else {
         <div> {str("(Please set visibility)")} </div>;
       }}
    </form>
  </div>;
};
