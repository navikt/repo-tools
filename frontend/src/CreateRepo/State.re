type visibility =
  | PublicRepo
  | PrivateRepo
  | Unset;

type team = {
  id: int,
  name: string,
  slug: string,
  description: option(string),
};

type teams =
  | NotLoaded
  | Loaded(list(team))
  | LoadFailed;

type state = {
  visibility,
  teams,
  acceptedTerms: Belt.Set.String.t,
  title: string,
  description: string,
  selectedTeam: option(string),
};

type createRepo = {
  title: string,
  owner: string,
  description: string,
};

let terms = [
  "I will not commit passwords into the git repository",
  "I will not commit personal details (like social security numbers) into the git repository",
  "The repository will not contain any sensitive data",
];

let initialState = {
  visibility: Unset,
  teams: NotLoaded,
  acceptedTerms: Belt.Set.String.empty,
  title: "",
  description: "",
  selectedTeam: None,
};

let isEverythingAccepted = accepted => List.for_all(item => Belt.Set.String.has(accepted, item), terms);

type action =
  | TeamsUpdated(teams)
  | SwitchVisibility(visibility)
  | ToggleTermAccept(string, bool)
  | SetTitle(string)
  | SetDescription(string)
  | SetSelectedTeam(string);

let reducer = (state: state, action: action) => {
  switch (action) {
  | SwitchVisibility(visibility) => {...state, visibility}
  | ToggleTermAccept(term, accepted) => {
      ...state,
      acceptedTerms:
        if (accepted) {
          Belt.Set.String.add(state.acceptedTerms, term);
        } else {
          Belt.Set.String.remove(state.acceptedTerms, term);
        },
    }
  | TeamsUpdated(teams) => {...state, teams}
  | SetTitle(title) => {...state, title}
  | SetDescription(description) => {...state, description}
  | SetSelectedTeam(selectedTeam) => {...state, selectedTeam: Some(selectedTeam)}
  };
};

module Decode = {
  let team = team =>
    Json.Decode.{
      id: field("id", int, team),
      name: field("name", string, team),
      slug: field("slug", string, team),
      description: optional(field("description", string), team),
    };
  let teams = (json): list(team) => Json.Decode.list(team, json);
};

module Encode = {
  let createRepo = createRepo =>
    Json.Encode.(
      object_([
        ("title", string(createRepo.title)),
        ("owner", string(createRepo.owner)),
        ("description", string(createRepo.description)),
      ])
    );
};
