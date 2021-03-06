open State;

let fetchTeams = (dispatch: action => unit) => {
  Js.Promise.(
    Fetch.fetch("/api/teams")
    |> then_(Fetch.Response.json)
    |> then_(json =>
         json
         |> Decode.teams
         |> (
           teams => {
             dispatch(TeamsUpdated(Loaded(teams)));
             resolve(None);
           }
         )
       )
    |> catch(_err => {
         Js.log(_err);
         dispatch(TeamsUpdated(LoadFailed));
         resolve(None);
       })
  );
  ();
};

let redirect: string => unit = [%bs.raw {|
 function (url) {
   window.location = url;
 }
|}];

let createRepo = (data: createRepo, dispatch: action => unit) => {
  dispatch(CreatingRepo);
  Js.Promise.(
    Fetch.fetchWithInit(
      "/api/repo/create",
      Fetch.RequestInit.make(
        ~method_=Post,
        ~body=Fetch.BodyInit.make(data |> Encode.createRepo |> Js.Json.stringify),
        ~headers=Fetch.HeadersInit.make({"Content-Type": "application/json"}),
        (),
      ),
    )
    |> then_(Fetch.Response.json)
    |> then_(json =>
         json
         |> Decode.createRepoResponse
         |> (
           repo => {
             dispatch(RepoCreated);
             redirect(repo.html_url);
             resolve(None);
           }
         )
       )
    |> catch(_err => {
         Js.log(_err);
         resolve(None);
       })
  );
  ();
};
