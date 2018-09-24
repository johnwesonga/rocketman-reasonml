let str = ReasonReact.string;

let url = "https://api.spacexdata.com/v2/launchpads";

type launchpad = {
  padid: int,
  id: string,
  full_name: string,
  status: string,
  location,
  vehicles_launched: list(string),
  details: string,
}
and location = {
  name: string,
  region: string,
  latitude: float,
  longitude: float,
};

type state =
  | Loading
  | Failure
  | Success(list(launchpad));

module Decode = {
  let location = json =>
    Json.Decode.{
      name: json |> field("name", string),
      region: json |> field("region", string),
      latitude: json |> field("latitude", float),
      longitude: json |> field("longitude", float),
    };

  let launchpad = json =>
    Json.Decode.{
      padid: json |> field("padid", int),
      id: json |> field("id", string),
      full_name: json |> field("full_name", string),
      status: json |> field("status", string),
      location: json |> field("location", location),
      vehicles_launched: json |> field("vehicles_launched", list(string)),
      details: json |> field("details", string),
    };

  /*let launchpads = json => json |> Json.Decode.array(launchpad);*/
  let launchpads = json: list(launchpad) =>
    Json.Decode.list(launchpad, json);
};

let fetchLaunchPads = () =>
  Js.Promise.(
    Fetch.fetch(url)
    |> then_(Fetch.Response.json)
    |> then_(json =>
         json
         |> Decode.launchpads
         |> (launchpads => Some(launchpads) |> resolve)
       )
    |> catch(_err => resolve(None))
  );

type action =
  | LoadingLaunchPads
  | LoadedLaunchedPads(list(launchpad))
  | LoadLaunchPadFailed;

let component = ReasonReact.reducerComponent("Launches");

let make = _children => {
  ...component,
  initialState: _state => Loading,
  didMount: self => self.send(LoadingLaunchPads),
  reducer: (action, _state) =>
    switch (action) {
    | LoadingLaunchPads =>
      ReasonReact.UpdateWithSideEffects(
        Loading,
        (
          self =>
            Js.Promise.(
              fetchLaunchPads()
              |> then_(result =>
                   switch (result) {
                   | Some(launchpads) =>
                     resolve(self.send(LoadedLaunchedPads(launchpads)))
                   | None => resolve(self.send(LoadLaunchPadFailed))
                   }
                 )
              |> ignore
            )
        ),
      )
    | LoadedLaunchedPads(launchpads) =>
      ReasonReact.Update(Success(launchpads))
    | LoadLaunchPadFailed => ReasonReact.Update(Failure)
    },
  render: self =>
    switch (self.state) {
    | Loading => <div> {str("Loading...")} </div>
    | Failure => <div> {str("Error fetching launchpads...")} </div>
    | Success(launchpads) =>
      Js.log(launchpads);
      <div id="launches">
        <h2> {str("Launchpads")} </h2>
        <ul>
          {
            launchpads
            |> List.map(launchpad =>
                 <li key={launchpad.id}>
                   <b> {str(launchpad.full_name)} </b>
                   {str(":")}
                   {str(launchpad.details)}
                   <Vehicles vehicles={launchpad.vehicles_launched} />
                 </li>
               )
            |> Array.of_list
            |> ReasonReact.array
          }
        </ul>
      </div>;
    },
};