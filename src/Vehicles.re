let component = ReasonReact.statelessComponent("Vehicles");
let make = (~vehicles: list(string), _children) => {
  ...component,
  render: _self =>
    <div id="vehicles">
      <ul>
        {
          Js.log(vehicles);
          vehicles
          |> List.mapi((index, vehicle) =>
               <li key={string_of_int(index)}>
                 {ReasonReact.string(vehicle)}
               </li>
             )
          |> Array.of_list
          |> ReasonReact.array;
        }
      </ul>
    </div>,
};