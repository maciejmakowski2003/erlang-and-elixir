defmodule PollutiondbappWeb.StationLive do
  use PollutiondbappWeb, :live_view

  alias Pollutiondbapp.Station

  def mount(_params, _session, socket) do
    socket = assign(socket, stations: Station.get_all(), name: "", lat: "", lon: "", station_name: "")
    {:ok, socket}
  end

  defp to_float(value, default) do
    case value do
      "" -> default
      _ -> value |> String.to_float()
    end
  end

  def handle_event("insert", %{"name" => name, "lat" => lat, "lon" => lon}, socket) do
    Station.add(name, to_float(lat, 0.0), to_float(lon, 0.0))
    socket = assign(socket, stations: Station.get_all(), name: "", lat: "", lon: "", station_name: "")
    {:noreply, socket}
  end

  def handle_event("find", %{"station_name" => station_name}, socket) do
    stations =
      case station_name do
        "" -> Station.get_all()
        _ -> Station.find_by_name(station_name)
      end

    socket = assign(socket, stations: stations, station_name: station_name)
    {:noreply, socket}
  end

  def render(assigns) do
    ~H"""
    <b>Create new station</b>
    <form phx-submit="insert">
      Name: <input type="text" name="name" value={@name} /><br/>
      Lat: <input type="number" name="lat" step="0.000001" value={@lat} /><br/>
      Lon: <input type="number" name="lon" step="0.000001" value={@lon} /><br/>
      <input type="submit" />
    </form><br/>

    <b>Find station by name</b>
    <form phx-change="find">
      Station name: <input type="text" name="station_name" value={@station_name} /><br/>
    </form><br/>

    <b>Stations</b>
    <table>
      <tr>
        <th>Name</th><th>Longitude</th><th>Latitude</th>
      </tr>
      <%= for station <- @stations do %>
        <tr>
          <td><%= station.name %></td>
          <td><%= station.lon %></td>
          <td><%= station.lat %></td>
        </tr>
      <% end %>
    </table>
    """
  end
end