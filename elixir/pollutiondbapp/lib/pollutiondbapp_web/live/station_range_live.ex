defmodule PollutiondbappWeb.StationRangeLive do
  use PollutiondbappWeb, :live_view

  alias Pollutiondbapp.Station

  def mount(_params, _session, socket) do
    socket = assign(socket, stations: Station.get_all(), lat_min: -90.0, lat_max: 90.0, lon_min: -180.0, lon_max: 180.0)
    {:ok, socket}
  end

  def handle_event("update", %{"lat_min" => lat_min, "lat_max" => lat_max, "lon_min" => lon_min, "lon_max" => lon_max}, socket) do
    stations = Station.find_by_location_range(lon_min, lon_max, lat_min, lat_max)
    socket = assign(socket, stations: stations, lat_min: lat_min, lat_max: lat_max, lon_min: lon_min, lon_max: lon_max)
    {:noreply, socket}
  end

  def render(assigns) do
    ~H"""
    <form phx-change="update">
    Lat min: <input type="range" min="-90" max="90" name="lat_min" value={@lat_min}/><br/>
    Lat max: <input type="range" min="-90" max="90" name="lat_max" value={@lat_max}/><br/>
    Lon min: <input type="range" min="-180" max="180" name="lon_min" value={@lon_min}/><br/>
    Lon max: <input type="range" min="-180" max="180" name="lon_max" value={@lon_max}/><br/>
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