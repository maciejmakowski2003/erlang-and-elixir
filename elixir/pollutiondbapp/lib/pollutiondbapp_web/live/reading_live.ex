defmodule PollutiondbappWeb.ReadingLive do
  use PollutiondbappWeb, :live_view

  alias Pollutiondbapp.Reading
  alias Pollutiondbapp.Station

  def mount(_params, _session, socket) do
    socket = assign(socket, readings: Reading.get_last_ten(), date: "", stations: Station.get_all(), station_id: "", date: "", time: "", type: "", value: "")
    {:ok, socket}
  end

  def handle_event("get_by_date", %{"date" => date}, socket) do
    socket = assign(socket, readings: Reading.find_by_date(date), date: date)
    {:noreply, socket}
  end

  def handle_event("create", %{"station_id" => station_id, "date" => date, "time" => time, "type" => type, "value" => value}, socket) do
    Reading.add(station_id, date, time, type, value)
    socket = assign(socket, station_id: "", date: "", time: "", type: "", value: "", readings: Reading.get_last_ten())
    {:noreply, socket}
  end

  def render(assigns) do
    ~H"""
    <b>Create Reading</b>
    <form phx-submit="create">
      Station:
        <select name="station_id">
          <%= for station <- @stations do %>
            <option label={station.name} value={station.id} selected={station.id == @station_id}/>
          <% end %>
        </select><br/>
      Date: <input type="date" name="date" /><br/>
      Time: <input type="time" name="time" /><br/>
      Type: <input type="text" name="type" /><br/>
      Value: <input type="number" step="0.1" name="value" /><br/>
      <button type="submit">Create</button>
    </form><br/>

    <b>Find last ten readings by date</b>
    <form phx-change="get_by_date">
      Date: <input type="date" name="date" value={@date} /><br/>
    </form><br/>

    <b>Readings</b>
    <table>
      <tr>
        <th>Station</th><th>Date</th><th>Time</th><th>Type</th><th>Value</th>
      </tr>
      <%= for reading <- @readings do %>
        <tr>
          <td><%= reading.station.name %></td>
          <td><%= reading.date %></td>
          <td><%= reading.time %></td>
          <td><%= reading.type %></td>
          <td><%= reading.value %></td>
        </tr>
      <% end %>
    </table>
    """
  end
end