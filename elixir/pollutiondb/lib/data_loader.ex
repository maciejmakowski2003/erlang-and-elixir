defmodule PollutionDataLoader do
  defstruct datetime: "",
            location: "",
            stationName: "",
            pollutionType: "",
            pollutionLevel: ""


  def load_data() do
    data = get_lines("C:/Users/macie/OneDrive/Pulpit/Studia/YEAR 2/erlang-and-elixir/elixir/AirlyData.csv")
    |> Enum.map(&parse/1)

    data |> uniq_stations() |> Enum.map(&process_station/1)
    data |> Enum.map(&process_reading/1)
  end

  defp add_station(name, lon, lat) do
    Pollutiondb.Station.add(name, lon, lat)
  end

  defp add_reading(station, date, time, type, value) do
    Pollutiondb.Reading.add(station, date, time, type, value)
  end

  defp get_lines(filename) do
    File.read!(filename)
    |> String.split("\n")
    |> List.delete_at(-1)
  end

  defp parse(data_line) do
    [date_str, pollution_type, pollution_level_str, station_id_str, location_str, coords_str] =
      data_line
      |> String.trim()
      |> String.split(";", trim: true)

    %PollutionDataLoader{
      datetime: parseDate(date_str),
      location: parseCoords(coords_str),
      stationName: location_str <> " " <> station_id_str,
      pollutionType: pollution_type,
      pollutionLevel: String.to_float(pollution_level_str)
    }
  end

  defp parseDate(date_str) do
    {{year, month, day}, {hour, minute, second}} =
      case String.slice(date_str, 0..18) |> String.split("T") do
        [date_part, time_part] ->
          [year, month, day] = String.split(date_part, "-") |> Enum.map(&String.to_integer/1)
          [hour, minute, second] = String.split(time_part, ":") |> Enum.map(&String.to_integer/1)
          {{year, month, day}, {hour, minute, second}}

        _ ->
          {{0, 0, 0}, {0, 0, 0}}
      end

    {{year, month, day}, {hour, minute, second}}
  end

  defp parseCoords(coordinates) do
    coordinates |> String.split(",") |> Enum.map(&String.to_float/1) |> List.to_tuple()
  end

  defp process_station(station) do
    {lon, lat} = station.location
    add_station(station.stationName, lon, lat)
  end

  defp process_reading(reading) do
    {date, time} = reading.datetime
    station = Pollutiondb.Station.find_by_name(reading.stationName)
    add_reading(station, date, time, reading.pollutionType, reading.pollutionLevel)
  end

  defp uniq_stations(readings) do
    readings
    |> Enum.uniq_by(& &1.location)
  end
end
