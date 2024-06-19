defmodule Pollutiondbapp.Reading do
  use Ecto.Schema
  import Ecto.Changeset
  require Ecto.Query

  schema "readings" do
    field :date, :date
    field :time, :time
    field :type, :string
    field :value, :float

    belongs_to :station, Pollutiondbapp.Station
  end

  defp changeset(reading, attrs) do
    reading
    |> cast(attrs, [:date, :time, :type, :value, :station_id])
    |> validate_required([:date, :time, :type, :value, :station_id])
  end

  def add_now(station, type, value) do
    %__MODULE__{}
    |> changeset(%{
      date: Date.utc_today(),
      time: Time.utc_now(),
      type: type,
      value: value,
      station_id: station.id
    })
    |> Pollutiondbapp.Repo.insert()
  end

  def get_last_ten() do
    Ecto.Query.from(r in Pollutiondbapp.Reading,
      order_by: [desc: r.date, desc: r.time],
      limit: 10
    )
    |> Pollutiondbapp.Repo.all()
    |> Pollutiondbapp.Repo.preload(:station)
  end

  def find_by_date(date) do
    Ecto.Query.from(r in Pollutiondbapp.Reading,
      where: r.date == ^date,
      order_by: [desc: r.time],
      limit: 10
    )
    |> Pollutiondbapp.Repo.all()
    |> Pollutiondbapp.Repo.preload(:station)
  end

  def add(station_id, date, time, type, value) do
    %__MODULE__{}
    |> changeset(%{
      date: date,
      time: time,
      type: type,
      value: value,
      station_id: station_id
    })
    |> Pollutiondbapp.Repo.insert()
  end
end