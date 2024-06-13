defmodule Pollutiondb.Reading do
  use Ecto.Schema
  import Ecto.Changeset
  require Ecto.Query

  schema "readings" do
    field :date, :date
    field :time, :time
    field :type, :string
    field :value, :float

    belongs_to :station, Pollutiondb.Station
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
    |> Pollutiondb.Repo.insert()
  end

  def find_by_date(date) do
    Ecto.Query.from(r in Pollutiondb.Reading,
      where: r.date == ^date
    )
    |> Pollutiondb.Repo.all()
  end

  def add(station, date, time, type, value) do
    %__MODULE__{}
    |> changeset(%{
      date: Date.from_erl!(date),
      time: Time.from_erl!(time),
      type: type,
      value: value,
      station_id: station.id
    })
    |> Pollutiondb.Repo.insert()
  end
end