defmodule Pollutiondbapp.Station do
  use Ecto.Schema
  import Ecto.Changeset
  require Ecto.Query

  schema "stations" do
    field :name, :string
    field :lon, :float
    field :lat, :float

    has_many :readings, Pollutiondbapp.Reading
  end

  defp changeset(station, changes) do
    station
    |> cast(changes, [:name, :lon, :lat])
    |> validate_required([:name, :lon, :lat])
    |> validate_coordinates()
  end

  defp validate_coordinates(changeset) do
    lon = get_field(changeset, :lon)
    lat = get_field(changeset, :lat)

    if lon < -180 or lon > 180 do
      changeset
      |> add_error(:lon, "Longitude must be between -180 and 180")
    end

    if lat < -90 or lat > 90 do
      changeset
      |> add_error(:lat, "Latitude must be between -90 and 90")
    end

    changeset
  end

  def add(name, lon, lat) do
    %__MODULE__{}
    |> changeset(%{name: name, lon: lon, lat: lat})
    |> Pollutiondbapp.Repo.insert()
  end

  def get_all() do
    Pollutiondbapp.Repo.all(Pollutiondbapp.Station)
  end

  def get_by_id(id) do
    Pollutiondbapp.Repo.get(Pollutiondbapp.Station, id)
  end

  def delete(station) do
    Pollutiondbapp.Repo.delete(station)
  end

  def find_by_name(name) do
    Ecto.Query.from(s in Pollutiondbapp.Station,
      where: s.name == ^name)
    |> Pollutiondbapp.Repo.all()
  end

  def find_by_location(lon, lat) do
    Ecto.Query.from(s in Pollutiondbapp.Station,
      where: s.lon == ^lon,
      where: s.lat == ^lat)
    |> Pollutiondbapp.Repo.all()
  end

  def find_by_location_range(lon_min, lon_max, lat_min, lat_max) do
    Ecto.Query.from(s in Pollutiondbapp.Station,
      where: s.lon >= ^lon_min,
      where: s.lon <= ^lon_max,
      where: s.lat >= ^lat_min,
      where: s.lat <= ^lat_max)
    |> Pollutiondbapp.Repo.all()
  end

  def update_name(station, newname) do
    station
    |> changeset(%{name: newname})
    |> Pollutiondbapp.Repo.update()
  end
end