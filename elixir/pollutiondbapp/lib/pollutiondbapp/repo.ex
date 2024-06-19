defmodule Pollutiondbapp.Repo do
  use Ecto.Repo,
    otp_app: :pollutiondbapp,
    adapter: Ecto.Adapters.SQLite3
end
