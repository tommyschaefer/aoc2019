defmodule Day1.Part1 do
  def run(path) do
    read_input_file(path)
    |> required_fuel_for()
    |> IO.puts()
  end

  def read_input_file(path) do
    path
    |> File.stream!()
    |> Enum.map(&read_input_line/1)
  end

  def read_input_line(line) do
    line
    |> String.trim()
    |> String.to_integer()
  end

  defp required_fuel_for(masses) when is_list(masses) do
    masses
    |> Enum.map(&required_fuel_for/1)
    |> Enum.sum()
  end

  defp required_fuel_for(mass), do: (mass / 3 - 2) |> floor
end

System.argv() |> Day1.Part1.run()
