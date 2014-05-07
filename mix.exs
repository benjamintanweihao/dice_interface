defmodule DiceInterface.Mixfile do
  use Mix.Project

  def project do
    [app: :dice_interface,
     version: "0.0.1",
     elixir: "~> 0.13.1-dev",
     deps: deps]
  end

  def application do
    [ applications: [],
      mod: {DiceInterface, []} ]
  end

  defp deps do
    [{:json, github: "cblage/elixir-json"}]
  end
end
