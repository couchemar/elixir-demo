defmodule Kvs.Mixfile do
  use Mix.Project

  def project do
    [ app: :kvs,
      version: "0.0.1",
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [ registered: [:store_sup],
      applications: [:ranch],
      mod: {Kvs, []},
      env: [ listen_port: 8090,
             nodes: [ :"node1@couchemar",
                      :"node2@couchemar",
                      :"node3@couchemar" ]
           ] ]
  end

  # Returns the list of dependencies in the format:
  # { :foobar, "0.1", git: "https://github.com/elixir-lang/foobar.git" }
  defp deps do
    [{:ranch, git: "https://github.com/extend/ranch.git", tag: "0.8.3"},
     {:luerl, git: "https://github.com/rvirding/luerl.git", branch: "new-engine",
      compile: "make && cp src/luerl.app.src ebin/luerl.app"}]
  end
end
