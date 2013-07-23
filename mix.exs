Code.append_path "deps/relex/ebin"

defmodule Kvs.Mixfile do
  use Mix.Project

  def project do
    [ app: :kvs,
      version: "0.0.1",
      deps: deps,
      release_options: [path: "rel"] ]
  end

  # Configuration for the OTP application
  def application do
    [ registered: [:store_sup],
      applications: [:ranch],
      mod: {Kvs, []},
      env: [ listen_port: 8090,
             nodes: [ :"node1@couchemar-P5KR",
                      :"node2@couchemar-P5KR",
                      :"node3@couchemar-P5KR" ]
           ] ]
  end

  # Returns the list of dependencies in the format:
  # { :foobar, "0.1", git: "https://github.com/elixir-lang/foobar.git" }
  defp deps do
    [{:ranch, git: "https://github.com/extend/ranch.git", tag: "0.8.3"},
     {:luerl, git: "https://github.com/rvirding/luerl.git", branch: "new-engine",
      compile: "make && cp src/luerl.app.src ebin/luerl.app"},
     {:relex, github: "yrashk/relex"}]
  end

  if Code.ensure_loaded?(Relex.Release) do
    defmodule Release do
      use Relex.Release

      def name, do: atom_to_binary(Mix.project()[:app], :utf8)
      def version, do: Mix.project[:version]
      def applications, do: [Mix.project()[:app], :luerl]
      def lib_dirs, do: ["deps"]
    end
  end

end
