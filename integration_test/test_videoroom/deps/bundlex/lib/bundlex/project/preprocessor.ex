defmodule Bundlex.Project.Preprocessor do
  @moduledoc """
  Behaviour for preprocessing Bundlex projects.

  Precompiling may involve either generating additional resources or altering the project itself.
  Currently, preprocessing native configuration (`c:preprocess_native_config/3`)
  and parsed natives (`c:preprocess_native/1`) is supported.
  """
  alias Bundlex.{Native, Project}

  @type t :: module

  @callback preprocess_native_config(
              name :: atom,
              app :: atom,
              config :: Project.native_config_t()
            ) ::
              Project.native_config_t()
  @callback preprocess_native(native :: Native.t()) :: Native.t()
end
