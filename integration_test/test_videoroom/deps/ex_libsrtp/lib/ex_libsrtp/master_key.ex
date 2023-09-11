defmodule ExLibSRTP.MasterKey do
  @moduledoc """
  Struct containing SRTP master key and its identifier.
  """
  @type t :: %__MODULE__{
          key: binary(),
          mki: binary()
        }

  @enforce_keys [:key, :mki]
  defstruct @enforce_keys
end
