defmodule Membrane.RTC.Engine.Endpoint.Remote.ConnectionSetup do
  @moduledoc """
  """

  @typedoc """
  * `stream_format` - defines audio mixer output stream_format.
  """
  @type t() :: %__MODULE__{
          token: String.t() | nil,
          link_to: {atom(), Node.t()} | nil
        }

  defstruct token: nil,
            link_to: nil
  # end
end
