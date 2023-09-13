defmodule Membrane.RTC.Engine.Endpoint.Remote.LinkProposal do
  @moduledoc """
  Module that represents data needed to create link between Remote Endpoints
  """
  @typedoc """
  * `token`
  * `link_to`
  """
  @type t() :: %__MODULE__{
          token: String.t() | nil,
          link_to: {atom(), Node.t()} | nil
        }

  defstruct token: nil,
            link_to: nil
end
