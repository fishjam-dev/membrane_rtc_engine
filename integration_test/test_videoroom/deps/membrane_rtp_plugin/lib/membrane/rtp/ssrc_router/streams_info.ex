defmodule Membrane.RTP.SSRCRouter.StreamsInfo do
  @moduledoc """
  A struct sent as a message to `Membrane.RTP.SSRCRouter` with info about known streams
  and streams that will be identified by extensions and should not be reported until they are present

  In particular, simulcast tracks need a RID extension in header in order to be handled.
  """

  alias Membrane.RTP

  @type t() :: %__MODULE__{
          accept_ssrcs: [RTP.ssrc_t()],
          require_extensions: %{RTP.payload_type_t() => [RTP.Header.Extension.identifier_t()]}
        }
  @enforce_keys [:require_extensions]
  defstruct @enforce_keys ++ [accept_ssrcs: []]
end
