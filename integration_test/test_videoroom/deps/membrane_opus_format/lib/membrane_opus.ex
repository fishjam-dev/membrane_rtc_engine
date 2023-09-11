defmodule Membrane.Opus do
  @moduledoc """
  This module implements struct describing an Opus-encoded audio stream.

  Based on [RFC 6716](https://tools.ietf.org/html/rfc6716).
  """

  @typedoc """
  Number of channels transmitted in the stream.
  """
  @type channels_t :: 1 | 2

  @typedoc """
  Determines if stream uses self-delimiting framing.

  Self-delimiting framing provides information necessary to parse
  uncontainerized Opus stream.
  """
  @type self_delimiting_t :: boolean

  @type t :: %__MODULE__{
          channels: channels_t,
          self_delimiting?: self_delimiting_t
        }

  @enforce_keys [:channels]
  defstruct @enforce_keys ++ [self_delimiting?: false]
end
