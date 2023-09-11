defmodule Membrane.File.SplitEvent do
  @moduledoc """
  Default event that closes current and opens new file in
  `Membrane.File.Sink.Multi`.
  """
  @derive Membrane.EventProtocol

  defstruct []
end
