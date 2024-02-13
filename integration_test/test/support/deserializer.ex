defmodule Recording.Deserializer do
  use Membrane.Pipeline

  import Membrane.ChildrenSpec

  @impl true
  def handle_init(_ctx, opts = %{type: :video}) do
    structure = [
      child(:source, %Membrane.File.Source{location: opts.source})
      |> child(:deserializer, Membrane.Stream.Deserializer)
      |> child(:rtp, %Membrane.RTP.DepayloaderBin{
        depayloader: Membrane.RTP.H264.Depayloader,
        clock_rate: 90_000
      })
      |> child(:parser, %Membrane.H264.Parser{
        generate_best_effort_timestamps: %{framerate: {60, 1}}
      })
      |> child(:sink, %Membrane.File.Sink{location: opts.output})
    ]

    {[spec: structure], %{owner: opts.owner}}
  end

  @impl true
  def handle_init(_ctx, opts = %{type: :audio}) do
    structure = [
      child(:source, %Membrane.File.Source{location: opts.source})
      |> child(:deserializer, Membrane.Stream.Deserializer)
      |> child(:rtp, %Membrane.RTP.DepayloaderBin{
        depayloader: Membrane.RTP.Opus.Depayloader,
        clock_rate: 48_000
      })
      |> child(:sink, %Membrane.File.Sink{location: opts.output})
    ]

    {[spec: structure], %{owner: opts.owner}}
  end

  @impl true
  def handle_element_end_of_stream(:sink, _pad, _context, state) do
    send(state.owner, {:deserializer, :finished})
    {[], state}
  end

  @impl true
  def handle_element_end_of_stream(_child, _pad, _context, state) do
    {[], state}
  end
end
