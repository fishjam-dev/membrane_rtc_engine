defmodule Membrane.H264.FFmpeg.Parser do
  @moduledoc """
  Membrane element providing parser for H264 encoded video stream.
  Uses the parser provided by FFmpeg.

  By default, this parser splits the stream into h264 access units,
  each of which is a sequence of NAL units corresponding to one
  video frame, and equips them with the following metadata entries
  under `:h264` key:
  - `key_frame?: boolean` - determines whether the frame is a h264
    I frame.

  Setting custom packetization options affects metadata, see `alignment`
  and `attach_nalus?` options for details.

  This Parser is also capable of handling out-of-band parameters in the form of Decoder Configuration Record.
  To inject it, simply send `t:Membrane.H264.RemoteStream.t/0` stream_format containing the Decoder Configuration Record to this element.
  There are however some limitations:
  - `t:Membrane.H264.RemoteStream.t/0` stream_format needs to be send only before the first buffer.
    Sending them during the stream will cause an error
  - SPS and PPS will be extracted from Decoder Configuration Record and added to the payload of the very first buffer without any checks of in-band parameters.
    This might result in duplicated SPS and PPS. It shouldn't be a problem, unless you send an incorrect Decoder Configuration Record that doesn't match the stream.
  """
  use Membrane.Filter
  use Bunch

  require Membrane.Logger

  alias __MODULE__.{NALu, Native}
  alias Membrane.Buffer
  alias Membrane.H264

  def_input_pad :input,
    demand_unit: :buffers,
    demand_mode: :auto,
    accepted_format: %format{} when format in [Membrane.RemoteStream, H264, H264.RemoteStream]

  def_output_pad :output,
    demand_mode: :auto,
    accepted_format: %H264{}

  def_options framerate: [
                spec: H264.framerate_t() | nil,
                default: nil,
                description: """
                Framerate of video stream, see `t:Membrane.H264.framerate_t/0`
                """
              ],
              sps: [
                spec: binary(),
                default: <<>>,
                description: """
                Sequence Parameter Set NAL unit - if absent in the stream, should
                be provided via this option.
                """
              ],
              pps: [
                spec: binary(),
                default: <<>>,
                description: """
                Picture Parameter Set NAL unit - if absent in the stream, should
                be provided via this option.
                """
              ],
              alignment: [
                spec: :au | :nal,
                default: :au,
                description: """
                Stream units carried by each output buffer. See `t:Membrane.H264.alignment_t/0`.

                If alignment is `:nal`, the following metadata entries are added:
                - `type` - h264 nalu type
                - `new_access_unit: access_unit_metadata` - added whenever the new access unit starts.
                  `access_unit_metadata` is the metadata that would be merged into the buffer metadata
                   normally (if `alignment` was `:au`).
                - `end_access_unit: true` - added for each NALu that ends an access unit.
                """
              ],
              attach_nalus?: [
                spec: boolean(),
                default: false,
                description: """
                Determines whether to attach NAL units list to the metadata when `alignment` option
                is set to `:au`. For details see `t:Membrane.H264.nalu_in_metadata_t/0`.
                """
              ],
              skip_until_keyframe?: [
                spec: boolean(),
                default: false,
                description: """
                Determines whether to drop the stream until the first key frame is received.
                """
              ],
              skip_until_parameters?: [
                spec: boolean(),
                default: true,
                description: """
                Determines whether to drop the stream until the first set of SPS and PPS is received.
                """
              ],
              max_frame_reorder: [
                spec: non_neg_integer(),
                default: 15,
                description: """
                Defines the maximum expected number of consequent b-frames in the stream.
                """
              ]

  @impl true
  def handle_init(_ctx, opts) do
    state =
      opts
      |> Map.from_struct()
      |> reset_state()

    {[], state}
  end

  @impl true
  def handle_setup(_ctx, state) do
    {[], %{state | parser_ref: Native.create!()}}
  end

  @impl true
  def handle_playing(_ctx, %{skip_until_keyframe: true} = state) do
    {[event: {:input, %Membrane.KeyframeRequestEvent{}}], state}
  end

  @impl true
  def handle_playing(_ctx, state) do
    {[], state}
  end

  @impl true
  def handle_process(
        :input,
        buffer,
        _ctx,
        %{skip_until_parameters?: true, frame_prefix: <<>>} = state
      ) do
    data = state.acc <> buffer.payload

    {parsed, _au_info, unparsed} = NALu.parse(data)

    case find_parameters(parsed) do
      {:error, :not_enough_buffers} ->
        {[], %{state | acc: data}}

      {:ok, buffers} ->
        contents =
          Enum.map_join(buffers, fn %{prefixed_poslen: {pos, len}} ->
            <<_prefix::binary-size(pos), nalu::binary-size(len), _rest::binary>> = data
            nalu
          end)

        do_process(%{buffer | payload: contents <> unparsed}, %{
          state
          | skip_until_parameters?: false
        })
    end
  end

  # If frame prefix has been applied, proceed to parsing the buffer
  @impl true
  def handle_process(:input, buffer, _ctx, %{frame_prefix: <<>>} = state) do
    do_process(buffer, state)
  end

  # If there is a frame prefix to be applied, check that there are no in-band parameters and write the prefix if necessary
  @impl true
  def handle_process(:input, %Buffer{} = buffer, _ctx, state) when state.frame_prefix != <<>> do
    buffer = Map.update!(buffer, :payload, &(state.frame_prefix <> &1))
    do_process(buffer, %{state | frame_prefix: <<>>})
  end

  defp reset_state(state) do
    Map.merge(state, %{
      pending_stream_format: nil,
      parser_ref: nil,
      partial_frame: <<>>,
      frame_prefix: state.sps <> state.pps,
      metadata: nil,
      acc: <<>>,
      profile_has_b_frames?: nil
    })
  end

  defp do_process(%Buffer{payload: payload} = buffer, state) do
    case Native.parse(payload, state.parser_ref) do
      {:ok, sizes, decoding_order_numbers, presentation_order_numbers, resolution_changes} ->
        metadata = %{buffer_metadata: buffer.metadata, pts: buffer.pts, dts: buffer.dts}

        profile = Native.get_profile!(state.parser_ref)
        state = %{state | profile_has_b_frames?: profile_has_b_frames?(profile)}

        {bufs, state} =
          parse_access_units(
            payload,
            Enum.with_index(sizes),
            metadata,
            decoding_order_numbers,
            presentation_order_numbers,
            state
          )

        parse_resolution_changes(state, bufs, resolution_changes)

      {:error, reason} ->
        raise "Native parser failed to parse the payload: #{inspect(reason)}"
    end
  end

  # analyze resolution changes and generate appropriate stream_format before corresponding buffers
  defp parse_resolution_changes(state, bufs, resolution_changes, acc \\ [])

  defp parse_resolution_changes(state, [], [], []) do
    {[], state}
  end

  defp parse_resolution_changes(state, bufs, [], acc) do
    bufs = Enum.map(bufs, fn {_au, buf} -> buf end)

    stream_format =
      if state.pending_stream_format != nil do
        [stream_format: {:output, state.pending_stream_format}]
      else
        []
      end

    actions = Enum.reverse([buffer: {:output, bufs}] ++ stream_format ++ acc)

    {actions, %{state | pending_stream_format: nil}}
  end

  defp parse_resolution_changes(state, bufs, [meta | resolution_changes], acc) do
    {old_bufs, next_bufs} = Enum.split_while(bufs, fn {au, _buf} -> au < meta.index end)
    next_stream_format = mk_stream_format(state, meta.width, meta.height)

    {stream_format, state} =
      if old_bufs == [],
        do: {[], %{state | pending_stream_format: next_stream_format}},
        else: {[stream_format: {:output, next_stream_format}], state}

    buffers_before_change =
      case old_bufs do
        [] ->
          []

        _non_empty ->
          old_bufs = Enum.map(old_bufs, fn {_au, buf} -> buf end)
          [buffer: {:output, old_bufs}]
      end

    {pending_stream_format, state} =
      if state.pending_stream_format != nil and buffers_before_change != [] do
        {[stream_format: {:output, state.pending_stream_format}],
         %{state | pending_stream_format: nil}}
      else
        {[], state}
      end

    parse_resolution_changes(
      state,
      next_bufs,
      resolution_changes,
      stream_format ++ buffers_before_change ++ pending_stream_format ++ acc
    )
  end

  @impl true
  def handle_stream_format(:input, %Membrane.H264.RemoteStream{} = stream_format, ctx, state)
      when ctx.pads.input.start_of_stream? do
    Membrane.Logger.debug("Disposing old parser due to stream format change")

    state = reset_state(state)
    state = %{state | parser_ref: Native.create!()}

    do_handle_stream_format(stream_format, state)
  end

  def handle_stream_format(:input, %Membrane.H264.RemoteStream{} = stream_format, _ctx, state) do
    do_handle_stream_format(stream_format, state)
  end

  def handle_stream_format(:input, _stream_format, _ctx, state) do
    # ignoring stream_format, new onew will be generated in handle_process
    {[], state}
  end

  defp do_handle_stream_format(
         %Membrane.H264.RemoteStream{decoder_configuration_record: dcr},
         state
       )
       when dcr != nil do
    {:ok, %{sps: sps, pps: pps}} = Membrane.H264.FFmpeg.Parser.DecoderConfiguration.parse(dcr)

    frame_prefix =
      Enum.concat([[state.frame_prefix], sps, pps])
      |> Enum.join(<<0, 0, 1>>)

    if state.skip_until_parameters? do
      Membrane.Logger.warn("""
      Flag skip_until_parameters? is not compatible with Membrane.H264.RemoteStream stream_format.
      It is being automatically disabled.
      """)
    end

    {[], %{state | frame_prefix: frame_prefix, skip_until_parameters?: false}}
  end

  defp do_handle_stream_format(_stream_format, state) do
    # ignoring stream_format, new ones will be generated in handle_process
    {[], state}
  end

  @impl true
  def handle_end_of_stream(:input, ctx, state) do
    with {:ok, sizes, decoding_order_numbers, presentation_order_numbers, resolution} <-
           Native.flush(state.parser_ref) do
      {bufs, state} =
        parse_access_units(
          <<>>,
          Enum.with_index(sizes),
          state.metadata,
          decoding_order_numbers,
          presentation_order_numbers,
          state
        )

      if state.partial_frame != <<>> do
        Membrane.Logger.warn("Discarding incomplete frame because of end of stream")
      end

      stream_format = mk_stream_format(state, resolution.width, resolution.height)

      stream_format_actions =
        if stream_format != ctx.pads.output.stream_format,
          do: [stream_format: {:output, stream_format}],
          else: []

      bufs = Enum.map(bufs, fn {_au, buf} -> buf end)
      actions = stream_format_actions ++ [buffer: {:output, bufs}, end_of_stream: :output]
      {actions, state}
    else
      {:error, reason} -> raise "Native parser failed to flush: #{inspect(reason)}"
    end
  end

  defp parse_access_units(
         input,
         au_sizes,
         metadata,
         decoding_order_numbers,
         presentation_order_numbers,
         %{partial_frame: <<>>} = state
       ) do
    state = %{state | metadata: metadata}

    {buffers, input, state} =
      do_parse_access_units(
        input,
        au_sizes,
        metadata,
        decoding_order_numbers,
        presentation_order_numbers,
        state,
        []
      )

    {buffers, %{state | partial_frame: input}}
  end

  defp parse_access_units(
         input,
         [],
         _metadata,
         _decoding_order_numbers,
         _presentation_order_numbers,
         state
       ) do
    {[], %{state | partial_frame: state.partial_frame <> input}}
  end

  defp parse_access_units(
         input,
         [au_size | au_sizes],
         metadata,
         [decoding_order_number | decoding_order_numbers],
         [presentation_order_number | presentation_order_numbers],
         state
       ) do
    {first_au_buffers, input, state} =
      do_parse_access_units(
        state.partial_frame <> input,
        [au_size],
        state.metadata,
        [decoding_order_number],
        [presentation_order_number],
        state,
        []
      )

    state = %{state | metadata: metadata}

    {buffers, input, state} =
      do_parse_access_units(
        input,
        au_sizes,
        state.metadata,
        decoding_order_numbers,
        presentation_order_numbers,
        state,
        []
      )

    {first_au_buffers ++ buffers, %{state | partial_frame: input}}
  end

  defp do_parse_access_units(
         input,
         [],
         _metadata,
         _decoding_order_numbers,
         _presentation_order_numbers,
         state,
         acc
       ) do
    {acc |> Enum.reverse() |> List.flatten(), input, state}
  end

  defp do_parse_access_units(
         input,
         [{au_size, au_number} | au_sizes],
         metadata,
         [decoding_order_number | decoding_order_numbers],
         [presentation_order_number | presentation_order_numbers],
         state,
         acc
       ) do
    <<au::binary-size(au_size), rest::binary>> = input

    {pts, dts} =
      withl framerate: {frames, seconds} <- state.framerate,
            positive_order_number: true <- presentation_order_number >= 0 do
        pts =
          div(
            presentation_order_number * seconds * Membrane.Time.second(),
            frames
          )

        decoding_order_number =
          if state.profile_has_b_frames?,
            do: decoding_order_number - state.max_frame_reorder,
            else: decoding_order_number

        dts =
          div(
            decoding_order_number * seconds * Membrane.Time.second(),
            frames
          )

        {pts, dts}
      else
        positive_order_number: false -> {nil, nil}
        framerate: nil -> {metadata.pts, metadata.dts}
      end

    {nalus, au_metadata, _unparsed} = NALu.parse(au, complete_nalu?: true)
    au_metadata = Map.merge(metadata.buffer_metadata, au_metadata)
    state = Map.update!(state, :skip_until_keyframe?, &(&1 and not au_metadata.h264.key_frame?))

    buffers =
      case state do
        %{skip_until_keyframe?: true} ->
          []

        %{alignment: :au, attach_nalus?: true} ->
          [
            {au_number,
             %Buffer{
               pts: pts,
               dts: dts,
               payload: au,
               metadata: put_in(au_metadata, [:h264, :nalus], nalus)
             }}
          ]

        %{alignment: :au, attach_nalus?: false} ->
          [{au_number, %Buffer{pts: pts, dts: dts, payload: au, metadata: au_metadata}}]

        %{alignment: :nal} ->
          Enum.map(nalus, fn nalu ->
            {au_number,
             %Buffer{
               pts: pts,
               dts: dts,
               payload: :binary.part(au, nalu.prefixed_poslen),
               metadata: Map.merge(metadata.buffer_metadata, nalu.metadata)
             }}
          end)
      end

    do_parse_access_units(
      rest,
      au_sizes,
      metadata,
      decoding_order_numbers,
      presentation_order_numbers,
      state,
      [buffers | acc]
    )
  end

  defp mk_stream_format(state, width, height) do
    profile = Native.get_profile!(state.parser_ref)

    %H264{
      width: width,
      height: height,
      framerate: state.framerate || {0, 1},
      alignment: state.alignment,
      nalu_in_metadata?: state.attach_nalus?,
      profile: profile
    }
  end

  defp profile_has_b_frames?(profile) do
    profile not in [:constrained_baseline, :baseline]
  end

  defp find_parameters(data, looking_for \\ [:sps, :pps])

  defp find_parameters(data, []) do
    {:ok, data}
  end

  defp find_parameters([], _looking_for), do: {:error, :not_enough_buffers}

  defp find_parameters(data, looking_for) do
    {before_buffers, after_buffers} =
      Enum.split_while(data, &(not Enum.member?(looking_for, &1.metadata.h264.type)))

    before_buffers =
      Enum.reject(before_buffers, &Enum.member?([:idr, :non_idr], &1.metadata.h264.type))

    with [%{metadata: %{h264: %{type: type}}} | _rest] <- after_buffers,
         {:ok, after_buffers} <- find_parameters(after_buffers, looking_for -- [type]) do
      {:ok, before_buffers ++ after_buffers}
    else
      [] -> {:error, :not_enough_buffers}
      error -> error
    end
  end
end
