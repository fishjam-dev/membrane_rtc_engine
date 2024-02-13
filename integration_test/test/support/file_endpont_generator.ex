defmodule FileEndpointGenerator do
  @moduledoc false

  import Membrane.ChildrenSpec

  alias ExSDP.Attribute.FMTP
  alias Membrane.RTC.Engine.Endpoint.File, as: FileEndpoint

  def create_video_file_endpoint(
        rtc_engine,
        video_file_path,
        opts \\ []
      ) do
    video_track_config = %FileEndpoint.TrackConfig{
      type: :video,
      stream_id: Keyword.get(opts, :stream_id),
      encoding: :H264,
      clock_rate: 90_000,
      fmtp: %FMTP{
        pt: 96
      },
      opts: [
        metadata: %{"mainPresenter" => true, "isScreenSharing" => false},
        framerate: {60, 1}
      ]
    }

    %FileEndpoint{
      rtc_engine: rtc_engine,
      file_path: video_file_path,
      track_config: video_track_config,
      payload_type: 96,
      playback_mode: Keyword.get(opts, :playback_mode, :wait_for_first_subscriber),
      autoend: Keyword.get(opts, :autoend, true)
    }
  end

  def create_audio_file_endpoint(rtc_engine, audio_file_path, stream_id \\ nil, opts \\ []) do
    audio_track_config = %FileEndpoint.TrackConfig{
      type: :audio,
      stream_id: stream_id,
      encoding: :OPUS,
      clock_rate: 48_000,
      fmtp: %FMTP{
        pt: 108
      }
    }

    %FileEndpoint{
      rtc_engine: rtc_engine,
      file_path: audio_file_path,
      track_config: audio_track_config,
      payload_type: 108,
      after_source_transformation: &transform_aac_to_opus/1,
      playback_mode: Keyword.get(opts, :playback_mode, :wait_for_first_subscriber),
      autoend: Keyword.get(opts, :autoend, true)
    }
  end

  defp transform_aac_to_opus(link_builder) do
    link_builder
    |> child(:decoder, Membrane.AAC.FDK.Decoder)
    |> child(:encoder, %Membrane.Opus.Encoder{
      input_stream_format: %Membrane.RawAudio{
        channels: 1,
        sample_rate: 48_000,
        sample_format: :s16le
      }
    })
  end
end
