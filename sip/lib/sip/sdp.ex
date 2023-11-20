defmodule Membrane.RTC.Engine.Endpoint.SIP.SDP do

  @accepted_payload_types [8]

  def proposal do
    create_proposal |> to_string()
    {sdp, byte_size(sdp)}
  end

  defp create_proposal do
    connection_data = %ExSDP.ConnectionData{address: {:IP4, "49.13.56.174"}, network_type: "IN"}
    media = ExSDP.Media.new(:audio, 60200, "RTP/AVP", @accepted_payload_types)
    sdp = ExSDP.new(address: {:IP4, "49.13.56.174"}, session_name: "Telefish", username: "Jellyfish")
      |> Map.replace(:connection_data, connection_data)
      |> ExSDP.add_media(media)
      |> ExSDP.add_attribute("sendrecv")
      |> ExSDP.add_attribute("rtcp-mux")
  end

  def extract_data(body) do
    with {:ok, sdp} <- ExSDP.parse(body),
         audio_media when not is_nil(audio_media) <- Enum.find(sdp.media, fn media -> media.type == :audio end),
         common_payload_types when length(common_payload_types) != 0 <- @accepted_payload_types -- @accepted_payload_types -- audio_media.fmt,
         chosen_payload_type <- @accepted_payload_types -- @accepted_payload_types -- audio_media.fmt do
      {:ok, %{
        rptmap: Membrane.RTP.PayloadFormat.get_payload_type_mapping(chosen_payload_type)
        connection_data: audio_media.connection_data || sdp.connection_data,
        port: audio_media.port
      }}
    else
      {:error, reason} ->
        {:error, reason}
      {:not_supported_addr_type, _addr} ->
        {:error, :not_supported_addr_type}
      [] ->
        {:error, :no_common_fmt}
      nil ->
        {:error, :no_audio_media}
    end
  end

  def create_answer(body) do
    with {:ok, sdp} <- ExSDP.parse(body),
         audio_media when not is_nil(audio_media) <- Enum.find(sdp.media, fn media -> media.type == :audio end),
         common_payload_types when length(common_payload_types) != 0 <- @accepted_payload_types -- @accepted_payload_types -- audio_media.fmt,
         choosen_payload_type <- List.first(common_payload_types)do
      {:ok, %{
        rptmap: {choosen_payload_type, Membrane.RTP.PayloadFormat.get_payload_type_mapping()},
        connection_data: audio_media.connection_data || sdp.connection_data,
        port: audio_media.port
      }}
    else
      {:error, reason} ->
        {:error, reason}
      {:not_supported_addr_type, _addr} ->
        {:error, :not_supported_addr_type}
      [] ->
        {:error, :no_common_fmt}
      nil ->
        {:error, :no_audio_media}
    end
  end

%ExSDP{
   version: 0,
   session_name: "Vars.pl SoftSwitch PBX",
   origin: %ExSDP.Origin{
     username: "root",
     network_type: "IN",
     session_id: 1830545717,
     session_version: 1830545717,
     address: {91, 226, 26, 34}
   },
   email: nil,
   encryption: nil,
   uri: nil,
   phone_number: nil,
   session_information: nil,
   timing: %ExSDP.Timing{start_time: 0, stop_time: 0},
   time_zones_adjustments: nil,
   connection_data: %ExSDP.ConnectionData{
     address: {91, 226, 26, 34},
     address_count: nil,
     ttl: nil,
     network_type: "IN"
   },
   attributes: [],
   bandwidth: [%ExSDP.Bandwidth{type: :CT, bandwidth: 384}],
   media: [
     %ExSDP.Media{
       type: :audio,
       port: 11642,
       protocol: "RTP/AVP",
       fmt: [8, 3, 111, 9, 97, 0, 101],
       title: nil,
       encryption: nil,
       port_count: 1,
       connection_data: %ExSDP.ConnectionData{
         address: {91, 226, 26, 34},
         address_count: nil,
         ttl: nil,
         network_type: "IN"
       },
       bandwidth: [%ExSDP.Bandwidth{type: :CT, bandwidth: 384}],
       attributes: [
         %ExSDP.Attribute.RTPMapping{
           payload_type: 8,
           encoding: "PCMA",
           clock_rate: 8000,
           params: 1
         },
         %ExSDP.Attribute.RTPMapping{
           payload_type: 3,
           encoding: "GSM",
           clock_rate: 8000,
           params: 1
         },
         %ExSDP.Attribute.RTPMapping{
           payload_type: 111,
           encoding: "G726-32",
           clock_rate: 8000,
           params: 1
         },
         %ExSDP.Attribute.RTPMapping{
           payload_type: 9,
           encoding: "G722",
           clock_rate: 8000,
           params: 1
         },
         %ExSDP.Attribute.RTPMapping{
           payload_type: 97,
           encoding: "iLBC",
           clock_rate: 8000,
           params: 1
         },
         %ExSDP.Attribute.FMTP{
           pt: 97,
           profile_level_id: nil,
           level_asymmetry_allowed: nil,
           packetization_mode: nil,
           max_mbps: nil,
           max_smbps: nil,
           max_fs: nil,
           max_dpb: nil,
           max_br: nil,
           sprop_parameter_sets: nil,
           maxaveragebitrate: nil,
           maxplaybackrate: nil,
           sprop_maxcapturerate: nil,
           maxptime: nil,
           ptime: nil,
           minptime: nil,
           stereo: nil
         },
         %ExSDP.Attribute.RTPMapping{
           payload_type: 0,
           encoding: "PCMU",
           clock_rate: 8000,
           params: 1
         },
         %ExSDP.Attribute.RTPMapping{
           payload_type: 101,
           encoding: "telephone-event",
           clock_rate: 8000,
           params: 1
         },
         %ExSDP.Attribute.FMTP{
           pt: 101,
           profile_level_id: nil,
           level_asymmetry_allowed: nil,
           packetization_mode: nil,
           max_mbps: nil,
           max_smbps: nil,
           max_fs: nil,
           max_dpb: nil,
           max_br: nil,
           sprop_parameter_sets: nil,
           maxaveragebitrate: nil,
           maxplaybackrate: nil,
           sprop_maxcapturerate: nil,
           maxptime: nil
         },
         {:ptime, 20},
         :sendrecv
       ]
     },
     %ExSDP.Media{
       type: :video,
       port: 10392,
       protocol: "RTP/AVP",
       fmt: ~c"c",
       title: nil,
       encryption: nil,
       port_count: 1,
       connection_data: %ExSDP.ConnectionData{
         address: {91, 226, 26, 34},
         address_count: nil,
         ttl: nil,
         network_type: "IN"
       },
       bandwidth: [%ExSDP.Bandwidth{type: :CT, bandwidth: 384}],
       attributes: [
         %ExSDP.Attribute.RTPMapping{
           payload_type: 99,
           encoding: "H264", 
           clock_rate: 90000,
           params: nil
         },
         :sendrecv
       ]
     }
   ],
   time_repeats: []
 }


end