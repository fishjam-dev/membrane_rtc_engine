defmodule Membrane.RTC.Engine.Endpoint.SIP.SDP do

  @accepted_payload_types [8]

  def proposal(external_ip, rtp_port) do
    create_proposal(external_ip, rtp_port) |> to_string()
    {sdp, byte_size(sdp)}
  end

  defp create_proposal(external_ip, rtp_port) do
    connection_data = %ExSDP.ConnectionData{address: {:IP4, external_ip}, network_type: "IN"}
    media = ExSDP.Media.new(:audio, rtp_port, "RTP/AVP", @accepted_payload_types)
    sdp = ExSDP.new(address: {:IP4, external_ip}, session_name: "Telefish", username: "Jellyfish")
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

end