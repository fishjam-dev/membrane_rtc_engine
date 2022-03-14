defmodule Membrane.RTC.Engine.Endpoint.WebRTC.VP8MungerTest do
  use ExUnit.Case
  use Bitwise

  alias Membrane.RTC.Engine.Endpoint.WebRTC.VP8Munger
  alias Membrane.RTP.VP8

  test "VP8Munger handles encoding switch properly" do
    vp8_munger = VP8Munger.new()

    buffer =
      %VP8.PayloadDescriptor{
        i: 1,
        k: 1,
        keyidx: 28,
        l: 1,
        m: 1,
        n: 0,
        partition_index: 0,
        picture_id: 14_617,
        s: 0,
        t: 1,
        tid: 0,
        tl0picidx: 19,
        x: 1,
        y: 1
      }
      |> VP8.PayloadDescriptor.serialize()
      |> then(fn serialized_payload_descriptor ->
        %Membrane.Buffer{payload: serialized_payload_descriptor <> generate_random_payload(100)}
      end)

    buffer2 =
      %VP8.PayloadDescriptor{
        i: 1,
        k: 1,
        keyidx: 10,
        l: 1,
        m: 1,
        n: 0,
        partition_index: 0,
        picture_id: 11_942,
        s: 1,
        t: 1,
        tid: 0,
        tl0picidx: 4,
        x: 1,
        y: 1
      }
      |> VP8.PayloadDescriptor.serialize()
      |> then(fn serialized_payload_descriptor ->
        %Membrane.Buffer{payload: serialized_payload_descriptor <> generate_random_payload(100)}
      end)

    vp8_munger = VP8Munger.init(vp8_munger, buffer)
    {vp8_munger, munged_buffer} = VP8Munger.munge(vp8_munger, buffer)

    # nothing should change
    assert buffer == munged_buffer

    vp8_munger = VP8Munger.update(vp8_munger, buffer2)
    {_vp8_munger, munged_buffer2} = VP8Munger.munge(vp8_munger, buffer2)

    {:ok, {payload_descriptor, _payload}} =
      VP8.PayloadDescriptor.parse_payload_descriptor(munged_buffer2.payload)

    assert %VP8.PayloadDescriptor{
             i: 1,
             k: 1,
             keyidx: 29,
             l: 1,
             m: 1,
             n: 0,
             partition_index: 0,
             picture_id: 14_618,
             s: 1,
             t: 1,
             tid: 0,
             tl0picidx: 20,
             x: 1,
             y: 1
           } = payload_descriptor
  end

  test "VP8Munger handles rollovers properly" do
    vp8_munger = VP8Munger.new()

    buffer =
      %VP8.PayloadDescriptor{
        i: 1,
        k: 1,
        # max keyidx
        keyidx: (1 <<< 5) - 1,
        l: 1,
        m: 1,
        n: 0,
        partition_index: 0,
        # max picture id
        picture_id: (1 <<< 15) - 1,
        s: 0,
        t: 1,
        tid: 0,
        # max tl0picidx
        tl0picidx: (1 <<< 8) - 1,
        x: 1,
        y: 1
      }
      |> VP8.PayloadDescriptor.serialize()
      |> then(fn serialized_payload_descriptor ->
        %Membrane.Buffer{payload: serialized_payload_descriptor <> generate_random_payload(100)}
      end)

    buffer2 =
      %VP8.PayloadDescriptor{
        i: 1,
        k: 1,
        keyidx: 30,
        l: 1,
        m: 1,
        n: 0,
        partition_index: 0,
        picture_id: 11_942,
        s: 1,
        t: 1,
        tid: 0,
        tl0picidx: 4,
        x: 1,
        y: 1
      }
      |> VP8.PayloadDescriptor.serialize()
      |> then(fn serialized_payload_descriptor ->
        %Membrane.Buffer{payload: serialized_payload_descriptor <> generate_random_payload(100)}
      end)

    vp8_munger = VP8Munger.init(vp8_munger, buffer)
    {vp8_munger, munged_buffer} = VP8Munger.munge(vp8_munger, buffer)

    # nothing should change
    assert buffer == munged_buffer

    vp8_munger = VP8Munger.update(vp8_munger, buffer2)
    {_vp8_munger, munged_buffer2} = VP8Munger.munge(vp8_munger, buffer2)

    {:ok, {payload_descriptor, _payload}} =
      VP8.PayloadDescriptor.parse_payload_descriptor(munged_buffer2.payload)

    assert %VP8.PayloadDescriptor{
             i: 1,
             k: 1,
             keyidx: 0,
             l: 1,
             m: 1,
             n: 0,
             partition_index: 0,
             picture_id: 0,
             s: 1,
             t: 1,
             tid: 0,
             tl0picidx: 0,
             x: 1,
             y: 1
           } = payload_descriptor
  end

  test "VP8Munger doesn't create negative numbers after rollovers" do
    vp8_munger = VP8Munger.new()

    buffer =
      %VP8.PayloadDescriptor{
        i: 1,
        k: 1,
        keyidx: 20,
        l: 1,
        m: 1,
        n: 0,
        partition_index: 0,
        picture_id: 11_942,
        s: 1,
        t: 1,
        tid: 0,
        tl0picidx: 4,
        x: 1,
        y: 1
      }
      |> VP8.PayloadDescriptor.serialize()
      |> then(fn serialized_payload_descriptor ->
        %Membrane.Buffer{payload: serialized_payload_descriptor <> generate_random_payload(100)}
      end)

    buffer2 =
      %VP8.PayloadDescriptor{
        i: 1,
        k: 1,
        # max keyidx
        keyidx: (1 <<< 5) - 1,
        l: 1,
        m: 1,
        n: 0,
        partition_index: 0,
        # max picture id
        picture_id: (1 <<< 15) - 1,
        s: 0,
        t: 1,
        tid: 0,
        # max tl0picidx
        tl0picidx: (1 <<< 8) - 1,
        x: 1,
        y: 1
      }
      |> VP8.PayloadDescriptor.serialize()
      |> then(fn serialized_payload_descriptor ->
        %Membrane.Buffer{payload: serialized_payload_descriptor <> generate_random_payload(100)}
      end)

    buffer3 =
      %VP8.PayloadDescriptor{
        i: 1,
        k: 1,
        keyidx: 0,
        l: 1,
        m: 1,
        n: 0,
        partition_index: 0,
        picture_id: 0,
        s: 0,
        t: 1,
        tid: 0,
        tl0picidx: 0,
        x: 1,
        y: 1
      }
      |> VP8.PayloadDescriptor.serialize()
      |> then(fn serialized_payload_descriptor ->
        %Membrane.Buffer{payload: serialized_payload_descriptor <> generate_random_payload(100)}
      end)

    vp8_munger = VP8Munger.init(vp8_munger, buffer)
    {vp8_munger, munged_buffer} = VP8Munger.munge(vp8_munger, buffer)

    # nothing should change
    assert buffer == munged_buffer

    vp8_munger = VP8Munger.update(vp8_munger, buffer2)
    {_vp8_munger, munged_buffer2} = VP8Munger.munge(vp8_munger, buffer2)

    {:ok, {payload_descriptor, _payload}} =
      VP8.PayloadDescriptor.parse_payload_descriptor(munged_buffer2.payload)

    assert %VP8.PayloadDescriptor{
             i: 1,
             k: 1,
             keyidx: 21,
             l: 1,
             m: 1,
             n: 0,
             partition_index: 0,
             picture_id: 11_943,
             s: 0,
             t: 1,
             tid: 0,
             tl0picidx: 5,
             x: 1,
             y: 1
           } = payload_descriptor

    {_vp8_munger, munged_buffer3} = VP8Munger.munge(vp8_munger, buffer3)

    {:ok, {payload_descriptor, _payload}} =
      VP8.PayloadDescriptor.parse_payload_descriptor(munged_buffer3.payload)

    # check if picture_id, tl0picidx and keyidx are not negative
    assert %VP8.PayloadDescriptor{
             i: 1,
             k: 1,
             keyidx: 22,
             l: 1,
             m: 1,
             n: 0,
             partition_index: 0,
             picture_id: 11_944,
             s: 0,
             t: 1,
             tid: 0,
             tl0picidx: 6,
             x: 1,
             y: 1
           } = payload_descriptor
  end

  defp generate_random_payload(size) do
    for _i <- 1..size, into: <<>>, do: <<Enum.random(0..255)>>
  end
end
