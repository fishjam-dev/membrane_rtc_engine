defmodule Membrane.RTC.Engine.Endpoint.WebRTC.RTPMungerTest do
  use ExUnit.Case
  import Bitwise

  import Membrane.RTC.Engine.Support.Utils

  alias Membrane.RTC.Engine.Endpoint.WebRTC.RTPMunger

  test "RTP Munger handles encoding switch properly" do
    h_encoding = generate_encoding(30_000, 35_345, 10)

    rtp_munger =
      RTPMunger.new(90_000)
      |> RTPMunger.init(List.first(h_encoding))

    {rtp_munger, munged_h_encoding} =
      Enum.reduce(h_encoding, {rtp_munger, []}, fn h_buffer, {rtp_munger, munged_h_encoding} ->
        {rtp_munger, munged_h_buffer} = RTPMunger.munge(rtp_munger, h_buffer)
        {rtp_munger, munged_h_encoding ++ [munged_h_buffer]}
      end)

    # nothing should change
    assert h_encoding == munged_h_encoding

    l_buffer = %Membrane.Buffer{
      payload: generate_random_payload(100),
      metadata: %{
        rtp: %{
          sequence_number: 1001,
          timestamp: 12_345
        }
      }
    }

    l_buffer2 = %Membrane.Buffer{
      payload: generate_random_payload(100),
      metadata: %{
        rtp: %{
          sequence_number: 1002,
          timestamp: 15_345
        }
      }
    }

    # switch to new encoding
    rtp_munger = RTPMunger.update(rtp_munger, l_buffer)
    {rtp_munger, munged_l_buffer} = RTPMunger.munge(rtp_munger, l_buffer)
    {_rtp_munger, munged_l_buffer2} = RTPMunger.munge(rtp_munger, l_buffer2)

    last_munged_h_buffer = List.last(munged_h_encoding)
    assert munged_l_buffer.metadata.rtp.sequence_number == 30_010
    # in real world those timestamps might not be monotonic but in tests we generate
    # packets in such a way that they have to be
    assert munged_l_buffer.metadata.rtp.timestamp > last_munged_h_buffer.metadata.rtp.timestamp

    assert munged_l_buffer2.metadata.rtp.sequence_number == 30_011
    assert munged_l_buffer2.metadata.rtp.timestamp > munged_l_buffer.metadata.rtp.timestamp
  end

  test "RTP Munger handles out-of-order packets correctly" do
    h_encoding = generate_encoding(30_000, 32_345, 10)

    rtp_munger =
      RTPMunger.new(90_000)
      |> RTPMunger.init(List.first(h_encoding))

    h_encoding = swap(h_encoding, 4, 6)

    {rtp_munger, munged_h_encoding} =
      Enum.reduce(h_encoding, {rtp_munger, []}, fn h_buffer, {rtp_munger, munged_h_encoding} ->
        {rtp_munger, munged_h_buffer} = RTPMunger.munge(rtp_munger, h_buffer)
        {rtp_munger, munged_h_encoding ++ [munged_h_buffer]}
      end)

    # nothing should change
    assert h_encoding == munged_h_encoding

    l_buffer = %Membrane.Buffer{
      payload: generate_random_payload(100),
      metadata: %{
        rtp: %{
          sequence_number: 1001,
          timestamp: 12_345
        }
      }
    }

    l_buffer2 = %Membrane.Buffer{
      payload: generate_random_payload(100),
      metadata: %{
        rtp: %{
          sequence_number: 1003,
          timestamp: 18_345
        }
      }
    }

    l_buffer3 = %Membrane.Buffer{
      payload: generate_random_payload(100),
      metadata: %{
        rtp: %{
          sequence_number: 1002,
          timestamp: 15_345
        }
      }
    }

    # switch to new encoding
    rtp_munger = RTPMunger.update(rtp_munger, l_buffer)
    {rtp_munger, munged_l_buffer} = RTPMunger.munge(rtp_munger, l_buffer)
    {rtp_munger, munged_l_buffer2} = RTPMunger.munge(rtp_munger, l_buffer2)
    {_rtp_munger, munged_l_buffer3} = RTPMunger.munge(rtp_munger, l_buffer3)

    last_munged_h_buffer = List.last(munged_h_encoding)
    assert munged_l_buffer.metadata.rtp.sequence_number == 30_010
    # in real world those timestamps might not be monotonic but in tests we generate
    # packets in such a way that they have to be
    assert munged_l_buffer.metadata.rtp.timestamp > last_munged_h_buffer.metadata.rtp.timestamp

    assert munged_l_buffer2.metadata.rtp.sequence_number == 30_012
    assert munged_l_buffer2.metadata.rtp.timestamp > munged_l_buffer.metadata.rtp.timestamp

    assert munged_l_buffer3.metadata.rtp.sequence_number == 30_011
    assert munged_l_buffer3.metadata.rtp.timestamp > munged_l_buffer.metadata.rtp.timestamp
    assert munged_l_buffer3.metadata.rtp.timestamp < munged_l_buffer2.metadata.rtp.timestamp
  end

  test "RTP Munger handles sequence number rollover properly" do
    h_buffer = %Membrane.Buffer{
      payload: generate_random_payload(100),
      metadata: %{
        rtp: %{
          sequence_number: 45_000,
          timestamp: 34_567
        }
      }
    }

    h_buffer2 = %Membrane.Buffer{
      payload: generate_random_payload(100),
      metadata: %{
        rtp: %{
          sequence_number: 45_001,
          timestamp: 37_567
        }
      }
    }

    rtp_munger =
      RTPMunger.new(90_000)
      |> RTPMunger.init(h_buffer)

    {rtp_munger, munged_h_buffer} = RTPMunger.munge(rtp_munger, h_buffer)
    # nothing should change
    assert h_buffer == munged_h_buffer

    l_buffer = %Membrane.Buffer{
      payload: generate_random_payload(100),
      metadata: %{
        rtp: %{
          # max seq num
          sequence_number: (1 <<< 16) - 1,
          timestamp: 12_345
        }
      }
    }

    l_buffer2 = %Membrane.Buffer{
      payload: generate_random_payload(100),
      metadata: %{
        rtp: %{
          sequence_number: 0,
          timestamp: 15_345
        }
      }
    }

    # switch to new encoding
    rtp_munger = RTPMunger.update(rtp_munger, l_buffer)
    {rtp_munger, munged_l_buffer} = RTPMunger.munge(rtp_munger, l_buffer)

    assert munged_l_buffer.metadata.rtp.sequence_number == 45_001
    # in real world those timestamps might not be monotonic but in tests we generate
    # packets in such a way that they have to be
    assert munged_l_buffer.metadata.rtp.timestamp > h_buffer.metadata.rtp.timestamp

    {rtp_munger, munged_l_buffer2} = RTPMunger.munge(rtp_munger, l_buffer2)
    assert munged_l_buffer2.metadata.rtp.sequence_number == 45_002
    assert munged_l_buffer2.metadata.rtp.timestamp > munged_l_buffer.metadata.rtp.timestamp

    # return to h encoding
    rtp_munger = RTPMunger.update(rtp_munger, h_buffer2)
    {_rtp_munger, munged_h_buffer2} = RTPMunger.munge(rtp_munger, h_buffer2)
    assert munged_h_buffer2.metadata.rtp.sequence_number == 45_003
    assert munged_h_buffer2.metadata.rtp.timestamp > munged_l_buffer2.metadata.rtp.timestamp
  end

  test "RTP Munger handles timestamp rollover properly" do
    h_buffer = %Membrane.Buffer{
      payload: generate_random_payload(100),
      metadata: %{
        rtp: %{
          sequence_number: 45_000,
          timestamp: 34_567
        }
      }
    }

    rtp_munger =
      RTPMunger.new(90_000)
      |> RTPMunger.init(h_buffer)

    {rtp_munger, munged_h_buffer} = RTPMunger.munge(rtp_munger, h_buffer)
    # nothing should change
    assert h_buffer == munged_h_buffer

    l_buffer = %Membrane.Buffer{
      payload: generate_random_payload(100),
      metadata: %{
        rtp: %{
          sequence_number: 5_000,
          # max timestamp
          timestamp: (1 <<< 32) - 1
        }
      }
    }

    l_buffer2 = %Membrane.Buffer{
      payload: generate_random_payload(100),
      metadata: %{
        rtp: %{
          sequence_number: 5_001,
          timestamp: 3_000
        }
      }
    }

    rtp_munger = RTPMunger.update(rtp_munger, l_buffer)
    {rtp_munger, munged_l_buffer} = RTPMunger.munge(rtp_munger, l_buffer)

    assert munged_l_buffer.metadata.rtp.sequence_number == 45_001
    # in real world those timestamps might not be monotonic but in tests we generate
    # packets in such a way that they have to be
    assert munged_l_buffer.metadata.rtp.timestamp > h_buffer.metadata.rtp.timestamp
    assert munged_l_buffer.metadata.rtp.timestamp <= (1 <<< 32) - 1

    {_rtp_munger, munged_l_buffer2} = RTPMunger.munge(rtp_munger, l_buffer2)
    assert munged_l_buffer2.metadata.rtp.sequence_number == 45_002
    assert munged_l_buffer2.metadata.rtp.timestamp > munged_l_buffer.metadata.rtp.timestamp
    assert munged_l_buffer2.metadata.rtp.timestamp <= (1 <<< 32) - 1
  end

  test "RTPMunger drops buffers that would cause duplicated sequence number" do
    [first_h_buffer | h_encoding] = generate_encoding(0, 0, 10)
    l_encoding = generate_encoding(0, 0, 10)

    [first_l_buffer | _rest] = l_encoding = swap(l_encoding, 0, 1)

    rtp_munger =
      RTPMunger.new(90_000)
      |> RTPMunger.init(first_h_buffer)

    # Munge h encoding
    rtp_munger =
      Enum.reduce(h_encoding, rtp_munger, fn h_buffer, rtp_munger ->
        {rtp_munger, _munged_h_buffer} = RTPMunger.munge(rtp_munger, h_buffer)
        rtp_munger
      end)

    # Switch the layer
    rtp_munger = RTPMunger.update(rtp_munger, first_l_buffer)

    # Munge l encoding
    {_rtp_munger, munged_l_encoding} =
      Enum.reduce(l_encoding, {rtp_munger, []}, fn l_buffer, {rtp_munger, munged_l_encoding} ->
        {rtp_munger, munged_l_buffer} = RTPMunger.munge(rtp_munger, l_buffer)
        {rtp_munger, munged_l_encoding ++ [munged_l_buffer]}
      end)

    # Second buffer should be discarded by the munger
    assert [_first, nil | rest_munged_l_encoding] = munged_l_encoding
    assert length(rest_munged_l_encoding) == 8
  end

  defp generate_encoding(seq_num_base, timestamp_base, packets_num) do
    for i <- 0..(packets_num - 1), into: [] do
      %Membrane.Buffer{
        payload: generate_random_payload(100),
        metadata: %{
          rtp: %{
            sequence_number: seq_num_base + i,
            timestamp: timestamp_base + 3000 * i
          }
        }
      }
    end
  end

  defp swap(list, index1, index2) do
    e1 = Enum.at(list, index1)
    e2 = Enum.at(list, index2)

    list
    |> List.replace_at(index1, e2)
    |> List.replace_at(index2, e1)
  end
end
