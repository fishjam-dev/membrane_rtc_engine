defmodule Membrane.RTP.Vad.VadParams do
  @moduledoc false

  # The information about selected parameters used in multiple modules concerning VAD.
  # Additionally it computes the `target_levels_length` which is the number audio levels needed for proper voice activity estimation. It is directly dependent on the VAD parameters.

  # The parameters values are different for each environment and can be tweaked in the configuration files placed in `config` directory.

  # The meaning of the parameters is described in the `Membrane.RTP.Vad.IsSpeakingEstimator`.

  @default_parameters %{
    immediate: %{
      subunits: 1,
      score_threshold: 0,
      lambda: 1
    },
    medium: %{
      subunits: 10,
      score_threshold: 20,
      subunit_threshold: 1,
      lambda: 24
    },
    long: %{
      subunits: 7,
      score_threshold: 20,
      subunit_threshold: 3,
      lambda: 47
    }
  }

  @params Application.compile_env(
            :membrane_rtp_plugin,
            :vad_estimation_parameters,
            @default_parameters
          )

  @immediate @params.immediate
  @medium @params.medium
  @long @params.long

  @spec vad_params() :: map()
  def vad_params(), do: @params

  @spec immediate() :: map()
  def immediate(), do: @immediate

  @spec medium() :: map()
  def medium(), do: @medium

  @spec long() :: map()
  def long(), do: @long

  @spec target_levels_length() :: pos_integer()
  def target_levels_length(), do: @immediate.subunits * @medium.subunits * @long.subunits
end
