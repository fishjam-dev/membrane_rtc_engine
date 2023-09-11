defmodule Membrane.RTP.Vad.IsSpeakingEstimator do
  @moduledoc """
  Module for estimating if the user is speaking inspired by
  [*Dominant Speaker Identification for Multipoint Videoconferencing*](https://israelcohen.com/wp-content/uploads/2018/05/IEEEI2012_Volfin.pdf)
  by Ilana Volfin and Israel Cohen

  ------

  The `estimate_is_speaking/2` function takes a list of audio levels in range [0, 127]
  and based on a threshold given as a second input
  computes if the person is speaking.

  The input levels are interpreted on 3 tiers. Each tier consists of intervals specified below:

  | Name         | Interpretation       | Number of RTP packets (default) | length    |
  |--------------|----------------------|---------------------------------|-----------|
  | `immediate`  | smallest sound chunk | 1                               | ~20 [ms]  |
  | `medium`     | one word             | 1 \* 10 = 10                    | ~200 [ms] |
  | `long`       | half/one sentence    | 1 \\* 10 \* 7 = 70              | ~1.4 [s]  |

  Each tier interval is computed based on the smaller tier intervals (subunits).
  Immediates are computed based on levels, mediums on top of immediates and longs on top of mediums.
  The number of subunits in one interval is given as a module parameter.

  Each interval is a number of active subunits (that is: lower tier intervals) above a threshold of a given tier.

  > **Example**
  >
  > If `level_threshold` is `90`, levels are `[80, 90, 100, 90]` and there are 2 levels in a immediate
  > then `immediates` would be equal to `[1, 2]` since subunit of `[80, 90]` has 1 item above or equal to
  > the threshold and subunit `[100, 90]` has 2 such items.
  >
  > Same goes for mediums. If `medium subunit threshold` is 2 and number of subunits is 2
  > then `mediums` are equal to `[1]` since the only subunit `[1, 2]` had only one element above or equal to the threshold.

  The number of levels the function requires equals the product of the subunits number required for each tier.
  This way we compute only one long interval because only one is needed.
  If the number of levels is smaller than the required minimum, the algorithm returns silence.

  The most recent interval in each tier serves as a basis for computing an activity score.
  The activity score is a logarithm of a quotient of:
    - the probability of `k` active items in `n` total items under an assumption that a person is speaking (modeled as a *binomial distribution*)
    - probability same as above but under an assumption that a person is not speaking (modeled as an *exponential distribution*)

  The activity score for each tier is then thresholded again.
  A threshold for every tier is given as a module parameter.
  If all activity scores are over the threshold, the algorithm returns that the input contains speech.
  Otherwise the algorithm returns that input contains silence.

  -----------------
  Overall the parameters for each tier are:

  | Parameter | Description |
  |-|-|
  | `subunits` | number of smaller units in one bigger unit |
  | `score_threshold`  | number equal or above which the activity score of the given tier must be to be counted as indicating speech |
  | `subunit_threshold` | number equal or above which the number of active subunits must be for the given tier to be marked as active (for `immediates` it is equal to the threshold given as a `estimate_is_speaking/2` argument) |
  | `lambda` | parameter for the exponential distribution (element of the activity score computations) |

  You can set them, by adding the following code to your `config.exs`

  ```
  config :membrane_rtp_plugin,
    vad_estimation_parameters: %{
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
  ```

  A thorough explanation with images can be found in the Jellyfish book in the [Voice Activity Detection](https://jellyfish-dev.github.io/book/webrtc/voice_activity_detection/vad.html) chapter.
  """
  alias Membrane.RTP.Vad.VadParams

  @params VadParams.vad_params()
  @immediate VadParams.immediate()
  @medium VadParams.medium()
  @long VadParams.long()

  @minimum_levels_length VadParams.target_levels_length()

  # If we would have accepted 0 as minimum, the log functions in activity score function would break.
  @min_activity_score 1.0e-8

  @doc """
  Estimates if the user is speaking based on the audio levels and a threshold.
  """
  @spec estimate_is_speaking([integer], integer) :: :speech | :silence
  def estimate_is_speaking(levels, _level_threshold) when length(levels) < @minimum_levels_length,
    do: :silence

  def estimate_is_speaking(levels, level_threshold) do
    immediates = compute_immediates(levels, level_threshold)
    mediums = compute_interval(immediates, :medium)
    longs = compute_interval(mediums, :long)

    immediate_score = immediate_activity_score(immediates)
    medium_score = medium_activity_score(mediums)
    long_score = long_activity_score(longs)

    above_threshold? = scores_above_threshold?(immediate_score, medium_score, long_score)

    if above_threshold?,
      do: :speech,
      else: :silence
  end

  defp scores_above_threshold?(immediate_score, medium_score, long_score) do
    immediate_score > @immediate[:score_threshold] and
      medium_score > @medium[:score_threshold] and
      long_score > @long[:score_threshold]
  end

  defp compute_immediates(levels, level_threshold),
    do: compute_interval(levels, @immediate[:subunits], level_threshold)

  defp compute_interval(littles, tier) when tier in [:medium, :long],
    do: compute_interval(littles, @params[tier][:subunits], @params[tier][:subunit_threshold])

  defp compute_interval(littles, sub_list_length, threshold) do
    littles
    |> Enum.chunk_every(sub_list_length)
    |> Enum.map(&count_active(&1, threshold))
  end

  defp count_active(data, threshold) do
    Enum.count(data, &(&1 >= threshold))
  end

  defp immediate_activity_score([newest_interval | _rest]),
    do: compute_activity_score(newest_interval, :immediate)

  defp medium_activity_score([newest_interval | _rest]),
    do: compute_activity_score(newest_interval, :medium)

  defp long_activity_score([newest_interval | _rest]),
    do: compute_activity_score(newest_interval, :long)

  defp compute_activity_score(newest_interval, tier) when tier in [:immediate, :medium, :long],
    do: compute_activity_score(newest_interval, @params[tier][:subunits], @params[tier][:lambda])

  defp compute_activity_score(_newest_interval, tier),
    do: raise("Wrong arguments. #{tier} is not a valid interval tier atom")

  defp compute_activity_score(vl, n_r, lambda) do
    p = 0.5

    score =
      :math.log(binomial_coefficient(n_r, vl)) +
        vl * :math.log(p) +
        (n_r - vl) * :math.log(1 - p) -
        :math.log(lambda) + lambda * vl

    max(score, @min_activity_score)
  end

  defp binomial_coefficient(n, k) when k < 0 or k > n, do: 0
  defp binomial_coefficient(n, k) when k == 0 or n == k, do: 1

  defp binomial_coefficient(n, k) do
    k = min(k, n - k)

    Enum.reduce(1..k, 1, fn i, acc -> div(acc * (n - i + 1), i) end)
  end
end
