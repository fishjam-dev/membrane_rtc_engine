defimpl Inspect, for: Qex do
  import Inspect.Algebra

  def inspect(%Qex{} = q, opts) do
    concat ["#Qex<", to_doc(Enum.to_list(q), opts), ">"]
  end
end
