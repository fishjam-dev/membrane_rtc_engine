# SecureRandom.ex [![[travis]](https://travis-ci.org/patricksrobertson/secure_random.ex.png)](https://travis-ci.org/patricksrobertson/secure_random.ex)


SecureRandom is an elixir module loosely based on Ruby's SecureRandom.
I needed urlsafe, random, base64 strings and UUID generation, so I ported over
what I needed :).

This gets its random from Erlang's `strong_rand_bytes/1` and is strongly based
from this [gist](https://gist.github.com/Myuzu/7367461). I had to remove some
things that didn't make it to Elixir 1.0, and cut the stuff that I do not
currently have use for.

Will accept PR's to flesh out further.



# INSTALL

Add this to your mix.exs:
```elixir
defp deps do
  [{:secure_random, "~> 0.5"}]
end
```

Fetch this motherlover:

```sh
mix deps.get
```

# USAGE

UUID:
```elixir
SecureRandom.uuid # => "e8bc6fde-3c11-cc2e-903b-745221154d8a"
```

base64 string:
```elixir
SecureRandom.base64(8) # => "VsifwaD2HCk="
```

urlsafe_base64 string:
```elixir
SecureRandom.urlsafe_base64 #=> "WAut546EWdXM3O_9sJGvmQ"
```

# AUTHOR
Brought to you by [Adequate Kitchen](http://adequate.io) which is a vague way
saying Patrick Robertson.

# LICENSE

Apache 2.0

