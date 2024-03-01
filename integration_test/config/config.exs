import Config

if config_env() == :test, do: import_config("#{config_env()}.exs")
