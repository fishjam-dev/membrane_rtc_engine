[
  inputs: [
    "{lib,test,config}/**/*.{ex,exs}",
    "c_src/**/*.spec.exs",
    "./*.exs"
  ],
  import_deps: [:bundlex, :unifex]
]
