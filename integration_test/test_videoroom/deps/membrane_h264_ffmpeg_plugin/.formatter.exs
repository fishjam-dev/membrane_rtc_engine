[
  inputs: [
    "{lib,test,spec,config}/**/*.{ex,exs}",
    "c_src/**/*.spec.exs",
    "*.exs"
  ],
  import_deps: [:membrane_core, :bundlex, :unifex]
]
