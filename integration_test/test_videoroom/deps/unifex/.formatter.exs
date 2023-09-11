common = [
  locals_without_parens: [
    callback: 1,
    callback: 2,
    module: 1,
    spec: 1,
    dirty: 2,
    sends: 1,
    interface: 1,
    state_type: 1
  ]
]

[
  inputs: [
    "{lib,test,config}/**/*.{ex,exs}",
    "*.exs"
  ],
  import_deps: [:bundlex],
  export: common
] ++ common
