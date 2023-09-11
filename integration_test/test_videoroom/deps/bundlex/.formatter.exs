locals_without_parens = [
  defnif: 1,
  defnifp: 1
]

[
  inputs: [
    "{lib,test,config}/**/*.{ex,exs}",
    ".formatter.exs",
    "mix.exs"
  ],
  locals_without_parens: locals_without_parens,
  export: [
    locals_without_parens: locals_without_parens
  ]
]
