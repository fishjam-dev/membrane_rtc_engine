{application,membrane_tee_plugin,
             [{applications,[kernel,stdlib,elixir,membrane_core,bunch]},
              {description,"Plugin for splitting data from a single input to multiple outputs"},
              {modules,['Elixir.Membrane.Tee.Master',
                        'Elixir.Membrane.Tee.Parallel',
                        'Elixir.Membrane.Tee.PushOutput']},
              {registered,[]},
              {vsn,"0.10.1"}]}.
