port_compiler
=====

A port compiler for rebar3.

This plugin is intended to replicate the rebar2 support for compiling native
code. It is not a drop-in replacement in terms of command-line interface but the
exact configuration interface in projects' `rebar.config`s have been preserved.

Use In Your Project
---------------------

Add the plugin to your `rebar.config`:

```erlang
{plugins, [pc]}.
{provider_hooks,
 [
  {pre,
   [
    {compile, {pc, compile}},
    {clean, {pc, clean}}
   ]
  }
 ]
}.
```

If you want to use tools like clang-tidy, don't forget to enable the compile commands db like this:

```erlang
{pc_clang_db, true}.
```

From your existing application:


    $ rebar3 pc compile
    ===> Fetching pc
    ===> Compiling pc
    ===> Verifying dependencies...
    Compiling ...

You should now have native code compiled.

Use with Existing Dependency
-----------------------------

If your project depends on a dependency that used the rebar2 port compiler instead of forking and changing the `rebar.config` of that dependency you can use [overrides](http://www.rebar3.org/v3.0/docs/configuration#overrides) to inject the changes from your top level `rebar.config`. Using [jiffy](https://github.com/davisp/jiffy) as an example:


```erlang
{deps, [jiffy]}.

{overrides,
 [{override, jiffy, [
     {plugins, [pc]},
     {artifacts, ["priv/jiffy.so"]},
     {provider_hooks, [
         {post,
             [
             {compile, {pc, compile}},
             {clean, {pc, clean}}
             ]
          }]
      }
  ]}
]}.
```

Example
---

Looking for an example? See my fork of jiffy here and the changes to its
`rebar.config`: https://github.com/blt/jiffy/commit/d4a0103daec5a646e71045bdf40f12a3eb82ace5

- - -

BELOW HERE BE DRAGONS

```
%% Supported configuration variables:
%%
%% * port_specs - Erlang list of tuples of the forms
%%                {ArchRegex, TargetFile, Sources, Options}
%%                {ArchRegex, TargetFile, Sources}
%%                {TargetFile, Sources}
%%
%%                Note that if you want to use any of the rebar3 variables
%%                below you must MUST use a ${}-style to get the expansion
%%                to work. e.g. to expand REBAR_DEPS_DIR, do something like:
%%
%%                {port_specs, [{"priv/nif.so",
%%                               ["c_src/nif.c",
%%                                "${REBAR_DEPS_DIR}/foo/bar.c"]}]}.
%%
%%                This is a _very_ good way to be able to use your code both
%%                as a top level app and a dependency.
%%
%%                CAVEAT! Not using {} is broken for the moment.
%%
%% * port_env - Erlang list of key/value pairs which will control
%%              the environment when running the compiler and linker.
%%              Variables set in the surrounding system shell are taken
%%              into consideration when expanding port_env. Note that
%%              for ERL_LDFLAGS, -lerl_interface is used for only those
%%              Erlang/OTP versions where it exists (those prior to
%%              version 23.0).
%%
%%              By default, the following variables are defined:
%%              CC       - C compiler
%%              CXX      - C++ compiler
%%              CFLAGS   - C compiler
%%              CXXFLAGS - C++ compiler
%%              LDFLAGS  - Link flags
%%              ERL_CFLAGS  - default -I paths for erts and ei
%%              ERL_LDFLAGS - default -L and -lerl_interface -lei
%%              DRV_CFLAGS  - flags that will be used for compiling
%%              DRV_LDFLAGS - flags that will be used for linking
%%              EXE_CFLAGS  - flags that will be used for compiling
%%              EXE_LDFLAGS - flags that will be used for linking
%%              ERL_EI_LIBDIR - ei library directory
%%              DRV_CXX_TEMPLATE      - C++ command template
%%              DRV_CC_TEMPLATE       - C command template
%%              DRV_LINK_TEMPLATE     - C Linker command template
%%              DRV_LINK_CXX_TEMPLATE - C++ Linker command template
%%              EXE_CXX_TEMPLATE      - C++ command template
%%              EXE_CC_TEMPLATE       - C command template
%%              EXE_LINK_TEMPLATE     - C Linker command template
%%              EXE_LINK_CXX_TEMPLATE - C++ Linker command template
%%
%%              Note that if you wish to extend (vs. replace) these variables,
%%              you MUST include a shell-style reference in your definition.
%%              e.g. to extend CFLAGS, do something like:
%%
%%              {port_env, [{"CFLAGS", "$CFLAGS -MyOtherOptions"}]}
%%
%%              It is also possible to specify platform specific options
%%              by specifying a triplet where the first string is a regex
%%              that is checked against Erlang's system architecture string.
%%              e.g. to specify a CFLAG that only applies to x86_64 on linux
%%              do:
%%
%%              {port_env, [{"x86_64.*-linux", "CFLAGS",
%%                           "$CFLAGS -X86Options"}]}
%%
%%              Cross-arch environment variables to configure toolchain:
%%              GET_ARCH to set the tool chain name to use
%%              GET_ARCH_WORDSIZE (optional - to determine word size)"
%%              word size is 32
%%              GET_ARCH_VSN (optional - "
%%              l version of CC/CXX is requested),
```
