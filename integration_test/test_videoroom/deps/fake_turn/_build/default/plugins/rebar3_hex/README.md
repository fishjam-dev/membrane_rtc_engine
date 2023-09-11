# rebar3_hex

[![Build Status](https://github.com/erlef/rebar3_hex/actions/workflows/ci.yml/badge.svg)](https://github.com/erlef/rebar3_hex/actions/workflows/ci.yml) 
[![Hex pm](https://img.shields.io/hexpm/v/rebar3_hex.svg)](https://hex.pm/packages/rebar3_hex)
[![Docs](https://img.shields.io/badge/hex-docs-green.svg?style=flat)](https://hexdocs.pm/rebar3_hex)
[![Erlang Versions](https://img.shields.io/badge/Supported%20Erlang%2FOTP-22.0%20to%2024.0-blue)](http://www.erlang.org)

rebar3_hex is a rebar3 plugin which bundles providers for interacting with the Erlang ecosystem package manager [hex.pm](https://hex.pm/).

## Setup

Add to your global rebar3 config in `~/.config/rebar3/rebar.config`:

```erlang
{plugins, [rebar3_hex]}.
```

Alternatively, you can add to your project's rebar3 config in project plugins : 

```erlang
{project_plugins, [rebar3_hex]}.
```

**NOTE** : Be sure **not** to add `rebar3_hex` to the plugins section within a projects rebar3 config as this will 
become a dependency for others downloading your package from hex.pm

### Documentation

rebar3_hex by default expects you to configure a documentation provider. We recommend using
[rebar3_ex_doc](https://hexdocs.pm/rebar3_ex_doc/) for publishing documentation along with your package for a 
consistent format and style on [hex.pm](https://hex.pm/). 

Example : 

```erlang

{ex_doc, [
    {extras, ["README.md", "LICENSE"]},
    {main, "README.md"},
    {source_url, "https://github.com/namespace/your_app"}
]}.

{hex, [{doc, ex_doc}]}.
```

Alternatively, or on Erlang versions older than OTP-24, you can use the edoc provider that ships with rebar3 : 

```erlang
{hex, [{doc, edoc}]}.
```

## Basic usage 

You should read the [docs](https://hexdocs.pm/rebar3_hex/) for a complete over for each provider, but below is a
brief overview of basic usage. 

### Authenticating User

If you already have a user for [hex.pm](https://hex.pm) run:


```shell
$ rebar3 hex user auth
```

Note that this will ask for your hex.pm username and password, as well as a password for encrypting your api token that 
has write permissions to the repository. When publishing a package you will have to give this password to decrypt the 
token in order to publish.

See the [docs](https://hexdocs.pm/rebar3_hex/rebar3_hex_user.html) for more information.

### Building

The `build` provider is very useful for seeing exactly what would be published using either the `publish` or `cut` task
without any chance of actually publishing the package or docs. Tarballs for the package and docs are written to
`_build/<profile>/lib/your_app/hex/` by default. 


```
$ rebar3 hex build
```

See the [docs](https://hexdocs.pm/rebar3_hex/rebar3_hex_build.html) for more information.

### Publishing 

Two providers are available for publishing packages, `publish` and `cut` 

By default `publish` builds and pushes your package and docs to a repository. See the 
[docs](https://hexdocs.pm/rebar3_hex/rebar3_hex_publish.html) for more information. 

``` shell
$ rebar3 hex publish
```

`cut` is available to provide some additional functionality around versioning and git tags. See the 
[docs](https://hexdocs.pm/hex/rebar3_hex_cut.html) for more information.

``` shell
$ rebar3 hex cut
```

In either case, both providers will display the details of what is being published 
(what files, the version, dependencies) and ask if it should continue, so be sure to read the 
output carefully and make sure it is publishing what you expected.

### Managing package owners 

Owners can be added, removed, and listed for packages you are an owner of with the `hex owner` command. Packages
can also be transfered to other registered users on hexpm as well. 

``` shell
$ rebar3 hex owner [add | remove | list | transfer] <package> <email>
```

See the [docs](https://hexdocs.pm/rebar3_hex/rebar3_hex_owner.html) for more information.

### Retiring packages 

Packages can be flagged as retired on hexpm via the `retire` provider : 

```
$ rebar3 hex retire PACKAGE VERSION REASON --message
```

They can also be unretired in case a mistake was made : 

```
$ rebar3 hex retire PACKAGE VERSION --unretire 
```

See the [docs](https://hexdocs.pm/rebar3_hex/rebar3_hex_retire.html) for more information.

### Organizations

rebar3_hex supports working with organizations via the `organization` provider. 

Simply add your organization to either your global rebar.config (i.e., `~/.config/rebar3/rebar.config` ) or a local
project rebar.config. 

```erlang
{hex, [{repos, [#{name => <<"hexpm:your_org">>}]}]}.
```

You can then authenticate with with the organization repository. Be sure you have already authenticated with the main
repository first:

```
$ rebar3 hex auth    # make sure you're authenticated to the main repo first 
$ rebar3 hex organization auth hexpm:your_org  # authenticate to the org
```

Now you can generate, revoke, and list keys for your organizations. See below for an example of generating a key for use
in CI.

See the [docs](https://hexdocs.pm/rebar3_hex/rebar3_hex_organization.html) for more information.

#### Read-Only Repo Key for CI

If you have a private organization or other private repository it is recommended that you use a repo specific 
auth token for reading from the repository in CI. To generate a token:

```shell
$ rebar3 hex organization auth hexpm:myrepo
Successfully authenticated to hexpm:myrepo
```

Now you can generate a key to use in CI for your organization 

```
$ rebar3 hex organization key hexpm:myrepo generate
abc123
```

Then in CI use whatever method is available for setting an environment variable to the token and add this call at the 
beginning of your CI runs to add the token to your rebar3 hex config. Below we'll use the environment `REPO_KEY` as an
example. 

```shell
$ rebar3 hex organization auth hexpm:myrepo --key $REPO_KEY
```

### Searching hexpm 

A `search` provider is available to search packages across hex.pm as a convenience.

```
$ rebar3 hex search
```

# Further reading 
See [Publishing packages](https://hex.pm/docs/rebar3_publish) on hexpm for more setup and usage instructions. See the 
[docs](https://hexdocs.pm/rebar3_hex) for detailed documentation for all available providers.
