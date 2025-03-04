# embryo

An Erlang library for managing Emergence configuration and embryo objects.

## Features

- Create and manipulate embryo objects
- Read Emergence configuration files
- Retrieve discovery service URLs
- Merge lists of embryos

## Installation

Add to your `rebar.config`:

```erlang
{deps, [
    {embryo, "0.1.1"}
]}.
```

## Usage

```erlang
% Create a new embryo
Properties = #{<<"url">> => <<"http://example.com">>},
Embryo = embryo:new(Properties),

% Get the discovery service URL
DiscoUrl = embryo:get_em_disco_url(),

% Read the Emergence configuration
Config = embryo:read_emergence_conf(),

% Merge lists of embryos
MergedList = embryo:merge_lists_by_url(List1, List2).
```

## Configuration

The library looks for the Emergence configuration file in the following locations:

- Unix: `~/.config/emergence/emergence.conf`
- Windows: `%APPDATA%\emergence\emergence.conf` or `%HOME%\AppData\Roaming\emergence\emergence.conf`

## License

Apache 2.0
