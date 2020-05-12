# uaparserl

[![See build results on Travis CI][status]][travis]

A simple user-agent parsing library based on [BrowserScope's UA database][1].

## Installation

### Enable HiPE

If you want to use a HiPE compiled version, please add following code
to your `rebar.config`.

```
{overrides, [
    {override, uaparserl, [{erl_opts, [{d, 'ENABLE_HIPE'}, inline]}]}
]}.
```

## Usage

```erlang
%% Obtain the resouce files from BrowserScope
> uaparserl:update().
ok

%% Parse
> uaparserl:parse("Luminary/70 CFNetwork/978.0.7 Darwin/18.5.0").
[{useragent,[{family,"Luminary"},
             {major,"70"},
             {minor,nil},
             {patch,nil}]},
 {os,[{family,"iOS"},
      {major,"12"},
      {minor,nil},
      {patch,nil},
      {patch_minor,nil}]},
 {device,[{family,"iOS-Device"},
          {brand,"Apple"},
          {model,"iOS-Device"}]}]
```

## Licence


[1]: https://github.com/ua-parser/uap-core
[travis]: https://travis-ci.org/channelme/uaparserl
[status]: https://api.travis-ci.org/channelme/uaparserl.svg?branch=master
