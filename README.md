# uaparserl

[![See build results on Travis CI][status]][travis]

A simple user-agent parsing library based on [BrowserScope's UA database][1].

## Installation

Add `uaparserl` to your dependencies.

### Enable HiPE

If you want to use a HiPE compiled version, add following code to your
`rebar.config`.

```
{overrides, [
    {override, uaparserl, [{erl_opts, [{d, 'ENABLE_HIPE'}, inline]}]}
]}.
```

## Usage

### Important

Please note that this library returns captures "as is"; it is the
responsibility of the developer to ensure that results are properly
escaped when used in potentially harmful scenarios.

Also be aware that this library expects and returns strings. You may
need to convert the return value to for correct use within your system.

### Example

```erlang
%% Obtain the resource files from BrowserScope's github repository
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

```
Copyright 2020 Channel.me B.V.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
```

[1]: https://github.com/ua-parser/uap-core
[travis]: https://travis-ci.org/channelme/uaparserl
[status]: https://api.travis-ci.org/channelme/uaparserl.svg?branch=master
