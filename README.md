# MatterMonad

## What is a mattermonad?
The name given to iasoon's adventures in writing silly mattermost bots in haskell!

Since a bot is essentially a program that responds to events happening in its environment, it makes conceptual sense to me to use [functional reactive programming](https://wiki.haskell.org/Functional_Reactive_Programming) for this. I have dabbled in FRP in the past and I think I caught the bug, but didn't really achieve anything serious. This time I decided to check out [Reflex](https://reflex-frp.org/).

I know [Matterhorns MatterMost API](https://github.com/matterhorn-chat/mattermost-api) exists, and there is nothing wrong with that. but it's not invented here so I'm rolling my own. On a more serious note, I'm trying to generate API bindings from the [MatterMost OpenAPI spec](https://github.com/mattermost/mattermost-api-reference) using Template Haskell (which is currently an open  issue on the Matterhorn bindings), and I would also like the bindings to be implemented as 'effects', which can then run in either a test simulation, a native environment, or the web browser using GHCJS.

## Status
Currently I have a toy FRP network using hand-rolled API bindings that announces its existance to people.
I'm working on the code generation. Currently I am barely parsing the api spec, but I'm working on it! Exciting stuff!

## Disclaimer
Don't read my code without a hazmat suit, I don't know how to program.