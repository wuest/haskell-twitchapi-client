[![Version](https://img.shields.io/hackage/v/twitchapi-client)][badges:0-hackage]
[![Build](https://img.shields.io/github/actions/workflow/status/wuest/haskell-twitchapi/ci.yaml?branch=main)][badges:1-CI]
[![License](https://img.shields.io/github/license/wuest/haskell-twitchapi-client)][badges:2-license]

# Twitch API Client

This library provides a client for Twitch's APIs, providing a client for
[EventSub][main:0-eventsub] via WebSockets.  The client is provided separate
from [twitchapi][main:1-api] to avoid incurring unneeded library dependencies in
cases where a project has a separate websocket client.

## License

This library is released under the [BSD 3-Clause License][license-1:BSD]

## Contributions

Whether the contribution concerns code, documentation, bug reports, or something
else entirely, contributions are welcome and appreciated!

If the contribution is relating to a security concern, please see
[SECURITY.md][SECURITY].

For all other contributions, please see
[CONTRIBUTING.md][CONTRIBUTING].  In short:

  * Fork the project.
  * Add tests for any new functionality/to verify that bugs have been fixed.
    - In particular, generating data for parsers is valuable
  * Send a pull request on GitHub.

## Code of Conduct

The this project is governed by a [Code of Conduct][code-of-conduct].

[badges:0-hackage]: https://hackage.haskell.org/package/twitchapi-client
[badges:1-CI]: https://github.com/wuest/haskell-twitchapi-client/actions/workflows/ci.yaml
[badges:2-license]: https://github.com/wuest/haskell-twitchapi-client/blob/main/LICENSE
[main:0-eventsub]: https://dev.twitch.tv/docs/eventsub/
[main:1-api]: https://hackage.haskell.org/package/twitchapi
[license-1:BSD]: https://github.com/wuest/haskell-twitchapi-client/blob/main/LICENSE
[SECURITY]: https://github.com/wuest/haskell-twitchapi-client/blob/main/SECURITY.md
[CONTRIBUTING]: https://github.com/wuest/haskell-twitchapi-client/blob/main/CONTRIBUTING.md
[code-of-conduct]: https://github.com/wuest/haskell-twitchapi-client/blob/main/CODE_OF_CONDUCT.md
