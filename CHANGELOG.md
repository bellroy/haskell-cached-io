# Revision history for cached-io

## 1.2.0.0

Thank you [glasserc](https://github.com/glasserc) for your work on previous versions, and a special thanks to
[Arguggi](https://github.com/Arguggi) for contributing many of the improvements incorporated into this
version.

- [Bellroy](https://github.com/bellroy) is the new maintainer of this package. See https://github.com/glasserc/haskell-cached-io/pull/1.
- New `cachedIO'` and `cachedIOWith'` support generating an action depending on the most recent cached value and its timestamp, if there was one.
- `cachedIO ttl f` can now be run in a different monad to `f`. Similarly for `cachedIO'`, `cachedIOWith`, `cachedIOWith'`.
- Fixes uncaught exceptions leaving the cache in a deadlocked state and other problems.

## 1.1.0.0 and prior

These versions were published by @glasserc.
