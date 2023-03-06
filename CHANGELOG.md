# Revision history for cached-io

## 1.2.0.0

- New `cachedIO'` and `cachedIOWith'` support generating an action depending on the most recent cached value and its timestamp, if there was one.
- `cachedIO ttl f` can now be run in a different monad to `f`. Similarly for `cachedIO'`, `cachedIOWith`, `cachedIOWith'`.
- Fixes uncaught exceptions leaving the cache in a deadlocked state and other problems.
