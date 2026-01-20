# AGENTS

## Hiding (fail) policy

When importing modules that used to re-export `fail` (via `Control.Monad` or `Prelude`),
use CPP guards to keep `hiding (fail)` only for versions that still export it.

- `base < 4.13`: `Prelude`/`Control.Monad` export `fail`; use `hiding (fail)`.
- `mtl < 2.3.1`: `Control.Monad.Cont` and `Control.Monad.State.Strict` re-export
  `Control.Monad` (and thus `fail`); use `hiding (fail)`.
- `base < 4.13`: import `Control.Monad.Fail (MonadFail)` only when needed; for
  `base >= 4.13` it is in `Prelude`, so the import becomes redundant.

Rationale: newer GHC/base and mtl drop these re-exports, and unguarded `hiding (fail)`
triggers warnings. The guards keep older toolchains clean without penalizing modern ones.
