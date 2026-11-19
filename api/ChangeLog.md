# Changelog for hs-opentelemetry-api

## Unreleased changes

- Add `Ord` and `Hashable` instances for `Attributes`, so `InstrumentationLibrary` builds on GHC 9.2
- Depend on `safe-exceptions` and `transformers` directly. `SemanticsConfig` and the logs API import them
- Restore `SpanProcessor` and `SpanExporter`, and keep the old `Processor` and `Exporter` names as patterns. `ShutdownResult` and `ExportResult` live only in `OpenTelemetry.Internal.Common.Types`
- List `Paths_hs_opentelemetry_api` in `other-modules` so the shared library exports it

### Breaking changes

- Use `HashMap Text Attribute` instead of `[(Text, Attribute)]` as attributes

## 0.0.3.6

- GHC 9.4 support
- Add Show instances to several api types

## 0.0.3.1

- `adjustContext` uses an empty context if one hasn't been created on the current thread yet instead of acting as a no-op.

## 0.0.2.1

- Doc enhancements

## 0.0.2.0

- Separate `Link` and `NewLink` into two different datatypes to improve Link creation interface.
- Add some version bounds
- Catch & print all synchronous exceptions when calling span processor
  start and end hooks

## 0.0.1.0

- Initial release
