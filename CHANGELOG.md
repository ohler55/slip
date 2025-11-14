# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.2.0] - unreleased
### Added
- Added the `discover-json` function to match the latest OjG discover package.

## [1.2.0] - 2025-09-06
### Added
- Add CLOS implementation.
- Added generics.

## [1.1.0] - 2025-06-23
### Added
- Added better pretty printing.
- Added stand alone application builder with `make-app` function.
- Added `snapshot` function that save a snapshot of the current state with some limitations.

## [1.0.2] - 2025-04-28
### Added
- Added plugin test.

## [1.0.1] - 2025-04-27
### Changed
- Updated OjG to v1.26.4

## [1.0.0] - 2025-04-27
### Added
- Added support for bit and byte functions.

## [0.9.9] - 2025-01-18
### Changed
- Updated OjG version to v1.26.1.
- Added support for OjG jp.Path script procedures.

## [0.9.8] - 2024-12-01
### Added
- The _select_ function now supports more cases and all channel types.

## [0.9.7] - 2024-11-26
### Added
- Added functions:
  - time-after
  - time-ticker
  - time-unix
  - time-elapsed
  - time-add
  - time-components
  - time :describe
  - unix-time
  - select
- Added methods to the time class which now can be a target of the _send_ function.

## [0.9.6] - 2024-11-08
### Added
- Added functions
  - case
  - ecase
  - map
  - merge
  - nreconc
  - progv
  - reduce
  - revappend
  - slot-boundp
  - slot-exists-p
  - slot-makunbound
  - slot-missing
  - slot-value
  - the
  - with-input-from-string
  - with-open-stream
  - with-output-to-string

## [0.9.5] - 2024-10-11
### Added
- Sockets added.

## [0.9.0] - 2024-04-26
### Changed
- Still under development.

## [0.8.0] - 2023-12-05
### Changed
- Package funcs, vars, and lambdas are now private with mutex protected access.
### Added
- Added a xml package.

## [0.7.1] - 2023-11-10
### Fixed
- Numerous fixes to the test package.

## [0.7.0] - 2023-11-05
### Fixed
- Multiple bug fixes and enhancements.

## [0.5.0] - 2023-04-28
### Added
- Initial release.
