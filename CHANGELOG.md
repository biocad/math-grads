# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.1.6.8] - 2023-02-03
### Fixed
- Fixes for GHC 9.2.5.

## [0.1.6.7] - 2020-03-31
### Fixed
- Compilation with `--pedantic`.

## [0.1.6.6] - 2020-01-15
### Fixed
- Calculate full PID and PID' matrices instead of half matrices.

## [0.1.6.5] - 2020-01-15
### Fixed
- Find SSSR for each fused cycle system separately

## [0.1.6.4] - 2020-01-10
### Fixed
- SSSR for multi fused cycles

## [0.1.6.3] - 2019-12-04
### Added
- `Semigroup`/`Monoid` instances for `GenericGraph`

## [0.1.6.2] - 2019-11-13
### Fixed
- Origin indexation for `SSSR`

## [0.1.6.1] - 2019-11-11
### Added
- `subgraphWithReindex` function

## [0.1.5.2] - 2019-08-09
### Added
- Export `findSimpleCycles` function

## [0.1.4.4] - 2018-12-17
### Changed
- isConnected, getCompsIndices

## [0.1.4.3] - 2018-11-20
### Changed
- duplicate vertices in BFS fix

## [0.1.4.2] - 2018-11-14
### Changed
- isIso now checks for equal number of bonds

## [0.1.4.1] - 2018-10-31
### Added
- To/From JSON instances

## [0.1.4.0] - 2018-09-27

### Changed
- EComparator type. From `e1 -> e2 -> Bool` to `GraphEdge e1 -> GraphEdge e2 -> Bool`
