# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html),
specifically the [variant used by Rust](http://doc.crates.io/manifest.html#the-version-field).

## unreleased
### Changed
- Ignore unexpected SDP lines.

## [0.1.5] - 2022-10-28
### Changed
- Fix some clippy warnings.
- Update to `bstr` 1.0.

## [0.1.4] - 2022-03-08
### Changed
- Support parsing SDPs without `o=` lines by creating a dummy `Origin` value.

## [0.1.3] - 2021-09-09
### Fixed
- Use 1-based line numbers in errors.

### Changed
- Be more permissive about the ordering of fields in both session and media
  descriptions.
- Allow `t=` to be missing from the session description.

## [0.1.2] - 2021-06-05
### Fixed
- Ignore blank lines inside the SDP.
- Don't panic on overflowing time zone offset.
- Don't panic on "v\n".
- Various clippy warnings.

### Added
- `cargo-fuzz` integration.

## [0.1.1] - 2020-02-05
### Fixed
- Fix parsing of SDP attributes/keys that contain another ':' inside their
  value.

## 0.1.0 - 2019-12-23
- Initial release of the `sdp-types` crate.

[Unreleased]: https://github.com/sdroege/sdp-types/compare/0.1.4...HEAD
[0.1.5]: https://github.com/sdroege/sdp-types/compare/0.1.4...0.1.5
[0.1.4]: https://github.com/sdroege/sdp-types/compare/0.1.3...0.1.4
[0.1.3]: https://github.com/sdroege/sdp-types/compare/0.1.2...0.1.3
[0.1.2]: https://github.com/sdroege/sdp-types/compare/0.1.1...0.1.2
[0.1.1]: https://github.com/sdroege/sdp-types/compare/0.1.0...0.1.1
