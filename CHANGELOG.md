# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html),
specifically the [variant used by Rust](http://doc.crates.io/manifest.html#the-version-field).

## [Unreleased]
### Added
- `TransportProto`: Classification helpers `is_rtp()`, `is_sctp()`, `is_secure()`
  and `is_avpf()`.
- `Rid` (`a=rid`) attribute and `RidDirection` enum (RFC 8851).
- `KeyMgmt` (`a=key-mgmt`) attribute and `KeyMgmtMethod` enum (RFC 4567).
- `RemoteCandidates` (`a=remote-candidates`) attribute and `RemoteCandidate`
  (RFC 5245, RFC 8839).
- `Connection::is_multicast()`, `Connection::try_is_multicast()`,
  `Connection::is_ipv4()` and `Connection::is_ipv6()` classification helpers.
- `Connection::from_ip_addr_ttl()` constructor with an IPv4 multicast
  time-to-live in `address/ttl` notation (RFC 8866, section 5.7).
- `Connection::try_address()` with `ConnectionAddress` and
  `ConnectionAddressError`, parsing all components of a connection address
  (RFC 8866, section 5.7) at once.
- `Rtcp::try_address()`, `Rtcp::is_multicast()`, `Rtcp::try_is_multicast()`,
  `Rtcp::is_ipv4()` and `Rtcp::is_ipv6()`, mirroring the `Connection` helpers
  for the `address/ttl[/count]` notation of the `a=rtcp` address (RFC 3605,
  RFC 8866 section 5.7).
- `SourceFilter` (`a=source-filter`) attribute with `SourceFilterMode`,
  `SourceFilterAddrType`, `SourceFilterDestination` and `SourceFilterAddress`
  (RFC 4570).
- `Session`: `Display` implementation that formats the session description,
  same as `Session::write()`.
- `Mid` (`a=mid`) media stream identification tag attribute
  (RFC 5888, section 4).
- `RtcpMux` (`a=rtcp-mux`) RTP/RTCP multiplexing property attribute
  (RFC 5761, section 5).
- `RtcpMuxOnly` (`a=rtcp-mux-only`) RTP/RTCP multiplexing-only property
  attribute (RFC 8858, section 3).
- `RtcpRsize` (`a=rtcp-rsize`) reduced-size RTCP property attribute
  (RFC 5506, section 5).
- `IceLite` (`a=ice-lite`) ICE-lite implementation property attribute
  (RFC 5245, section 15.3).
- `IceUfrag` (`a=ice-ufrag`) ICE username fragment attribute
  (RFC 5245, section 15.4).
- `IcePwd` (`a=ice-pwd`) ICE password attribute (RFC 5245, section 15.4).
- `IceOptions` (`a=ice-options`) ICE extension option tags attribute
  (RFC 5245, section 15.5).
- `EndOfCandidates` (`a=end-of-candidates`) end of ICE candidates
  property attribute (RFC 8840, section 8.1).
- `Msid` (`a=msid`) MediaStream identification attribute
  (RFC 8830, section 2).
- `SctpPort` (`a=sctp-port`) SCTP port attribute (RFC 8841, section 5.2).
- `MaxMessageSize` (`a=max-message-size`) maximum SCTP user message
  size attribute (RFC 8841, section 6.2).
- `SctpMap` (`a=sctpmap`) legacy SCTP port map attribute
  (draft-ietf-mmusic-sctp-sdp, superseded by RFC 8841).

### Changed
- `Session::attributes_typed()`, `Media::attributes_typed()` and
  `Attribute::get_typed()` pass an empty string to attributes without a value
  instead of failing with `AttributeError::Other`, so property attributes can
  be looked up as typed attributes.
- The blanket `From<T: TypedAttribute> for Attribute` now stores `value: None`
  for attributes that format as the empty string (property attributes).
- `Connection::try_parse_connection_ip_address()` now ignores an optional
  time-to-live suffix in `address/ttl` notation (RFC 8866, section 5.7)
  instead of failing to parse the address.
- `Rtcp::try_parse_connection_ip_address()` now ignores an optional
  time-to-live suffix in `address/ttl` notation (RFC 8866, section 5.7)
  instead of failing to parse the address.

### Deprecated
- `Connection::try_parse_connection_ip_address()` and
  `Rtcp::try_parse_connection_ip_address()`, superseded by
  `Connection::try_address()` and `Rtcp::try_address()` which parse all
  components at once and return typed errors.

## [0.3.0]
### Fixed
- Parser: reject repeat (`r=`) lines without a preceding time (`t=`) line
  (RFC 8866, section 4) instead of silently dropping them.
  This is a breaking change: such session descriptions now fail to parse
  with `ParserError::RepeatWithoutTime`.
- Don't silently skip malformed lines in the parser, propagate the error.
- `mediaclk` attribute: don't panic when an `id=` value has no clock source.
- `mediaclk` attribute: reject garbage appended to the `direct` clock token
  instead of silently dropping it.
- `rtcp` attribute: accept the optional address part (RFC 3605).
  `Rtcp`'s `nettype`, `addrtype` and `connection_address` are now optional,
  and `try_parse_connection_ip_address()` returns an `Option`.
  Runs of spaces are treated as a single separator, like in `o=` and `c=`
  lines, and empty fields are no longer accepted.
- Writer: strip NUL, CR and LF characters from string fields (RFC 8866,
  section 5). Previously they could be used to inject additional lines into
  the generated session description.
  An empty `username` is now written as `-`, like a missing one, as an
  empty field would not be valid SDP.
- `crypto` attribute: parse the key lifetime as `u64` instead of `u32` and
  reject values larger than the maximum of 2^48 (RFC 4568, section 8.1).
  This is a breaking change: `SrtpKeyParam::lifetime` and the related
  constructors, setters and builder methods now use `u64`.
- `crypto` attribute: reject key lifetimes with leading zeroes or a sign,
  which RFC 4568 (section 8.1) does not allow.
- Writer: don't overflow when writing a `z=` time zone offset of
  `i64::MIN`, which panicked on negation.
- `mediaclk` attribute: reject a rate with a zero denominator, e.g.
  `direct rate=1000/0`.
- `ts-refclk` attribute: accept bare IPv6 NTP server addresses (e.g.
  `ntp=::1`), which were previously mistaken for a host:port split.
  Bracketed addresses (`ntp=[::1]:123`) now store the bare address in
  `NtpServerAddr::HostPort::hostname` and get re-bracketed on display.
- Parser: treat runs of spaces as a single separator in `o=` and `c=` lines
  and reject extra fields at the end instead of absorbing them into the last
  field. Previously extra spaces could shift or corrupt the parsed fields.
  This is a breaking change: session descriptions with extra fields in
  `o=` or `c=` lines now fail to parse with `ParserError::FieldTrailingData`.
- `has_attribute()` and `get_attribute_values()` now match the attribute
  name case-insensitively on both `Session` and `Media`, like
  `attributes_typed()` already did. The stored attribute names keep their
  original case.
- Parser: `nettype` and `addrtype` in `o=` and `c=` lines must be tokens
  (RFC 8866, section 9). Previously arbitrary bytes were accepted, which
  corrupted the line structure when written again.
  This is a breaking change: such session descriptions now fail to parse
  with `ParserError::InvalidFieldFormat`.
- Parser: sessions without an `o=` line now get the placeholder origin
  `o=- 0 0 IN IP4 0.0.0.0` instead of one with empty fields, so that such
  sessions can be written and parsed again.
- Parser: reject NUL and CR bytes inside lines (RFC 8866, section 9).
  Previously they were accepted and broke a round trip pass: now the
  writer strips them, which can leave mandatory fields empty.
  This is a breaking change: such session descriptions now fail to parse
  with `ParserError::InvalidLineFormat`.
- `rtcp` attribute: validate `nettype` and `addrtype` in the optional
  address part as tokens (RFC 3605, RFC 8866 section 9), like in `o=`
  and `c=` lines.
  This is a breaking change: attribute values with other characters in
  these fields now fail to parse with `AttributeError::InvalidParamValue`.

## [0.2.0]
### Improved
- Ignore trailing spaces in some fields.

### Added
- Define specific types for commonly used fields & attributes.
  E.g.: `RtpMap`, `MediaType`, `Direction`, `TransportProto`, `Ssrc`, ...
- Allow retrieving parsed typed attributes instead of name / value pairs.
- Implement constructors.
- Implement setters for optional fields.
- Define builders for types which allow optional fields.
- Add clock-signalling related types.
- Execute a fuzzing step on CI.

### Changed
- Update MSRV to 1.71.
- Update dependencies.

## [0.1.8] - 2025-03-10
### Fixed
- Ignore extra trailing spaces in time/repeat/timezone fields.

## [0.1.7] - 2024-09-06
### Added
- Add serde-derived `Serialize` / `Deserialize` implementations behind a
  feature flag.

## [0.1.6] - 2024-02-14
### Changed
- Ignore unexpected SDP lines.
- Declare 1.65 as MSRV.
- Don't enable default features of `bstr` to reduce dependencies considerably.

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

[Unreleased]: https://github.com/sdroege/sdp-types/compare/0.3.0...HEAD
[0.3.0]: https://github.com/sdroege/sdp-types/compare/0.2.0...0.3.0
[0.2.0]: https://github.com/sdroege/sdp-types/compare/0.1.8...0.2.0
[0.1.8]: https://github.com/sdroege/sdp-types/compare/0.1.7...0.1.8
[0.1.7]: https://github.com/sdroege/sdp-types/compare/0.1.6...0.1.7
[0.1.6]: https://github.com/sdroege/sdp-types/compare/0.1.5...0.1.6
[0.1.5]: https://github.com/sdroege/sdp-types/compare/0.1.4...0.1.5
[0.1.4]: https://github.com/sdroege/sdp-types/compare/0.1.3...0.1.4
[0.1.3]: https://github.com/sdroege/sdp-types/compare/0.1.2...0.1.3
[0.1.2]: https://github.com/sdroege/sdp-types/compare/0.1.1...0.1.2
[0.1.1]: https://github.com/sdroege/sdp-types/compare/0.1.0...0.1.1
