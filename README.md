# sdp-types [![crates.io](https://img.shields.io/crates/v/sdp-types.svg)](https://crates.io/crates/sdp-types) [![Build Status](https://travis-ci.org/sdroege/sdp-types.svg?branch=master)](https://travis-ci.org/sdroege/sdp-types) [![docs.rs](https://docs.rs/sdp-types/badge.svg)](https://docs.rs/sdp-types)

Crate for handling SDP ([RFC 4566](https://tools.ietf.org/html/rfc4566))
session descriptions, including a parser and serializer.

See the [documentation](https://docs.rs/sdp-types) for details.

## Limitations

 * SDP session descriptions are by default in UTF-8 but an optional `charset`
   attribute can change this for various SDP fields, including various other
   attributes. This is currently not supported, only UTF-8 is supported.

 * Network addresses, Phone numbers, E-Mail addresses and various other fields
   are currently parsed as a plain string and not according to the SDP
   grammar.

## LICENSE

sdp-types is licensed under the MIT license ([LICENSE](LICENSE) or
http://opensource.org/licenses/MIT).

## Contribution

Any kinds of contributions are welcome as a pull request.

Unless you explicitly state otherwise, any contribution intentionally
submitted for inclusion in sdp-types by you shall be licensed under the MIT
license as above, without any additional terms or conditions.
