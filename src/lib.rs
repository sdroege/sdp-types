// Copyright (C) 2019 Sebastian Dröge <sebastian@centricular.com>
//
// Licensed under the MIT license, see the LICENSE file or <http://opensource.org/licenses/MIT>

//! Crate for handling SDP ([RFC 8866](https://tools.ietf.org/html/rfc8866))
//! session descriptions, including a parser and serializer.
//!
//! ## Serializing an SDP
//!
//! ```rust,ignore
//! // Create SDP session description
//! let sdp = sdp_types::Session {
//!     ...
//! };
//!
//! // And write it to an `Vec<u8>`
//! let mut output = Vec::new();
//! sdp.write(&mut output).unwrap();
//! ```
//!
//! ## Parsing an SDP
//!
//! ```rust,no_run
//! # let data = [0u8];
//! // Parse SDP session description from a byte slice
//! let sdp = sdp_types::Session::parse(&data).unwrap();
//!
//! // Access the 'tool' attribute
//! match sdp.get_first_attribute_value("tool") {
//!     Ok(Some(tool)) => println!("tool: {}", tool),
//!     Ok(None) => println!("tool: empty"),
//!     Err(_) => println!("no tool attribute"),
//! }
//!
//! // Access all the 'rtpmap' attributes as a `RtpMap` type
//! // returns an iterator of type `Iterator<Item = Result<RtpMap, AttributeError>>`
//! let r = sdp.attributes_typed::<sdp_types::RtpMap>();
//! ```
//!
//! ## Limitations
//!
//!  * SDP session descriptions are by default in UTF-8 but an optional `charset`
//!    attribute can change this for various SDP fields, including various other
//!    attributes. This is currently not supported, only UTF-8 is supported.
//!
//!  * Network addresses, Phone numbers, E-Mail addresses and various other fields
//!    are currently parsed as a plain string and not according to the SDP
//!    grammar.

use std::{
    net::{AddrParseError, IpAddr},
    str::FromStr,
};

use bstr::*;
use fallible_iterator::FallibleIterator;

pub mod attributes;
pub mod enums;
mod parser;
mod writer;

pub use attributes::*;
pub use enums::*;
pub use parser::ParserError;

/// Originator of the session.
///
/// See [RFC 8866 Section 5.2](https://tools.ietf.org/html/rfc8866#section-5.2) for more details.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Origin {
    /// User's login on the originating host.
    pub username: Option<String>,
    /// Session ID to make the whole `Origin` unique.
    ///
    /// Must be a numeric string but this is *not* checked.
    pub sess_id: String,
    /// Session version number.
    pub sess_version: u64,
    /// Type of network for this session.
    pub nettype: String,
    /// Type of the `unicast_address`.
    pub addrtype: String,
    /// Address where the session was created.
    pub unicast_address: String,
}

impl Origin {
    /// Tries to parse the `addrtype` `String` of `self` as `AddrType`
    pub fn try_parse_addrtype(&self) -> Result<AddrType, ParseEnumError> {
        AddrType::from_str(self.addrtype.as_str())
    }

    /// Sets the `addrtype` `String` of `self` from the specified `AddrType`
    pub fn set_addrtype(&mut self, addrtype: AddrType) {
        self.addrtype = addrtype.to_string();
    }

    /// Tries to parse the `nettype` `String` of `self` as `NetType`
    pub fn try_parse_nettype(&self) -> Result<NetType, ParseEnumError> {
        NetType::from_str(self.nettype.as_str())
    }

    /// Sets the `nettype` `String` of `self` from the specified `NetType`
    pub fn set_nettype(&mut self, nettype: NetType) {
        self.nettype = nettype.to_string();
    }

    /// Tries to parse the `unicast_address` `String` of `self` as `IpAddr`
    pub fn try_parse_unicast_address(&self) -> Result<IpAddr, AddrParseError> {
        self.unicast_address.parse()
    }

    /// Sets the `unicast_address` `String` of `self` from the specified `IpAddr`
    pub fn set_unicast_address(&mut self, unicast_address: IpAddr) {
        self.unicast_address = unicast_address.to_string();
    }
}

/// Connection data for the session or media.
///
/// See [RFC 8866 Section 5.7](https://tools.ietf.org/html/rfc8866#section-5.7) for more details.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Connection {
    /// Type of network for this connection.
    pub nettype: String,
    /// Type of the `connection_address`.
    pub addrtype: String,
    /// Connection address.
    pub connection_address: String,
}

impl Connection {
    /// Tries to parse the `addrtype` `String` of `self` as `AddrType`
    pub fn try_parse_addrtype(&self) -> Result<AddrType, ParseEnumError> {
        AddrType::from_str(self.addrtype.as_str())
    }

    /// Sets the `addrtype` `String` of `self` from the specified `AddrType`
    pub fn set_addrtype(&mut self, addrtype: AddrType) {
        self.addrtype = addrtype.to_string();
    }

    /// Tries to parse the `nettype` `String` of `self` as `NetType`
    pub fn try_parse_nettype(&self) -> Result<NetType, ParseEnumError> {
        NetType::from_str(self.nettype.as_str())
    }

    /// Sets the `nettype` `String` of `self` from the specified `NetType`
    pub fn set_nettype(&mut self, nettype: NetType) {
        self.nettype = nettype.to_string();
    }

    /// Tries to parse the `connection_address` `String` of `self` as `IpAddr`
    pub fn try_parse_connection_address(&self) -> Result<IpAddr, AddrParseError> {
        self.connection_address.parse()
    }

    /// Sets the `connection_address` `String` of `self` from the specified `IpAddr`
    pub fn set_connection_address(&mut self, connection_address: IpAddr) {
        self.connection_address = connection_address.to_string();
    }
}

/// Bandwidth information for the session or media.
///
/// See [RFC 8866 Section 5.8](https://tools.ietf.org/html/rfc8866#section-5.8) for more details.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Bandwidth {
    /// Bandwidth type, usually "CT" or "AS".
    pub bwtype: String,
    /// Bandwidth.
    pub bandwidth: u64,
}

impl Bandwidth {
    /// Tries to parse the `bwtype` `String` of `self` as `BandwidthType`
    pub fn try_parse_bwtype(&self) -> Result<BandwidthType, ParseEnumError> {
        BandwidthType::from_str(self.bwtype.as_str())
    }

    /// Sets the `bwtype` `String` of `self` from the specified `BandwidthType`
    pub fn set_bwtype(&mut self, bwtype: BandwidthType) {
        self.bwtype = bwtype.to_string();
    }
}

/// Timing information of the session.
///
/// See [RFC 8866 Section 5.9](https://tools.ietf.org/html/rfc8866#section-5.9) for more details.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Time {
    /// Start time of the session in seconds since 1900.
    pub start_time: u64,
    /// Stop time of the session in seconds since 1900.
    pub stop_time: u64,
    /// Repeat times.
    pub repeats: Vec<Repeat>,
}

/// Repeat times for timing information.
///
/// See [RFC 8866 Section 5.10](https://tools.ietf.org/html/rfc8866#section-5.10) for more details.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Repeat {
    /// Repeat interval in seconds.
    pub repeat_interval: u64,
    /// Duration of one repeat.
    pub active_duration: u64,
    /// Offsets for the repeats from the `start_time`.
    pub offsets: Vec<u64>,
}

/// Time zone information for the session.
///
/// See [RFC 8866 Section 5.11](https://tools.ietf.org/html/rfc8866#section-5.11) for more details.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct TimeZone {
    /// Time in seconds since 1900 when the adjustment happens.
    pub adjustment_time: u64,
    /// Amount of the adjustment in seconds.
    pub offset: i64,
}

/// Encryption key for the session or media.
///
/// Note: This field is obsolete and MUST NOT be used. It is included only for legacy reasons
/// See [RFC 8866 Section 5.12](https://tools.ietf.org/html/rfc8866#section-5.12) for more details.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Key {
    /// Encryption method that is used.
    pub method: String,
    /// Encryption key or information to obtain the encryption key.
    pub encryption_key: Option<String>,
}

impl Key {
    /// Tries to parse the `method` `String` of `self` as `KeyMethod`
    pub fn try_parse_keymethod(&self) -> Result<KeyMethod, ParseEnumError> {
        KeyMethod::from_str(self.method.as_str())
    }

    /// Sets the `method` `String` of `self` from the specified `KeyMethod`
    pub fn set_keymethod(&mut self, method: KeyMethod) {
        self.method = method.to_string();
    }
}

/// Attributes for the session or media.
///
/// See [RFC 8866 Section 5.13](https://tools.ietf.org/html/rfc8866#section-5.13) for more details.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Attribute {
    /// Attribute name.
    pub attribute: String,
    /// Attribute value.
    pub value: Option<String>,
}

/// Media description.
///
/// See [RFC 8866 Section 5.14](https://tools.ietf.org/html/rfc8866#section-5.14) for more details.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Media {
    /// Media type, e.g. "audio", "video", "text", "application" or "message".
    pub media: String,
    /// Transport port to which the media is sent.
    pub port: u16,
    /// Number of ports starting at `port` used for the media.
    pub num_ports: Option<u16>,
    /// Transport protocol.
    pub proto: String,
    /// Media format description.
    pub fmt: String,
    /// Media title.
    pub media_title: Option<String>,
    /// Connection data for the media.
    pub connections: Vec<Connection>,
    /// Bandwidth information for the media.
    pub bandwidths: Vec<Bandwidth>,
    /// Encryption key for the media.
    pub key: Option<Key>,
    /// Attributes of the media.
    pub attributes: Vec<Attribute>,
}

/// Error returned when an attribute is not found.
#[derive(Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct AttributeNotFoundError;

impl std::error::Error for AttributeNotFoundError {}

impl std::fmt::Display for AttributeNotFoundError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "Attribute not found")
    }
}

impl Media {
    /// Checks if the given attribute exists.
    pub fn has_attribute(&self, name: &str) -> bool {
        self.attributes.iter().any(|a| a.attribute == name)
    }

    /// Gets the first value of the given attribute, if existing.
    pub fn get_first_attribute_value(
        &self,
        name: &str,
    ) -> Result<Option<&str>, AttributeNotFoundError> {
        self.attributes
            .iter()
            .find(|a| a.attribute == name)
            .ok_or(AttributeNotFoundError)
            .map(|a| a.value.as_deref())
    }

    /// Gets an iterator over all attribute values of the given name, if existing.
    pub fn get_attribute_values<'a>(
        &'a self,
        name: &'a str,
    ) -> Result<impl Iterator<Item = Option<&'a str>> + 'a, AttributeNotFoundError> {
        let mut iter = self
            .attributes
            .iter()
            .filter(move |a| a.attribute == name)
            .map(|a| a.value.as_deref())
            .peekable();
        if iter.peek().is_some() {
            Ok(iter)
        } else {
            Err(AttributeNotFoundError)
        }
    }

    /// Tries to parse the `media` `String` of `self` as `MediaType`
    pub fn try_parse_mediatype(&self) -> Result<MediaType, ParseEnumError> {
        MediaType::from_str(self.media.as_str())
    }

    /// Sets the `media` `String` of `self` from the specified `MediaType`
    pub fn set_mediatype(&mut self, media: MediaType) {
        self.media = media.to_string();
    }

    /// Tries to parse the `proto` `String` of `self` as `TransportProto`
    pub fn try_parse_transport_proto(&self) -> Result<TransportProto, ParseEnumError> {
        TransportProto::from_str(self.proto.as_str())
    }

    /// Sets the `proto` `String` of `self` from the specified `TransportProto`
    pub fn set_transport_proto(&mut self, proto: TransportProto) {
        self.proto = proto.to_string();
    }

    /// Gets an iterator over all attribute values of the given name.
    ///
    /// Each item is a `Result` with `T` in `Ok` and `AttributeError` in `Err`.
    ///
    /// The iterator does not terminate upon an error item; continues with the next attribute
    pub fn attributes_typed<'a, T: TypedAttribute>(
        &'a self,
    ) -> impl Iterator<Item = Result<T, AttributeError>> + 'a {
        self.attributes
            .iter()
            .filter(move |a| a.attribute.eq_ignore_ascii_case(T::NAME))
            .map(|a| {
                let Some(s) = &a.value else {
                    return Err(AttributeError::Other {
                        error: "No value for the attribute".to_string(),
                        attr: T::NAME.to_string(),
                    });
                };

                T::from_str(s)
            })
    }
}

/// SDP session description.
///
/// See [RFC 8866 Section 5](https://tools.ietf.org/html/rfc8866#section-5) for more details.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Session {
    /// Originator of the session.
    pub origin: Origin,
    /// Name of the session.
    pub session_name: String,
    /// Session description.
    pub session_description: Option<String>,
    /// URI to additional information about the session.
    pub uri: Option<String>,
    /// E-Mail contacts for the session.
    pub emails: Vec<String>,
    /// Phone contacts for the session.
    pub phones: Vec<String>,
    /// Connection data for the session.
    pub connection: Option<Connection>,
    /// Bandwidth information for the session.
    pub bandwidths: Vec<Bandwidth>,
    /// Timing information for the session.
    pub times: Vec<Time>,
    /// Time zone information for the session.
    pub time_zones: Vec<TimeZone>,
    /// Encryption key for the session.
    pub key: Option<Key>,
    /// Attributes of the session.
    pub attributes: Vec<Attribute>,
    /// Media descriptions for this session.
    pub medias: Vec<Media>,
}

impl Session {
    /// Checks if the given attribute exists.
    pub fn has_attribute(&self, name: &str) -> bool {
        self.attributes.iter().any(|a| a.attribute == name)
    }

    /// Gets the first value of the given attribute, if existing.
    pub fn get_first_attribute_value(
        &self,
        name: &str,
    ) -> Result<Option<&str>, AttributeNotFoundError> {
        self.attributes
            .iter()
            .find(|a| a.attribute == name)
            .ok_or(AttributeNotFoundError)
            .map(|a| a.value.as_deref())
    }

    /// Gets an iterator over all attribute values of the given name, if existing.
    pub fn get_attribute_values<'a>(
        &'a self,
        name: &'a str,
    ) -> Result<impl Iterator<Item = Option<&'a str>> + 'a, AttributeNotFoundError> {
        let mut iter = self
            .attributes
            .iter()
            .filter(move |a| a.attribute == name)
            .map(|a| a.value.as_deref())
            .peekable();
        if iter.peek().is_some() {
            Ok(iter)
        } else {
            Err(AttributeNotFoundError)
        }
    }

    /// Gets an iterator over all attribute values of the given name.
    ///
    /// Each item is a `Result` with `T` in `Ok` and `AttributeError` in `Err`.
    ///
    /// The iterator does not terminate upon an error item; continues with the next attribute
    pub fn attributes_typed<'a, T: TypedAttribute>(
        &'a self,
    ) -> impl Iterator<Item = Result<T, AttributeError>> + 'a {
        self.attributes
            .iter()
            .filter(move |a| a.attribute.eq_ignore_ascii_case(T::NAME))
            .map(|a| {
                let Some(s) = &a.value else {
                    return Err(AttributeError::Other {
                        error: "No value for the attribute".to_string(),
                        attr: T::NAME.to_string(),
                    });
                };

                T::from_str(s)
            })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_write() {
        let sdp = "v=0\r
o=jdoe 2890844526 2890842807 IN IP4 10.47.16.5\r
s=SDP Seminar\r
i=A Seminar on the session description protocol\r
u=http://www.example.com/seminars/sdp.pdf\r
e=j.doe@example.com (Jane Doe)\r
p=+1 617 555-6011\r
c=IN IP4 224.2.17.12/127\r
b=AS:128\r
t=2873397496 2873404696\r
r=7d 1h 0 25h\r
z=2882844526 -1h 2898848070 0\r
k=clear:1234\r
a=recvonly\r
m=audio 49170 RTP/AVP 0\r
a=fmtp:0 0-15\r
m=video 51372/2 RTP/AVP 99 97 98\r
a=rtpmap:99 h263-1998/90000\r
a=fingerprint:sha-256 3A:96:6D:57:B2:C2:C7:61:A0:46:3E:1C:97:39:D3:F7:0A:88:A0:B1:EC:03:FB:10:A5:5D:3A:37:AB:DD:02:AA\r
a=extmap:2/sendrecv http://example.com/082005/ext.htm#xmeta short\r
";
        let parsed = Session::parse(sdp.as_bytes()).unwrap();
        let mut written = vec![];
        parsed.write(&mut written).unwrap();
        assert_eq!(String::from_utf8_lossy(&written), sdp);
        assert_eq!(parsed.origin.try_parse_addrtype(), Ok(AddrType::Ip4));
        assert_ne!(parsed.origin.try_parse_nettype(), Ok(NetType::Pstn));
        assert_eq!(
            parsed.origin.try_parse_unicast_address(),
            Ok(IpAddr::V4(std::net::Ipv4Addr::new(10, 47, 16, 5)))
        );
        assert_eq!(parsed.medias[0].try_parse_mediatype(), Ok(MediaType::Audio));
        assert_ne!(
            parsed.medias[1].try_parse_transport_proto(),
            Ok(TransportProto::RtpSavpf)
        );
        let f = fallible_iterator::convert(parsed.medias[0].attributes_typed::<Fmtp>())
            .collect::<Vec<_>>()
            .expect("Valid vector of attributes");
        assert_eq!(f.len(), 1);
        assert_eq!(f[0].format_specific_params[0].param, "0-15");

        let e = fallible_iterator::convert(parsed.medias[1].attributes_typed::<ExtMap>())
            .collect::<Vec<_>>()
            .expect("Vector of extmap attributes");
        assert_eq!(e[0].id, 2);
        assert_eq!(e[0].direction, Some(Direction::SendRecv));
        assert_eq!(
            e[0].uri,
            "http://example.com/082005/ext.htm#xmeta".to_string()
        );
        assert_eq!(e[0].attributes, Some("short".to_string()));

        let f = fallible_iterator::convert(parsed.medias[1].attributes_typed::<Fingerprint>())
            .collect::<Vec<_>>()
            .expect("Vector of fingerprint attributes");

        assert_eq!(f[0].hash_func, HashFunc::SHA256);
        assert_eq!(f[0].fingerprint[4], 0xB2);
        assert_eq!(f[0].fingerprint.last(), Some(&0xAA));
    }

    #[test]
    fn parse_media_attributes() {
        let media = Media {
            media: "video".into(),
            port: 51372,
            num_ports: Some(2),
            proto: "RTP/AVP".into(),
            fmt: "99 100".into(),
            media_title: None,
            connections: vec![],
            bandwidths: vec![],
            key: None,
            attributes: vec![
                Attribute {
                    attribute: "rtpmap".into(),
                    value: Some("99 h263-1998/90000".into()),
                },
                Attribute {
                    attribute: "rtpmap".into(),
                    value: Some("100 h264/90000".into()),
                },
                Attribute {
                    attribute: "rtpmap".into(),
                    value: None,
                },
                Attribute {
                    attribute: "rtpmap".into(),
                    value: Some(
                        RtpMap {
                            payload_type: 101,
                            encoding_name: "L16".into(),
                            clock_rate: 16000,
                            encoding_params: Some("2".into()),
                        }
                        .to_string(),
                    ),
                },
                Attribute {
                    attribute: "fmtp".into(),
                    value: Some(
                        Fmtp {
                            fmt: 100,
                            format_specific_params: Vec::from([
                                FmtpParam {
                                    param: "profile-level-id".to_string(),
                                    val: Some("42e016".to_string()),
                                },
                                FmtpParam {
                                    param: "max-mbps".to_string(),
                                    val: Some("108000".to_string()),
                                },
                                FmtpParam {
                                    param: "max-fs".to_string(),
                                    val: Some("3600".to_string()),
                                },
                            ]),
                        }
                        .to_string(),
                    ),
                },
                Attribute {
                    attribute: "rtcp".into(),
                    value: Some(
                        Rtcp {
                            port: 53020,
                            nettype: NetType::In,
                            addrtype: AddrType::Ip4,
                            connection_address: IpAddr::V4(std::net::Ipv4Addr::new(126, 16, 64, 4)),
                        }
                        .to_string(),
                    ),
                },
                Attribute {
                    attribute: "fingerprint".into(),
                    value: Some(
                        Fingerprint {
                            hash_func: HashFunc::Other(("custom").to_string()),
                            fingerprint: [0xA1, 0xB2, 0xC3, 0xD4, 0xE5, 0xF6].to_vec(),
                        }
                        .to_string(),
                    ),
                },
            ],
        };

        assert!(media.has_attribute("rtpmap"));
        assert!(media.has_attribute("rtcp"));
        assert!(!media.has_attribute("foo"));

        assert_eq!(
            media.get_first_attribute_value("rtpmap"),
            Ok(Some("99 h263-1998/90000"))
        );
        assert_eq!(
            media.get_first_attribute_value("rtcp"),
            Ok(Some("53020 IN IP4 126.16.64.4"))
        );
        assert_eq!(
            media.get_first_attribute_value("foo"),
            Err(AttributeNotFoundError)
        );

        assert_eq!(
            media
                .get_attribute_values("rtpmap")
                .unwrap()
                .collect::<Vec<_>>(),
            &[
                Some("99 h263-1998/90000"),
                Some("100 h264/90000"),
                None,
                Some("101 L16/16000/2"),
            ]
        );

        let v = media
            .attributes_typed::<RtpMap>()
            .collect::<Vec<Result<RtpMap, AttributeError>>>();
        assert_eq!(v[0].as_ref().unwrap().clock_rate, 90000);
        assert_eq!(v[1].as_ref().unwrap().encoding_name, "h264");

        let v = media
            .attributes_typed::<RtpMap>()
            .filter(|attr| {
                let Ok(at) = attr else { return false };
                at.payload_type == 99
            })
            .collect::<Vec<_>>();
        assert_eq!(v.len(), 1);
        assert_eq!(v[0].as_ref().unwrap().encoding_name, "h263-1998");

        assert_eq!(
            media.get_first_attribute_value("fmtp"),
            Ok(Some(
                "100 profile-level-id=42e016;max-mbps=108000;max-fs=3600"
            ))
        );

        let r = media.attributes_typed::<Rtcp>().collect::<Vec<_>>();
        assert_eq!(r[0].as_ref().unwrap().addrtype, AddrType::Ip4);
        assert_eq!(r[0].as_ref().unwrap().port, 53020);

        assert_eq!(
            media
                .get_attribute_values("fingerprint")
                .unwrap()
                .collect::<Vec<_>>(),
            &[Some("custom A1:B2:C3:D4:E5:F6")]
        );

        assert!(media.get_attribute_values("foo").is_err());
    }

    #[test]
    fn parse_session_attributes() {
        let session = Session {
            origin: Origin {
                username: Some("jdoe".into()),
                sess_id: "2890844526".into(),
                sess_version: 2890842807,
                nettype: "IN".into(),
                addrtype: "IP4".into(),
                unicast_address: "10.47.16.5".into(),
            },
            session_name: "SDP Seminar".into(),
            session_description: None,
            uri: None,
            emails: vec![],
            phones: vec![],
            connection: None,
            bandwidths: vec![],
            times: vec![Time {
                start_time: 0,
                stop_time: 0,
                repeats: vec![],
            }],
            time_zones: vec![],
            key: None,
            attributes: vec![
                Attribute {
                    attribute: "rtpmap".into(),
                    value: Some("99 h263-1998/90000".into()),
                },
                Attribute {
                    attribute: "rtpmap".into(),
                    value: Some("100 h264/90000".into()),
                },
                Attribute {
                    attribute: "rtpmap".into(),
                    value: Some(
                        RtpMap {
                            payload_type: 101,
                            encoding_name: "L16".into(),
                            clock_rate: 16000,
                            encoding_params: Some("2".into()),
                        }
                        .to_string(),
                    ),
                },
                Attribute {
                    attribute: "rtcp".into(),
                    value: None,
                },
                Attribute {
                    attribute: "extmap".into(),
                    value: Some(
                        ExtMap {
                            id: 1,
                            direction: None,
                            uri: "URI-toffset".to_string(),
                            attributes: None,
                        }
                        .to_string(),
                    ),
                },
            ],
            medias: vec![],
        };

        assert!(session.has_attribute("rtpmap"));
        assert!(session.has_attribute("rtcp"));
        assert!(!session.has_attribute("foo"));

        assert_eq!(
            session.get_first_attribute_value("rtpmap"),
            Ok(Some("99 h263-1998/90000"))
        );

        let v = session
            .get_first_attribute_value("rtpmap")
            .unwrap()
            .unwrap();
        let rtpmap = RtpMap::from_str(v).unwrap();
        assert_eq!(rtpmap.clock_rate, 90000);
        assert_eq!(rtpmap.encoding_name, "h263-1998");
        assert_ne!(rtpmap.encoding_name, "h263");
        assert_ne!(rtpmap.encoding_params, Some("2".to_string()));
        assert_eq!(rtpmap.payload_type, 99);

        assert_eq!(session.get_first_attribute_value("rtcp"), Ok(None));
        assert_eq!(
            session.get_first_attribute_value("foo"),
            Err(AttributeNotFoundError)
        );

        assert_eq!(
            session
                .get_attribute_values("rtpmap")
                .unwrap()
                .collect::<Vec<_>>(),
            &[
                Some("99 h263-1998/90000"),
                Some("100 h264/90000"),
                Some("101 L16/16000/2")
            ]
        );
        assert_eq!(
            session
                .get_attribute_values("rtcp")
                .unwrap()
                .collect::<Vec<_>>(),
            &[None]
        );

        assert_eq!(
            session
                .get_attribute_values("extmap")
                .unwrap()
                .collect::<Vec<_>>(),
            &[Some("1 URI-toffset")]
        );

        assert!(session.get_attribute_values("foo").is_err());

        let a = fallible_iterator::convert(session.attributes_typed::<RtpMap>())
            .collect::<Vec<_>>()
            .expect("Valid vector of attributes");
        assert_eq!(a[2].encoding_name, "L16");
        assert_eq!(a[0].payload_type, 99);
    }
}
