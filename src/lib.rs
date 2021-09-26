// Copyright (C) 2019 Sebastian Dröge <sebastian@centricular.com>
//
// Licensed under the MIT license, see the LICENSE file or <http://opensource.org/licenses/MIT>

//! Crate for handling SDP ([RFC 4566](https://tools.ietf.org/html/rfc4566))
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

mod parser;
mod writer;

pub use parser::ParserError;

/// Originator of the session.
///
/// See [RFC 4566 Section 5.2](https://tools.ietf.org/html/rfc4566#section-5.2) for more details.
#[derive(Debug, PartialEq, Eq, Clone)]
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

/// Connection data for the session or media.
///
/// See [RFC 4566 Section 5.7](https://tools.ietf.org/html/rfc4566#section-5.7) for more details.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Connection {
    /// Type of network for this connection.
    pub nettype: String,
    /// Type of the `connection_address`.
    pub addrtype: String,
    /// Connection address.
    pub connection_address: String,
}

/// Bandwidth information for the session or media.
///
/// See [RFC 4566 Section 5.8](https://tools.ietf.org/html/rfc4566#section-5.8) for more details.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Bandwidth {
    /// Bandwidth type, usually "CT" or "AS".
    pub bwtype: String,
    /// Bandwidth.
    pub bandwidth: u64,
}

/// Timing information of the session.
///
/// See [RFC 4566 Section 5.9](https://tools.ietf.org/html/rfc4566#section-5.9) for more details.
#[derive(Debug, PartialEq, Eq, Clone)]
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
/// See [RFC 4566 Section 5.10](https://tools.ietf.org/html/rfc4566#section-5.10) for more details.
#[derive(Debug, PartialEq, Eq, Clone)]
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
/// See [RFC 4566 Section 5.11](https://tools.ietf.org/html/rfc4566#section-5.11) for more details.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TimeZone {
    /// Time in seconds since 1900 when the adjustment happens.
    pub adjustment_time: u64,
    /// Amount of the adjustment in seconds.
    pub offset: i64,
}

/// Encryption key for the session or media.
///
/// See [RFC 4566 Section 5.12](https://tools.ietf.org/html/rfc4566#section-5.12) for more details.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Key {
    /// Encryption method that is used.
    pub method: String,
    /// Encryption key or information to obtain the encryption key.
    pub encryption_key: Option<String>,
}

/// Attributes for the session or media.
///
/// See [RFC 4566 Section 5.13](https://tools.ietf.org/html/rfc4566#section-5.13) for more details.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Attribute {
    /// Attribute name.
    pub attribute: String,
    /// Attribute value.
    pub value: Option<String>,
}

/// Media description.
///
/// See [RFC 4566 Section 5.14](https://tools.ietf.org/html/rfc4566#section-5.14) for more details.
#[derive(Debug, PartialEq, Eq, Clone)]
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

/// SDP session description.
///
/// See [RFC 4566 Section 5](https://tools.ietf.org/html/rfc4566#section-5) for more details.
#[derive(Debug, PartialEq, Eq, Clone)]
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

/// Error returned when an attribute is not found.
#[derive(Debug, PartialEq, Eq)]
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
m=video 51372/2 RTP/AVP 99\r
a=rtpmap:99 h263-1998/90000\r
a=fingerprint:sha-256 3A:96:6D:57:B2:C2:C7:61:A0:46:3E:1C:97:39:D3:F7:0A:88:A0:B1:EC:03:FB:10:A5:5D:3A:37:AB:DD:02:AA\r
";
        let parsed = Session::parse(sdp.as_bytes()).unwrap();
        let mut written = vec![];
        parsed.write(&mut written).unwrap();
        assert_eq!(String::from_utf8_lossy(&written), sdp);
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
                    attribute: "rtcp".into(),
                    value: None,
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
        assert_eq!(media.get_first_attribute_value("rtcp"), Ok(None));
        assert_eq!(
            media.get_first_attribute_value("foo"),
            Err(AttributeNotFoundError)
        );

        assert_eq!(
            media
                .get_attribute_values("rtpmap")
                .unwrap()
                .collect::<Vec<_>>(),
            &[Some("99 h263-1998/90000"), Some("100 h264/90000")]
        );
        assert_eq!(
            media
                .get_attribute_values("rtcp")
                .unwrap()
                .collect::<Vec<_>>(),
            &[None]
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
                    attribute: "rtcp".into(),
                    value: None,
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
            &[Some("99 h263-1998/90000"), Some("100 h264/90000")]
        );
        assert_eq!(
            session
                .get_attribute_values("rtcp")
                .unwrap()
                .collect::<Vec<_>>(),
            &[None]
        );
        assert!(session.get_attribute_values("foo").is_err());
    }
}
