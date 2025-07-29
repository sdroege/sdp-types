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
    fmt::Display,
    net::{AddrParseError, IpAddr},
    str::FromStr,
};

use bstr::*;
use fallible_iterator::FallibleIterator;

mod parser;
mod writer;

pub use parser::ParserError;

/// Errors while parsing strings to Enum
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ParseEnumError {
    Invalid(String),
}

/// Type of network of the originator or a connection of the session.
///
/// See [RFC 8866 Section 5.2](https://tools.ietf.org/html/rfc8866#section-5.2),
/// [RFC 8866 Section 5.7](https://tools.ietf.org/html/rfc8866#section-5.7) and
/// [RFC 8866 Section 8.2.6](https://datatracker.ietf.org/doc/html/rfc8866#section-8.2.6) for more details
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum NetType {
    /// Internet
    In,
    /// Telephone Network
    Tn,
    /// ATM Bearer Connection
    Atm,
    /// Public Switched Telephone Network
    Pstn,
}

impl NetType {
    pub fn as_str(&self) -> &'static str {
        match self {
            NetType::In => "IN",
            NetType::Tn => "TN",
            NetType::Atm => "ATM",
            NetType::Pstn => "PSTN",
        }
    }
}

impl FromStr for NetType {
    type Err = ParseEnumError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_ascii_uppercase().as_str() {
            "IN" => Ok(NetType::In),
            "TN" => Ok(NetType::Tn),
            "ATM" => Ok(NetType::Atm),
            "PSTN" => Ok(NetType::Pstn),
            _ => Err(ParseEnumError::Invalid(s.to_string())),
        }
    }
}

impl Display for NetType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

/// Type of address of the originator or a connection of the session
///
/// See [RFC 8866 Section 5.2](https://tools.ietf.org/html/rfc8866#section-5.2),
/// [RFC 8866 Section 5.7](https://tools.ietf.org/html/rfc8866#section-5.7)
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum AddrType {
    /// IPv4 address
    Ip4,
    /// Ipv6 address
    Ip6,
}

impl AddrType {
    pub fn as_str(&self) -> &'static str {
        match self {
            AddrType::Ip4 => "IP4",
            AddrType::Ip6 => "IP6",
        }
    }
}

impl FromStr for AddrType {
    type Err = ParseEnumError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_ascii_uppercase().as_str() {
            "IP4" => Ok(AddrType::Ip4),
            "IP6" => Ok(AddrType::Ip6),
            _ => Err(ParseEnumError::Invalid(s.to_string())),
        }
    }
}

impl Display for AddrType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

/// Type of the Bandwidth value.
///
/// See [RFC 8866 Section 5.8](https://tools.ietf.org/html/rfc8866#section-5.8) for more details.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BwType {
    /// Conference total - maximum bandwith a session will use
    Ct,
    /// Application Specific maximum bandwidth
    As,
    /// Bandwidth assigned for RTCP reports by active senders. See [RFC 3890 Section 1.1.3](https://datatracker.ietf.org/doc/html/rfc3890#section-1.1.3)
    Rr,
    /// Bandwidth assigned for RTCP reports by active receivers. See [RFC 3890 Section 1.1.3](https://datatracker.ietf.org/doc/html/rfc3890#section-1.1.3)
    Rs,
}

impl BwType {
    pub fn as_str(&self) -> &'static str {
        match self {
            BwType::As => "AS",
            BwType::Ct => "CT",
            BwType::Rr => "RR",
            BwType::Rs => "RS",
        }
    }
}

impl FromStr for BwType {
    type Err = ParseEnumError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_ascii_uppercase().as_str() {
            "AS" => Ok(BwType::As),
            "CT" => Ok(BwType::Ct),
            "RR" => Ok(BwType::Rr),
            "RS" => Ok(BwType::Rs),
            _ => Err(ParseEnumError::Invalid(s.to_string())),
        }
    }
}

impl Display for BwType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

/// Method of encryption (Obselete)
///
/// Note: This field is obsolete and and MUST NOT be used. It is included in only for legacy reasons
/// See [RFC 8866 Section 5.12](https://datatracker.ietf.org/doc/html/rfc8866#section-5.12)
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum KeyMethod {
    /// Untransformed
    Clear,
    /// Base64 encoded
    Base64,
    /// URI to obtain the key
    Uri,
    /// User should be prompted for the key
    Prompt,
}

impl KeyMethod {
    pub fn as_str(&self) -> &'static str {
        match self {
            KeyMethod::Clear => "clear",
            KeyMethod::Base64 => "base64",
            KeyMethod::Uri => "uri",
            KeyMethod::Prompt => "prompt",
        }
    }
}

impl FromStr for KeyMethod {
    type Err = ParseEnumError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // case-sensitive
        match s {
            "clear" => Ok(KeyMethod::Clear),
            "base64" => Ok(KeyMethod::Base64),
            "uri" => Ok(KeyMethod::Uri),
            "prompt" => Ok(KeyMethod::Prompt),
            _ => Err(ParseEnumError::Invalid(s.to_string())),
        }
    }
}

impl Display for KeyMethod {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

/// The media type
///
/// See [RFC 8866 Section 5.14](https://datatracker.ietf.org/doc/html/rfc8866#section-5.14)
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum MediaType {
    /// Audio type
    Audio,
    /// Video type
    Video,
    /// Text type
    Text,
    /// Application type
    Application,
    /// Message type
    Message,
    /// Image type. See [RFC 6466](https://datatracker.ietf.org/doc/html/rfc6466)
    Image,
}

impl MediaType {
    pub fn as_str(&self) -> &'static str {
        match self {
            MediaType::Audio => "audio",
            MediaType::Video => "video",
            MediaType::Text => "text",
            MediaType::Application => "application",
            MediaType::Message => "message",
            MediaType::Image => "image",
        }
    }
}

impl FromStr for MediaType {
    type Err = ParseEnumError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_ascii_lowercase().as_str() {
            "audio" => Ok(MediaType::Audio),
            "video" => Ok(MediaType::Video),
            "text" => Ok(MediaType::Text),
            "application" => Ok(MediaType::Application),
            "message" => Ok(MediaType::Message),
            "image" => Ok(MediaType::Image),
            _ => Err(ParseEnumError::Invalid(s.to_string())),
        }
    }
}

impl Display for MediaType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

/// Transport Protocol for the media
///
/// See [RFC 8866 Section 5.14](https://datatracker.ietf.org/doc/html/rfc8866#section-5.14)
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TransportProto {
    /// Direct UDP
    Udp,
    /// RTP over UDP
    RtpAvp,
    /// Secure RTP over UDP
    RtpSavp,
    /// Secure RTP over UDP with RTCP-based feedback
    RtpSavpf,
}

impl TransportProto {
    pub fn as_str(&self) -> &'static str {
        match self {
            // The strings are case-insensitive, but the spec (RFC 8866) uses lower-case of the "udp" protocol
            // and upper-case for the others so keeping it the same
            TransportProto::Udp => "udp",
            TransportProto::RtpAvp => "RTP/AVP",
            TransportProto::RtpSavp => "RTP/SAVP",
            TransportProto::RtpSavpf => "RTP/SAVPF",
        }
    }
}

impl FromStr for TransportProto {
    type Err = ParseEnumError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // The strings are case-insensitive, but the spec (RFC 8866) uses lower-case of the "udp" protocol
        // and upper-case for the others so keeping it the same
        if "udp".eq_ignore_ascii_case(s) {
            Ok(TransportProto::Udp)
        } else if "RTP/AVP".eq_ignore_ascii_case(s) {
            Ok(TransportProto::RtpAvp)
        } else if "RTP/SAVP".eq_ignore_ascii_case(s) {
            Ok(TransportProto::RtpSavp)
        } else if "RTP/SAVPF".eq_ignore_ascii_case(s) {
            Ok(TransportProto::RtpSavpf)
        } else {
            Err(ParseEnumError::Invalid(s.to_string()))
        }
    }
}

impl Display for TransportProto {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

/// RtpMap Attribute
///
/// See [RFC 8866 Section 6.6](https://datatracker.ietf.org/doc/html/rfc8866#section-6.6) for more details
#[derive(Debug, Clone, PartialEq)]
pub struct RtpMap {
    /// Payload type, a numerical value between 0 and 127
    pub pt: u8,
    /// Name of the encoding
    // TODO: is it useful to have an enum for all known encoding?
    pub encoding: String,
    /// Clock rate
    pub clock_rate: u32,
    /// Encoding parameters. Currently used only for audio channel count
    pub params: Option<String>,
}

impl RtpMap {
    /// Converts the RtpMap to String
    pub fn as_string(&self) -> String {
        let mut s = format!("{} {}/{}", self.pt, self.encoding, self.clock_rate);
        if let Some(params) = &self.params {
            s += format!("/{params}").as_str();
        }
        s
    }
}

impl FromStr for RtpMap {
    type Err = AttributeErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let Some((pt, rest)) = s.split_once(' ') else {
            return Err(AttributeErr("Failed to split the rtpmap using a space"));
        };

        let Ok(pt) = pt.parse::<u8>() else {
            return Err(AttributeErr("Failed to parse payload type in rtpmap"));
        };

        let mut i = rest.split('/');
        let Some(encoding) = i.next() else {
            return Err(AttributeErr("Failed to get encoding name in the rtpmap"));
        };

        let Some(clock_rate) = i.next() else {
            return Err(AttributeErr("Failed to get clock rate in the rtpmap"));
        };

        let Ok(clock_rate) = clock_rate.parse::<u32>() else {
            return Err(AttributeErr("Failed to parse clock rate in rtpmap"));
        };

        let params = i.next().map(String::from);

        Ok(Self {
            pt,
            encoding: encoding.to_owned(),
            clock_rate,
            params,
        })
    }
}

impl Display for RtpMap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.as_string())
    }
}

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
/// Note: This field is obsolete and and MUST NOT be used. It is included in only for legacy reasons
/// See [RFC 8866 Section 5.12](https://tools.ietf.org/html/rfc8866#section-5.12) for more details.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Key {
    /// Encryption method that is used.
    pub method: String,
    /// Encryption key or information to obtain the encryption key.
    pub encryption_key: Option<String>,
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

/// Error returned when an attribute is not found.
#[derive(Debug, PartialEq, Eq)]
pub struct AttributeNotFoundError;

impl std::error::Error for AttributeNotFoundError {}

impl std::fmt::Display for AttributeNotFoundError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "Attribute not found")
    }
}

/// Attribute error with specific details
// TODO: combine this and AttributeNotFoundError?
#[derive(Debug, PartialEq, Eq)]
pub struct AttributeErr(&'static str);

impl std::error::Error for AttributeErr {}

impl std::fmt::Display for AttributeErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.0)
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

impl Origin {
    /// Constructs a `AddrType` from a string
    pub fn try_addrtype(&self) -> Result<AddrType, ParseEnumError> {
        AddrType::from_str(self.addrtype.as_str())
    }

    /// Converts a `AddrType` to string and sets it to the `addrtype`
    pub fn set_from_addrtype(&mut self, addrtype: AddrType) {
        self.addrtype = addrtype.to_string();
    }

    /// Constructs a `NetType` from a string
    pub fn try_nettype(&self) -> Result<NetType, ParseEnumError> {
        NetType::from_str(self.nettype.as_str())
    }

    /// Converts a `NetType` to string and sets it to the `nettype`
    pub fn set_from_nettype(&mut self, nettype: NetType) {
        self.nettype = nettype.to_string();
    }

    /// Constructs a `IpAddr` from a string
    pub fn try_unicast_address(&self) -> Result<IpAddr, AddrParseError> {
        self.unicast_address.parse()
    }

    /// Converts a `IpAddr` to string and sets it to the `unicast_address`
    pub fn set_unicast_address(&mut self, unicast_address: IpAddr) {
        self.unicast_address = unicast_address.to_string();
    }
}

impl Connection {
    /// Constructs a `AddrType` from a string
    pub fn try_addrtype(&self) -> Result<AddrType, ParseEnumError> {
        AddrType::from_str(self.addrtype.as_str())
    }

    /// Converts a `AddrType` to string and sets it to the `addrtype`
    pub fn set_from_addrtype(&mut self, addrtype: AddrType) {
        self.addrtype = addrtype.to_string();
    }

    /// Constructs a `NetType` from a string
    pub fn try_nettype(&self) -> Result<NetType, ParseEnumError> {
        NetType::from_str(self.nettype.as_str())
    }

    /// Converts a `NetType` to string and sets it to the `nettype`
    pub fn set_from_nettype(&mut self, nettype: NetType) {
        self.nettype = nettype.to_string();
    }

    /// Constructs a `IpAddr` from a string
    pub fn try_connection_address(&self) -> Result<IpAddr, AddrParseError> {
        self.connection_address.parse()
    }

    /// Converts a `IpAddr` to string and sets it to the `connection_address`
    pub fn set_connection_address(&mut self, connection_address: IpAddr) {
        self.connection_address = connection_address.to_string();
    }
}

impl Bandwidth {
    /// Constructs a `BwType` from a string
    pub fn try_bwtype(&self) -> Result<BwType, ParseEnumError> {
        BwType::from_str(self.bwtype.as_str())
    }

    /// Converts a `BwType` to string and sets it to the `bwtype`
    pub fn set_from_bwtype(&mut self, bwtype: BwType) {
        self.bwtype = bwtype.to_string();
    }
}

impl Key {
    /// Constructs a `KeyMethod` from a string
    pub fn try_keymethod(&self) -> Result<KeyMethod, ParseEnumError> {
        KeyMethod::from_str(self.method.as_str())
    }

    /// Converts a `KeyMethod` to string and sets it to the `method`
    pub fn set_from_keymethod(&mut self, method: KeyMethod) {
        self.method = method.to_string();
    }
}

impl Media {
    /// Constructs a `MediaType` from a string
    pub fn try_mediatype(&self) -> Result<MediaType, ParseEnumError> {
        MediaType::from_str(self.media.as_str())
    }

    /// Converts a `MediaType` to string and sets it to the `media`
    pub fn set_from_mediatype(&mut self, media: MediaType) {
        self.media = media.to_string();
    }

    /// Constructs a `TransportProto` from a string
    pub fn try_transport_proto(&self) -> Result<TransportProto, ParseEnumError> {
        TransportProto::from_str(self.proto.as_str())
    }

    /// Converts a `TransportProto` to string and sets it to the `proto`
    pub fn set_from_transport_proto(&mut self, proto: TransportProto) {
        self.proto = proto.to_string();
    }

    /// Gets an iterator over all attribute values of the given name.
    /// Each item is a `Result` with the inferred type in `Ok` and `AttributeError` in `Err`
    pub fn get_attribute_values_typed<'a, T: FromStr<Err = AttributeErr>>(
        &'a self,
        name: &'a str,
    ) -> impl Iterator<Item = Result<T, AttributeErr>> + 'a {
        self.attributes
            .iter()
            .filter(move |a| a.attribute == name)
            .map(|a| {
                let Some(s) = &a.value else {
                    // does not have a value for the attribute
                    return Err(AttributeErr("No value for the attribute"));
                };

                match T::from_str(s) {
                    Ok(t) => Ok(t),
                    Err(e) => Err(e),
                }
            })
    }
}

impl Session {
    /// Gets an iterator over all attribute values of the given name.
    /// Each item is a `Result` with the inferred type in `Ok` and `AttributeError` in `Err`
    pub fn get_attribute_values_typed<'a, T: FromStr<Err = AttributeErr>>(
        &'a self,
        name: &'a str,
    ) -> impl Iterator<Item = Result<T, AttributeErr>> + 'a {
        self.attributes
            .iter()
            .filter(move |a| a.attribute == name)
            .map(|a| {
                let Some(s) = &a.value else {
                    // does not have a value for the attribute
                    return Err(AttributeErr("No value for the attribute"));
                };

                match T::from_str(s) {
                    Ok(t) => Ok(t),
                    Err(e) => Err(e),
                }
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
m=video 51372/2 RTP/AVP 99 97 98\r
a=rtpmap:99 h263-1998/90000\r
a=fingerprint:sha-256 3A:96:6D:57:B2:C2:C7:61:A0:46:3E:1C:97:39:D3:F7:0A:88:A0:B1:EC:03:FB:10:A5:5D:3A:37:AB:DD:02:AA\r
";
        let parsed = Session::parse(sdp.as_bytes()).unwrap();
        let mut written = vec![];
        parsed.write(&mut written).unwrap();
        assert_eq!(String::from_utf8_lossy(&written), sdp);
        assert_eq!(parsed.origin.try_addrtype(), Ok(AddrType::Ip4));
        assert_ne!(parsed.origin.try_nettype(), Ok(NetType::Pstn));
        assert_eq!(
            parsed.origin.try_unicast_address(),
            Ok(IpAddr::V4(std::net::Ipv4Addr::new(10, 47, 16, 5)))
        );
        assert_eq!(parsed.medias[0].try_mediatype(), Ok(MediaType::Audio));
        assert_ne!(
            parsed.medias[1].try_transport_proto(),
            Ok(TransportProto::RtpSavpf)
        );
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
                            pt: 101,
                            encoding: "L16".into(),
                            clock_rate: 16000,
                            params: Some("2".into()),
                        }
                        .as_string(),
                    ),
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
            &[
                Some("99 h263-1998/90000"),
                Some("100 h264/90000"),
                None,
                Some("101 L16/16000/2"),
            ]
        );

        let v = media
            .get_attribute_values_typed("rtpmap")
            .collect::<Vec<Result<RtpMap, AttributeErr>>>();
        assert_eq!(v[0].as_ref().unwrap().clock_rate, 90000);
        assert_eq!(v[1].as_ref().unwrap().encoding, "h264");
        assert_eq!(v[2], Err(AttributeErr("No value for the attribute")));
        assert_eq!(v[3].as_ref().unwrap().params.as_ref().unwrap(), "2");

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

        let v = session
            .get_first_attribute_value("rtpmap")
            .unwrap()
            .unwrap();
        let rtpmap = RtpMap::from_str(v).unwrap();
        assert_eq!(rtpmap.clock_rate, 90000);
        assert_eq!(rtpmap.encoding, "h263-1998");
        assert_ne!(rtpmap.encoding, "h263");
        assert_ne!(rtpmap.params, Some("2".to_string()));
        assert_eq!(rtpmap.pt, 99);

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
