//
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
//! // or
//! let sdp = sdp_types::Session::new(sdp_types::Origin::new(...), "my-session");
//!
//! // or
//! let sdp = sdp_types::Session::builder(sdp_types::Origin::new(...), "my-session")
//!     .attribute(...)
//!     .attribute(...)
//!     .media(...)
//!     .media(...)
//!     ...
//!     .build();
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
//!     Some(Some(tool)) => println!("tool: {tool}"),
//!     Some(None) => println!("tool: empty"),
//!     None => println!("no tool attribute"),
//! }
//!
//! // Access all the 'rtpmap' attributes as an `RtpMap` type
//! // returns an iterator of type `Iterator<Item = Result<RtpMap, AttributeError>>`
//! let r = sdp.attributes_typed::<sdp_types::RtpMap>();
//!
//! // Access the first 'rtpmap' attribute as an `RtpMap` type:
//! match sdp.get_first_attribute_typed::<sdp_types::RtpMap>() {
//!     Some(Ok(rtpmap)) => println!("rtpmap: {rtpmap}"),
//!     Some(Err(err)) => println!("rtpmap: parsing error: {err}"),
//!     None => println!("no rtpmap attribute"),
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

use std::{net::IpAddr, str::FromStr};

use bstr::*;
use fallible_iterator::FallibleIterator;

pub mod attributes;
pub mod builders;
pub mod clock_signalling;
pub mod enums;
mod parser;
mod writer;

pub use attributes::*;
pub use clock_signalling::*;
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
    pub nettype: NetType,
    /// Type of the `unicast_address`.
    pub addrtype: AddrType,
    /// Address where the session was created.
    pub unicast_address: String,
}

impl Origin {
    /// Construct an [`Origin`] with the specified IP `unicast_address`
    pub fn with_ip_addr(
        sess_id: impl ToString,
        sess_version: u64,
        unicast_address: impl Into<IpAddr>,
    ) -> Self {
        let unicast_address = unicast_address.into();
        Origin {
            username: Default::default(),
            sess_id: sess_id.to_string(),
            sess_version,
            nettype: NetType::In,
            addrtype: unicast_address.into(),
            unicast_address: unicast_address.to_string(),
        }
    }

    /// Construct an [`Origin`]
    ///
    /// See also [`Origin::with_ip_addr`]
    pub fn new(
        sess_id: impl ToString,
        sess_version: u64,
        nettype: NetType,
        addrtype: AddrType,
        unicast_address: impl ToString,
    ) -> Self {
        Origin {
            username: Default::default(),
            sess_id: sess_id.to_string(),
            sess_version,
            nettype,
            addrtype,
            unicast_address: unicast_address.to_string(),
        }
    }

    /// Sets the `unicast_address` & `addrtype` of `self` from the specified `IpAddr`
    pub fn set_unicast_ip_address(&mut self, unicast_address: impl Into<IpAddr>) {
        let unicast_address = unicast_address.into();
        self.addrtype = unicast_address.into();
        self.unicast_address = unicast_address.to_string();
    }
    ///
    /// Returns the `Ok` with the parsed `IpAddr` or `Err` with the string address
    /// if parsing failed.
    pub fn try_parse_unicast_ip_address(&self) -> Result<IpAddr, &str> {
        self.unicast_address
            .parse::<IpAddr>()
            .map_err(|_| self.unicast_address.as_str())
    }

    pub fn set_username(&mut self, username: impl ToString) {
        self.username = Some(username.to_string());
    }

    /// Construct an [`crate::builders::Origin`] with the specified IP `unicast_address`
    pub fn builder_with_ip_addr(
        sess_id: impl ToString,
        sess_version: u64,
        unicast_address: impl Into<IpAddr>,
    ) -> builders::Origin {
        builders::Origin::with_ip_addr(sess_id, sess_version, unicast_address)
    }

    /// Construct an [`crate::builders::Origin`]
    ///
    /// See also [`Origin::builder_with_ip_addr`]
    pub fn builder(
        sess_id: impl ToString,
        sess_version: u64,
        nettype: NetType,
        addrtype: AddrType,
        unicast_address: impl ToString,
    ) -> builders::Origin {
        builders::Origin::new(sess_id, sess_version, nettype, addrtype, unicast_address)
    }
}

/// Connection data for the session or media.
///
/// See [RFC 8866 Section 5.7](https://tools.ietf.org/html/rfc8866#section-5.7) for more details.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Connection {
    /// Type of network for this connection.
    pub nettype: NetType,
    /// Type of the `connection_address`.
    pub addrtype: AddrType,
    /// Connection address.
    pub connection_address: String,
}

impl Connection {
    /// Construct a [`Connection`] with the specified IP `connection_address`
    pub fn from_ip_addr(connection_address: impl Into<IpAddr>) -> Self {
        let connection_address = connection_address.into();
        Connection {
            nettype: NetType::In,
            addrtype: connection_address.into(),
            connection_address: connection_address.to_string(),
        }
    }

    /// Construct a [`Connection`]
    ///
    /// See also [`Connection::from_ip_addr`]
    pub fn new(nettype: NetType, addrtype: AddrType, connection_address: impl ToString) -> Self {
        Connection {
            nettype,
            addrtype,
            connection_address: connection_address.to_string(),
        }
    }

    /// Sets the `connection_address` & `addrtype` of `self` from the specified `IpAddr`
    pub fn set_connection_ip_address(&mut self, connection_address: impl Into<IpAddr>) {
        let connection_address = connection_address.into();
        self.addrtype = connection_address.into();
        self.connection_address = connection_address.to_string();
    }

    /// Tries to parse the `connection_address` `String` of `self` as `IpAddr`
    ///
    /// Returns the `Ok` with the parsed `IpAddr` or `Err` with the string address
    /// if parsing failed.
    pub fn try_parse_connection_ip_address(&self) -> Result<IpAddr, &str> {
        self.connection_address
            .parse::<IpAddr>()
            .map_err(|_| self.connection_address.as_str())
    }
}

impl<T: Into<IpAddr>> From<T> for Connection {
    fn from(ip_addr: T) -> Self {
        Connection::from_ip_addr(ip_addr)
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
    pub fn new(bwtype: BandwidthType, bandwidth: u64) -> Self {
        Bandwidth {
            bwtype: bwtype.to_string(),
            bandwidth,
        }
    }

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

impl Time {
    pub fn new(start_time: u64, stop_time: u64) -> Self {
        Time {
            start_time,
            stop_time,
            repeats: Default::default(),
        }
    }

    pub fn add_repeat(&mut self, repeat: Repeat) {
        self.repeats.push(repeat);
    }

    pub fn add_repeats(&mut self, repeats: impl IntoIterator<Item = Repeat>) {
        self.repeats.extend(repeats)
    }

    pub fn builder(start_time: u64, stop_time: u64) -> builders::Time {
        builders::Time::new(start_time, stop_time)
    }
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

impl Repeat {
    pub fn new(repeat_interval: u64, active_duration: u64) -> Self {
        Repeat {
            repeat_interval,
            active_duration,
            offsets: Default::default(),
        }
    }

    pub fn add_offset(&mut self, offset: u64) {
        self.offsets.push(offset);
    }

    pub fn add_offsets(&mut self, offsets: impl IntoIterator<Item = u64>) {
        self.offsets.extend(offsets)
    }

    pub fn builder(repeat_interval: u64, active_duration: u64) -> builders::Repeat {
        builders::Repeat::new(repeat_interval, active_duration)
    }
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

impl TimeZone {
    pub fn new(adjustment_time: u64, offset: i64) -> Self {
        TimeZone {
            adjustment_time,
            offset,
        }
    }
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
    pub fn new(method: KeyMethod) -> Self {
        Key {
            method: method.to_string(),
            encryption_key: Default::default(),
        }
    }

    /// Constructs a [`crate::Key`] and define its `encryption_key`.
    pub fn with_encryption_key(method: KeyMethod, encryption_key: impl ToString) -> Self {
        Key {
            method: method.to_string(),
            encryption_key: Some(encryption_key.to_string()),
        }
    }

    /// Tries to parse the `method` `String` of `self` as `KeyMethod`
    pub fn try_parse_keymethod(&self) -> Result<KeyMethod, ParseEnumError> {
        KeyMethod::from_str(self.method.as_str())
    }

    /// Sets the `method` `String` of `self` from the specified `KeyMethod`
    pub fn set_keymethod(&mut self, method: KeyMethod) {
        self.method = method.to_string();
    }
}

impl From<KeyMethod> for Key {
    fn from(method: KeyMethod) -> Self {
        Key {
            method: method.to_string(),
            encryption_key: None,
        }
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

impl Attribute {
    /// Constructs an [`crate::Attribute`].
    ///
    /// Use [`crate::Attribute::with_value`] if you need to define the `value`.
    pub fn new(attribute: impl ToString) -> Self {
        Attribute {
            attribute: attribute.to_string(),
            value: Default::default(),
        }
    }

    /// Constructs an [`crate::Attribute`] and define its `value`.
    pub fn with_value(attribute: impl ToString, value: impl ToString) -> Self {
        Attribute {
            attribute: attribute.to_string(),
            value: Some(value.to_string()),
        }
    }
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

impl Media {
    pub fn new(media: MediaType, port: u16, proto: TransportProto, fmt: impl ToString) -> Self {
        Media {
            media: media.to_string(),
            port,
            num_ports: Default::default(),
            proto: proto.to_string(),
            fmt: fmt.to_string(),
            media_title: Default::default(),
            connections: Default::default(),
            bandwidths: Default::default(),
            key: Default::default(),
            attributes: Default::default(),
        }
    }

    pub fn builder(
        media: MediaType,
        port: u16,
        proto: TransportProto,
        fmt: impl ToString,
    ) -> builders::Media {
        builders::Media::new(media, port, proto, fmt)
    }

    pub fn set_num_ports(&mut self, num_ports: u16) {
        self.num_ports = Some(num_ports);
    }

    pub fn set_media_title(&mut self, media_title: impl ToString) {
        self.media_title = Some(media_title.to_string());
    }

    pub fn add_connection(&mut self, connection: impl Into<Connection>) {
        self.connections.push(connection.into());
    }

    pub fn add_connections(
        &mut self,
        connections: impl IntoIterator<Item = impl Into<Connection>>,
    ) {
        self.connections
            .extend(connections.into_iter().map(|c| c.into()))
    }

    pub fn add_bandwidth(&mut self, bandwidth: Bandwidth) {
        self.bandwidths.push(bandwidth);
    }

    pub fn add_bandwidths(&mut self, bandwidths: impl IntoIterator<Item = Bandwidth>) {
        self.bandwidths.extend(bandwidths)
    }

    pub fn set_encryption_key(&mut self, key: impl Into<Key>) {
        self.key = Some(key.into());
    }

    pub fn add_attribute(&mut self, attribute: impl Into<Attribute>) {
        self.attributes.push(attribute.into());
    }

    pub fn add_attribute_from_str(&mut self, attribute: impl ToString) {
        self.attributes.push(Attribute::new(attribute));
    }

    pub fn add_attribute_with_value(&mut self, attribute: impl ToString, value: impl ToString) {
        self.attributes
            .push(Attribute::with_value(attribute, value));
    }

    pub fn add_attributes(&mut self, attributes: impl IntoIterator<Item = impl Into<Attribute>>) {
        self.attributes
            .extend(attributes.into_iter().map(|a| a.into()))
    }

    pub fn add_attributes_from_strs(
        &mut self,
        attributes: impl IntoIterator<Item = impl ToString>,
    ) {
        self.attributes
            .extend(attributes.into_iter().map(|a| Attribute::new(a)))
    }

    /// Checks if the given attribute exists.
    pub fn has_attribute(&self, name: &str) -> bool {
        self.attributes.iter().any(|a| a.attribute == name)
    }

    /// Gets the first value of the given attribute.
    ///
    /// The outter `Option` reflects the availability of the attribute.
    /// The inner `Option` reflects the optional value of the attribute.
    pub fn get_first_attribute_value<'a>(&'a self, name: &'a str) -> Option<Option<&'a str>> {
        self.get_attribute_values(name).next()
    }

    /// Gets an iterator over all attribute values of the given name.
    pub fn get_attribute_values<'a>(
        &'a self,
        name: &'a str,
    ) -> impl Iterator<Item = Option<&'a str>> {
        self.attributes
            .iter()
            .filter(move |a| a.attribute == name)
            .map(|a| a.value.as_deref())
    }

    /// Tries to parse the `media` `String` of `self` as `MediaType`
    pub fn try_parse_mediatype(&self) -> Result<MediaType, ParseEnumError> {
        MediaType::from_str(self.media.as_str())
    }

    /// Sets the `media` `String` of `self` from the specified `MediaType`
    pub fn set_mediatype(&mut self, media: MediaType) {
        self.media = media.to_string();
    }

    /// Parses the `proto` `String` of `self` as `TransportProto`
    pub fn parse_transport_proto(&self) -> TransportProto {
        TransportProto::from(self.proto.as_str())
    }

    /// Sets the `proto` `String` of `self` from the specified `TransportProto`
    pub fn set_transport_proto(&mut self, proto: TransportProto) {
        self.proto = proto.to_string();
    }

    /// Gets the first value of the given typed attribute.
    pub fn get_first_attribute_typed<T: TypedAttribute>(
        &self,
    ) -> Option<Result<T, AttributeError>> {
        self.attributes_typed().next()
    }

    /// Gets an iterator over all attribute values of the given name.
    ///
    /// Each item represents the attribute parsing `Result`.
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
    pub fn new(origin: Origin, session_name: impl ToString) -> Self {
        Session {
            origin,
            session_name: session_name.to_string(),
            session_description: Default::default(),
            uri: Default::default(),
            emails: Default::default(),
            phones: Default::default(),
            connection: Default::default(),
            bandwidths: Default::default(),
            times: Default::default(),
            time_zones: Default::default(),
            key: Default::default(),
            attributes: Default::default(),
            medias: Default::default(),
        }
    }

    pub fn builder(origin: Origin, session_name: impl ToString) -> builders::Session {
        builders::Session::new(origin, session_name)
    }

    pub fn set_session_description(&mut self, session_description: impl ToString) {
        self.session_description = Some(session_description.to_string());
    }

    pub fn set_uri(&mut self, uri: impl ToString) {
        self.uri = Some(uri.to_string());
    }

    pub fn add_email(&mut self, email: impl ToString) {
        self.emails.push(email.to_string());
    }

    pub fn add_emails(&mut self, emails: impl IntoIterator<Item = impl ToString>) {
        self.emails
            .extend(emails.into_iter().map(|e| e.to_string()));
    }

    pub fn add_phone(&mut self, phone: impl ToString) {
        self.phones.push(phone.to_string());
    }

    pub fn add_phones(&mut self, phones: impl IntoIterator<Item = impl ToString>) {
        self.phones
            .extend(phones.into_iter().map(|p| p.to_string()));
    }

    pub fn set_connection(&mut self, connection: impl Into<Connection>) {
        self.connection = Some(connection.into());
    }

    pub fn add_bandwidth(&mut self, bandwidth: Bandwidth) {
        self.bandwidths.push(bandwidth);
    }

    pub fn add_bandwidths(&mut self, bandwidths: impl IntoIterator<Item = Bandwidth>) {
        self.bandwidths.extend(bandwidths);
    }

    pub fn add_time(&mut self, time: Time) {
        self.times.push(time);
    }

    pub fn add_times(&mut self, times: impl IntoIterator<Item = Time>) {
        self.times.extend(times);
    }

    pub fn add_time_zone(&mut self, time_zone: TimeZone) {
        self.time_zones.push(time_zone);
    }

    pub fn add_time_zones(&mut self, time_zones: impl IntoIterator<Item = TimeZone>) {
        self.time_zones.extend(time_zones);
    }

    pub fn set_encryption_key(&mut self, key: impl Into<Key>) {
        self.key = Some(key.into());
    }

    pub fn add_attribute(&mut self, attribute: impl Into<Attribute>) {
        self.attributes.push(attribute.into());
    }

    pub fn add_attribute_from_str(&mut self, attribute: impl ToString) {
        self.attributes.push(Attribute::new(attribute));
    }

    pub fn add_attribute_with_value(&mut self, attribute: impl ToString, value: impl ToString) {
        self.attributes
            .push(Attribute::with_value(attribute, value));
    }

    pub fn add_attributes(&mut self, attributes: impl IntoIterator<Item = impl Into<Attribute>>) {
        self.attributes
            .extend(attributes.into_iter().map(|a| a.into()))
    }

    pub fn add_attributes_from_strs(
        &mut self,
        attributes: impl IntoIterator<Item = impl ToString>,
    ) {
        self.attributes
            .extend(attributes.into_iter().map(|a| Attribute::new(a)))
    }

    pub fn add_media(&mut self, media: Media) {
        self.medias.push(media);
    }

    pub fn add_medias(&mut self, medias: impl IntoIterator<Item = Media>) {
        self.medias.extend(medias)
    }

    /// Checks if the given attribute exists.
    pub fn has_attribute(&self, name: &str) -> bool {
        self.attributes.iter().any(|a| a.attribute == name)
    }

    /// Gets the first value of the given attribute.
    ///
    /// The outter `Option` reflects the availability of the attribute.
    /// The inner `Option` reflects the optional value of the attribute.
    pub fn get_first_attribute_value<'a>(&'a self, name: &'a str) -> Option<Option<&'a str>> {
        self.get_attribute_values(name).next()
    }

    /// Gets an iterator over all attribute values of the given name.
    pub fn get_attribute_values<'a>(
        &'a self,
        name: &'a str,
    ) -> impl Iterator<Item = Option<&'a str>> {
        self.attributes
            .iter()
            .filter(move |a| a.attribute == name)
            .map(|a| a.value.as_deref())
    }

    /// Gets the first value of the given typed attribute.
    pub fn get_first_attribute_typed<T: TypedAttribute>(
        &self,
    ) -> Option<Result<T, AttributeError>> {
        self.attributes_typed().next()
    }

    /// Gets an iterator over all attribute values of the given name.
    ///
    /// Each item represents the attribute parsing `Result`.
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
    use std::net::Ipv4Addr;

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
        assert_eq!(parsed.origin.addrtype, AddrType::Ip4);
        assert_ne!(parsed.origin.nettype, NetType::Pstn);
        assert_eq!(
            parsed.origin.try_parse_unicast_ip_address(),
            Ok(IpAddr::V4(std::net::Ipv4Addr::new(10, 47, 16, 5)))
        );
        assert_eq!(parsed.medias[0].try_parse_mediatype(), Ok(MediaType::Audio));
        assert_ne!(
            parsed.medias[1].parse_transport_proto(),
            TransportProto::RtpSavpf,
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

        assert_eq!(f[0].hash_func, HashFunc::Sha256);
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
                            connection_address: std::net::Ipv4Addr::new(126, 16, 64, 4).to_string(),
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
            Some(Some("99 h263-1998/90000"))
        );
        assert_eq!(
            media.get_first_attribute_value("rtcp"),
            Some(Some("53020 IN IP4 126.16.64.4"))
        );
        assert!(media.get_first_attribute_value("foo").is_none());

        assert_eq!(
            media.get_attribute_values("rtpmap").collect::<Vec<_>>(),
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
            Some(Some(
                "100 profile-level-id=42e016;max-mbps=108000;max-fs=3600"
            ))
        );

        let r = media.attributes_typed::<Rtcp>().collect::<Vec<_>>();
        assert_eq!(r[0].as_ref().unwrap().addrtype, AddrType::Ip4);
        assert_eq!(r[0].as_ref().unwrap().port, 53020);

        let rtcp_attr = media.get_first_attribute_typed::<Rtcp>().unwrap().unwrap();
        assert_eq!(rtcp_attr.addrtype, AddrType::Ip4);
        assert_eq!(rtcp_attr.port, 53020);

        assert_eq!(
            media
                .get_attribute_values("fingerprint")
                .collect::<Vec<_>>(),
            &[Some("custom A1:B2:C3:D4:E5:F6")]
        );

        assert!(media.get_attribute_values("foo").next().is_none());
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
            Some(Some("99 h263-1998/90000"))
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

        let rtpmap = session
            .get_first_attribute_typed::<RtpMap>()
            .unwrap()
            .unwrap();
        assert_eq!(rtpmap.clock_rate, 90000);
        assert_eq!(rtpmap.encoding_name, "h263-1998");
        assert_ne!(rtpmap.encoding_name, "h263");
        assert_ne!(rtpmap.encoding_params, Some("2".to_string()));
        assert_eq!(rtpmap.payload_type, 99);

        assert_eq!(session.get_first_attribute_value("rtcp"), Some(None));

        assert!(session.get_first_attribute_value("foo").is_none());

        assert_eq!(
            session.get_attribute_values("rtpmap").collect::<Vec<_>>(),
            &[
                Some("99 h263-1998/90000"),
                Some("100 h264/90000"),
                Some("101 L16/16000/2")
            ]
        );
        assert_eq!(
            session.get_attribute_values("rtcp").collect::<Vec<_>>(),
            &[None]
        );

        assert_eq!(
            session.get_attribute_values("extmap").collect::<Vec<_>>(),
            &[Some("1 URI-toffset")]
        );

        assert!(session.get_attribute_values("foo").next().is_none());

        let a = fallible_iterator::convert(session.attributes_typed::<RtpMap>())
            .collect::<Vec<_>>()
            .expect("Valid vector of attributes");
        assert_eq!(a[2].encoding_name, "L16");
        assert_eq!(a[0].payload_type, 99);
    }

    #[test]
    fn origin_parse_address_error() {
        use std::net::Ipv6Addr;

        let mut origin = Origin {
            username: None,
            sess_id: "1234".to_string(),
            sess_version: 0,
            nettype: NetType::In,
            addrtype: AddrType::Ip4,
            unicast_address: "127.0.0.1".to_string(),
        };

        assert_eq!(origin.addrtype, AddrType::Ip4);
        assert_eq!(
            origin.try_parse_unicast_ip_address().unwrap(),
            IpAddr::V4(Ipv4Addr::LOCALHOST),
        );

        origin.unicast_address = "127.0.0.1/24".to_string();
        assert_eq!(
            origin.try_parse_unicast_ip_address().unwrap_err(),
            origin.unicast_address
        );

        origin.unicast_address = "non-ip".to_string();
        assert_eq!(
            origin.try_parse_unicast_ip_address().unwrap_err(),
            origin.unicast_address
        );

        origin.set_unicast_ip_address(Ipv6Addr::LOCALHOST);
        assert_eq!(origin.addrtype, AddrType::Ip6);
        assert_eq!(
            origin.try_parse_unicast_ip_address().unwrap(),
            IpAddr::V6(Ipv6Addr::LOCALHOST),
        );
    }

    #[test]
    fn connection_parse_address_error() {
        use std::net::Ipv6Addr;

        let mut connection = Connection {
            nettype: NetType::In,
            addrtype: AddrType::Ip4,
            connection_address: "127.0.0.1".to_string(),
        };

        assert_eq!(connection.addrtype, AddrType::Ip4);
        assert_eq!(
            connection.try_parse_connection_ip_address().unwrap(),
            IpAddr::V4(Ipv4Addr::LOCALHOST),
        );

        connection.connection_address = "127.0.0.1/24".to_string();
        assert_eq!(
            connection.try_parse_connection_ip_address().unwrap_err(),
            connection.connection_address
        );

        connection.connection_address = "non-ip".to_string();
        assert_eq!(
            connection.try_parse_connection_ip_address().unwrap_err(),
            connection.connection_address
        );

        connection.set_connection_ip_address(Ipv6Addr::LOCALHOST);
        assert_eq!(connection.addrtype, AddrType::Ip6);
        assert_eq!(
            connection.try_parse_connection_ip_address().unwrap(),
            IpAddr::V6(Ipv6Addr::LOCALHOST),
        );
    }
}
