// Licensed under the MIT license, see the LICENSE file or <http://opensource.org/licenses/MIT>

//! Builders for the `sdp_types` `struct`s

use crate::{attributes, enums};
use std::net::IpAddr;

/// A [`crate::Origin`] builder.
#[derive(Debug)]
pub struct Origin(crate::Origin);
impl Origin {
    /// Construct an [`crate::builders::Origin`] with the specified IP `unicast_address`
    pub fn with_ip_addr(
        sess_id: impl ToString,
        sess_version: u64,
        unicast_address: impl Into<IpAddr>,
    ) -> Self {
        Self(crate::Origin::with_ip_addr(
            sess_id,
            sess_version,
            unicast_address,
        ))
    }

    /// Construct an [`crate::builders::Origin`]
    ///
    /// See also [`crate::Origin::builder_with_ip_addr`]
    pub fn new(
        sess_id: impl ToString,
        sess_version: u64,
        nettype: enums::NetType,
        addrtype: enums::AddrType,
        unicast_address: impl ToString,
    ) -> Self {
        Self(crate::Origin::new(
            sess_id,
            sess_version,
            nettype,
            addrtype,
            unicast_address,
        ))
    }

    pub fn username(mut self, username: impl ToString) -> Self {
        self.0.set_username(username);
        self
    }

    pub fn username_if(mut self, username: impl ToString, predicate: bool) -> Self {
        if predicate {
            self.0.set_username(username);
        }
        self
    }

    pub fn username_if_some(mut self, username: Option<impl ToString>) -> Self {
        if let Some(username) = username {
            self.0.set_username(username);
        }
        self
    }

    pub fn build(self) -> crate::Origin {
        self.0
    }
}

/// A [`crate::Origin`] builder.
#[derive(Debug)]
pub struct Time(crate::Time);
impl Time {
    pub fn new(start_time: u64, stop_time: u64) -> Self {
        Self(crate::Time::new(start_time, stop_time))
    }

    pub fn repeat(mut self, repeat: crate::Repeat) -> Self {
        self.0.add_repeat(repeat);
        self
    }

    pub fn repeat_if(mut self, repeat: crate::Repeat, predicate: bool) -> Self {
        if predicate {
            self.0.add_repeat(repeat);
        }
        self
    }

    pub fn repeat_if_some(mut self, repeat: Option<crate::Repeat>) -> Self {
        if let Some(repeat) = repeat {
            self.0.add_repeat(repeat);
        }
        self
    }

    pub fn repeats(mut self, repeats: impl IntoIterator<Item = crate::Repeat>) -> Self {
        self.0.add_repeats(repeats);
        self
    }

    pub fn repeats_if(
        mut self,
        repeats: impl IntoIterator<Item = crate::Repeat>,
        predicate: bool,
    ) -> Self {
        if predicate {
            self.0.add_repeats(repeats);
        }
        self
    }

    pub fn build(self) -> crate::Time {
        self.0
    }
}

/// A [`crate::Repeat`] builder.
#[derive(Debug)]
pub struct Repeat(crate::Repeat);
impl Repeat {
    pub fn new(repeat_interval: u64, active_duration: u64) -> Self {
        Self(crate::Repeat::new(repeat_interval, active_duration))
    }

    pub fn offset(mut self, repeat: u64) -> Self {
        self.0.add_offset(repeat);
        self
    }

    pub fn offset_if(mut self, offset: u64, predicate: bool) -> Self {
        if predicate {
            self.0.add_offset(offset);
        }
        self
    }

    pub fn offset_if_some(mut self, offset: Option<u64>) -> Self {
        if let Some(offset) = offset {
            self.0.add_offset(offset);
        }
        self
    }

    pub fn offsets(mut self, offsets: impl IntoIterator<Item = u64>) -> Self {
        self.0.add_offsets(offsets);
        self
    }

    pub fn offsets_if(mut self, offsets: impl IntoIterator<Item = u64>, predicate: bool) -> Self {
        if predicate {
            self.0.add_offsets(offsets);
        }
        self
    }

    pub fn build(self) -> crate::Repeat {
        self.0
    }
}

/// A [`crate::Media`] builder.
#[derive(Debug)]
pub struct Media(crate::Media);
impl Media {
    pub fn new(
        media: enums::MediaType,
        port: u16,
        proto: enums::TransportProto,
        fmt: impl ToString,
    ) -> Self {
        Self(crate::Media::new(media, port, proto, fmt))
    }

    pub fn num_ports(mut self, num_ports: u16) -> Self {
        self.0.set_num_ports(num_ports);
        self
    }

    pub fn num_ports_if(mut self, num_ports: u16, predicate: bool) -> Self {
        if predicate {
            self.0.set_num_ports(num_ports);
        }
        self
    }

    pub fn num_ports_if_some(mut self, num_ports: Option<u16>) -> Self {
        if let Some(num_ports) = num_ports {
            self.0.set_num_ports(num_ports);
        }
        self
    }

    pub fn media_title(mut self, media_title: impl ToString) -> Self {
        self.0.set_media_title(media_title);
        self
    }

    pub fn media_title_if(mut self, media_title: impl ToString, predicate: bool) -> Self {
        if predicate {
            self.0.set_media_title(media_title);
        }
        self
    }

    pub fn media_title_if_some(mut self, media_title: Option<impl ToString>) -> Self {
        if let Some(media_title) = media_title {
            self.0.set_media_title(media_title);
        }
        self
    }

    pub fn connection(mut self, connection: impl Into<crate::Connection>) -> Self {
        self.0.add_connection(connection);
        self
    }

    pub fn connection_if(
        mut self,
        connection: impl Into<crate::Connection>,
        predicate: bool,
    ) -> Self {
        if predicate {
            self.0.add_connection(connection);
        }
        self
    }

    pub fn connection_if_some(mut self, connection: Option<impl Into<crate::Connection>>) -> Self {
        if let Some(connection) = connection {
            self.0.add_connection(connection);
        }
        self
    }

    pub fn connections(
        mut self,
        connections: impl IntoIterator<Item = impl Into<crate::Connection>>,
    ) -> Self {
        self.0.add_connections(connections);
        self
    }

    pub fn connections_if(
        mut self,
        connections: impl IntoIterator<Item = impl Into<crate::Connection>>,
        predicate: bool,
    ) -> Self {
        if predicate {
            self.0.add_connections(connections);
        }
        self
    }

    pub fn bandwidth(mut self, bandwidth: crate::Bandwidth) -> Self {
        self.0.add_bandwidth(bandwidth);
        self
    }

    pub fn bandwidth_if(mut self, bandwidth: crate::Bandwidth, predicate: bool) -> Self {
        if predicate {
            self.0.add_bandwidth(bandwidth);
        }
        self
    }

    pub fn bandwidth_if_some(mut self, bandwidth: Option<crate::Bandwidth>) -> Self {
        if let Some(bandwidth) = bandwidth {
            self.0.add_bandwidth(bandwidth);
        }
        self
    }

    pub fn bandwidths(mut self, bandwidths: impl IntoIterator<Item = crate::Bandwidth>) -> Self {
        self.0.add_bandwidths(bandwidths);
        self
    }

    pub fn bandwidths_if(
        mut self,
        bandwidths: impl IntoIterator<Item = crate::Bandwidth>,
        predicate: bool,
    ) -> Self {
        if predicate {
            self.0.add_bandwidths(bandwidths);
        }
        self
    }

    pub fn encryption_key(mut self, key: impl Into<crate::Key>) -> Self {
        self.0.set_encryption_key(key);
        self
    }

    pub fn encryption_key_if(
        mut self,
        encryption_key: impl Into<crate::Key>,
        predicate: bool,
    ) -> Self {
        if predicate {
            self.0.set_encryption_key(encryption_key);
        }
        self
    }

    pub fn encryption_key_if_some(mut self, encryption_key: Option<impl Into<crate::Key>>) -> Self {
        if let Some(encryption_key) = encryption_key {
            self.0.set_encryption_key(encryption_key);
        }
        self
    }

    pub fn attribute(mut self, attribute: impl Into<crate::Attribute>) -> Self {
        self.0.add_attribute(attribute);
        self
    }

    pub fn attribute_if(mut self, attribute: impl Into<crate::Attribute>, predicate: bool) -> Self {
        if predicate {
            self.0.add_attribute(attribute);
        }
        self
    }

    pub fn attribute_if_some(mut self, attribute: Option<impl Into<crate::Attribute>>) -> Self {
        if let Some(attribute) = attribute {
            self.0.add_attribute(attribute);
        }
        self
    }

    pub fn attribute_from_str(mut self, attribute: impl ToString) -> Self {
        self.0.add_attribute_from_str(attribute);
        self
    }

    pub fn attribute_from_str_if(mut self, attribute: impl ToString, predicate: bool) -> Self {
        if predicate {
            self.0.add_attribute_from_str(attribute);
        }
        self
    }

    pub fn attribute_from_str_if_some(mut self, attribute: Option<impl ToString>) -> Self {
        if let Some(attribute) = attribute {
            self.0.add_attribute_from_str(attribute);
        }
        self
    }

    pub fn attribute_with_value(mut self, attribute: impl ToString, value: impl ToString) -> Self {
        self.0.add_attribute_with_value(attribute, value);
        self
    }

    pub fn attribute_with_value_if(
        mut self,
        attribute: impl ToString,
        value: impl ToString,
        predicate: bool,
    ) -> Self {
        if predicate {
            self.0.add_attribute_with_value(attribute, value);
        }
        self
    }

    pub fn attributes(
        mut self,
        attributes: impl IntoIterator<Item = impl Into<crate::Attribute>>,
    ) -> Self {
        self.0.add_attributes(attributes);
        self
    }

    pub fn attributes_if(
        mut self,
        attributes: impl IntoIterator<Item = impl Into<crate::Attribute>>,
        predicate: bool,
    ) -> Self {
        if predicate {
            self.0.add_attributes(attributes);
        }
        self
    }

    pub fn attributes_from_strs(
        mut self,
        attributes: impl IntoIterator<Item = impl ToString>,
    ) -> Self {
        self.0.add_attributes_from_strs(attributes);
        self
    }

    pub fn attributes_from_strs_if(
        mut self,
        attributes: impl IntoIterator<Item = impl ToString>,
        predicate: bool,
    ) -> Self {
        if predicate {
            self.0.add_attributes_from_strs(attributes);
        }
        self
    }

    pub fn build(self) -> crate::Media {
        self.0
    }
}

/// A [`crate::Session`] builder.
#[derive(Debug)]
pub struct Session(crate::Session);
impl Session {
    pub fn new(origin: crate::Origin, session_name: impl ToString) -> Self {
        Self(crate::Session::new(origin, session_name))
    }

    pub fn session_description(mut self, session_description: impl ToString) -> Self {
        self.0.set_session_description(session_description);
        self
    }

    pub fn session_description_if(
        mut self,
        session_description: impl ToString,
        predicate: bool,
    ) -> Self {
        if predicate {
            self.0.set_session_description(session_description);
        }
        self
    }

    pub fn session_description_if_some(
        mut self,
        session_description: Option<impl ToString>,
    ) -> Self {
        if let Some(session_description) = session_description {
            self.0.set_session_description(session_description);
        }
        self
    }

    pub fn uri(mut self, uri: impl ToString) -> Self {
        self.0.set_uri(uri);
        self
    }

    pub fn uri_if(mut self, uri: impl ToString, predicate: bool) -> Self {
        if predicate {
            self.0.set_uri(uri);
        }
        self
    }

    pub fn uri_if_some(mut self, uri: Option<impl ToString>) -> Self {
        if let Some(uri) = uri {
            self.0.set_uri(uri);
        }
        self
    }

    pub fn email(mut self, email: impl ToString) -> Self {
        self.0.add_email(email);
        self
    }

    pub fn email_if(mut self, email: impl ToString, predicate: bool) -> Self {
        if predicate {
            self.0.add_email(email);
        }
        self
    }

    pub fn email_if_some(mut self, email: Option<impl ToString>) -> Self {
        if let Some(email) = email {
            self.0.add_email(email);
        }
        self
    }

    pub fn emails(mut self, emails: impl IntoIterator<Item = impl ToString>) -> Self {
        self.0.add_emails(emails);
        self
    }

    pub fn emails_if(
        mut self,
        emails: impl IntoIterator<Item = impl ToString>,
        predicate: bool,
    ) -> Self {
        if predicate {
            self.0.add_emails(emails);
        }
        self
    }

    pub fn phone(mut self, phone: impl ToString) -> Self {
        self.0.add_phone(phone);
        self
    }

    pub fn phone_if(mut self, phone: impl ToString, predicate: bool) -> Self {
        if predicate {
            self.0.add_phone(phone);
        }
        self
    }

    pub fn phone_if_some(mut self, phone: Option<impl ToString>) -> Self {
        if let Some(phone) = phone {
            self.0.add_phone(phone);
        }
        self
    }

    pub fn phones(mut self, phones: impl IntoIterator<Item = impl ToString>) -> Self {
        self.0.add_phones(phones);
        self
    }

    pub fn phones_if(
        mut self,
        phones: impl IntoIterator<Item = impl ToString>,
        predicate: bool,
    ) -> Self {
        if predicate {
            self.0.add_phones(phones);
        }
        self
    }

    pub fn connection(mut self, connection: impl Into<crate::Connection>) -> Self {
        self.0.set_connection(connection);
        self
    }

    pub fn connection_if(
        mut self,
        connection: impl Into<crate::Connection>,
        predicate: bool,
    ) -> Self {
        if predicate {
            self.0.set_connection(connection);
        }
        self
    }

    pub fn connection_if_some(mut self, connection: Option<impl Into<crate::Connection>>) -> Self {
        if let Some(connection) = connection {
            self.0.set_connection(connection);
        }
        self
    }

    pub fn bandwidth(mut self, bandwidth: crate::Bandwidth) -> Self {
        self.0.add_bandwidth(bandwidth);
        self
    }

    pub fn bandwidth_if(mut self, bandwidth: crate::Bandwidth, predicate: bool) -> Self {
        if predicate {
            self.0.add_bandwidth(bandwidth);
        }
        self
    }

    pub fn bandwidth_if_some(mut self, bandwidth: Option<crate::Bandwidth>) -> Self {
        if let Some(bandwidth) = bandwidth {
            self.0.add_bandwidth(bandwidth);
        }
        self
    }

    pub fn bandwidths(mut self, bandwidths: impl IntoIterator<Item = crate::Bandwidth>) -> Self {
        self.0.add_bandwidths(bandwidths);
        self
    }

    pub fn bandwidths_if(
        mut self,
        bandwidths: impl IntoIterator<Item = crate::Bandwidth>,
        predicate: bool,
    ) -> Self {
        if predicate {
            self.0.add_bandwidths(bandwidths);
        }
        self
    }

    pub fn time(mut self, time: crate::Time) -> Self {
        self.0.add_time(time);
        self
    }

    pub fn time_if(mut self, time: crate::Time, predicate: bool) -> Self {
        if predicate {
            self.0.add_time(time);
        }
        self
    }

    pub fn time_if_some(mut self, time: Option<crate::Time>) -> Self {
        if let Some(time) = time {
            self.0.add_time(time);
        }
        self
    }

    pub fn times(mut self, times: impl IntoIterator<Item = crate::Time>) -> Self {
        self.0.add_times(times);
        self
    }

    pub fn times_if(
        mut self,
        times: impl IntoIterator<Item = crate::Time>,
        predicate: bool,
    ) -> Self {
        if predicate {
            self.0.add_times(times);
        }
        self
    }

    pub fn times_if_some(mut self, times: Option<impl IntoIterator<Item = crate::Time>>) -> Self {
        if let Some(times) = times {
            self.0.add_times(times);
        }
        self
    }

    pub fn time_zone(mut self, time_zone: crate::TimeZone) -> Self {
        self.0.add_time_zone(time_zone);
        self
    }

    pub fn time_zone_if(mut self, time_zone: crate::TimeZone, predicate: bool) -> Self {
        if predicate {
            self.0.add_time_zone(time_zone);
        }
        self
    }

    pub fn time_zone_if_some(mut self, time_zone: Option<crate::TimeZone>) -> Self {
        if let Some(time_zone) = time_zone {
            self.0.add_time_zone(time_zone);
        }
        self
    }

    pub fn time_zones(mut self, time_zones: impl IntoIterator<Item = crate::TimeZone>) -> Self {
        self.0.add_time_zones(time_zones);
        self
    }

    pub fn time_zones_if(
        mut self,
        time_zones: impl IntoIterator<Item = crate::TimeZone>,
        predicate: bool,
    ) -> Self {
        if predicate {
            self.0.add_time_zones(time_zones);
        }
        self
    }

    pub fn encryption_key(mut self, key: impl Into<crate::Key>) -> Self {
        self.0.set_encryption_key(key);
        self
    }

    pub fn encryption_key_if(
        mut self,
        encryption_key: impl Into<crate::Key>,
        predicate: bool,
    ) -> Self {
        if predicate {
            self.0.set_encryption_key(encryption_key);
        }
        self
    }

    pub fn encryption_key_if_some(mut self, encryption_key: Option<impl Into<crate::Key>>) -> Self {
        if let Some(encryption_key) = encryption_key {
            self.0.set_encryption_key(encryption_key);
        }
        self
    }

    pub fn attribute(mut self, attribute: impl Into<crate::Attribute>) -> Self {
        self.0.add_attribute(attribute);
        self
    }

    pub fn attribute_if(mut self, attribute: impl Into<crate::Attribute>, predicate: bool) -> Self {
        if predicate {
            self.0.add_attribute(attribute);
        }
        self
    }

    pub fn attribute_if_some(mut self, attribute: Option<impl Into<crate::Attribute>>) -> Self {
        if let Some(attribute) = attribute {
            self.0.add_attribute(attribute);
        }
        self
    }

    pub fn attribute_from_str(mut self, attribute: impl ToString) -> Self {
        self.0.add_attribute_from_str(attribute);
        self
    }

    pub fn attribute_from_str_if(mut self, attribute: impl ToString, predicate: bool) -> Self {
        if predicate {
            self.0.add_attribute_from_str(attribute);
        }
        self
    }

    pub fn attribute_from_str_if_some(mut self, attribute: Option<impl ToString>) -> Self {
        if let Some(attribute) = attribute {
            self.0.add_attribute_from_str(attribute);
        }
        self
    }

    pub fn attribute_with_value(mut self, attribute: impl ToString, value: impl ToString) -> Self {
        self.0.add_attribute_with_value(attribute, value);
        self
    }

    pub fn attribute_with_value_if(
        mut self,
        attribute: impl ToString,
        value: impl ToString,
        predicate: bool,
    ) -> Self {
        if predicate {
            self.0.add_attribute_with_value(attribute, value);
        }
        self
    }

    pub fn attributes(
        mut self,
        attributes: impl IntoIterator<Item = impl Into<crate::Attribute>>,
    ) -> Self {
        self.0.add_attributes(attributes);
        self
    }

    pub fn attributes_if(
        mut self,
        attributes: impl IntoIterator<Item = impl Into<crate::Attribute>>,
        predicate: bool,
    ) -> Self {
        if predicate {
            self.0.add_attributes(attributes);
        }
        self
    }

    pub fn attributes_if_some(
        mut self,
        attributes: Option<impl IntoIterator<Item = impl Into<crate::Attribute>>>,
    ) -> Self {
        if let Some(attributes) = attributes {
            self.0.add_attributes(attributes);
        }
        self
    }

    pub fn attributes_from_strs(
        mut self,
        attributes: impl IntoIterator<Item = impl ToString>,
    ) -> Self {
        self.0.add_attributes_from_strs(attributes);
        self
    }

    pub fn attributes_from_strs_if(
        mut self,
        attributes: impl IntoIterator<Item = impl ToString>,
        predicate: bool,
    ) -> Self {
        if predicate {
            self.0.add_attributes_from_strs(attributes);
        }
        self
    }

    pub fn media(mut self, media: crate::Media) -> Self {
        self.0.add_media(media);
        self
    }

    pub fn media_if(mut self, media: crate::Media, predicate: bool) -> Self {
        if predicate {
            self.0.add_media(media);
        }
        self
    }

    pub fn media_if_some(mut self, media: Option<crate::Media>) -> Self {
        if let Some(media) = media {
            self.0.add_media(media);
        }
        self
    }

    pub fn medias(mut self, medias: impl IntoIterator<Item = crate::Media>) -> Self {
        self.0.add_medias(medias);
        self
    }

    pub fn medias_if(
        mut self,
        medias: impl IntoIterator<Item = crate::Media>,
        predicate: bool,
    ) -> Self {
        if predicate {
            self.0.add_medias(medias);
        }
        self
    }

    pub fn build(self) -> crate::Session {
        self.0
    }
}

// attributes

/// A [`crate::RtpMap`] builder.
#[derive(Debug)]
pub struct RtpMap(attributes::RtpMap);
impl RtpMap {
    pub fn new(payload_type: u8, encoding_name: impl ToString, clock_rate: u32) -> Self {
        Self(attributes::RtpMap::new(
            payload_type,
            encoding_name,
            clock_rate,
        ))
    }

    pub fn encoding_params(mut self, encoding_params: impl ToString) -> Self {
        self.0.set_encoding_params(encoding_params);
        self
    }

    pub fn encoding_params_if(mut self, encoding_params: impl ToString, predicate: bool) -> Self {
        if predicate {
            self.0.set_encoding_params(encoding_params);
        }
        self
    }

    pub fn build(self) -> attributes::RtpMap {
        self.0
    }
}

/// An [`crate::FmtpParam`] builder.
#[derive(Debug)]
pub struct FmtpParam(attributes::FmtpParam);
impl FmtpParam {
    pub fn new(param: impl ToString) -> Self {
        Self(attributes::FmtpParam::new(param))
    }

    pub fn value(mut self, value: impl ToString) -> Self {
        self.0.set_value(value);
        self
    }

    pub fn value_if(mut self, value: impl ToString, predicate: bool) -> Self {
        if predicate {
            self.0.set_value(value);
        }
        self
    }

    pub fn value_if_some(mut self, value: Option<impl ToString>) -> Self {
        if let Some(value) = value {
            self.0.set_value(value);
        }
        self
    }

    pub fn build(self) -> attributes::FmtpParam {
        self.0
    }
}

/// An [`crate::Fmtp`] builder.
#[derive(Debug)]
pub struct Fmtp(attributes::Fmtp);
impl Fmtp {
    pub fn new(fmt: u8) -> Self {
        Self(attributes::Fmtp::new(fmt))
    }

    pub fn format_specific_param(mut self, format_specific_param: attributes::FmtpParam) -> Self {
        self.0.add_format_specific_param(format_specific_param);
        self
    }

    pub fn format_specific_param_if(
        mut self,
        format_specific_param: attributes::FmtpParam,
        predicate: bool,
    ) -> Self {
        if predicate {
            self.0.add_format_specific_param(format_specific_param);
        }
        self
    }

    pub fn format_specific_param_if_some(
        mut self,
        format_specific_param: Option<attributes::FmtpParam>,
    ) -> Self {
        if let Some(format_specific_param) = format_specific_param {
            self.0.add_format_specific_param(format_specific_param);
        }
        self
    }

    pub fn format_specific_params(
        mut self,
        format_specific_params: impl IntoIterator<Item = attributes::FmtpParam>,
    ) -> Self {
        self.0.add_format_specific_params(format_specific_params);
        self
    }

    pub fn format_specific_params_if(
        mut self,
        format_specific_params: impl IntoIterator<Item = attributes::FmtpParam>,
        predicate: bool,
    ) -> Self {
        if predicate {
            self.0.add_format_specific_params(format_specific_params);
        }
        self
    }

    pub fn build(self) -> attributes::Fmtp {
        self.0
    }
}

/// An [`crate::ExtMap`] builder.
#[derive(Debug)]
pub struct ExtMap(attributes::ExtMap);
impl ExtMap {
    pub fn new(id: u8, uri: impl ToString) -> Self {
        Self(attributes::ExtMap::new(id, uri))
    }

    pub fn direction(mut self, direction: attributes::Direction) -> Self {
        self.0.set_direction(direction);
        self
    }

    pub fn direction_if(mut self, direction: attributes::Direction, predicate: bool) -> Self {
        if predicate {
            self.0.set_direction(direction);
        }
        self
    }

    pub fn direction_if_some(mut self, direction: Option<attributes::Direction>) -> Self {
        if let Some(direction) = direction {
            self.0.set_direction(direction);
        }
        self
    }

    pub fn attributes(mut self, attributes: impl ToString) -> Self {
        self.0.set_attributes(attributes);
        self
    }

    pub fn attributes_if(mut self, attributes: impl ToString, predicate: bool) -> Self {
        if predicate {
            self.0.set_attributes(attributes);
        }
        self
    }

    pub fn build(self) -> attributes::ExtMap {
        self.0
    }
}

/// A [`crate::Fingerprint`] builder.
#[derive(Debug)]
pub struct Fingerprint(attributes::Fingerprint);
impl Fingerprint {
    pub fn new(hash_func: enums::HashFunc) -> Self {
        Self(attributes::Fingerprint::new(hash_func))
    }

    pub fn fingerprint(mut self, fingerprint: impl IntoIterator<Item = u8>) -> Self {
        self.0.set_fingerprint(fingerprint);
        self
    }

    pub fn fingerprint_if(
        mut self,
        fingerprint: impl IntoIterator<Item = u8>,
        predicate: bool,
    ) -> Self {
        if predicate {
            self.0.set_fingerprint(fingerprint);
        }
        self
    }

    pub fn fingerprint_if_some(
        mut self,
        fingerprint: Option<impl IntoIterator<Item = u8>>,
    ) -> Self {
        if let Some(fingerprint) = fingerprint {
            self.0.set_fingerprint(fingerprint);
        }
        self
    }

    pub fn build(self) -> attributes::Fingerprint {
        self.0
    }
}

/// A [`crate::Group`] builder.
#[derive(Debug)]
pub struct Group(attributes::Group);
impl Group {
    pub fn new(semantics: enums::GroupSemantics) -> Self {
        Self(attributes::Group::new(semantics))
    }

    pub fn mid_tag(mut self, mid_tag: impl ToString) -> Self {
        self.0.add_mid_tag(mid_tag);
        self
    }

    pub fn mid_tag_if(mut self, mid_tag: impl ToString, predicate: bool) -> Self {
        if predicate {
            self.0.add_mid_tag(mid_tag);
        }
        self
    }

    pub fn mid_tag_if_some(mut self, mid_tag: Option<impl ToString>) -> Self {
        if let Some(mid_tag) = mid_tag {
            self.0.add_mid_tag(mid_tag);
        }
        self
    }

    pub fn mid_tags(mut self, mid_tags: impl IntoIterator<Item = impl ToString>) -> Self {
        self.0.add_mid_tags(mid_tags);
        self
    }

    pub fn mid_tags_if(
        mut self,
        mid_tags: impl IntoIterator<Item = impl ToString>,
        predicate: bool,
    ) -> Self {
        if predicate {
            self.0.add_mid_tags(mid_tags);
        }
        self
    }

    pub fn build(self) -> attributes::Group {
        self.0
    }
}

/// An [`crate::Ssrc`] builder.
#[derive(Debug)]
pub struct Ssrc(attributes::Ssrc);
impl Ssrc {
    pub fn new(ssrc_id: u32, attribute: enums::SsrcAttribute) -> Self {
        Self(attributes::Ssrc::new(ssrc_id, attribute))
    }

    pub fn value(mut self, value: impl ToString) -> Self {
        self.0.set_value(value);
        self
    }

    pub fn value_if(mut self, value: impl ToString, predicate: bool) -> Self {
        if predicate {
            self.0.set_value(value);
        }
        self
    }

    pub fn value_if_some(mut self, value: Option<impl ToString>) -> Self {
        if let Some(value) = value {
            self.0.set_value(value);
        }
        self
    }

    pub fn build(self) -> attributes::Ssrc {
        self.0
    }
}

/// An [`crate::SsrcGroup`] builder.
#[derive(Debug)]
pub struct SsrcGroup(attributes::SsrcGroup);
impl SsrcGroup {
    pub fn new(semantics: enums::GroupSemantics) -> Self {
        Self(attributes::SsrcGroup::new(semantics))
    }

    pub fn ssrc_id(mut self, ssrc_id: u32) -> Self {
        self.0.add_ssrc_id(ssrc_id);
        self
    }

    pub fn ssrc_id_if(mut self, ssrc_id: u32, predicate: bool) -> Self {
        if predicate {
            self.0.add_ssrc_id(ssrc_id);
        }
        self
    }

    pub fn ssrc_id_if_some(mut self, ssrc_id: Option<u32>) -> Self {
        if let Some(ssrc_id) = ssrc_id {
            self.0.add_ssrc_id(ssrc_id);
        }
        self
    }

    pub fn ssrc_ids(mut self, ssrc_ids: impl IntoIterator<Item = u32>) -> Self {
        self.0.add_ssrc_ids(ssrc_ids);
        self
    }

    pub fn ssrc_ids_if(mut self, ssrc_ids: impl IntoIterator<Item = u32>, predicate: bool) -> Self {
        if predicate {
            self.0.add_ssrc_ids(ssrc_ids);
        }
        self
    }

    pub fn build(self) -> attributes::SsrcGroup {
        self.0
    }
}

/// A [`crate::SrtpKeyParam`] builder.
#[derive(Debug)]
pub struct SrtpKeyParam(attributes::SrtpKeyParam);
impl SrtpKeyParam {
    pub fn new(key_and_salt: impl ToString) -> Self {
        Self(attributes::SrtpKeyParam::new(key_and_salt))
    }

    pub fn lifetime(mut self, lifetime: u32) -> Self {
        self.0.set_lifetime(lifetime);
        self
    }

    pub fn lifetime_if(mut self, lifetime: u32, predicate: bool) -> Self {
        if predicate {
            self.0.set_lifetime(lifetime);
        }
        self
    }

    pub fn lifetime_if_some(mut self, lifetime: Option<u32>) -> Self {
        if let Some(lifetime) = lifetime {
            self.0.set_lifetime(lifetime);
        }
        self
    }

    pub fn mki_and_length(mut self, mki: u32, length: u32) -> Self {
        self.0.set_mki_and_length(mki, length);
        self
    }

    pub fn mki_and_length_if(mut self, mki: u32, length: u32, predicate: bool) -> Self {
        if predicate {
            self.0.set_mki_and_length(mki, length);
        }
        self
    }

    pub fn build(self) -> attributes::SrtpKeyParam {
        self.0
    }
}

/// A [`crate::Crypto`] builder.
#[derive(Debug)]
pub struct Crypto(attributes::Crypto);
impl Crypto {
    pub fn new(tag: u32, crypto_suite: enums::CryptoSuite) -> Self {
        Self(attributes::Crypto::new(tag, crypto_suite))
    }

    pub fn key_param(mut self, key_param: attributes::SrtpKeyParam) -> Self {
        self.0.add_key_param(key_param);
        self
    }

    pub fn key_param_if(mut self, key_param: attributes::SrtpKeyParam, predicate: bool) -> Self {
        if predicate {
            self.0.add_key_param(key_param);
        }
        self
    }

    pub fn key_param_if_some(mut self, key_param: Option<attributes::SrtpKeyParam>) -> Self {
        if let Some(key_param) = key_param {
            self.0.add_key_param(key_param);
        }
        self
    }

    pub fn key_params(
        mut self,
        key_params: impl IntoIterator<Item = attributes::SrtpKeyParam>,
    ) -> Self {
        self.0.add_key_params(key_params);
        self
    }

    pub fn key_params_if(
        mut self,
        key_params: impl IntoIterator<Item = attributes::SrtpKeyParam>,
        predicate: bool,
    ) -> Self {
        if predicate {
            self.0.add_key_params(key_params);
        }
        self
    }

    pub fn key_params_if_some(
        mut self,
        key_params: Option<impl IntoIterator<Item = attributes::SrtpKeyParam>>,
    ) -> Self {
        if let Some(key_params) = key_params {
            self.0.add_key_params(key_params);
        }
        self
    }

    pub fn session_param(mut self, session_param: enums::SrtpSessionParam) -> Self {
        self.0.add_session_param(session_param);
        self
    }

    pub fn session_param_if(
        mut self,
        session_param: enums::SrtpSessionParam,
        predicate: bool,
    ) -> Self {
        if predicate {
            self.0.add_session_param(session_param);
        }
        self
    }

    pub fn session_param_if_some(mut self, session_param: Option<enums::SrtpSessionParam>) -> Self {
        if let Some(session_param) = session_param {
            self.0.add_session_param(session_param);
        }
        self
    }

    pub fn session_params(
        mut self,
        session_params: impl IntoIterator<Item = enums::SrtpSessionParam>,
    ) -> Self {
        self.0.add_session_params(session_params);
        self
    }

    pub fn session_params_if(
        mut self,
        session_params: impl IntoIterator<Item = enums::SrtpSessionParam>,
        predicate: bool,
    ) -> Self {
        if predicate {
            self.0.add_session_params(session_params);
        }
        self
    }

    pub fn build(self) -> attributes::Crypto {
        self.0
    }
}

/// A [`crate::Candidate`] builder.
#[derive(Debug)]
pub struct Candidate(attributes::Candidate);
impl Candidate {
    pub fn new(
        foundation: impl ToString,
        component_id: u32,
        transport: impl ToString,
        priority: u64,
        address: enums::CandidateAddress,
        port: u16,
        typ: enums::CandidateType,
    ) -> Self {
        Self(attributes::Candidate::new(
            foundation,
            component_id,
            transport,
            priority,
            address,
            port,
            typ,
        ))
    }

    pub fn rel_addr(mut self, rel_addr: impl Into<IpAddr>) -> Self {
        self.0.set_rel_addr(rel_addr);
        self
    }

    pub fn rel_addr_if(mut self, rel_addr: impl Into<IpAddr>, predicate: bool) -> Self {
        if predicate {
            self.0.set_rel_addr(rel_addr);
        }
        self
    }

    pub fn rel_addr_if_some(mut self, rel_addr: Option<impl Into<IpAddr>>) -> Self {
        if let Some(rel_addr) = rel_addr {
            self.0.set_rel_addr(rel_addr);
        }
        self
    }

    pub fn rel_port(mut self, rel_port: u16) -> Self {
        self.0.set_rel_port(rel_port);
        self
    }

    pub fn rel_port_if(mut self, rel_port: u16, predicate: bool) -> Self {
        if predicate {
            self.0.set_rel_port(rel_port);
        }
        self
    }

    pub fn rel_port_if_some(mut self, rel_port: Option<u16>) -> Self {
        if let Some(rel_port) = rel_port {
            self.0.set_rel_port(rel_port);
        }
        self
    }

    pub fn extension(mut self, name: impl ToString, value: impl ToString) -> Self {
        self.0.add_extension(name, value);
        self
    }

    pub fn extension_if(
        mut self,
        name: impl ToString,
        value: impl ToString,
        predicate: bool,
    ) -> Self {
        if predicate {
            self.0.add_extension(name, value);
        }
        self
    }

    pub fn build(self) -> attributes::Candidate {
        self.0
    }
}

#[cfg(test)]
mod test {
    use crate::Rtcp;

    #[test]
    fn builder() {
        use crate::{
            Direction, Media, MediaType, Origin, RtpMap, Session, Ssrc, SsrcAttribute,
            TransportProto,
        };
        use std::{net::Ipv4Addr, str::FromStr};

        const SESSION_ID: &str = "1234";
        const SESSION_NAME: &str = "sdp-type-builder-test";
        const USER_NAME: &str = "test-user";
        const UNICAST_ADDRESS: &str = "192.168.0.2";
        const CONNECTION_ADDRESS: &str = "192.168.0.3";
        const CONNECTION_PORT: u16 = 5000;
        const SENDER_ADDRESS: &str = "192.168.0.4";
        const SENDER_RTCP_PORT: u16 = 5001;
        const PT: u8 = 96;
        const ENCODING_NAME: &str = "L24";
        const AUDIO_RATE: u32 = 48_000;
        const AUDIO_PARAMS: u8 = 2;
        const SSRC: u32 = 1234;
        const CNAME: &str = "user@test.org";
        const REFCLK: &str = "local";

        let sdp = Session::builder(
            Origin::builder_with_ip_addr(
                SESSION_ID,
                0,
                Ipv4Addr::from_str(UNICAST_ADDRESS).unwrap(),
            )
            .username(USER_NAME)
            .build(),
            SESSION_NAME,
        )
        .connection(Ipv4Addr::from_str(CONNECTION_ADDRESS).unwrap())
        .attribute(Direction::SendOnly)
        .media(
            Media::builder(
                MediaType::Audio,
                CONNECTION_PORT,
                TransportProto::RtpAvp,
                PT,
            )
            .attribute(RtpMap::with_encoding_params(
                PT,
                ENCODING_NAME,
                AUDIO_RATE,
                AUDIO_PARAMS,
            ))
            .attribute(Ssrc::with_value(SSRC, SsrcAttribute::Cname, CNAME))
            .attribute(Ssrc::with_value(
                SSRC,
                SsrcAttribute::new("ts-refclk"),
                REFCLK,
            ))
            .attribute(Ssrc::with_typed_attribute(
                SSRC,
                Rtcp::with_ip_addr(
                    SENDER_RTCP_PORT,
                    Ipv4Addr::from_str(SENDER_ADDRESS).unwrap(),
                ),
            ))
            .attribute_from_str("rtcp-mux")
            .build(),
        )
        .build();

        use crate::{AddrType, Attribute, Connection, NetType};
        assert_eq!(
            sdp,
            Session {
                origin: Origin {
                    username: Some(USER_NAME.to_string()),
                    sess_id: SESSION_ID.to_string(),
                    sess_version: 0,
                    nettype: NetType::In,
                    addrtype: AddrType::Ip4,
                    unicast_address: UNICAST_ADDRESS.to_string(),
                },
                session_name: SESSION_NAME.to_string(),
                connection: Some(Connection {
                    nettype: NetType::In,
                    addrtype: AddrType::Ip4,
                    connection_address: CONNECTION_ADDRESS.to_string(),
                }),
                session_description: None,
                uri: None,
                emails: vec![],
                phones: vec![],
                bandwidths: vec![],
                times: vec![],
                time_zones: vec![],
                key: None,
                attributes: vec![Attribute {
                    attribute: "sendonly".to_string(),
                    value: None
                }],
                medias: vec![Media {
                    media: MediaType::Audio.to_string(),
                    port: CONNECTION_PORT,
                    num_ports: None,
                    proto: TransportProto::RtpAvp.to_string(),
                    fmt: PT.to_string(),
                    media_title: None,
                    connections: vec![],
                    bandwidths: vec![],
                    key: None,
                    attributes: vec![
                        Attribute {
                            attribute: "rtpmap".to_string(),
                            value: Some(format!(
                                "{PT} {ENCODING_NAME}/{AUDIO_RATE}/{AUDIO_PARAMS}"
                            )),
                        },
                        Attribute {
                            attribute: "ssrc".to_string(),
                            value: Some(format!("{SSRC} cname:{CNAME}")),
                        },
                        Attribute {
                            attribute: "ssrc".to_string(),
                            value: Some(format!("{SSRC} ts-refclk:{REFCLK}")),
                        },
                        Attribute {
                            attribute: "ssrc".to_string(),
                            value: Some(format!(
                                "{SSRC} rtcp:{SENDER_RTCP_PORT} IN IP4 {SENDER_ADDRESS}"
                            )),
                        },
                        Attribute {
                            attribute: "rtcp-mux".to_string(),
                            value: None,
                        },
                    ],
                }],
            }
        )
    }
}
