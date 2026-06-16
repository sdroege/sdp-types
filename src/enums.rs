// Copyright (C) 2026 Taruntej Kanakamalla <tarun@centricular.com>
//
// Licensed under the MIT license, see the LICENSE file or <http://opensource.org/licenses/MIT>

//! Contains all the helper enums used by a Session, Media and other Attribute structs

use std::{
    fmt::{Display, Write},
    net::IpAddr,
    str::FromStr,
};

use crate::{attributes::SrtpKeyParam, TypedAttribute};

/// Errors while parsing strings to Enum
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum ParseEnumError {
    Invalid(String),
}

impl std::error::Error for ParseEnumError {}

impl std::fmt::Display for ParseEnumError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseEnumError::Invalid(s) => {
                write!(f, "Failed to parse {s} as an enum type")
            }
        }
    }
}

/// Type of network of the originator or a connection of the session.
///
/// See [RFC 8866 Section 5.2](https://datatracker.ietf.org/doc/html/rfc8866#section-5.2),
/// [RFC 8866 Section 5.7](https://datatracker.ietf.org/doc/html/rfc8866#section-5.7) and
/// [RFC 8866 Section 8.2.6](https://datatracker.ietf.org/doc/html/rfc8866#section-8.2.6) for more details
#[derive(Debug, PartialEq, Eq, Clone)]
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
    /// Other
    Other(String),
}

impl NetType {
    pub fn as_str(&self) -> &str {
        match self {
            NetType::In => "IN",
            NetType::Tn => "TN",
            NetType::Atm => "ATM",
            NetType::Pstn => "PSTN",
            NetType::Other(nettype) => nettype.as_str(),
        }
    }
}

impl FromStr for NetType {
    // FIXME use the never type when it is stable
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if "IN".eq_ignore_ascii_case(s) {
            Ok(NetType::In)
        } else if "TN".eq_ignore_ascii_case(s) {
            Ok(NetType::Tn)
        } else if "ATM".eq_ignore_ascii_case(s) {
            Ok(NetType::Atm)
        } else if "PSTN".eq_ignore_ascii_case(s) {
            Ok(NetType::Pstn)
        } else {
            Ok(NetType::Other(s.to_string()))
        }
    }
}

impl From<&str> for NetType {
    fn from(value: &str) -> Self {
        NetType::from_str(value).expect("infallible")
    }
}

impl Display for NetType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

/// Type of address of the originator or a connection of the session
///
/// See [RFC 8866 Section 5.2](https://datatracker.ietf.org/doc/html/rfc8866#section-5.2),
/// [RFC 8866 Section 5.7](https://datatracker.ietf.org/doc/html/rfc8866#section-5.7)
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum AddrType {
    /// IPv4 address
    Ip4,
    /// IPv6 address
    Ip6,
    /// Other
    Other(String),
}

impl AddrType {
    pub fn new(addrtype: impl AsRef<str>) -> Self {
        let addrtype = addrtype.as_ref();
        if "IP4".eq_ignore_ascii_case(addrtype) {
            AddrType::Ip4
        } else if "IP6".eq_ignore_ascii_case(addrtype) {
            AddrType::Ip6
        } else {
            AddrType::Other(addrtype.to_string())
        }
    }

    /// Whether `self` matches an IPv4 or IPv6 address.
    pub fn is_ip(&self) -> bool {
        matches!(self, AddrType::Ip4 | AddrType::Ip6)
    }

    pub fn as_str(&self) -> &str {
        match self {
            AddrType::Ip4 => "IP4",
            AddrType::Ip6 => "IP6",
            AddrType::Other(other) => other.as_str(),
        }
    }
}

impl FromStr for AddrType {
    // FIXME use the never type when it is stable
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if "IP4".eq_ignore_ascii_case(s) {
            Ok(AddrType::Ip4)
        } else if "IP6".eq_ignore_ascii_case(s) {
            Ok(AddrType::Ip6)
        } else {
            Ok(AddrType::Other(s.to_string()))
        }
    }
}

impl From<&str> for AddrType {
    fn from(value: &str) -> Self {
        AddrType::from_str(value).expect("infallible")
    }
}

impl From<IpAddr> for AddrType {
    fn from(addr: IpAddr) -> Self {
        match addr {
            IpAddr::V4(_) => AddrType::Ip4,
            IpAddr::V6(_) => AddrType::Ip6,
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
/// See [RFC 8866 Section 5.8](https://datatracker.ietf.org/doc/html/rfc8866#section-5.8) for more details.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum BandwidthType {
    /// Conference total - maximum bandwidth a session will use
    Ct,
    /// Application Specific maximum bandwidth
    As,
    /// Bandwidth assigned for RTCP reports by active receivers. See [RFC 3890 Section 1.1.3](https://datatracker.ietf.org/doc/html/rfc3890#section-1.1.3)
    Rr,
    /// Bandwidth assigned for RTCP reports by active senders. See [RFC 3890 Section 1.1.3](https://datatracker.ietf.org/doc/html/rfc3890#section-1.1.3)
    Rs,
}

impl BandwidthType {
    pub fn as_str(&self) -> &'static str {
        match self {
            BandwidthType::As => "AS",
            BandwidthType::Ct => "CT",
            BandwidthType::Rr => "RR",
            BandwidthType::Rs => "RS",
        }
    }
}

impl FromStr for BandwidthType {
    type Err = ParseEnumError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if "AS".eq_ignore_ascii_case(s) {
            Ok(BandwidthType::As)
        } else if "CT".eq_ignore_ascii_case(s) {
            Ok(BandwidthType::Ct)
        } else if "RR".eq_ignore_ascii_case(s) {
            Ok(BandwidthType::Rr)
        } else if "RS".eq_ignore_ascii_case(s) {
            Ok(BandwidthType::Rs)
        } else {
            Err(ParseEnumError::Invalid(s.to_string()))
        }
    }
}

impl Display for BandwidthType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

/// Method of encryption (Obsolete)
///
/// Note: This field is obsolete and MUST NOT be used. It is included only for legacy reasons
/// See [RFC 8866 Section 5.12](https://datatracker.ietf.org/doc/html/rfc8866#section-5.12)
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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
        if "audio".eq_ignore_ascii_case(s) {
            Ok(MediaType::Audio)
        } else if "video".eq_ignore_ascii_case(s) {
            Ok(MediaType::Video)
        } else if "text".eq_ignore_ascii_case(s) {
            Ok(MediaType::Text)
        } else if "application".eq_ignore_ascii_case(s) {
            Ok(MediaType::Application)
        } else if "message".eq_ignore_ascii_case(s) {
            Ok(MediaType::Message)
        } else if "image".eq_ignore_ascii_case(s) {
            Ok(MediaType::Image)
        } else {
            Err(ParseEnumError::Invalid(s.to_string()))
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
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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

/// Secure Hash functions
///
/// See [RFC 8122 Table 1](https://datatracker.ietf.org/doc/html/rfc8122#section-8)
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum HashFunc {
    SHA1,
    SHA224,
    SHA256,
    SHA384,
    SHA512,
    MD5,
    MD2,
    Other(String),
}

impl HashFunc {
    pub fn new(hash_func: impl AsRef<str>) -> Self {
        let hash_func = hash_func.as_ref();

        if hash_func.eq_ignore_ascii_case("sha-1") {
            HashFunc::SHA1
        } else if hash_func.eq_ignore_ascii_case("sha-224") {
            HashFunc::SHA224
        } else if hash_func.eq_ignore_ascii_case("sha-256") {
            HashFunc::SHA256
        } else if hash_func.eq_ignore_ascii_case("sha-384") {
            HashFunc::SHA384
        } else if hash_func.eq_ignore_ascii_case("sha-512") {
            HashFunc::SHA512
        } else if hash_func.eq_ignore_ascii_case("md-5") {
            HashFunc::MD5
        } else if hash_func.eq_ignore_ascii_case("md-2") {
            HashFunc::MD2
        } else {
            HashFunc::Other(hash_func.to_string())
        }
    }

    pub fn as_str(&self) -> &str {
        match self {
            HashFunc::SHA1 => "sha-1",
            HashFunc::SHA224 => "sha-224",
            HashFunc::SHA256 => "sha-256",
            HashFunc::SHA384 => "sha-384",
            HashFunc::SHA512 => "sha-512",
            HashFunc::MD5 => "md-5",
            HashFunc::MD2 => "md-2",
            HashFunc::Other(s) => s.as_str(),
        }
    }
}

/// Semantics for Group Attribute
///
/// See [RFC 5576 Section 12.3](https://datatracker.ietf.org/doc/html/rfc5576#section-12.3)
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum GroupSemantics {
    /// Lip Synchronization
    ///
    /// See [RFC 5888 Section 7](https://datatracker.ietf.org/doc/html/rfc5888#section-7)
    LS,
    /// Flow Identification
    ///
    /// See [RFC 5888 Section 8](https://datatracker.ietf.org/doc/html/rfc5888#section-8)
    FID,
    /// Single Reservation Flow
    ///
    /// See [RFC 3524 Section 2](https://datatracker.ietf.org/doc/html/rfc3524#section-2)
    SRF,
    /// Alternative Network Address Types
    ///
    /// See [RFC 4091 Section 3](https://datatracker.ietf.org/doc/html/rfc4091#section-3)
    ANAT,
    /// Forward Error Correction
    ///
    /// See [RFC 4756 Section 4](https://datatracker.ietf.org/doc/html/rfc4756#section-4)
    FEC,
    /// Decoding Dependency
    ///
    /// See [RFC 5583 Section 5.2.1](https://datatracker.ietf.org/doc/html/rfc5583#section-5.2.1)
    DDP,
    /// Other Semantics
    Other(String),
}

impl GroupSemantics {
    pub fn new(semantics: impl AsRef<str>) -> Self {
        let semantics = semantics.as_ref();

        if "LS".eq_ignore_ascii_case(semantics) {
            GroupSemantics::LS
        } else if "FID".eq_ignore_ascii_case(semantics) {
            GroupSemantics::FID
        } else if "SRF".eq_ignore_ascii_case(semantics) {
            GroupSemantics::SRF
        } else if "ANAT".eq_ignore_ascii_case(semantics) {
            GroupSemantics::ANAT
        } else if "FEC".eq_ignore_ascii_case(semantics) {
            GroupSemantics::FEC
        } else if "DDP".eq_ignore_ascii_case(semantics) {
            GroupSemantics::DDP
        } else {
            GroupSemantics::Other(semantics.to_string())
        }
    }

    pub fn as_str(&self) -> &str {
        match self {
            GroupSemantics::LS => "LS",
            GroupSemantics::FID => "FID",
            GroupSemantics::SRF => "SRF",
            GroupSemantics::ANAT => "ANAT",
            GroupSemantics::DDP => "DDP",
            GroupSemantics::FEC => "FEC",
            GroupSemantics::Other(s) => s.as_str(),
        }
    }
}

/// Encryption and authentication algorithms for Crypto attribute
///
/// See [RFC 4568 Section 10.3.2](https://datatracker.ietf.org/doc/html/rfc4568#section-10.3.2)
///
/// Note: `F8_128_HMAC_SHA1_32` appears in the [RFC 4568 Section 9.2](https://datatracker.ietf.org/doc/html/rfc4568#section-9.2)
/// grammar but was never registered in the IANA registry, so it is not defined as a variant here.
/// It will be parsed as `Other`.
#[derive(Debug, PartialEq, Clone, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum CryptoSuite {
    /// AES_CM_128_HMAC_SHA1_80
    AesCm128HmacSha1_80,
    /// AES_CM_128_HMAC_SHA1_32
    AesCm128HmacSha1_32,
    /// F8_128_HMAC_SHA1_80
    F8_128HmacSha1_80,
    /// Other Crypto Suite
    Other(String),
}

impl CryptoSuite {
    pub fn new(crypto_suite: impl AsRef<str>) -> Self {
        let crypto_suite = crypto_suite.as_ref();

        if "AES_CM_128_HMAC_SHA1_32".eq_ignore_ascii_case(crypto_suite) {
            CryptoSuite::AesCm128HmacSha1_32
        } else if "F8_128_HMAC_SHA1_80".eq_ignore_ascii_case(crypto_suite) {
            CryptoSuite::F8_128HmacSha1_80
        } else if "AES_CM_128_HMAC_SHA1_80".eq_ignore_ascii_case(crypto_suite) {
            CryptoSuite::AesCm128HmacSha1_80
        } else {
            CryptoSuite::Other(crypto_suite.to_string())
        }
    }

    pub fn as_str(&self) -> &str {
        match self {
            CryptoSuite::AesCm128HmacSha1_80 => "AES_CM_128_HMAC_SHA1_80",
            CryptoSuite::AesCm128HmacSha1_32 => "AES_CM_128_HMAC_SHA1_32",
            CryptoSuite::F8_128HmacSha1_80 => "F8_128_HMAC_SHA1_80",
            CryptoSuite::Other(s) => s.as_str(),
        }
    }
}

/// Signals whether FEC is applied before or after SRTP processing
///
/// See [RFC 4568 Section 6.3.5](https://datatracker.ietf.org/doc/html/rfc4568#section-6.3.5)
#[derive(Debug, PartialEq, Clone, Eq, Copy)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum FecOrder {
    /// FEC is applied before SRTP processing
    FecSrtp,
    /// FEC is applied after SRTP processing
    SrtpFec,
}

/// SRTP session parameters
///
/// See [RFC 4568 Section 6.3](https://datatracker.ietf.org/doc/html/rfc4568#section-6.3)
#[derive(Debug, PartialEq, Clone, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum SrtpSessionParam {
    /// Key Derivation Rate
    ///
    /// See [RFC 4568 Section 6.3.1](https://datatracker.ietf.org/doc/html/rfc4568#section-6.3.1)
    Kdr(u8),
    /// Signals that the SRTP packets are without encryption
    ///
    /// See [RFC 4568 Section 6.3.2](https://datatracker.ietf.org/doc/html/rfc4568#section-6.3.2)
    UnencryptedSrtp,
    /// Signals that the SRTCP packets are without encryption
    ///
    /// See [RFC 4568 Section 6.3.2](https://datatracker.ietf.org/doc/html/rfc4568#section-6.3.2)
    UnencryptedSrtcp,
    /// Signals that the SRTP packets are not authenticated. (Not recommended)
    ///
    /// See [RFC 4568 Section 6.3.3](https://datatracker.ietf.org/doc/html/rfc4568#section-6.3.3)
    UnauthenticatedSrtp,
    /// Signals whether FEC is applied before or after SRTP processing
    ///
    /// See [RFC 4568 Section 6.3.4](https://datatracker.ietf.org/doc/html/rfc4568#section-6.3.4)
    FecOrder(FecOrder),
    /// Signals the use of separate master key(s) for forward error correction
    ///
    /// See [RFC 4568 Section 6.3.5](https://datatracker.ietf.org/doc/html/rfc4568#section-6.3.5)
    FecKey(Vec<SrtpKeyParam>),
    /// Window Size Hint - provides a hint for how big the SRTP Window size should be
    ///
    /// See [RFC 4568 Section 6.3.6](https://datatracker.ietf.org/doc/html/rfc4568#section-6.3.6)
    Wsh(u8),
    /// Unknown parameter
    Extension(String),
}

/// Type of the Candidate
///
/// See [RFC 8839 Section 5.1](https://datatracker.ietf.org/doc/html/rfc8839#section-5.1)
#[derive(Debug, PartialEq, Clone, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum CandidateType {
    /// Host
    Host,
    /// Server-reflexive
    Srflx,
    /// Peer-reflexive
    Prflx,
    /// Relay
    Relay,
    /// Unknown type
    Other(String),
}

impl CandidateType {
    pub fn new(cand_type: impl AsRef<str>) -> Self {
        let cand_type = cand_type.as_ref();

        if "host".eq_ignore_ascii_case(cand_type) {
            CandidateType::Host
        } else if "srflx".eq_ignore_ascii_case(cand_type) {
            CandidateType::Srflx
        } else if "prflx".eq_ignore_ascii_case(cand_type) {
            CandidateType::Prflx
        } else if "relay".eq_ignore_ascii_case(cand_type) {
            CandidateType::Relay
        } else {
            CandidateType::Other(cand_type.to_string())
        }
    }

    pub fn as_str(&self) -> &str {
        match self {
            CandidateType::Host => "host",
            CandidateType::Srflx => "srflx",
            CandidateType::Prflx => "prflx",
            CandidateType::Relay => "relay",
            CandidateType::Other(o) => o.as_str(),
        }
    }
}

/// RTCP Positive feedback values
///
/// See [RFC 4585 Section 4.2](https://datatracker.ietf.org/doc/html/rfc4585#section-4.2)
#[derive(Debug, PartialEq, Clone, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum RtcpFbAck {
    /// Reference Picture Selection Indication
    Rpsi,
    /// Application layer feedback
    App(Option<String>),
    /// Congestion Control Feedback
    ///
    /// See [RFC 8888 Section 6](https://datatracker.ietf.org/doc/html/rfc8888#section-6)
    Ccfb,
    /// Other Ack types
    Other(String),
}

/// RTCP Negative feedback values
///
/// See [RFC 4585 Section 4.2](https://datatracker.ietf.org/doc/html/rfc4585#section-4.2)
#[derive(Debug, PartialEq, Clone, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum RtcpFbNack {
    /// Picture Loss Indication
    Pli,
    /// Slice Loss Indication
    Sli,
    /// Reference Picture Selection Indication
    Rpsi,
    /// Application layer feedback
    App(Option<String>),
    /// Explicit Congestion Notification
    ///
    /// See [RFC 6679 Section 6.2](https://datatracker.ietf.org/doc/html/rfc6679#section-6.2)
    Ecn,
    /// Other Nack types
    Other(String),
}

/// Codec Control using RTCP feedback messages
///
/// See [RFC 5104 Section 7.1](https://datatracker.ietf.org/doc/html/rfc5104#section-7.1)
#[derive(Debug, PartialEq, Clone, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum RtcpFbCcm {
    /// Full Intra Request
    Fir,
    /// Temporary Maximum Media Stream Bit Rate
    Tmmbr(Option<String>),
    /// Temporal-Spatial Trade-off
    Tstr,
    /// Video Back Channel Messages
    Vbcm(Vec<u8>),
    /// Other messages (for future commands/Indications)
    Other(String),
}

/// Types of RTCP feedback values
#[derive(Debug, PartialEq, Clone, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum RtcpFbVal {
    /// Positive Acknowledgement
    Ack(Option<RtcpFbAck>),
    /// Negative Acknowledgement
    Nack(Option<RtcpFbNack>),
    /// Minimum interval between two Regular RTCP packets in milliseconds
    TrrInt(u64),
    /// Codec Control messages
    Ccm(RtcpFbCcm),
    /// Others Rtcp Fb types
    Other(String),
}

impl Display for RtcpFbVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RtcpFbVal::Ack(ack) => {
                write!(f, "ack")?;
                if let Some(ack) = ack {
                    match ack {
                        RtcpFbAck::Rpsi => write!(f, " rpsi")?,
                        RtcpFbAck::Ccfb => write!(f, " ccfb")?,
                        RtcpFbAck::App(app) => {
                            write!(f, " app")?;
                            if let Some(app_param) = app {
                                f.write_char(' ')?;
                                f.write_str(app_param)?;
                            }
                        }
                        RtcpFbAck::Other(other) => {
                            f.write_char(' ')?;
                            f.write_str(other)?;
                        }
                    }
                }
                Ok(())
            }
            RtcpFbVal::Nack(nack) => {
                write!(f, "nack")?;
                if let Some(nack) = nack {
                    match nack {
                        RtcpFbNack::Pli => write!(f, " pli")?,
                        RtcpFbNack::Sli => write!(f, " sli")?,
                        RtcpFbNack::Rpsi => write!(f, " rpsi")?,
                        RtcpFbNack::Ecn => write!(f, " ecn")?,
                        RtcpFbNack::App(app) => {
                            write!(f, " app")?;
                            if let Some(app_param) = app {
                                f.write_char(' ')?;
                                f.write_str(app_param)?;
                            }
                        }
                        RtcpFbNack::Other(other) => {
                            f.write_char(' ')?;
                            f.write_str(other)?;
                        }
                    }
                }
                Ok(())
            }
            RtcpFbVal::TrrInt(trr_int) => write!(f, "trr-int {trr_int}"),
            RtcpFbVal::Ccm(ccm) => {
                write!(f, "ccm")?;
                match ccm {
                    RtcpFbCcm::Fir => write!(f, " fir")?,
                    RtcpFbCcm::Tstr => write!(f, " tstr")?,
                    RtcpFbCcm::Tmmbr(smaxpr) => {
                        write!(f, " tmmbr")?;
                        if let Some(smaxpr) = smaxpr {
                            f.write_char(' ')?;
                            f.write_str(smaxpr)?;
                        }
                    }
                    RtcpFbCcm::Vbcm(vbcm) => {
                        write!(f, " vbcm")?;
                        for v in vbcm {
                            f.write_char(' ')?;
                            write!(f, "{v}")?;
                        }
                    }
                    RtcpFbCcm::Other(other) => {
                        f.write_char(' ')?;
                        f.write_str(other)?;
                    }
                }
                Ok(())
            }
            RtcpFbVal::Other(other) => f.write_str(other),
        }
    }
}

/// Source attribute types
///
/// See [RFC 5576 Section 6](https://datatracker.ietf.org/doc/html/rfc5576#section-6)
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum SsrcAttribute {
    /// See [RFC 5576 Section 6.1](https://datatracker.ietf.org/doc/html/rfc5576#section-6.1)
    Cname,
    /// See [RFC 5576 Section 6.2](https://datatracker.ietf.org/doc/html/rfc5576#section-6.2)
    PreviousSsrc,
    /// See [RFC 5576 Section 6.3](https://datatracker.ietf.org/doc/html/rfc5576#section-6.3)
    Fmtp,
    /// See [RFC 5576 Section 6.4](https://datatracker.ietf.org/doc/html/rfc5576#section-6.4)
    Other(String),
}

impl SsrcAttribute {
    pub fn new(attribute: impl AsRef<str>) -> Self {
        let attr = attribute.as_ref();
        if "cname".eq_ignore_ascii_case(attr) {
            SsrcAttribute::Cname
        } else if "previous-ssrc".eq_ignore_ascii_case(attr) {
            SsrcAttribute::PreviousSsrc
        } else if "fmtp".eq_ignore_ascii_case(attr) {
            SsrcAttribute::Fmtp
        } else {
            SsrcAttribute::Other(attr.to_string())
        }
    }
}

impl<T: TypedAttribute> From<T> for SsrcAttribute {
    fn from(_attr: T) -> Self {
        SsrcAttribute::new(T::NAME)
    }
}

/// Payload format for which feedback messages may be used
#[derive(Debug, PartialEq, Clone, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum RtcpFbPt {
    /// Fixed payload format
    Fmt(u8),
    /// Applies to all formats
    Wildcard,
}

/// Candidate connection address type
///
/// Can be IPv4, IPv6 or a FQDN
#[derive(Debug, PartialEq, Clone, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum CandidateAddress {
    IpAddr(std::net::IpAddr),
    FQDN(String),
}
