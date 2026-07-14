// Copyright (C) 2026 Taruntej Kanakamalla <tarun@centricular.com>
//
// Licensed under the MIT license, see the LICENSE file or <http://opensource.org/licenses/MIT>

//! Contains all the Session description Attributes defined as Structs/Enums

use std::{
    fmt::{Display, Write},
    net::IpAddr,
    str::FromStr,
};

use crate::{builders, enums::*, Attribute};

/// Trait for Typed Attribute structs
pub trait TypedAttribute: Display + FromStr<Err = AttributeError> {
    const NAME: &'static str;
}

impl<T: TypedAttribute> From<T> for Attribute {
    fn from(attr: T) -> Attribute {
        Attribute {
            attribute: T::NAME.to_string(),
            value: Some(attr.to_string()),
        }
    }
}

/// Attribute error with specific details
#[derive(Debug, PartialEq, Eq, thiserror::Error)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum AttributeError {
    /// If an Attribute is not found in a media or the session
    #[error("Attribute {} not found", .0)]
    NotFound(String),
    /// If a parameter is missing in an attribute
    #[error("Param {} not found in {}", .param, .attr)]
    ParamNotFound { param: String, attr: String },
    /// If a parameter value is not valid type or not in range
    #[error("Invalid value {} for Param {} in {}", .val ,.param, .attr)]
    InvalidParamValue {
        param: String,
        val: String,
        attr: String,
    },
    /// If an attribute is not in expected format
    #[error("Unsupported attribute format: {} for {}", .val, .attr)]
    UnsupportedFormat { val: String, attr: String },
    /// If there are more than expected items trailing in the attribute parameters
    #[error("Unexpected trailing item {} for in {}", .val, .attr)]
    UnexpectedTrailingItem { val: String, attr: String },
    /// Unspecified error
    #[error("{}: {}", .attr, .error)]
    Other { error: String, attr: String },
}

impl AttributeError {
    pub fn is_attribute_not_found(&self) -> bool {
        matches!(self, AttributeError::NotFound(_))
    }
}

/// RtpMap Attribute
///
/// See [RFC 8866 Section 6.6](https://datatracker.ietf.org/doc/html/rfc8866#section-6.6) for more details
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct RtpMap {
    /// Payload type, a numerical value between 0 and 127
    pub payload_type: u8,
    /// Name of the encoding
    // TODO: is it useful to have an enum for all known encoding?
    pub encoding_name: String,
    /// Clock rate
    pub clock_rate: u32,
    /// Encoding parameters.
    ///
    /// Currently used only for audio channel count
    pub encoding_params: Option<String>,
}

impl RtpMap {
    pub fn new(payload_type: u8, encoding_name: impl ToString, clock_rate: u32) -> Self {
        RtpMap {
            payload_type,
            encoding_name: encoding_name.to_string(),
            clock_rate,
            encoding_params: None,
        }
    }

    pub fn builder(
        payload_type: u8,
        encoding_name: impl ToString,
        clock_rate: u32,
    ) -> builders::RtpMap {
        builders::RtpMap::new(payload_type, encoding_name, clock_rate)
    }

    pub fn with_encoding_params(
        payload_type: u8,
        encoding_name: impl ToString,
        clock_rate: u32,
        encoding_params: impl ToString,
    ) -> Self {
        RtpMap {
            payload_type,
            encoding_name: encoding_name.to_string(),
            clock_rate,
            encoding_params: Some(encoding_params.to_string()),
        }
    }

    pub fn set_encoding_params(&mut self, encoding_params: impl ToString) {
        self.encoding_params = Some(encoding_params.to_string());
    }
}

impl FromStr for RtpMap {
    type Err = AttributeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let Some((pt, rest)) = s.split_once(' ') else {
            return Err(AttributeError::UnsupportedFormat {
                val: s.to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        let Ok(pt) = pt.parse::<u8>() else {
            return Err(AttributeError::InvalidParamValue {
                param: "Payload type".to_string(),
                val: pt.to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        if pt > 127 {
            return Err(AttributeError::InvalidParamValue {
                param: "Payload type".to_string(),
                val: format!("{pt}(expected 0-127)"),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        }

        let mut i = rest.splitn(3, '/');
        let Some(encoding) = i.next() else {
            return Err(AttributeError::ParamNotFound {
                param: "Encoding name".to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        let Some(clock_rate) = i.next() else {
            return Err(AttributeError::ParamNotFound {
                param: "Clock rate".to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        let Ok(clock_rate) = clock_rate.parse::<u32>() else {
            return Err(AttributeError::InvalidParamValue {
                param: "Clock rate".to_string(),
                val: clock_rate.to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        let params = i.next().map(String::from);

        Ok(Self {
            payload_type: pt,
            encoding_name: encoding.to_owned(),
            clock_rate,
            encoding_params: params,
        })
    }
}

impl Display for RtpMap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {}/{}",
            self.payload_type, self.encoding_name, self.clock_rate
        )?;
        if let Some(params) = &self.encoding_params {
            f.write_char('/')?;
            f.write_str(params)?;
        }
        Ok(())
    }
}

impl TypedAttribute for RtpMap {
    const NAME: &'static str = "rtpmap";
}

/// Format specific parameters
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct FmtpParam {
    pub param: String,
    pub val: Option<String>,
}

impl FmtpParam {
    pub fn new(param: impl ToString) -> Self {
        FmtpParam {
            param: param.to_string(),
            val: None,
        }
    }

    pub fn builder(param: impl ToString) -> builders::FmtpParam {
        builders::FmtpParam::new(param)
    }

    pub fn with_value(param: impl ToString, value: impl ToString) -> Self {
        FmtpParam {
            param: param.to_string(),
            val: Some(value.to_string()),
        }
    }

    pub fn set_value(&mut self, value: impl ToString) {
        self.val = Some(value.to_string());
    }
}

/// Format Parameters
///
/// See [RFC 8866 Section 6.15](https://datatracker.ietf.org/doc/html/rfc8866#section-6.15) for more details
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Fmtp {
    /// Payload format
    pub fmt: u8,
    /// Format specific parameters
    // Multiple params are expected to be semicolon separated
    // Each param can be a 'key=value' pair or just single parameter
    pub format_specific_params: Vec<FmtpParam>,
}

impl Fmtp {
    pub fn new(fmt: u8) -> Self {
        Fmtp {
            fmt,
            format_specific_params: vec![],
        }
    }

    pub fn builder(fmt: u8) -> builders::Fmtp {
        builders::Fmtp::new(fmt)
    }

    pub fn add_format_specific_param(&mut self, format_specific_param: FmtpParam) {
        self.format_specific_params.push(format_specific_param)
    }

    pub fn add_format_specific_params(
        &mut self,
        format_specific_params: impl IntoIterator<Item = FmtpParam>,
    ) {
        self.format_specific_params.extend(format_specific_params)
    }
}

impl FromStr for Fmtp {
    type Err = AttributeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let Some((fmt, rest)) = s.split_once(' ') else {
            return Err(AttributeError::UnsupportedFormat {
                val: s.to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        let Ok(fmt) = fmt.parse::<u8>() else {
            return Err(AttributeError::InvalidParamValue {
                param: "fmtp".to_string(),
                val: fmt.to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        let mut params: Vec<FmtpParam> = Vec::new();
        for param in rest.split(';') {
            if let Some((key, value)) = param.split_once('=') {
                params.push(FmtpParam {
                    param: key.to_string(),
                    val: Some(value.to_string()),
                });
            } else {
                params.push(FmtpParam {
                    param: param.to_string(),
                    val: None,
                });
            }
        }

        Ok(Self {
            fmt,
            format_specific_params: params,
        })
    }
}

impl Display for Fmtp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ", self.fmt)?;
        let mut iter = self.format_specific_params.iter().peekable();
        while let Some(p) = iter.next() {
            write!(f, "{}", p.param)?;
            if let Some(val) = &p.val {
                f.write_char('=')?;
                f.write_str(val.as_str())?;
            }
            if iter.peek().is_some() {
                f.write_char(';')?;
            }
        }
        Ok(())
    }
}

impl TypedAttribute for Fmtp {
    const NAME: &'static str = "fmtp";
}

/// RTCP port number and address
///
/// To be used if not algorithmically derived
/// from the RTP port described in the media line
///
/// See [RFC 3605 Section 2.1](https://datatracker.ietf.org/doc/html/rfc3605#section-2.1)
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Rtcp {
    /// Port used for the RTCP stream
    pub port: u16,
    /// Network Type
    pub nettype: NetType,
    /// Address type
    pub addrtype: AddrType,
    /// Connection address: can be IP Address, unicast, multicast, ...
    /// Conformity may be checked against the `addrtype`.
    pub connection_address: String,
}

impl Rtcp {
    /// Construct an [`Rtcp`] with the specified IP `connection_address`
    pub fn with_ip_addr(port: u16, connection_address: impl Into<IpAddr>) -> Self {
        let connection_address = connection_address.into();
        Rtcp {
            port,
            nettype: NetType::In,
            addrtype: connection_address.into(),
            connection_address: connection_address.to_string(),
        }
    }

    /// Construct an [`Rtcp`]
    ///
    /// See also [`Rtcp::with_ip_addr`]
    pub fn new(
        port: u16,
        nettype: NetType,
        addrtype: AddrType,
        connection_address: impl ToString,
    ) -> Self {
        Rtcp {
            port,
            nettype,
            addrtype,
            connection_address: connection_address.to_string(),
        }
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

    /// Sets the `connection_address` & `addrtype` of `self` from the specified `IpAddr`
    pub fn set_connection_ip_address(&mut self, connection_address: impl Into<IpAddr>) {
        let connection_address = connection_address.into();
        self.addrtype = connection_address.into();
        self.connection_address = connection_address.to_string();
    }
}

impl FromStr for Rtcp {
    type Err = AttributeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut i = s.split(' ');
        let Some(port) = i.next() else {
            return Err(AttributeError::ParamNotFound {
                param: "Port".to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        let Ok(port) = port.parse::<u16>() else {
            return Err(AttributeError::InvalidParamValue {
                param: "Port".to_string(),
                val: port.to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        let Some(nettype) = i.next() else {
            return Err(AttributeError::ParamNotFound {
                param: "Network type".to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        let Ok(nettype) = NetType::from_str(nettype) else {
            return Err(AttributeError::InvalidParamValue {
                param: "Network type".to_string(),
                val: nettype.to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        let Some(addrtype) = i.next() else {
            return Err(AttributeError::ParamNotFound {
                param: "Address type".to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        let Ok(addrtype) = AddrType::from_str(addrtype) else {
            return Err(AttributeError::InvalidParamValue {
                param: "Address type".to_string(),
                val: addrtype.to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        let Some(connection_address) = i.next() else {
            return Err(AttributeError::ParamNotFound {
                param: "Connection address".to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        if let Some(unexpected) = i.next() {
            return Err(AttributeError::UnexpectedTrailingItem {
                val: unexpected.to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        }

        Ok(Self {
            port,
            nettype,
            addrtype,
            connection_address: connection_address.to_string(),
        })
    }
}

impl Display for Rtcp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} {} {}",
            self.port, self.nettype, self.addrtype, self.connection_address
        )
    }
}

impl TypedAttribute for Rtcp {
    const NAME: &'static str = "rtcp";
}

/// RTCP Feedback Capability
///
/// See [RFC 4585 Section 4.2](https://datatracker.ietf.org/doc/html/rfc4585#section-4.2)
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct RtcpFb {
    /// Payload format for which feedback messages may be used
    pub pt: RtcpFbPt,
    /// RTCP Feedback value
    pub val: RtcpFbVal,
}

impl RtcpFb {
    pub fn new(pt: RtcpFbPt, val: RtcpFbVal) -> Self {
        RtcpFb { pt, val }
    }
}

impl FromStr for RtcpFb {
    type Err = AttributeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut i = s.split(' ');
        let Some(pt) = i.next() else {
            return Err(AttributeError::ParamNotFound {
                param: "Payload format".to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        let pt = if let Ok(pt) = pt.parse::<u8>() {
            RtcpFbPt::Fmt(pt)
        } else if pt == "*" {
            RtcpFbPt::Wildcard
        } else {
            return Err(AttributeError::InvalidParamValue {
                param: "Payload format".to_string(),
                val: pt.to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        let Some(val) = i.next() else {
            return Err(AttributeError::ParamNotFound {
                param: "Rtcp feedback value".to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        let rtcp_fb_val = match val {
            "ack" => {
                if let Some(ack_val) = i.next() {
                    let ack_val = match ack_val {
                        "rpsi" => RtcpFbAck::Rpsi,
                        "app" => {
                            if let Some(app_param) = i.next() {
                                RtcpFbAck::App(Some(app_param.to_string()))
                            } else {
                                RtcpFbAck::App(None)
                            }
                        }
                        "ccfb" => {
                            // The payload type used with "ccfb" feedback MUST be the wildcard type
                            // See https://datatracker.ietf.org/doc/html/rfc8888#section-6
                            if let RtcpFbPt::Fmt(pt) = pt {
                                return Err(AttributeError::InvalidParamValue {
                                    param: "Payload type of Congestion control feedback (ccfb)"
                                        .to_string(),
                                    val: format!("{pt}(expected wildcard (*))"),
                                    attr: <Self as TypedAttribute>::NAME.to_string(),
                                });
                            } else {
                                RtcpFbAck::Ccfb
                            }
                        }
                        other => RtcpFbAck::Other(other.to_string()),
                    };
                    RtcpFbVal::Ack(Some(ack_val))
                } else {
                    RtcpFbVal::Ack(None)
                }
            }
            "nack" => {
                if let Some(nack_val) = i.next() {
                    let nack_val = match nack_val {
                        "pli" => RtcpFbNack::Pli,
                        "sli" => RtcpFbNack::Sli,
                        "rpsi" => RtcpFbNack::Rpsi,
                        "app" => {
                            if let Some(app_param) = i.next() {
                                RtcpFbNack::App(Some(app_param.to_string()))
                            } else {
                                RtcpFbNack::App(None)
                            }
                        }
                        "ecn" => RtcpFbNack::Ecn,
                        other => RtcpFbNack::Other(other.to_string()),
                    };
                    RtcpFbVal::Nack(Some(nack_val))
                } else {
                    RtcpFbVal::Nack(None)
                }
            }
            "trr-int" => {
                if let Some(val) = i.next() {
                    let Ok(i) = val.parse::<u64>() else {
                        return Err(AttributeError::InvalidParamValue {
                            param: "Minimum interval between RTCP packets (trr-int)".to_string(),
                            val: val.to_string(),
                            attr: <Self as TypedAttribute>::NAME.to_string(),
                        });
                    };
                    RtcpFbVal::TrrInt(i)
                } else {
                    return Err(AttributeError::Other {
                        error: "Minimum interval between RTCP packets (trr-int) not specified"
                            .to_string(),
                        attr: <Self as TypedAttribute>::NAME.to_string(),
                    });
                }
            }
            "ccm" => {
                if let Some(ccm_val) = i.next() {
                    let ccm_val = match ccm_val {
                        "fir" => RtcpFbCcm::Fir,
                        "tmmbr" => {
                            if let Some(tmmbr_val) = i.next() {
                                RtcpFbCcm::Tmmbr(Some(tmmbr_val.to_string()))
                            } else {
                                RtcpFbCcm::Tmmbr(None)
                            }
                        }
                        "tstr" => RtcpFbCcm::Tstr,
                        "vbcm" => {
                            let mut v = vec![];
                            for vbcm_val in i {
                                let Ok(p) = vbcm_val.parse::<u8>() else {
                                    return Err(AttributeError::InvalidParamValue {
                                        param: "Video backchannel messages (vbcm)".to_string(),
                                        val: vbcm_val.to_string(),
                                        attr: <Self as TypedAttribute>::NAME.to_string(),
                                    });
                                };
                                v.push(p);
                            }
                            RtcpFbCcm::Vbcm(v)
                        }
                        other => RtcpFbCcm::Other(other.to_string()),
                    };
                    RtcpFbVal::Ccm(ccm_val)
                } else {
                    return Err(AttributeError::ParamNotFound {
                        param: "Codec control messages (ccm)".to_string(),
                        attr: <Self as TypedAttribute>::NAME.to_string(),
                    });
                }
            }
            other => RtcpFbVal::Other(other.to_string()),
        };

        Ok(Self {
            pt,
            val: rtcp_fb_val,
        })
    }
}

impl Display for RtcpFb {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.pt {
            RtcpFbPt::Wildcard => f.write_char('*')?,
            RtcpFbPt::Fmt(pt) => write!(f, "{pt}")?,
        }

        f.write_char(' ')?;
        write!(f, "{}", self.val)
    }
}

impl TypedAttribute for RtcpFb {
    const NAME: &'static str = "rtcp-fb";
}

/// Media Direction Attributes
///
/// See [RFC 8866 Section 6.7](https://datatracker.ietf.org/doc/html/rfc8866#section-6.7)
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Direction {
    SendOnly,
    RecvOnly,
    SendRecv,
    Inactive,
}

impl Direction {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::SendOnly => "sendonly",
            Self::RecvOnly => "recvonly",
            Self::SendRecv => "sendrecv",
            Self::Inactive => "inactive",
        }
    }
}

impl FromStr for Direction {
    type Err = ParseEnumError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if "sendonly".eq_ignore_ascii_case(s) {
            Ok(Direction::SendOnly)
        } else if "recvonly".eq_ignore_ascii_case(s) {
            Ok(Direction::RecvOnly)
        } else if "sendrecv".eq_ignore_ascii_case(s) {
            Ok(Direction::SendRecv)
        } else if "inactive".eq_ignore_ascii_case(s) {
            Ok(Direction::Inactive)
        } else {
            Err(ParseEnumError::Invalid(s.to_string()))
        }
    }
}

impl Display for Direction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

impl From<Direction> for Attribute {
    fn from(attr: Direction) -> Attribute {
        Attribute {
            attribute: attr.to_string(),
            value: None,
        }
    }
}

/// RTP header extensions map
///
/// See [RFC 8285 Section 8](https://datatracker.ietf.org/doc/html/rfc8285#section-8)
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExtMap {
    /// The local identifier (ID) of this extension
    pub id: u8,
    /// Direction
    pub direction: Option<Direction>,
    /// The format and meaning of the extension
    pub uri: String,
    /// Extension attributes
    pub attributes: Option<String>,
}

impl ExtMap {
    pub fn new(id: u8, uri: impl ToString) -> Self {
        ExtMap {
            id,
            direction: None,
            uri: uri.to_string(),
            attributes: None,
        }
    }

    pub fn builder(id: u8, uri: impl ToString) -> builders::ExtMap {
        builders::ExtMap::new(id, uri)
    }

    pub fn with_direction(id: u8, direction: Direction, uri: impl ToString) -> Self {
        ExtMap {
            id,
            direction: Some(direction),
            uri: uri.to_string(),
            attributes: None,
        }
    }

    pub fn with_direction_and_attributes(
        id: u8,
        direction: Direction,
        uri: impl ToString,
        attributes: impl ToString,
    ) -> Self {
        ExtMap {
            id,
            direction: Some(direction),
            uri: uri.to_string(),
            attributes: Some(attributes.to_string()),
        }
    }

    pub fn set_direction(&mut self, direction: Direction) {
        self.direction = Some(direction);
    }

    pub fn set_attributes(&mut self, attributes: impl ToString) {
        self.attributes = Some(attributes.to_string());
    }
}

impl FromStr for ExtMap {
    type Err = AttributeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut i = s.splitn(3, ' ');

        let Some(id_direction) = i.next() else {
            return Err(AttributeError::ParamNotFound {
                param: "id/direction".to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        let mut d = id_direction.split('/');

        let Some(id) = d.next() else {
            return Err(AttributeError::ParamNotFound {
                param: "id".to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        let direction = if let Some(d) = d.next() {
            let Ok(dir) = Direction::from_str(d) else {
                return Err(AttributeError::InvalidParamValue {
                    param: "Direction".to_string(),
                    val: d.to_string(),
                    attr: <Self as TypedAttribute>::NAME.to_string(),
                });
            };
            Some(dir)
        } else {
            None
        };

        let Ok(id) = id.parse::<u8>() else {
            return Err(AttributeError::InvalidParamValue {
                param: "Id".to_string(),
                val: id.to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        if id == 0 {
            return Err(AttributeError::InvalidParamValue {
                param: "Id".to_string(),
                val: id.to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        }

        let Some(uri) = i.next() else {
            return Err(AttributeError::ParamNotFound {
                param: "URI".to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        let attributes = i.next().map(|attr| attr.to_string());

        Ok(Self {
            id,
            direction,
            uri: uri.to_string(),
            attributes,
        })
    }
}

impl Display for ExtMap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id)?;
        if let Some(direction) = &self.direction {
            f.write_char('/')?;
            f.write_str(direction.as_str())?;
        }

        f.write_char(' ')?;
        f.write_str(&self.uri)?;

        if let Some(attr) = &self.attributes {
            f.write_char(' ')?;
            f.write_str(attr.as_str())?;
        }
        Ok(())
    }
}

impl TypedAttribute for ExtMap {
    const NAME: &'static str = "extmap";
}

/// Fingerprint Attribute
///
/// See [RFC 8122 Section 5](https://datatracker.ietf.org/doc/html/rfc8122#section-5)
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Fingerprint {
    /// Name of hash function used
    pub hash_func: HashFunc,
    /// Hash value
    pub fingerprint: Vec<u8>,
}

impl Fingerprint {
    pub fn new(hash_func: HashFunc) -> Self {
        Fingerprint {
            hash_func,
            fingerprint: vec![],
        }
    }

    pub fn with_fingerprint(
        hash_func: HashFunc,
        fingerprint: impl IntoIterator<Item = u8>,
    ) -> Self {
        Fingerprint {
            hash_func,
            fingerprint: std::iter::FromIterator::from_iter(fingerprint),
        }
    }

    pub fn set_fingerprint(&mut self, fingerprint: impl IntoIterator<Item = u8>) {
        self.fingerprint.clear();
        self.fingerprint.extend(fingerprint);
    }
}

impl FromStr for Fingerprint {
    type Err = AttributeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut i = s.splitn(2, ' ');

        let hash_func = if let Some(hash_func) = i.next() {
            HashFunc::new(hash_func)
        } else {
            return Err(AttributeError::ParamNotFound {
                param: "Hash function".to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        let mut fingerprint: Vec<u8> = vec![];
        if let Some(fp) = i.next() {
            for f in fp.split(':') {
                let Ok(mut f) = hex::decode(f) else {
                    return Err(AttributeError::InvalidParamValue {
                        param: "Fingerprint value".to_string(),
                        val: f.to_string(),
                        attr: <Self as TypedAttribute>::NAME.to_string(),
                    });
                };

                fingerprint.append(&mut f);
            }
        } else {
            return Err(AttributeError::ParamNotFound {
                param: "Hash value".to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        Ok(Self {
            hash_func,
            fingerprint,
        })
    }
}

impl Display for Fingerprint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.hash_func.as_str())?;
        let mut first = true;
        for v in &self.fingerprint {
            if first {
                f.write_char(' ')?;
                first = false;
            } else {
                f.write_char(':')?;
            }
            write!(f, "{v:02X}")?;
        }
        Ok(())
    }
}

impl TypedAttribute for Fingerprint {
    const NAME: &'static str = "fingerprint";
}

/// Group Attribute
///
/// See [RFC 5888 Section 5](https://datatracker.ietf.org/doc/html/rfc5888#section-5)
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Group {
    pub semantics: GroupSemantics,
    pub mid_tags: Vec<String>,
}

impl Group {
    pub fn new(semantics: GroupSemantics) -> Self {
        Group {
            semantics,
            mid_tags: vec![],
        }
    }

    pub fn add_mid_tag(&mut self, mid_tag: impl ToString) {
        self.mid_tags.push(mid_tag.to_string())
    }

    pub fn add_mid_tags(&mut self, mid_tags: impl IntoIterator<Item = impl ToString>) {
        self.mid_tags
            .extend(mid_tags.into_iter().map(|i| i.to_string()))
    }
}

impl FromStr for Group {
    type Err = AttributeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut i = s.split(' ');

        let Some(semantics) = i.next() else {
            return Err(AttributeError::ParamNotFound {
                param: "Semantics".to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        let semantics = GroupSemantics::new(semantics);

        let mut mid_tags = vec![];
        for mid in i {
            mid_tags.push(mid.to_string());
        }

        if mid_tags.is_empty() {
            return Err(AttributeError::ParamNotFound {
                param: "Media identification tags".to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        }

        Ok(Self {
            semantics,
            mid_tags,
        })
    }
}

impl Display for Group {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.semantics.as_str())?;
        for m in &self.mid_tags {
            f.write_char(' ')?;
            f.write_str(m)?;
        }
        Ok(())
    }
}

impl TypedAttribute for Group {
    const NAME: &'static str = "group";
}

/// Setup attribute for the session or media.
///
/// See [RFC 4145 Section 4](https://datatracker.ietf.org/doc/html/rfc4145#section-4) for more details.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Setup {
    /// Initiator of the connection.
    Active,
    /// Acceptor of the connection.
    Passive,
    /// Act as either initiator or acceptor of the connection.
    ActPass,
    /// Do not establish a connection.
    HoldConn,
}

impl FromStr for Setup {
    type Err = AttributeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if "active".eq_ignore_ascii_case(s) {
            Ok(Setup::Active)
        } else if "passive".eq_ignore_ascii_case(s) {
            Ok(Setup::Passive)
        } else if "actpass".eq_ignore_ascii_case(s) {
            Ok(Setup::ActPass)
        } else if "holdconn".eq_ignore_ascii_case(s) {
            Ok(Setup::HoldConn)
        } else {
            Err(AttributeError::Other {
                error: format!("Invalid Setup value {s}"),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            })
        }
    }
}

impl Display for Setup {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Setup::Active => "active",
            Setup::Passive => "passive",
            Setup::ActPass => "actpass",
            Setup::HoldConn => "holdconn",
        };
        f.write_str(s)
    }
}

impl TypedAttribute for Setup {
    const NAME: &'static str = "setup";
}

/// SSRC media attribute.
///
/// See [RFC 5576 Section 4.1](https://datatracker.ietf.org/doc/html/rfc5576#section-4.1)
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Ssrc {
    pub ssrc_id: u32,
    pub attribute: SsrcAttribute,
    pub value: Option<String>,
}

impl Ssrc {
    pub fn new(ssrc_id: u32, attribute: SsrcAttribute) -> Self {
        Ssrc {
            ssrc_id,
            attribute,
            value: None,
        }
    }

    pub fn with_typed_attribute(ssrc_id: u32, attribute: impl TypedAttribute) -> Self {
        let value = attribute.to_string();
        Ssrc {
            ssrc_id,
            attribute: SsrcAttribute::from(attribute),
            value: Some(value),
        }
    }

    pub fn with_value(ssrc_id: u32, attribute: SsrcAttribute, value: impl ToString) -> Self {
        Ssrc {
            ssrc_id,
            attribute,
            value: Some(value.to_string()),
        }
    }

    pub fn set_value(&mut self, value: impl ToString) {
        self.value = Some(value.to_string());
    }

    /// Gets the inner attribute as a `TypedAttribute`.
    ///
    /// # Errors
    ///
    /// * `AttributeError::Other` if the inner attribute doesn't match
    ///   the specified `TypedAttribute` or if the value is empty.
    /// * a specific `AttributeError` if the typed attribute couldn't be built.
    pub fn get_typed<T: TypedAttribute>(&self) -> Result<T, AttributeError> {
        if !self.attribute.as_str().eq_ignore_ascii_case(T::NAME) {
            return Err(AttributeError::Other {
                error: format!("Attribute type mismatch (requested {})", T::NAME),
                attr: self.attribute.as_str().to_string(),
            });
        }

        let Some(ref value) = self.value else {
            return Err(AttributeError::Other {
                error: "No value for the attribute".to_string(),
                attr: T::NAME.to_string(),
            });
        };

        T::from_str(value)
    }
}

impl FromStr for Ssrc {
    type Err = AttributeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let Some((ssrc_id_str, rest)) = s.split_once(' ') else {
            return Err(AttributeError::ParamNotFound {
                param: "Ssrc id".to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        let Ok(ssrc_id) = ssrc_id_str.parse::<u32>() else {
            return Err(AttributeError::InvalidParamValue {
                param: "Ssrc id".to_string(),
                val: ssrc_id_str.to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        let (attr, value) = if let Some((attr_str, value)) = rest.split_once(':') {
            (attr_str, Some(value.to_string()))
        } else {
            (rest, None)
        };

        Ok(Self {
            ssrc_id,
            attribute: SsrcAttribute::new(attr),
            value,
        })
    }
}

impl Display for Ssrc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let attr_str = match &self.attribute {
            SsrcAttribute::Cname => "cname",
            SsrcAttribute::PreviousSsrc => "previous-ssrc",
            SsrcAttribute::Fmtp => "fmtp",
            SsrcAttribute::Other(other) => other.as_str(),
        };
        write!(f, "{} {attr_str}", self.ssrc_id)?;

        if let Some(value) = &self.value {
            f.write_char(':')?;
            f.write_str(value)?;
        }

        Ok(())
    }
}

impl TypedAttribute for Ssrc {
    const NAME: &'static str = "ssrc";
}

/// SSRC group attribute
///
/// See [RFC 5576 Section 4.2](https://datatracker.ietf.org/doc/html/rfc5576#section-4.2)
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct SsrcGroup {
    pub semantics: GroupSemantics,
    pub ssrc_ids: Vec<u32>,
}

impl SsrcGroup {
    pub fn new(semantics: GroupSemantics) -> Self {
        SsrcGroup {
            semantics,
            ssrc_ids: vec![],
        }
    }

    pub fn add_ssrc_id(&mut self, ssrc_id: u32) {
        self.ssrc_ids.push(ssrc_id)
    }

    pub fn add_ssrc_ids(&mut self, ssrc_ids: impl IntoIterator<Item = u32>) {
        self.ssrc_ids.extend(ssrc_ids)
    }
}

impl FromStr for SsrcGroup {
    type Err = AttributeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut i = s.split(' ');

        let Some(semantics) = i.next() else {
            return Err(AttributeError::ParamNotFound {
                param: "Semantics".to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        let semantics = if "FEC".eq_ignore_ascii_case(semantics) {
            GroupSemantics::FEC
        } else if "FID".eq_ignore_ascii_case(semantics) {
            GroupSemantics::FID
        } else {
            // The initial defined semantics for ssrc-group attribute are FID and FEC
            // The other registered group semantics are not useful for source grouping
            // But keep this open for any other new semantics that are not part of GroupSemantics
            GroupSemantics::Other(semantics.to_string())
        };

        let mut ssrc_ids = vec![];
        for ssrc_id in i {
            let Ok(ssrc_id) = ssrc_id.parse::<u32>() else {
                return Err(AttributeError::InvalidParamValue {
                    param: "Ssrc id".to_string(),
                    val: ssrc_id.to_string(),
                    attr: <Self as TypedAttribute>::NAME.to_string(),
                });
            };
            ssrc_ids.push(ssrc_id);
        }

        if ssrc_ids.is_empty() {
            return Err(AttributeError::ParamNotFound {
                param: "ssrc_id".to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        }

        Ok(Self {
            semantics,
            ssrc_ids,
        })
    }
}

impl Display for SsrcGroup {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let sem = match &self.semantics {
            GroupSemantics::FEC => "FEC",
            GroupSemantics::FID => "FID",
            // Semantics other than FEC and FID are not useful for source grouping but still displaying
            // them for debugging purpose
            GroupSemantics::LS => "LS",
            GroupSemantics::SRF => "SRF",
            GroupSemantics::ANAT => "ANAT",
            GroupSemantics::DDP => "DDP",
            GroupSemantics::Other(s) => s.as_str(),
        };

        f.write_str(sem)?;
        for ssrc_id in &self.ssrc_ids {
            f.write_char(' ')?;
            write!(f, "{ssrc_id}")?;
        }

        Ok(())
    }
}

impl TypedAttribute for SsrcGroup {
    const NAME: &'static str = "ssrc-group";
}

/// SRTP Key parameter
///
/// See [RFC 4568 Section 6.1](https://datatracker.ietf.org/doc/html/rfc4568#section-6.1)
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct SrtpKeyParam {
    /// Concatenated key and salt, base64 encoded
    pub key_and_salt: String,
    /// Master key lifetime (max number of SRTP or SRTCP packets using this master key)
    pub lifetime: Option<u32>,
    /// MKI (Master Key Identifier) and length of the MKI field in SRTP packets
    pub mki_and_length: Option<(u32, u32)>,
}

impl SrtpKeyParam {
    pub fn new(key_and_salt: impl ToString) -> Self {
        SrtpKeyParam {
            key_and_salt: key_and_salt.to_string(),
            lifetime: None,
            mki_and_length: None,
        }
    }

    pub fn with_lifetime(key_and_salt: impl ToString, lifetime: u32) -> Self {
        SrtpKeyParam {
            key_and_salt: key_and_salt.to_string(),
            lifetime: Some(lifetime),
            mki_and_length: None,
        }
    }

    pub fn with_lifetime_and_mki_and_length(
        key_and_salt: impl ToString,
        lifetime: u32,
        mki: u32,
        length: u32,
    ) -> Self {
        SrtpKeyParam {
            key_and_salt: key_and_salt.to_string(),
            lifetime: Some(lifetime),
            mki_and_length: Some((mki, length)),
        }
    }

    pub fn set_lifetime(&mut self, lifetime: u32) {
        self.lifetime = Some(lifetime);
    }

    pub fn set_mki_and_length(&mut self, mki: u32, length: u32) {
        self.mki_and_length = Some((mki, length));
    }
}

impl FromStr for SrtpKeyParam {
    type Err = AttributeError;
    fn from_str(key_param: &str) -> Result<Self, Self::Err> {
        let mut k = key_param.split('|');

        let Some(key_and_salt_with_method) = k.next() else {
            return Err(AttributeError::ParamNotFound {
                param: "Srtp Key and Salt".to_string(),
                attr: Crypto::NAME.to_string(),
            });
        };

        let key_and_salt = if key_and_salt_with_method
            .get(..7)
            .map_or(false, |p| p.eq_ignore_ascii_case("inline:"))
        {
            &key_and_salt_with_method[7..]
        } else {
            return Err(AttributeError::InvalidParamValue {
                param: "Srtp Key and Salt".to_string(),
                val: key_and_salt_with_method.to_string(),
                attr: Crypto::NAME.to_string(),
            });
        };

        let (lifetime, mki_and_length) = if let Some(next_param) = k.next() {
            match next_param.split_once(':') {
                Some(mki_and_length) => {
                    // lifetime is not specified, but only MKI and its length
                    let Ok(mki) = mki_and_length.0.parse::<u32>() else {
                        return Err(AttributeError::InvalidParamValue {
                            param: "MKI".to_string(),
                            val: next_param.to_string(),
                            attr: Crypto::NAME.to_string(),
                        });
                    };

                    let Ok(len) = mki_and_length.1.parse::<u32>() else {
                        return Err(AttributeError::InvalidParamValue {
                            param: "Length".to_string(),
                            val: next_param.to_string(),
                            attr: Crypto::NAME.to_string(),
                        });
                    };
                    (None, Some((mki, len)))
                }
                None => {
                    // lifetime is specified
                    let lifetime = match next_param.strip_prefix("2^") {
                        Some(exp) => {
                            let Ok(exp) = exp.parse::<u32>() else {
                                return Err(AttributeError::InvalidParamValue {
                                    param: "Lifetime".to_string(),
                                    val: next_param.to_string(),
                                    attr: Crypto::NAME.to_string(),
                                });
                            };
                            // 2u32.pow(exp) panics for exp >= 32
                            if exp >= 32 {
                                return Err(AttributeError::InvalidParamValue {
                                    param: "Lifetime".to_string(),
                                    val: format!("{exp}(expected 0-32)"),
                                    attr: Crypto::NAME.to_string(),
                                });
                            }
                            Some(2u32.pow(exp))
                        }
                        None => {
                            let Ok(lifetime) = next_param.parse::<u32>() else {
                                return Err(AttributeError::InvalidParamValue {
                                    param: "Lifetime".to_string(),
                                    val: next_param.to_string(),
                                    attr: Crypto::NAME.to_string(),
                                });
                            };
                            Some(lifetime)
                        }
                    };

                    // now parse the MKI and length
                    let mki_and_length = if let Some(m) = k.next() {
                        if let Some(p) = m.split_once(':') {
                            let Ok(mki) = p.0.parse::<u32>() else {
                                return Err(AttributeError::InvalidParamValue {
                                    param: "MKI".to_string(),
                                    val: m.to_string(),
                                    attr: Crypto::NAME.to_string(),
                                });
                            };

                            let Ok(len) = p.1.parse::<u32>() else {
                                return Err(AttributeError::InvalidParamValue {
                                    param: "Length".to_string(),
                                    val: m.to_string(),
                                    attr: Crypto::NAME.to_string(),
                                });
                            };
                            Some((mki, len))
                        } else {
                            return Err(AttributeError::ParamNotFound {
                                param: "MKI and Length".to_string(),
                                attr: Crypto::NAME.to_string(),
                            });
                        }
                    } else {
                        None
                    };

                    (lifetime, mki_and_length)
                }
            }
        } else {
            (None, None)
        };

        if let Some((_, len)) = mki_and_length {
            if !(1..=128).contains(&len) {
                return Err(AttributeError::InvalidParamValue {
                    param: "MKI length".to_string(),
                    val: len.to_string(),
                    attr: Crypto::NAME.to_string(),
                });
            }
        }

        if let Some(unexpected) = k.next() {
            return Err(AttributeError::UnexpectedTrailingItem {
                val: unexpected.to_string(),
                attr: Crypto::NAME.to_string(),
            });
        }

        Ok(Self {
            key_and_salt: key_and_salt.to_string(),
            lifetime,
            mki_and_length,
        })
    }
}

impl Display for SrtpKeyParam {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "inline:{}", self.key_and_salt)?;
        if let Some(lifetime) = self.lifetime {
            if lifetime.is_power_of_two() {
                write!(f, "|2^{}", lifetime.trailing_zeros())?;
            } else {
                write!(f, "|{lifetime}")?;
            }
        }
        if let Some((mki, length)) = self.mki_and_length {
            write!(f, "|{mki}:{length}")?;
        }
        Ok(())
    }
}

/// Cryptographic information for the media
///
/// See [RFC 4568 Section 4](https://datatracker.ietf.org/doc/html/rfc4568#section-4)
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Crypto {
    pub tag: u32,
    pub crypto_suite: CryptoSuite,
    pub key_params: Vec<SrtpKeyParam>,
    pub session_params: Vec<SrtpSessionParam>,
}

impl Crypto {
    pub fn new(tag: u32, crypto_suite: CryptoSuite) -> Self {
        Crypto {
            tag,
            crypto_suite,
            key_params: vec![],
            session_params: vec![],
        }
    }

    pub fn add_key_param(&mut self, key_param: SrtpKeyParam) {
        self.key_params.push(key_param)
    }

    pub fn add_key_params(&mut self, key_params: impl IntoIterator<Item = SrtpKeyParam>) {
        self.key_params.extend(key_params)
    }

    pub fn add_session_param(&mut self, session_param: SrtpSessionParam) {
        self.session_params.push(session_param)
    }

    pub fn add_session_params(
        &mut self,
        session_params: impl IntoIterator<Item = SrtpSessionParam>,
    ) {
        self.session_params.extend(session_params)
    }
}

impl FromStr for Crypto {
    type Err = AttributeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut i = s.split(' ');

        let Some(tag) = i.next() else {
            return Err(AttributeError::ParamNotFound {
                param: "Tag".to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        let Ok(tag) = tag.parse::<u32>() else {
            return Err(AttributeError::InvalidParamValue {
                param: "Tag".to_string(),
                val: tag.to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        let Some(crypto_suite) = i.next() else {
            return Err(AttributeError::ParamNotFound {
                param: "CryptoSuite".to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        let crypto_suite = CryptoSuite::new(crypto_suite);

        let Some(key_params_str) = i.next() else {
            return Err(AttributeError::ParamNotFound {
                param: "Key params".to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        let mut key_params: Vec<SrtpKeyParam> = Vec::new();

        for key_param in key_params_str.split(';') {
            let key_param = SrtpKeyParam::from_str(key_param)?;
            key_params.push(key_param);
        }

        let mut session_params: Vec<SrtpSessionParam> = Vec::new();
        for s in &mut i {
            let param = if s.get(..4).map_or(false, |p| p.eq_ignore_ascii_case("KDR=")) {
                let kdr_val = &s[4..];
                let Ok(kdr_val) = kdr_val.parse::<u8>() else {
                    return Err(AttributeError::InvalidParamValue {
                        param: "KDR".to_string(),
                        val: kdr_val.to_string(),
                        attr: <Self as TypedAttribute>::NAME.to_string(),
                    });
                };

                // Note: the range for KDR value is conflicting in the spec,
                // rfc4568#section-6.3.1 says the range should be 1,2,...24 and
                // the grammar in rfc4568#section-9.2 says it should be 0..24.
                // So using the bigger range i.e., 0..24 for now
                if !(0..=24).contains(&kdr_val) {
                    return Err(AttributeError::InvalidParamValue {
                        param: "KDR".to_string(),
                        val: format!("{kdr_val}(expected range 0..24)"),
                        attr: <Self as TypedAttribute>::NAME.to_string(),
                    });
                }
                SrtpSessionParam::Kdr(kdr_val)
            } else if s.eq_ignore_ascii_case("UNENCRYPTED_SRTCP") {
                SrtpSessionParam::UnencryptedSrtcp
            } else if s.eq_ignore_ascii_case("UNENCRYPTED_SRTP") {
                SrtpSessionParam::UnencryptedSrtp
            } else if s.eq_ignore_ascii_case("UNAUTHENTICATED_SRTP") {
                SrtpSessionParam::UnauthenticatedSrtp
            } else if s
                .get(..10)
                .map_or(false, |p| p.eq_ignore_ascii_case("FEC_ORDER="))
            {
                let fec_ord = &s[10..];
                if fec_ord.eq_ignore_ascii_case("FEC_SRTP") {
                    SrtpSessionParam::FecOrder(FecOrder::FecSrtp)
                } else if fec_ord.eq_ignore_ascii_case("SRTP_FEC") {
                    SrtpSessionParam::FecOrder(FecOrder::SrtpFec)
                } else {
                    return Err(AttributeError::InvalidParamValue {
                        param: "FEC order".to_string(),
                        val: s.to_string(),
                        attr: <Self as TypedAttribute>::NAME.to_string(),
                    });
                }
            } else if s
                .get(..8)
                .map_or(false, |p| p.eq_ignore_ascii_case("FEC_KEY="))
            {
                let key_params_str = &s[8..];
                let mut key_params: Vec<SrtpKeyParam> = Vec::new();

                for key_param in key_params_str.split(';') {
                    let key_param = SrtpKeyParam::from_str(key_param)?;
                    key_params.push(key_param);
                }
                SrtpSessionParam::FecKey(key_params)
            } else if s.get(..4).map_or(false, |p| p.eq_ignore_ascii_case("WSH=")) {
                let wsh_val = &s[4..];
                let Ok(wsh_val) = wsh_val.parse::<u8>() else {
                    return Err(AttributeError::InvalidParamValue {
                        param: "WSH".to_string(),
                        val: wsh_val.to_string(),
                        attr: <Self as TypedAttribute>::NAME.to_string(),
                    });
                };

                if wsh_val < 64 {
                    return Err(AttributeError::InvalidParamValue {
                        param: "WSH".to_string(),
                        val: wsh_val.to_string(),
                        attr: <Self as TypedAttribute>::NAME.to_string(),
                    });
                }
                SrtpSessionParam::Wsh(wsh_val)
            } else {
                // Extension
                SrtpSessionParam::Extension(s.to_string())
            };
            session_params.push(param);
        }

        Ok(Self {
            tag,
            key_params,
            crypto_suite,
            session_params,
        })
    }
}

impl Display for Crypto {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.tag, self.crypto_suite.as_str())?;

        for (i, key_param) in self.key_params.iter().enumerate() {
            if i == 0 {
                f.write_char(' ')?;
            } else {
                f.write_char(';')?;
            }

            write!(f, "{}", key_param)?;
        }

        for session_param in &self.session_params {
            match session_param {
                SrtpSessionParam::Kdr(kdr) => write!(f, " KDR={kdr}")?,
                SrtpSessionParam::UnencryptedSrtp => write!(f, " UNENCRYPTED_SRTP")?,
                SrtpSessionParam::UnencryptedSrtcp => write!(f, " UNENCRYPTED_SRTCP")?,
                SrtpSessionParam::UnauthenticatedSrtp => write!(f, " UNAUTHENTICATED_SRTP")?,
                SrtpSessionParam::FecOrder(fec_order) => {
                    let order = match fec_order {
                        FecOrder::FecSrtp => "FEC_SRTP",
                        FecOrder::SrtpFec => "SRTP_FEC",
                    };
                    write!(f, " FEC_ORDER={order}")?;
                }
                SrtpSessionParam::FecKey(srtp_key_params) => {
                    write!(f, " FEC_KEY")?;
                    for (i, key_param) in srtp_key_params.iter().enumerate() {
                        if i == 0 {
                            f.write_char('=')?;
                        } else {
                            f.write_char(';')?;
                        }

                        write!(f, "{}", key_param)?;
                    }
                }
                SrtpSessionParam::Wsh(wsh) => write!(f, " WSH={wsh}")?,
                SrtpSessionParam::Extension(extn) => {
                    f.write_char(' ')?;
                    f.write_str(extn)?;
                }
            }
        }
        Ok(())
    }
}

impl TypedAttribute for Crypto {
    const NAME: &'static str = "crypto";
}

/// ICE Candidate attribute of the media
///
/// See [RFC 8839 Section 5.1](https://datatracker.ietf.org/doc/html/rfc8839#section-5.1)
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Candidate {
    /// Arbitrary string used in the freezing algorithm to group similar candidates
    /// See [RFC 8445 Section 5.1.1.3](https://datatracker.ietf.org/doc/html/rfc8445#section-5.1.1.3)
    pub foundation: String,
    /// Identifies the specific component of the data stream
    /// 1 for RTP and 2 for RTCP
    pub component_id: u32,
    /// Transport protocol of the candidate
    pub transport: String,
    /// Candidate's priority
    pub priority: u64,
    /// IP address of the candidate
    /// IPv4, IPv6 addresses and FQDN allowed
    pub address: CandidateAddress,
    /// Port of the candidate
    pub port: u16,
    /// Type of the candidate
    pub typ: CandidateType,
    /// Address related to the candidate
    /// Required for srflx, prflx and relay type candidates
    pub rel_addr: Option<IpAddr>,
    /// Port related to the candidate
    /// Required for srflx, prflx and relay type candidates
    pub rel_port: Option<u16>,
    /// Extensions
    pub extensions: Vec<(String, String)>,
}

impl Candidate {
    pub fn new(
        foundation: impl ToString,
        component_id: u32,
        transport: impl ToString,
        priority: u64,
        address: CandidateAddress,
        port: u16,
        typ: CandidateType,
    ) -> Self {
        Candidate {
            foundation: foundation.to_string(),
            component_id,
            transport: transport.to_string(),
            priority,
            address,
            port,
            typ,
            rel_addr: None,
            rel_port: None,
            extensions: vec![],
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub fn with_rel_addr(
        foundation: impl ToString,
        component_id: u32,
        transport: impl ToString,
        priority: u64,
        address: CandidateAddress,
        port: u16,
        typ: CandidateType,
        rel_addr: impl Into<IpAddr>,
    ) -> Self {
        Candidate {
            foundation: foundation.to_string(),
            component_id,
            transport: transport.to_string(),
            priority,
            address,
            port,
            typ,
            rel_addr: Some(rel_addr.into()),
            rel_port: None,
            extensions: vec![],
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub fn with_rel_addr_and_port(
        foundation: impl ToString,
        component_id: u32,
        transport: impl ToString,
        priority: u64,
        address: CandidateAddress,
        port: u16,
        typ: CandidateType,
        rel_addr: impl Into<IpAddr>,
        rel_port: u16,
    ) -> Self {
        Candidate {
            foundation: foundation.to_string(),
            component_id,
            transport: transport.to_string(),
            priority,
            address,
            port,
            typ,
            rel_addr: Some(rel_addr.into()),
            rel_port: Some(rel_port),
            extensions: vec![],
        }
    }

    pub fn set_rel_addr(&mut self, rel_addr: impl Into<IpAddr>) {
        self.rel_addr = Some(rel_addr.into());
    }

    pub fn set_rel_port(&mut self, rel_port: u16) {
        self.rel_port = Some(rel_port);
    }

    pub fn add_extension(&mut self, name: impl ToString, value: impl ToString) {
        self.extensions.push((name.to_string(), value.to_string()))
    }
}

impl FromStr for Candidate {
    type Err = AttributeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut i = s.split(' ');

        let Some(foundation) = i.next() else {
            return Err(AttributeError::ParamNotFound {
                param: "Foundation".to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        let Some(comp_id) = i.next() else {
            return Err(AttributeError::ParamNotFound {
                param: "Component id".to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        let Ok(comp_id) = comp_id.parse::<u32>() else {
            return Err(AttributeError::InvalidParamValue {
                param: "Component id".to_string(),
                val: comp_id.to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        let Some(transport) = i.next() else {
            return Err(AttributeError::ParamNotFound {
                param: "Transport".to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        let Some(priority) = i.next() else {
            return Err(AttributeError::ParamNotFound {
                param: "Priority".to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        let Ok(priority) = priority.parse::<u64>() else {
            return Err(AttributeError::InvalidParamValue {
                param: "Priority".to_string(),
                val: priority.to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        let Some(address) = i.next() else {
            return Err(AttributeError::ParamNotFound {
                param: "Address".to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        let address = match address.parse::<IpAddr>() {
            Ok(a) => CandidateAddress::IpAddr(a),
            Err(_) => CandidateAddress::FQDN(address.to_string()),
        };

        let Some(port) = i.next() else {
            return Err(AttributeError::ParamNotFound {
                param: "Port".to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        let Ok(port) = port.parse::<u16>() else {
            return Err(AttributeError::InvalidParamValue {
                param: "Port".to_string(),
                val: port.to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        let Some(typ_str) = i.next() else {
            return Err(AttributeError::ParamNotFound {
                param: "'typ' string".to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        if !typ_str.eq_ignore_ascii_case("typ") {
            return Err(AttributeError::ParamNotFound {
                param: "'typ' string".to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        }

        let Some(cand_type) = i.next() else {
            return Err(AttributeError::ParamNotFound {
                param: "Candidate type".to_string(),
                attr: <Self as TypedAttribute>::NAME.to_string(),
            });
        };

        let cand_type = CandidateType::new(cand_type);

        let mut rel_addr: Option<IpAddr> = None;
        let mut rel_port: Option<u16> = None;
        let mut exts: Vec<(String, String)> = Vec::new();

        while let Some(key) = i.next() {
            if key.eq_ignore_ascii_case("raddr") {
                let Some(raddr) = i.next() else {
                    return Err(AttributeError::ParamNotFound {
                        param: "Relative address".to_string(),
                        attr: <Self as TypedAttribute>::NAME.to_string(),
                    });
                };

                if let Ok(raddr) = raddr.parse::<IpAddr>() {
                    rel_addr = Some(raddr);
                } else {
                    return Err(AttributeError::InvalidParamValue {
                        param: "Relative address".to_string(),
                        val: raddr.to_string(),
                        attr: <Self as TypedAttribute>::NAME.to_string(),
                    });
                };
            } else if key.eq_ignore_ascii_case("rport") {
                let Some(rport) = i.next() else {
                    return Err(AttributeError::ParamNotFound {
                        param: "Relative port".to_string(),
                        attr: <Self as TypedAttribute>::NAME.to_string(),
                    });
                };

                if let Ok(rport) = rport.parse::<u16>() {
                    rel_port = Some(rport);
                } else {
                    return Err(AttributeError::InvalidParamValue {
                        param: "Relative port".to_string(),
                        val: rport.to_string(),
                        attr: <Self as TypedAttribute>::NAME.to_string(),
                    });
                }
            } else {
                let Some(val) = i.next() else {
                    return Err(AttributeError::Other {
                        error: format!("No val for the extension {key}"),
                        attr: <Self as TypedAttribute>::NAME.to_string(),
                    });
                };

                exts.push((key.to_string(), val.to_string()));
            }
        }

        Ok(Self {
            foundation: foundation.to_string(),
            component_id: comp_id,
            transport: transport.to_string(),
            priority,
            address,
            port,
            typ: cand_type,
            rel_addr,
            rel_port,
            extensions: exts,
        })
    }
}

impl Display for Candidate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let candidate_addr = match &self.address {
            CandidateAddress::IpAddr(a) => a.to_string(),
            CandidateAddress::FQDN(d) => d.clone(),
        };
        write!(
            f,
            "{} {} {} {} {} {} typ {}",
            self.foundation,
            self.component_id,
            self.transport,
            self.priority,
            candidate_addr,
            self.port,
            self.typ.as_str(),
        )?;
        if let Some(rel_addr) = self.rel_addr {
            write!(f, " raddr {rel_addr}")?;
        }
        if let Some(rel_port) = self.rel_port {
            write!(f, " rport {rel_port}")?;
        }
        for (key, val) in &self.extensions {
            write!(f, " {key} {val}")?;
        }
        Ok(())
    }
}

impl TypedAttribute for Candidate {
    const NAME: &'static str = "candidate";
}

#[cfg(test)]
mod tests {
    use std::net::{IpAddr, Ipv4Addr, Ipv6Addr};

    use super::*;
    use crate::*;

    #[test]
    fn direction_parse() {
        assert_eq!("sendonly".parse::<Direction>(), Ok(Direction::SendOnly));
        assert_eq!("recvonly".parse::<Direction>(), Ok(Direction::RecvOnly));
        assert_eq!("sendrecv".parse::<Direction>(), Ok(Direction::SendRecv));
        assert_eq!("inactive".parse::<Direction>(), Ok(Direction::Inactive));
        assert!("invalid".parse::<Direction>().is_err());
    }

    #[test]
    fn direction_display() {
        assert_eq!(Direction::SendOnly.to_string(), "sendonly");
        assert_eq!(Direction::RecvOnly.to_string(), "recvonly");
        assert_eq!(Direction::SendRecv.to_string(), "sendrecv");
        assert_eq!(Direction::Inactive.to_string(), "inactive");
    }

    #[test]
    fn parse_rtcp_fb() {
        let sdp = "v=0\r
o=alice 3203093520 3203093520 IN IP4 host.example.com\r
s=Multicast video with feedback\r
t=3203130148 3203137348\r
m=audio 49170 RTP/AVP 0\r
c=IN IP4 224.2.1.183\r
a=rtpmap:0 PCMU/8000\r
m=video 51372 RTP/AVPF 98 99\r
c=IN IP4 224.2.1.184\r
a=rtpmap:98 H263-1998/90000\r
a=rtpmap:99 H261/90000\r
a=rtcp-fb:* nack\r
a=rtcp-fb:98 nack rpsi\r
a=rtcp-fb:* trr-int 1000\r
a=rtcp-fb:98 ccm vbcm 1 2\r
a=rtcp-fb:* ccm tmmbr smaxpr=120\r
";

        let parsed = Session::parse(sdp.as_bytes()).unwrap();
        let mut written = vec![];
        parsed.write(&mut written).unwrap();

        let v = fallible_iterator::convert(parsed.medias[1].attributes_typed::<RtcpFb>())
            .collect::<Vec<_>>()
            .expect("Valid vector of attributes");
        assert_eq!(v[0].pt, RtcpFbPt::Wildcard);
        assert_eq!(v[1].val, RtcpFbVal::Nack(Some(RtcpFbNack::Rpsi)));
        assert_eq!(v[2].val, RtcpFbVal::TrrInt(1000));
        assert_eq!(v[3].val, RtcpFbVal::Ccm(RtcpFbCcm::Vbcm(vec![1, 2])));
        assert_eq!(
            v[4].val,
            RtcpFbVal::Ccm(RtcpFbCcm::Tmmbr(Some("smaxpr=120".to_string())))
        );
    }

    #[test]
    fn parse_group_attribute() {
        let sdp = "v=0\r
o=Laura 289083124 289083124 IN IP4 two.example.com\r
c=IN IP4 233.252.0.1/127\r
t=0 0\r
a=group:LS 1 2\r
m=audio 30000 RTP/AVP 0\r
a=mid:1\r
m=video 30002 RTP/AVP 31\r
a=mid:2\r
m=audio 30004 RTP/AVP 0\r
i=This media stream contains the Spanish translation\r
a=mid:3\r
";
        let parsed = Session::parse(sdp.as_bytes()).unwrap();

        let g = parsed.attributes_typed::<Group>().collect::<Vec<_>>();
        assert_eq!(g.len(), 1);
        assert_eq!(g[0].as_ref().unwrap().semantics, GroupSemantics::LS);
        assert_eq!(
            g[0].as_ref().unwrap().mid_tags,
            vec!["1".to_string(), "2".to_string()]
        );
    }

    #[test]
    fn parse_setup_attribute() {
        let sdp = "v=0\r
m=image 54111 TCP t38\r
c=IN IP4 192.0.2.2\r
a=setup:actpass\r
a=connection:new\r
";
        let media = Session::parse(sdp.as_bytes()).unwrap().medias;

        let s = media[0].attributes_typed::<Setup>().collect::<Vec<_>>();

        assert_eq!(s.len(), 1);
        assert_eq!(s[0].as_ref().unwrap().to_owned(), Setup::ActPass);
    }

    #[test]
    fn parse_ssrc_attributes() {
        let sdp = "v=0\r
o=jdoe 2890844526 2890842807 IN IP4 10.47.16.5\r
m=video 49174 RTP/AVPF 96 98\r
a=rtpmap:98 rtx/90000\r
a=fmtp:98 apt=96;rtx-time=3000\r
a=ssrc-group:FID 11111 22222\r
a=ssrc:11111 cname:user3@example.com\r
a=ssrc:22222 fmtp:0 0-15\r
a=ssrc-group:FID 33333 44444\r
a=ssrc:33333 cname:user3@example.com\r
a=ssrc:44444 cname:user3@example.com\r
a=ssrc:1698359993 ts-refclk:ntp=pool.ntp.org
";

        let parsed = Session::parse(sdp.as_bytes()).unwrap();
        let m = &parsed.medias[0];

        let ssrcs = m
            .attributes_typed::<Ssrc>()
            .filter(|s| {
                let Ok(ssrc) = s else { return false };
                ssrc.attribute == SsrcAttribute::Fmtp
                    || matches!(ssrc.attribute, SsrcAttribute::Other(_))
            })
            .collect::<Vec<_>>();

        let ssrc_id = ssrcs[0].as_ref().unwrap().ssrc_id;

        let ssrc_groups = m
            .attributes_typed::<SsrcGroup>()
            .filter(|s| {
                let Ok(ssrc_group) = s else { return false };

                ssrc_group.ssrc_ids[1] == ssrc_id
            })
            .collect::<Vec<_>>();

        assert_eq!(
            ssrc_groups[0].as_ref().unwrap().semantics,
            GroupSemantics::FID
        );

        assert_eq!(
            ssrcs[1].as_ref().unwrap().attribute,
            SsrcAttribute::Other("ts-refclk".to_string())
        );
    }

    #[test]
    fn parse_crypto_attributes() {
        let sdp = "v=0\r
o=sam 2890844526 2890842807 IN IP4 10.47.16.5\r
s=SRTP Discussion\r
i=A discussion of Secure RTP\r
u=http://www.example.com/seminars/srtp.pdf\r
e=marge@example.com (Marge Simpson)\r
c=IN IP4 168.2.17.12\r
t=2873397496 2873404696\r
m=audio 49170 RTP/SAVP 0\r
a=crypto:1 AES_CM_128_HMAC_SHA1_80 inline:WVNfX19zZW1jdGwgKCkgewkyMjA7fQp9CnVubGVz|2^20|1:4 FEC_ORDER=SRTP_FEC\r
a=crypto:2 F8_128_HMAC_SHA1_80 inline:MTIzNDU2Nzg5QUJDREUwMTIzNDU2Nzg5QUJjZGVm|2^20|1:4;inline:QUJjZGVmMTIzNDU2Nzg5QUJDREUwMTIzNDU2Nzg5|2^20|2:4 FEC_ORDER=FEC_SRTP\r
m=video 51372 RTP/SAVP 31\r
a=crypto:1 AES_CM_128_HMAC_SHA1_80 inline:YUJDZGVmZ2hpSktMbW9QUXJzVHVWd3l6MTIzNDU2|1066:4\r
";

        let parsed = Session::parse(sdp.as_bytes()).unwrap();
        let a = &parsed.medias[0];

        let audio_cryptos = a.attributes_typed::<Crypto>().collect::<Vec<_>>();

        assert_eq!(
            audio_cryptos[0].as_ref().unwrap().crypto_suite,
            CryptoSuite::AesCm128HmacSha1_80
        );
        assert_eq!(audio_cryptos[1].as_ref().unwrap().key_params.len(), 2);

        assert_eq!(
            audio_cryptos[1].as_ref().unwrap().key_params[1].mki_and_length,
            Some((2, 4))
        );

        assert_eq!(
            audio_cryptos[1].as_ref().unwrap().session_params[0],
            SrtpSessionParam::FecOrder(FecOrder::FecSrtp)
        );

        let v = &parsed.medias[1];

        let video_cryptos = v
            .attributes_typed::<Crypto>()
            .filter(|c| {
                let Ok(crypto) = c else { return false };

                crypto.tag == 1
            })
            .collect::<Vec<_>>();

        let test_crypto = Crypto {
            tag: 1,
            crypto_suite: CryptoSuite::AesCm128HmacSha1_80,
            key_params: vec![SrtpKeyParam {
                key_and_salt: "YUJDZGVmZ2hpSktMbW9QUXJzVHVWd3l6MTIzNDU2".to_string(),
                lifetime: None,
                mki_and_length: Some((1066, 4)),
            }],
            session_params: Vec::new(),
        };

        assert_eq!(&test_crypto, video_cryptos[0].as_ref().unwrap());
    }

    #[test]
    fn write_crypto_attribute() {
        let crypto = Crypto {
            tag: 1,
            crypto_suite: CryptoSuite::AesCm128HmacSha1_80,
            key_params: vec![
                SrtpKeyParam {
                    key_and_salt: "WVNfX19zZW1jdGwgKCkgewkyMjA7fQp9CnVubGVz".to_string(),
                    lifetime: Some(1048576),
                    mki_and_length: Some((1, 4)),
                },
                SrtpKeyParam {
                    key_and_salt: "WVNfX19zZW1jdGwgKCkgewkyMjA7fQp9CnVubGVz".to_string(),
                    lifetime: Some(1048576),
                    mki_and_length: Some((1, 4)),
                },
            ],
            session_params: vec![SrtpSessionParam::FecOrder(FecOrder::SrtpFec)],
        };

        assert_eq!(
            crypto.to_string(),
            "1 AES_CM_128_HMAC_SHA1_80 inline:WVNfX19zZW1jdGwgKCkgewkyMjA7fQp9CnVubGVz|2^20|1:4;inline:WVNfX19zZW1jdGwgKCkgewkyMjA7fQp9CnVubGVz|2^20|1:4 FEC_ORDER=SRTP_FEC"
        );
    }

    #[test]
    fn parse_candidate_attributes() {
        use std::net::{Ipv4Addr, Ipv6Addr};

        let sdp = "v=0\r
o=- 2890844526 2890842807 IN IP4 192.168.1.1\r
s=-\r
c=IN IP4 192.168.1.1\r
t=0 0\r
m=audio 49152 RTP/AVP 0\r
a=candidate:1 1 UDP 2130706432 192.168.1.1 49152 typ host raddr 10.0.1.1 rport 49153 generation 0\r
a=candidate:2 1 UDP 1692467200 10.0.1.1 49152 typ srflx raddr 192.168.1.1 rport 49153\r
a=candidate:3 2 UDP 1692467184 192.168.1.1 49153 typ host\r
a=candidate:4 1 UDP 100 2001:db8::1 49152 typ host\r
a=candidate:5 1 UDP 50 192.168.1.1 49154 typ prflx\r
a=candidate:6 1 UDP 25 192.168.1.1 49155 typ relay raddr 10.0.0.1 rport 49156\r
a=candidate:7 1 UDP 10 192.168.1.1 49157 typ unknown_type\r
";

        let session = Session::parse(sdp.as_bytes()).unwrap();
        let candidates: Vec<Candidate> =
            fallible_iterator::convert(session.medias[0].attributes_typed::<Candidate>())
                .collect::<Vec<_>>()
                .expect("Valid vector of candidates");

        assert_eq!(candidates.len(), 7);

        assert_eq!(candidates[0].foundation, "1");
        assert_eq!(candidates[0].component_id, 1);
        assert_eq!(
            candidates[0].address,
            CandidateAddress::IpAddr(IpAddr::V4(Ipv4Addr::new(192, 168, 1, 1)))
        );
        assert_eq!(candidates[0].port, 49152);
        assert_eq!(candidates[0].typ, CandidateType::Host);
        assert_eq!(
            candidates[0].rel_addr,
            Some(IpAddr::V4(Ipv4Addr::new(10, 0, 1, 1)))
        );
        assert_eq!(candidates[0].rel_port, Some(49153));
        assert_eq!(
            candidates[0].extensions,
            vec![("generation".to_string(), "0".to_string())]
        );

        assert_eq!(candidates[1].foundation, "2");
        assert_eq!(candidates[1].typ, CandidateType::Srflx);
        assert_eq!(
            candidates[1].rel_addr,
            Some(IpAddr::V4(Ipv4Addr::new(192, 168, 1, 1)))
        );
        assert_eq!(candidates[1].rel_port, Some(49153));

        assert_eq!(candidates[2].foundation, "3");
        assert_eq!(candidates[2].component_id, 2);
        assert_eq!(candidates[2].typ, CandidateType::Host);

        assert_eq!(candidates[3].foundation, "4");
        assert_eq!(
            candidates[3].address,
            CandidateAddress::IpAddr(IpAddr::V6(Ipv6Addr::new(0x2001, 0xdb8, 0, 0, 0, 0, 0, 1)))
        );
        assert_eq!(candidates[3].typ, CandidateType::Host);

        assert_eq!(candidates[4].foundation, "5");
        assert_eq!(candidates[4].typ, CandidateType::Prflx);

        assert_eq!(candidates[5].foundation, "6");
        assert_eq!(candidates[5].typ, CandidateType::Relay);

        assert_eq!(candidates[6].foundation, "7");
        assert_eq!(
            candidates[6].typ,
            CandidateType::Other("unknown_type".to_string())
        );
    }

    #[test]
    fn write_candidate() {
        use std::net::Ipv4Addr;

        let candidate = Candidate {
            foundation: "abcd/1234".into(),
            component_id: 1,
            transport: "UDP".into(),
            priority: 2130706432,
            address: CandidateAddress::IpAddr(IpAddr::V4(Ipv4Addr::new(192, 168, 0, 1))),
            port: 49152,
            typ: CandidateType::Srflx,
            rel_addr: Some(IpAddr::V4(Ipv4Addr::new(10, 0, 0, 1))),
            rel_port: Some(49153),
            extensions: vec![("tcptype".to_string(), "active".to_string())],
        };

        assert_eq!(
            candidate.to_string(),
            "abcd/1234 1 UDP 2130706432 192.168.0.1 49152 typ srflx raddr 10.0.0.1 rport 49153 tcptype active"
        );
    }

    #[test]
    fn test_attribute_errors() {
        // Test RtpMap error paths
        assert_eq!(
            "99".parse::<RtpMap>().unwrap_err(),
            AttributeError::UnsupportedFormat {
                val: "99".to_string(),
                attr: "rtpmap".to_string()
            }
        );
        assert_eq!(
            "abc 90000".parse::<RtpMap>().unwrap_err(),
            AttributeError::InvalidParamValue {
                param: "Payload type".to_string(),
                val: "abc".to_string(),
                attr: "rtpmap".to_string()
            }
        );
        assert_eq!(
            "200 enc/90000".parse::<RtpMap>().unwrap_err(),
            AttributeError::InvalidParamValue {
                param: "Payload type".to_string(),
                val: "200(expected 0-127)".to_string(),
                attr: "rtpmap".to_string()
            }
        );
        assert_eq!(
            "99 ".parse::<RtpMap>().unwrap_err(),
            AttributeError::ParamNotFound {
                param: "Clock rate".to_string(),
                attr: "rtpmap".to_string()
            }
        );
        assert_eq!(
            "99 /".parse::<RtpMap>().unwrap_err(),
            AttributeError::InvalidParamValue {
                param: "Clock rate".to_string(),
                val: "".to_string(),
                attr: "rtpmap".to_string()
            }
        );

        assert_eq!(
            "invalid".parse::<Fmtp>().unwrap_err(),
            AttributeError::UnsupportedFormat {
                val: "invalid".to_string(),
                attr: "fmtp".to_string()
            }
        );
        assert_eq!(
            "abc profile=1".parse::<Fmtp>().unwrap_err(),
            AttributeError::InvalidParamValue {
                param: "fmtp".to_string(),
                val: "abc".to_string(),
                attr: "fmtp".to_string()
            }
        );

        // Test Rtcp error paths
        assert_eq!(
            "".parse::<Rtcp>().unwrap_err(),
            AttributeError::InvalidParamValue {
                param: "Port".to_string(),
                val: "".to_string(),
                attr: "rtcp".to_string()
            }
        );
        assert_eq!(
            "abc IN IP4 127.0.0.1".parse::<Rtcp>().unwrap_err(),
            AttributeError::InvalidParamValue {
                param: "Port".to_string(),
                val: "abc".to_string(),
                attr: "rtcp".to_string()
            }
        );

        // Test Fingerprint error paths
        assert_eq!(
            "".parse::<Fingerprint>().unwrap_err(),
            AttributeError::ParamNotFound {
                param: "Hash value".to_string(),
                attr: "fingerprint".to_string()
            }
        );
        assert_eq!(
            "SHA-1".parse::<Fingerprint>().unwrap_err(),
            AttributeError::ParamNotFound {
                param: "Hash value".to_string(),
                attr: "fingerprint".to_string()
            }
        );

        // Test Candidate error paths
        assert_eq!(
            "".parse::<Candidate>().unwrap_err(),
            AttributeError::ParamNotFound {
                param: "Component id".to_string(),
                attr: "candidate".to_string()
            }
        );
        assert_eq!(
            "1 1 UDP 100".parse::<Candidate>().unwrap_err(),
            AttributeError::ParamNotFound {
                param: "Address".to_string(),
                attr: "candidate".to_string()
            }
        );

        // Test ExtMap error paths
        assert_eq!(
            "".parse::<ExtMap>().unwrap_err(),
            AttributeError::InvalidParamValue {
                param: "Id".to_string(),
                val: "".to_string(),
                attr: "extmap".to_string()
            }
        );
        assert_eq!(
            "999999 http://example.com".parse::<ExtMap>().unwrap_err(),
            AttributeError::InvalidParamValue {
                param: "Id".to_string(),
                val: "999999".to_string(),
                attr: "extmap".to_string()
            }
        );

        // Test Group error paths
        assert_eq!(
            "".parse::<Group>().unwrap_err(),
            AttributeError::ParamNotFound {
                param: "Media identification tags".to_string(),
                attr: "group".to_string()
            }
        );
        assert_eq!(
            "LS".parse::<Group>().unwrap_err(),
            AttributeError::ParamNotFound {
                param: "Media identification tags".to_string(),
                attr: "group".to_string()
            }
        );

        // Test Ssrc error paths
        assert_eq!(
            "".parse::<Ssrc>().unwrap_err(),
            AttributeError::ParamNotFound {
                param: "Ssrc id".to_string(),
                attr: "ssrc".to_string()
            }
        );
        assert_eq!(
            "abc".parse::<Ssrc>().unwrap_err(),
            AttributeError::ParamNotFound {
                param: "Ssrc id".to_string(),
                attr: "ssrc".to_string()
            }
        );

        // Test Setup error paths
        let setup_err = "foo".parse::<Setup>().err().unwrap();
        assert!(matches!(setup_err, AttributeError::Other { .. }));
        assert_eq!(format!("{}", setup_err), "setup: Invalid Setup value foo");

        // Test Crypto error paths
        assert_eq!(
            "".parse::<Crypto>().unwrap_err(),
            AttributeError::InvalidParamValue {
                param: "Tag".to_string(),
                val: "".to_string(),
                attr: "crypto".to_string()
            }
        );
        assert_eq!(
            "abc AES_CM_128_HMAC_SHA1_32 inline:key"
                .parse::<Crypto>()
                .unwrap_err(),
            AttributeError::InvalidParamValue {
                param: "Tag".to_string(),
                val: "abc".to_string(),
                attr: "crypto".to_string()
            }
        );

        // Test RtcpFb error paths
        assert_eq!(
            "".parse::<RtcpFb>().unwrap_err(),
            AttributeError::InvalidParamValue {
                param: "Payload format".to_string(),
                val: "".to_string(),
                attr: "rtcp-fb".to_string()
            }
        );
        assert_eq!(
            "*".parse::<RtcpFb>().unwrap_err(),
            AttributeError::ParamNotFound {
                param: "Rtcp feedback value".to_string(),
                attr: "rtcp-fb".to_string()
            }
        );
        assert_eq!(
            "1 ack ccfb".parse::<RtcpFb>().unwrap_err(),
            AttributeError::InvalidParamValue {
                param: "Payload type of Congestion control feedback (ccfb)".to_string(),
                val: "1(expected wildcard (*))".to_string(),
                attr: "rtcp-fb".to_string()
            }
        );

        // Test attribute_typed with missing value
        let media = Media {
            media: "video".into(),
            port: 1234,
            num_ports: None,
            proto: "RTP/SAVPF".into(),
            fmt: "".into(),
            media_title: None,
            connections: vec![],
            bandwidths: vec![],
            key: None,
            attributes: vec![Attribute {
                attribute: "rtpmap".into(),
                value: None,
            }],
        };
        assert_eq!(
            media
                .attributes_typed::<RtpMap>()
                .collect::<Vec<Result<RtpMap, AttributeError>>>()
                .remove(0)
                .unwrap_err(),
            AttributeError::Other {
                error: "No value for the attribute".to_string(),
                attr: "rtpmap".to_string()
            }
        );
    }

    #[test]
    fn parse_rtcp_address() {
        let sdp = "v=0\r
o=alice 3203093520 3203093520 IN IP4 host.example.com\r
s=parse rtcp attribute address test\r
a=rtcp:5000 IN IP4 127.0.0.1\r
a=rtcp:5000 IN IP4 127.0.0.0/24\r
a=rtcp:5000 IN IP6 ::1\r
a=rtcp:5000 IN NONIP non-IP\r
";

        let parsed = Session::parse(sdp.as_bytes()).unwrap();
        let mut rtcp_attr_iter = parsed.attributes_typed::<Rtcp>();

        let rtcp = rtcp_attr_iter.next().unwrap().unwrap();
        assert_eq!(rtcp.addrtype, AddrType::Ip4);
        assert_eq!(&rtcp.connection_address, "127.0.0.1");
        assert_eq!(
            rtcp.try_parse_connection_ip_address().unwrap(),
            IpAddr::V4(Ipv4Addr::LOCALHOST),
        );

        let rtcp = rtcp_attr_iter.next().unwrap().unwrap();
        assert_eq!(rtcp.addrtype, AddrType::Ip4);
        assert_eq!(&rtcp.connection_address, "127.0.0.0/24");
        assert_eq!(
            &rtcp
                .try_parse_connection_ip_address()
                .expect_err("IPv4 with mask")
                .to_string(),
            "127.0.0.0/24"
        );

        let rtcp = rtcp_attr_iter.next().unwrap().unwrap();
        assert_eq!(rtcp.addrtype, AddrType::Ip6);
        assert_eq!(&rtcp.connection_address, "::1");
        assert_eq!(
            rtcp.try_parse_connection_ip_address().unwrap(),
            IpAddr::V6(Ipv6Addr::LOCALHOST),
        );

        let rtcp = rtcp_attr_iter.next().unwrap().unwrap();
        assert_eq!(rtcp.addrtype, AddrType::Other("NONIP".to_string()));
        assert_eq!(&rtcp.connection_address, "non-IP");
        assert_eq!(
            rtcp.try_parse_connection_ip_address().unwrap_err(),
            "non-IP",
        );
    }
}
