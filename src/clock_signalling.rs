use std::fmt;
use std::str::FromStr;

use crate::attributes::{AttributeError, ErrorContext, TypedAttribute};

/// Reference clock.
///
/// This maps to the `ts-refclk` attribute.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReferenceClock {
    Ntp(Ntp),
    Ptp(Ptp),
    Gps,
    Gal,
    Glonass,
    Local,
    Private(PrivateSource),
    Ext(ClockSourceExt),
}

impl ReferenceClock {
    /// Constructs an NTP [`ReferenceClock`] from the specified hostname.
    pub fn from_ntp_hostname(hostname: impl ToString) -> Self {
        NtpServerAddr::from_hostname(hostname).into()
    }

    /// Constructs an NTP [`ReferenceClock`] from the specified hostname and port.
    pub fn from_ntp_hostname_with_port(hostname: impl ToString, port: u16) -> Self {
        NtpServerAddr::from_hostname_with_port(hostname, port).into()
    }

    /// Constructs a traceable NTP [`ReferenceClock`].
    pub fn new_ntp_traceable() -> Self {
        NtpServerAddr::new_traceable().into()
    }

    /// Constructs a PTP [`ReferenceClock`] from the specified [`PtpVersion`] and GMID.
    pub fn from_ptp_gmid(version: PtpVersion, gmid: impl Into<Eui64>) -> Self {
        Ptp::from_gmid(version, gmid).into()
    }

    /// Constructs a PTP [`ReferenceClock`] from the specified GMID and [`PtpDomain`].
    pub fn from_ptp_gmid_with_domain(gmid: impl Into<Eui64>, domain: PtpDomain) -> Self {
        Ptp::from_gmid_with_domain(gmid, domain).into()
    }

    /// Constructs a PTP [`ReferenceClock`] from the specified GMID and domain name.
    ///
    /// This will assign version IEEE 1588-2002.
    pub fn from_ptp_gmid_with_domain_name(gmid: impl Into<Eui64>, name: impl ToString) -> Self {
        Ptp::from_gmid_with_domain_name(gmid, name).into()
    }

    /// Tries to construct a PTP [`ReferenceClock`] from the specified GMID and domain  number.
    ///
    /// This will assign version IEEE 1588-2008.
    ///
    /// Returns an `Error` if `number` is not in range (0-127) (inclusive)
    pub fn try_from_ptp_gmid_with_domain_number(
        gmid: impl Into<Eui64>,
        number: u8,
    ) -> Result<Self, AttributeError> {
        Ok(Ptp::try_from_gmid_with_domain_number(gmid, number)?.into())
    }

    /// Constructs a traceable PTP [`ReferenceClock`].
    pub fn new_ptp_traceable(version: PtpVersion) -> Self {
        Ptp::new_traceable(version).into()
    }

    /// Constructs a standard private source [`ReferenceClock`].
    pub fn new_private_source_standard() -> Self {
        PrivateSource::Standard.into()
    }

    /// Constructs a traceable private source [`ReferenceClock`].
    pub fn new_private_source_traceable() -> Self {
        PrivateSource::Traceable.into()
    }

    /// Constructs an extended clock source [`ReferenceClock`] from the specified name.
    pub fn from_clock_source_ext_name(name: impl ToString) -> Self {
        ClockSourceExt::new(name).into()
    }

    /// Constructs an extended clock source [`ReferenceClock`] from the specified name and value.
    pub fn from_clock_source_ext_name_with_value(
        name: impl ToString,
        value: impl ToString,
    ) -> Self {
        ClockSourceExt::with_value(name, value).into()
    }
}

impl FromStr for ReferenceClock {
    type Err = AttributeError;

    fn from_str(s: &str) -> Result<Self, AttributeError> {
        if let Some(ntp) = s.strip_prefix("ntp=") {
            Ok(Self::Ntp(
                Ntp::from_str(ntp).with_attr(<Self as TypedAttribute>::NAME)?,
            ))
        } else if let Some(ptp) = s.strip_prefix("ptp=") {
            Ok(Self::Ptp(
                Ptp::from_str(ptp).with_attr(<Self as TypedAttribute>::NAME)?,
            ))
        } else if s == "gps" {
            Ok(Self::Gps)
        } else if s == "gal" {
            Ok(Self::Gal)
        } else if s == "glonass" {
            Ok(Self::Glonass)
        } else if s == "local" {
            Ok(Self::Local)
        } else if s == "private" {
            Ok(Self::Private(PrivateSource::Standard))
        } else if s == "private:traceable" {
            Ok(Self::Private(PrivateSource::Traceable))
        } else {
            Ok(Self::Ext(
                ClockSourceExt::from_str(s).with_attr(<Self as TypedAttribute>::NAME)?,
            ))
        }
    }
}

impl fmt::Display for ReferenceClock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ntp(ntp) => write!(f, "ntp={ntp}"),
            Self::Ptp(ptp) => write!(f, "ptp={ptp}"),
            Self::Gps => write!(f, "gps"),
            Self::Gal => write!(f, "gal"),
            Self::Glonass => write!(f, "glonass"),
            Self::Local => write!(f, "local"),
            Self::Private(private) => {
                if *private == PrivateSource::Traceable {
                    write!(f, "private:traceable")
                } else {
                    write!(f, "private")
                }
            }
            Self::Ext(ext) => write!(f, "{ext}"),
        }
    }
}

impl TypedAttribute for ReferenceClock {
    const NAME: &'static str = "ts-refclk";
}

impl From<Ntp> for ReferenceClock {
    fn from(ntp: Ntp) -> Self {
        ReferenceClock::Ntp(ntp)
    }
}

impl From<NtpServerAddr> for ReferenceClock {
    fn from(server: NtpServerAddr) -> Self {
        ReferenceClock::Ntp(Ntp { server })
    }
}

impl From<Ptp> for ReferenceClock {
    fn from(ptp: Ptp) -> Self {
        ReferenceClock::Ptp(ptp)
    }
}

impl From<(PtpVersion, PtpServer)> for ReferenceClock {
    fn from(ptp: (PtpVersion, PtpServer)) -> Self {
        ReferenceClock::Ptp(Ptp::new(ptp.0, ptp.1))
    }
}

impl From<PrivateSource> for ReferenceClock {
    fn from(private_src: PrivateSource) -> Self {
        ReferenceClock::Private(private_src)
    }
}

impl From<ClockSourceExt> for ReferenceClock {
    fn from(clock_src_ext: ClockSourceExt) -> Self {
        ReferenceClock::Ext(clock_src_ext)
    }
}

/// NTP clock source with server address.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ntp {
    pub server: NtpServerAddr,
}

impl Ntp {
    /// Constructs an [`Ntp`] clock source from the specified hostname.
    pub fn from_hostname(hostname: impl ToString) -> Ntp {
        NtpServerAddr::from_hostname(hostname).into()
    }

    /// Constructs an [`Ntp`] clock source from the specified hostname and port.
    pub fn from_hostname_with_port(hostname: impl ToString, port: u16) -> Ntp {
        NtpServerAddr::from_hostname_with_port(hostname, port).into()
    }

    /// Constructs a traceable [`Ntp`] clock source.
    pub fn new_traceable() -> Ntp {
        NtpServerAddr::new_traceable().into()
    }
}

impl FromStr for Ntp {
    type Err = AttributeError;

    fn from_str(s: &str) -> Result<Self, AttributeError> {
        let server = NtpServerAddr::from_str(s)?;
        Ok(Self { server })
    }
}

impl fmt::Display for Ntp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.server)
    }
}

impl From<NtpServerAddr> for Ntp {
    fn from(server: NtpServerAddr) -> Self {
        Ntp { server }
    }
}

/// NTP server address: hostport or "/traceable/".
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NtpServerAddr {
    HostPort { hostname: String, port: Option<u16> },
    Traceable,
}

impl NtpServerAddr {
    /// Constructs an [`NtpServerAddr`] from the specified hostname.
    pub fn from_hostname(hostname: impl ToString) -> NtpServerAddr {
        NtpServerAddr::HostPort {
            hostname: hostname.to_string(),
            port: None,
        }
    }

    /// Constructs an [`NtpServerAddr`] from the specified hostname and port.
    pub fn from_hostname_with_port(hostname: impl ToString, port: u16) -> NtpServerAddr {
        NtpServerAddr::HostPort {
            hostname: hostname.to_string(),
            port: Some(port),
        }
    }

    /// Constructs a traceable [`NtpServerAddr`].
    pub fn new_traceable() -> NtpServerAddr {
        NtpServerAddr::Traceable
    }

    /// Constructs a [`crate::builders::NtpServerAddr`] from the specified hostname.
    pub fn builder(hostname: impl ToString) -> crate::builders::NtpServerAddr {
        crate::builders::NtpServerAddr::new(hostname)
    }
}

impl FromStr for NtpServerAddr {
    type Err = AttributeError;

    fn from_str(s: &str) -> Result<Self, AttributeError> {
        if s.is_empty() {
            return Err(AttributeError::ParamNotFound {
                param: "NTP server address".to_string(),
                attr: String::new(), // will be set bubbling up
            });
        }

        if s == "/traceable/" {
            Ok(Self::Traceable)
        } else if let Some((hostname, port_str)) = s.rsplit_once(':') {
            if hostname.is_empty() {
                return Err(AttributeError::ParamNotFound {
                    param: "hostname in NTP server address".to_string(),
                    attr: String::new(), // will be set bubbling up
                });
            }

            let port = port_str
                .parse::<u16>()
                .map_err(|_| AttributeError::InvalidParamValue {
                    param: "port in NTP server address".to_string(),
                    val: port_str.to_string(),
                    attr: String::new(), // will be set bubbling up
                })?;

            Ok(Self::HostPort {
                hostname: hostname.to_string(),
                port: Some(port),
            })
        } else {
            Ok(Self::HostPort {
                hostname: s.to_string(),
                port: None,
            })
        }
    }
}

impl fmt::Display for NtpServerAddr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::HostPort {
                hostname,
                port: Some(port),
            } => write!(f, "{hostname}:{port}"),
            Self::HostPort {
                hostname,
                port: None,
            } => write!(f, "{hostname}"),
            Self::Traceable => write!(f, "/traceable/"),
        }
    }
}

/// PTP clock source with version and server details.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ptp {
    pub version: PtpVersion,
    pub server: PtpServer,
}

impl Ptp {
    /// Constructs a [`Ptp`] with the specified [`PtpVersion`] and [`PtpServer`].
    pub fn new(version: PtpVersion, server: PtpServer) -> Self {
        Ptp { version, server }
    }

    /// Constructs a [`Ptp`] from the specified [`PtpVersion`] and GMID.
    pub fn from_gmid(version: PtpVersion, gmid: impl Into<Eui64>) -> Self {
        Ptp {
            version,
            server: PtpServer::from_gmid(gmid),
        }
    }

    /// Constructs a [`Ptp`] from the specified GMID and [`PtpDomain`].
    pub fn from_gmid_with_domain(gmid: impl Into<Eui64>, domain: PtpDomain) -> Self {
        let version = match domain {
            PtpDomain::DomainName { .. } => PtpVersion::Ieee1588_2002,
            PtpDomain::DomainNumber(_) => PtpVersion::Ieee1588_2008,
        };

        Ptp {
            version,
            server: PtpServer::from_gmid_with_domain(gmid, domain),
        }
    }

    /// Constructs a [`Ptp`] from the specified GMID and domain name.
    ///
    /// This will assign version IEEE 1588-2002.
    pub fn from_gmid_with_domain_name(gmid: impl Into<Eui64>, name: impl ToString) -> Self {
        Self::from_gmid_with_domain(gmid, PtpDomain::from_name(name))
    }

    /// Tries to construct a [`Ptp`] from the specified GMID and domain  number.
    ///
    /// This will assign version IEEE 1588-2008.
    ///
    /// Returns an `Error` if `number` is not in range (0-127) (inclusive)
    pub fn try_from_gmid_with_domain_number(
        gmid: impl Into<Eui64>,
        number: u8,
    ) -> Result<Self, AttributeError> {
        Ok(Self::from_gmid_with_domain(
            gmid,
            PtpDomain::try_from_number(number)?,
        ))
    }

    /// Constructs a traceable [`Ptp`].
    pub fn new_traceable(version: PtpVersion) -> Self {
        Ptp {
            version,
            server: PtpServer::new_traceable(),
        }
    }
}

impl From<(PtpVersion, PtpServer)> for Ptp {
    fn from(ptp: (PtpVersion, PtpServer)) -> Self {
        Ptp::new(ptp.0, ptp.1)
    }
}

impl FromStr for Ptp {
    type Err = AttributeError;

    fn from_str(s: &str) -> Result<Self, AttributeError> {
        let (version_str, s) = s
            .split_once(':')
            .ok_or_else(|| AttributeError::ParamNotFound {
                param: "PTP version".to_string(),
                attr: String::new(), // will be set bubbling up
            })?;
        let version = PtpVersion::from_str(version_str)?;

        if s.is_empty() {
            return Err(AttributeError::ParamNotFound {
                param: "PTP server".to_string(),
                attr: String::new(), // will be set bubbling up
            });
        }

        let server = PtpServer::from_str(s)?;

        Ok(Self { version, server })
    }
}

impl fmt::Display for Ptp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.version, self.server)
    }
}

/// PTP protocol version.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PtpVersion {
    Ieee1588_2002,
    Ieee1588_2008,
    #[allow(non_camel_case_types)]
    Ieee8021As_2011,
    Ext(String),
}

impl FromStr for PtpVersion {
    type Err = AttributeError;

    fn from_str(s: &str) -> Result<Self, AttributeError> {
        match s {
            "IEEE1588-2002" => Ok(Self::Ieee1588_2002),
            "IEEE1588-2008" => Ok(Self::Ieee1588_2008),
            "IEEE802.1AS-2011" => Ok(Self::Ieee8021As_2011),
            _ => {
                if s.is_empty() {
                    return Err(AttributeError::ParamNotFound {
                        param: "PTP version".to_string(),
                        attr: String::new(), // will be set bubbling up
                    });
                }
                Ok(Self::Ext(s.to_string()))
            }
        }
    }
}

impl fmt::Display for PtpVersion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ieee1588_2002 => write!(f, "IEEE1588-2002"),
            Self::Ieee1588_2008 => write!(f, "IEEE1588-2008"),
            Self::Ieee8021As_2011 => write!(f, "IEEE802.1AS-2011"),
            Self::Ext(v) => write!(f, "{v}"),
        }
    }
}

/// PTP server: GMID and domain or traceable.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PtpServer {
    GmidDomain {
        gmid: Eui64,
        domain: Option<PtpDomain>,
    },
    Traceable,
}

impl PtpServer {
    /// Constructs a [`PtpServer`] from the specified GMID.
    pub fn from_gmid(gmid: impl Into<Eui64>) -> Self {
        PtpServer::GmidDomain {
            gmid: gmid.into(),
            domain: None,
        }
    }

    /// Constructs a [`PtpServer`] from the specified GMID and [`PtpDomain`].
    pub fn from_gmid_with_domain(gmid: impl Into<Eui64>, domain: PtpDomain) -> Self {
        PtpServer::GmidDomain {
            gmid: gmid.into(),
            domain: Some(domain),
        }
    }

    /// Constructs a [`PtpServer`] from the specified GMID and domain name.
    ///
    /// This will assign version IEEE 1588-2002.
    pub fn from_gmid_with_domain_name(gmid: impl Into<Eui64>, name: impl ToString) -> Self {
        PtpServer::GmidDomain {
            gmid: gmid.into(),
            domain: Some(PtpDomain::from_name(name)),
        }
    }

    /// Tries to construct a [`PtpServer`] from the specified GMID and domain number.
    ///
    /// Returns an `Error` if `number` is not in range (0-127) (inclusive)
    pub fn try_from_ptp_gmid_with_domain_number(
        gmid: impl Into<Eui64>,
        number: u8,
    ) -> Result<Self, AttributeError> {
        Ok(PtpServer::GmidDomain {
            gmid: gmid.into(),
            domain: Some(PtpDomain::try_from_number(number)?),
        })
    }

    pub fn new_traceable() -> Self {
        PtpServer::Traceable
    }
}

impl FromStr for PtpServer {
    type Err = AttributeError;

    fn from_str(s: &str) -> Result<Self, AttributeError> {
        if s == "traceable" {
            return Ok(Self::Traceable);
        }

        let (gmid_str, domain_str) = s
            .split_once(':')
            .map(|(gmid, domain)| (gmid, Some(domain)))
            .unwrap_or((s, None));

        let gmid = Eui64::from_str(gmid_str).map_err(|_| AttributeError::InvalidParamValue {
            param: "PTP GMID".to_string(),
            val: gmid_str.to_string(),
            attr: String::new(), // will be set bubbling up
        })?;

        let domain = domain_str.map(PtpDomain::from_str).transpose()?;

        Ok(Self::GmidDomain { gmid, domain })
    }
}

impl fmt::Display for PtpServer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Traceable => write!(f, "traceable"),
            Self::GmidDomain { gmid, domain } => {
                if let Some(dom) = domain {
                    write!(f, "{gmid}:{dom}")
                } else {
                    write!(f, "{gmid}")
                }
            }
        }
    }
}

/// EUI-64 identifier.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Eui64 {
    pub bytes: [u8; 8],
}

impl Eui64 {
    pub fn new(id: u64) -> Self {
        Self {
            bytes: id.to_be_bytes(),
        }
    }

    pub fn as_u64(&self) -> u64 {
        u64::from_be_bytes(self.bytes)
    }
}

impl FromStr for Eui64 {
    type Err = AttributeError;

    fn from_str(s: &str) -> Result<Self, AttributeError> {
        if s.len() != 7 * 3 + 2 {
            return Err(AttributeError::InvalidParamValue {
                param: "EUI64".to_string(),
                val: s.to_string(),
                attr: String::new(), // will be set bubbling up
            });
        }
        let mut bytes = [0u8; 8];
        for (i, digit) in s.split('-').enumerate() {
            if digit.len() != 2 {
                return Err(AttributeError::InvalidParamValue {
                    param: "EUI64 segment".to_string(),
                    val: digit.to_string(),
                    attr: String::new(), // will be set bubbling up
                });
            }
            bytes[i] =
                u8::from_str_radix(digit, 16).map_err(|_| AttributeError::InvalidParamValue {
                    param: "EUI64 hex digit".to_string(),
                    val: digit.to_string(),
                    attr: String::new(), // will be set bubbling up
                })?;
        }

        Ok(Self { bytes })
    }
}

impl From<u64> for Eui64 {
    fn from(id: u64) -> Self {
        Eui64::new(id)
    }
}

impl fmt::Display for Eui64 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for byte in &self.bytes[..7] {
            write!(f, "{:02X}-", byte)?;
        }
        write!(f, "{:02X}", self.bytes[7])
    }
}

/// PTP domain.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PtpDomain {
    /// IEEE 1588-2002
    DomainName {
        name: String,
    },
    // IEEE 1588-2008 (Range: 0-127)
    DomainNumber(u8),
}

impl PtpDomain {
    /// Constructs a [`PtpDomain`] from the specified name.
    pub fn from_name(name: impl ToString) -> Self {
        PtpDomain::DomainName {
            name: name.to_string(),
        }
    }

    /// Tries to construct a [`PtpDomain`] from the specified number.
    ///
    /// Returns an `Error` if `number` is not in range (0-127) (inclusive)
    pub fn try_from_number(number: u8) -> Result<Self, AttributeError> {
        if number > 127 {
            return Err(AttributeError::InvalidParamValue {
                param: "PTP domain".to_string(),
                val: number.to_string(),
                attr: String::new(), // will be set bubbling up
            });
        }

        Ok(PtpDomain::DomainNumber(number))
    }
}

impl FromStr for PtpDomain {
    type Err = AttributeError;

    fn from_str(s: &str) -> Result<Self, AttributeError> {
        // XXX: The RFC grammar is wrong here. It defines that
        // both variants here should start with `domain-name=` or
        // `domain-nmbr=` but none of the examples do that so
        // let's assume the grammar is wrong.
        if s.len() == 16 {
            Ok(Self::DomainName {
                name: s.to_string(),
            })
        } else if let Ok(domain_num) = s.parse::<u8>() {
            if domain_num > 127 {
                return Err(AttributeError::InvalidParamValue {
                    param: "PTP domain".to_string(),
                    val: domain_num.to_string(),
                    attr: String::new(), // will be set bubbling up
                });
            }

            Ok(Self::DomainNumber(domain_num))
        } else {
            Err(AttributeError::InvalidParamValue {
                param: "PTP domain".to_string(),
                val: s.to_string(),
                attr: String::new(), // will be set bubbling up
            })
        }
    }
}

impl fmt::Display for PtpDomain {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::DomainName { name } => write!(f, "{name}"),
            Self::DomainNumber(n) => write!(f, "{n}"),
        }
    }
}

/// Private source with optional traceable flag.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrivateSource {
    Standard,
    Traceable,
}

/// Extended clock source.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClockSourceExt {
    pub name: String,
    pub value: Option<String>,
}

impl ClockSourceExt {
    /// Constructs a [`ClockSourceExt`] with the specified name.
    pub fn new(name: impl ToString) -> Self {
        ClockSourceExt {
            name: name.to_string(),
            value: None,
        }
    }

    /// Constructs a [`ClockSourceExt`] with the specified name and value.
    pub fn with_value(name: impl ToString, value: impl ToString) -> Self {
        ClockSourceExt {
            name: name.to_string(),
            value: Some(value.to_string()),
        }
    }

    pub fn set_value(&mut self, value: impl ToString) {
        self.value = Some(value.to_string());
    }

    /// Constructs a [`crate::builders::ClockSourceExt`].
    pub fn builder(name: impl ToString) -> crate::builders::ClockSourceExt {
        crate::builders::ClockSourceExt::new(name)
    }
}

impl FromStr for ClockSourceExt {
    type Err = AttributeError;

    fn from_str(s: &str) -> Result<Self, AttributeError> {
        let (name, value) = s
            .split_once('=')
            .map(|(name, value)| (name, Some(value)))
            .unwrap_or((s, None));
        if name.is_empty() {
            return Err(AttributeError::ParamNotFound {
                param: "Clock source name".to_string(),
                attr: String::new(), // will be set bubbling up
            });
        }

        Ok(Self {
            name: name.to_string(),
            value: value.map(|s| s.to_string()),
        })
    }
}

impl fmt::Display for ClockSourceExt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(ref value) = self.value {
            write!(f, "{}={value}", self.name)
        } else {
            write!(f, "{}", self.name)
        }
    }
}

/// Media clock source type enumeration.
///
/// This maps to the `mediaclk` attribute.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct MediaClockSource {
    pub id: Option<MediaClockId>,
    pub clock: MediaClock,
}

impl MediaClockSource {
    /// Constructs a [`MediaClockSource`] from the specified [`MediaClock`].
    pub fn new(clock: impl Into<MediaClock>) -> Self {
        MediaClockSource {
            id: None,
            clock: clock.into(),
        }
    }

    /// Constructs a [`MediaClockSource`] from the specified [`MediaClock`] and [`MediaClockId`].
    pub fn with_clock_id(clock: impl Into<MediaClock>, id: MediaClockId) -> Self {
        MediaClockSource {
            id: Some(id),
            clock: clock.into(),
        }
    }

    /// Constructs a direct [`MediaClockSource`].
    pub fn new_direct() -> Self {
        MediaClock::new_direct().into()
    }

    /// Constructs a direct [`MediaClockSource`] from the specified offset.
    pub fn from_direct_offset(offset: u32) -> Self {
        MediaClock::from_direct_offset(offset).into()
    }

    /// Constructs a direct [`MediaClockSource`] from the specified offset and [`MediaClockId`].
    pub fn from_direct_offset_with_id(offset: u32, id: MediaClockId) -> Self {
        MediaClockSource {
            id: Some(id),
            clock: MediaClock::from_direct_offset(offset),
        }
    }

    /// Constructs a direct [`MediaClockSource`] from the specified rate.
    pub fn from_direct_rate(rate: impl Into<Rate>) -> Self {
        MediaClock::from_direct_rate(rate).into()
    }

    /// Constructs a direct [`MediaClockSource`] from the specified rate and [`MediaClockId`].
    pub fn from_direct_rate_with_id(rate: impl Into<Rate>, id: MediaClockId) -> Self {
        MediaClockSource {
            id: Some(id),
            clock: MediaClock::from_direct_rate(rate),
        }
    }

    /// Constructs a direct [`MediaClockSource`] from the specified offset and rate.
    pub fn from_direct_offset_and_rate(offset: u32, rate: impl Into<Rate>) -> Self {
        MediaClock::from_direct_offset_and_rate(offset, rate).into()
    }

    /// Constructs a direct [`MediaClockSource`] from the specified offset, rate and [`MediaClockId`].
    pub fn from_direct_offset_and_rate_with_id(
        offset: u32,
        rate: impl Into<Rate>,
        id: MediaClockId,
    ) -> Self {
        MediaClockSource {
            id: Some(id),
            clock: MediaClock::from_direct_offset_and_rate(offset, rate),
        }
    }

    /// Constructs a [`MediaClockSource`] from the specified IEEE1722 stream id.
    pub fn from_ieee1722_stream_id(ieee1722_stream_id: Eui64) -> Self {
        MediaClock::from_ieee1722_stream_id(ieee1722_stream_id).into()
    }

    /// Constructs a [`MediaClockSource`] from the specified IEEE1722 stream id and [`MediaClockId`].
    pub fn from_ieee1722_stream_id_with_id(ieee1722_stream_id: Eui64, id: MediaClockId) -> Self {
        MediaClockSource {
            id: Some(id),
            clock: MediaClock::from_ieee1722_stream_id(ieee1722_stream_id),
        }
    }

    /// Constructs an extended [`MediaClockSource`] from the specified name.
    pub fn from_extended_name(name: impl ToString) -> Self {
        MediaClock::from_extended_name(name).into()
    }

    /// Constructs an extended [`MediaClockSource`] from the specified name and [`MediaClockId`].
    pub fn from_extended_name_with_id(name: impl ToString, id: MediaClockId) -> Self {
        MediaClockSource {
            id: Some(id),
            clock: MediaClock::from_extended_name(name),
        }
    }

    /// Constructs an extended [`MediaClockSource`] from the specified name and value.
    pub fn from_extended_name_with_value(name: impl ToString, value: impl ToString) -> Self {
        MediaClock::from_extended_name_with_value(name, value).into()
    }

    /// Constructs an extended [`MediaClockSource`] from the specified name and value and [`MediaClockId`].
    pub fn from_extended_name_with_value_with_id(
        name: impl ToString,
        value: impl ToString,
        id: MediaClockId,
    ) -> Self {
        MediaClockSource {
            id: Some(id),
            clock: MediaClock::from_extended_name_with_value(name, value),
        }
    }

    pub fn set_id(&mut self, id: MediaClockId) {
        self.id = Some(id);
    }

    /// Constructs a [`crate::builders::MediaClockSource`].
    pub fn builder(clock: impl Into<MediaClock>) -> crate::builders::MediaClockSource {
        crate::builders::MediaClockSource::new(clock)
    }
}

impl<T: Into<MediaClock>> From<T> for MediaClockSource {
    fn from(media_clock: T) -> Self {
        MediaClockSource {
            id: None,
            clock: media_clock.into(),
        }
    }
}

impl FromStr for MediaClockSource {
    type Err = AttributeError;

    fn from_str(mut s: &str) -> Result<Self, AttributeError> {
        let mut id = None;
        if let Some(id_str) = s.strip_prefix("id=") {
            let (id_str, rest) = id_str.split_once(' ').unwrap();
            id = Some(MediaClockId::from_str(id_str).with_attr(<Self as TypedAttribute>::NAME)?);

            s = rest;
        }

        let clock = MediaClock::from_str(s).with_attr(<Self as TypedAttribute>::NAME)?;

        Ok(Self { id, clock })
    }
}

impl fmt::Display for MediaClockSource {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(ref id) = self.id {
            write!(f, "id={id} {}", self.clock)?;
        } else {
            write!(f, "{}", self.clock)?;
        }
        Ok(())
    }
}

impl TypedAttribute for MediaClockSource {
    const NAME: &'static str = "mediaclk";
}

/// Media clock.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub enum MediaClock {
    #[default]
    Sender,
    Direct(Direct),
    Ieee1722StreamId(Eui64),
    Ext(MediaClockExt),
}

impl MediaClock {
    /// Constructs a direct [`MediaClock`].
    pub fn new_direct() -> Self {
        MediaClock::Direct(Direct::new())
    }

    /// Constructs a direct [`MediaClock`] from the specified offset.
    pub fn from_direct_offset(offset: u32) -> Self {
        MediaClock::Direct(Direct::with_offset(offset))
    }

    /// Constructs a direct [`MediaClock`] from the specified rate.
    pub fn from_direct_rate(rate: impl Into<Rate>) -> Self {
        MediaClock::Direct(Direct::with_rate(rate))
    }

    /// Constructs a direct [`MediaClock`] from the specified offset and rate.
    pub fn from_direct_offset_and_rate(offset: u32, rate: impl Into<Rate>) -> Self {
        MediaClock::Direct(Direct::with_offset_and_rate(offset, rate))
    }

    /// Constructs a [`MediaClock`] from the specified IEEE1722 stream id.
    pub fn from_ieee1722_stream_id(ieee1722_stream_id: Eui64) -> Self {
        MediaClock::Ieee1722StreamId(ieee1722_stream_id)
    }

    /// Constructs an extended [`MediaClock`] from the specified name.
    pub fn from_extended_name(name: impl ToString) -> Self {
        MediaClock::Ext(MediaClockExt::new(name))
    }

    /// Constructs an extended [`MediaClock`] from the specified name and value.
    pub fn from_extended_name_with_value(name: impl ToString, value: impl ToString) -> Self {
        MediaClock::Ext(MediaClockExt::with_value(name, value))
    }
}

impl From<Direct> for MediaClock {
    fn from(direct: Direct) -> Self {
        MediaClock::Direct(direct)
    }
}

impl From<Eui64> for MediaClock {
    fn from(ieee1722_stream_id: Eui64) -> Self {
        MediaClock::Ieee1722StreamId(ieee1722_stream_id)
    }
}

impl From<MediaClockExt> for MediaClock {
    fn from(media_clock_ext: MediaClockExt) -> Self {
        MediaClock::Ext(media_clock_ext)
    }
}

impl FromStr for MediaClock {
    type Err = AttributeError;

    fn from_str(s: &str) -> Result<Self, AttributeError> {
        if s == "sender" {
            Ok(Self::Sender)
        } else if let Some(s) = s.strip_prefix("direct") {
            Ok(Self::Direct(
                Direct::from_str(s).with_param_context("direct")?,
            ))
        } else if let Some(s) = s.strip_prefix("IEEE1722=") {
            let gmid = Eui64::from_str(s).with_param_context("IEEE1722 stream ID")?;
            Ok(Self::Ieee1722StreamId(gmid))
        } else {
            Ok(Self::Ext(MediaClockExt::from_str(s)?))
        }
    }
}

impl fmt::Display for MediaClock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Sender => write!(f, "sender"),
            Self::Direct(Direct { offset, rate }) => {
                if let Some(offset) = offset {
                    write!(f, "direct={offset}")?;
                } else {
                    write!(f, "direct")?;
                }
                if let Some(Rate {
                    numerator,
                    denominator,
                }) = rate
                {
                    write!(f, " rate={numerator}/{denominator}")?;
                }
                Ok(())
            }
            Self::Ieee1722StreamId(gmid) => write!(f, "IEEE1722={gmid}"),
            Self::Ext(ext) => {
                if let Some(ref value) = ext.value {
                    write!(f, "{}={value}", ext.name)
                } else {
                    write!(f, "{}", ext.name)
                }
            }
        }
    }
}

/// Media clock ID with optional source prefix.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MediaClockId {
    pub src: bool,
    pub tag: String,
}

impl MediaClockId {
    /// Constructs a [`MediaClockId`] with the specified tag and without the 'src' prefix.
    pub fn new(tag: impl ToString) -> Self {
        MediaClockId {
            src: false,
            tag: tag.to_string(),
        }
    }

    /// Constructs a [`MediaClockId`] with the specified tag and with the 'src' prefix.
    pub fn new_with_src_prefix(tag: impl ToString) -> Self {
        MediaClockId {
            src: true,
            tag: tag.to_string(),
        }
    }

    pub fn set_src_prefix(&mut self, src: bool) {
        self.src = src;
    }
}

impl FromStr for MediaClockId {
    type Err = AttributeError;

    fn from_str(s: &str) -> Result<Self, AttributeError> {
        let (src, tag_str) = if let Some(s) = s.strip_prefix("src:") {
            (true, s)
        } else {
            (false, s)
        };

        if tag_str.is_empty() {
            return Err(AttributeError::ParamNotFound {
                param: "media clock tag".to_string(),
                attr: String::new(), // will be set bubbling up
            });
        }

        Ok(Self {
            src,
            tag: tag_str.to_string(),
        })
    }
}

impl fmt::Display for MediaClockId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.src {
            write!(f, "src:{}", self.tag)
        } else {
            write!(f, "{}", self.tag)
        }
    }
}

/// Fractional rate.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub struct Rate {
    pub numerator: u32,
    pub denominator: u32,
}

impl Rate {
    pub fn new(numerator: u32, denominator: u32) -> Self {
        Rate {
            numerator,
            denominator,
        }
    }
}

impl From<(u32, u32)> for Rate {
    fn from(rate: (u32, u32)) -> Self {
        Rate {
            numerator: rate.0,
            denominator: rate.1,
        }
    }
}

/// Direct media clock with optional rate.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Direct {
    pub offset: Option<u32>,
    pub rate: Option<Rate>,
}

impl Direct {
    /// Constructs a [`Direct`] media clock with no offset nor rate.
    pub fn new() -> Self {
        Direct {
            offset: None,
            rate: None,
        }
    }

    /// Constructs a [`Direct`] media clock with the specified offset.
    pub fn with_offset(offset: u32) -> Self {
        Direct {
            offset: Some(offset),
            rate: None,
        }
    }

    pub fn set_offset(&mut self, offset: u32) {
        self.offset = Some(offset);
    }

    /// Constructs a [`Direct`] media clock with the specified rate.
    pub fn with_rate(rate: impl Into<Rate>) -> Self {
        Direct {
            offset: None,
            rate: Some(rate.into()),
        }
    }

    pub fn set_rate(&mut self, rate: impl Into<Rate>) {
        self.rate = Some(rate.into());
    }

    /// Constructs a [`Direct`] media clock with the specified offset & rate.
    pub fn with_offset_and_rate(offset: u32, rate: impl Into<Rate>) -> Self {
        Direct {
            offset: Some(offset),
            rate: Some(rate.into()),
        }
    }

    /// Constructs a [`crate::builders::Direct`].
    pub fn builder() -> crate::builders::Direct {
        crate::builders::Direct::new()
    }
}

impl FromStr for Direct {
    type Err = AttributeError;

    fn from_str(s: &str) -> Result<Self, AttributeError> {
        if s.is_empty() {
            return Ok(Self {
                offset: None,
                rate: None,
            });
        }

        let (offset, s) = if let Some(s) = s.strip_prefix('=') {
            let (offset_str, s) = s.split_once(' ').unwrap_or((s, ""));

            let offset =
                offset_str
                    .parse::<u32>()
                    .map_err(|_| AttributeError::InvalidParamValue {
                        param: "offset".to_string(),
                        val: offset_str.to_string(),
                        attr: String::new(), // will be set bubbling up
                    })?;

            (Some(offset), s)
        } else {
            (None, s.strip_prefix(' ').unwrap_or(""))
        };

        let rate = if s.is_empty() {
            None
        } else if let Some(s) = s.strip_prefix("rate=") {
            let (num_str, den_str) =
                s.split_once('/')
                    .ok_or_else(|| AttributeError::InvalidParamValue {
                        param: "rate format".to_string(),
                        val: s.to_string(),
                        attr: String::new(), // will be set bubbling up
                    })?;

            let numerator =
                num_str
                    .parse::<u32>()
                    .map_err(|_| AttributeError::InvalidParamValue {
                        param: "rate numerator".to_string(),
                        val: num_str.to_string(),
                        attr: String::new(), // will be set bubbling up
                    })?;
            let denominator =
                den_str
                    .parse::<u32>()
                    .map_err(|_| AttributeError::InvalidParamValue {
                        param: "rate denominator".to_string(),
                        val: den_str.to_string(),
                        attr: String::new(), // will be set bubbling up
                    })?;

            Some(Rate {
                numerator,
                denominator,
            })
        } else {
            return Err(AttributeError::InvalidParamValue {
                param: "direct clock parameters".to_string(),
                val: s.to_string(),
                attr: String::new(), // will be set bubbling up
            });
        };

        Ok(Self { offset, rate })
    }
}

/// Extended media clock.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MediaClockExt {
    pub name: String,
    pub value: Option<String>,
}

impl MediaClockExt {
    /// Constructs a [`MediaClockExt`] with the specified name.
    pub fn new(name: impl ToString) -> Self {
        MediaClockExt {
            name: name.to_string(),
            value: None,
        }
    }

    /// Constructs a [`MediaClockExt`] with the specified name and value.
    pub fn with_value(name: impl ToString, value: impl ToString) -> Self {
        MediaClockExt {
            name: name.to_string(),
            value: Some(value.to_string()),
        }
    }

    pub fn set_value(&mut self, value: impl ToString) {
        self.value = Some(value.to_string());
    }

    /// Constructs a [`crate::builders::MediaClockExt`].
    pub fn builder(name: impl ToString) -> crate::builders::MediaClockExt {
        crate::builders::MediaClockExt::new(name)
    }
}

impl FromStr for MediaClockExt {
    type Err = AttributeError;

    fn from_str(s: &str) -> Result<Self, AttributeError> {
        let (name, value) = s
            .split_once('=')
            .map(|(name, value)| (name, Some(value)))
            .unwrap_or((s, None));

        if name.is_empty() {
            return Err(AttributeError::ParamNotFound {
                param: "clock source name".to_string(),
                attr: String::new(), // will be set bubbling up
            });
        }

        Ok(Self {
            name: name.to_string(),
            value: value.map(|s| s.to_string()),
        })
    }
}

impl fmt::Display for MediaClockExt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(ref value) = self.value {
            write!(f, "{}={value}", self.name)
        } else {
            write!(f, "{}", self.name)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_reference_clock() {
        let test_cases = [
            (
                "ntp=/traceable/",
                ReferenceClock::Ntp(Ntp {
                    server: NtpServerAddr::Traceable,
                }),
            ),
            ("local", ReferenceClock::Local),
            (
                "ntp=203.0.113.10",
                ReferenceClock::Ntp(Ntp {
                    server: NtpServerAddr::HostPort {
                        hostname: "203.0.113.10".to_string(),
                        port: None,
                    },
                }),
            ),
            (
                "ntp=ntp.example.com:123",
                ReferenceClock::Ntp(Ntp {
                    server: NtpServerAddr::HostPort {
                        hostname: "ntp.example.com".to_string(),
                        port: Some(123),
                    },
                }),
            ),
            (
                "ntp=[::1]:123",
                ReferenceClock::Ntp(Ntp {
                    server: NtpServerAddr::HostPort {
                        hostname: "[::1]".to_string(),
                        port: Some(123),
                    },
                }),
            ),
            (
                "ptp=IEEE802.1AS-2011:39-A7-94-FF-FE-07-CB-D0",
                ReferenceClock::Ptp(Ptp {
                    version: PtpVersion::Ieee8021As_2011,
                    server: PtpServer::GmidDomain {
                        gmid: Eui64 {
                            bytes: [0x39, 0xA7, 0x94, 0xFF, 0xFE, 0x07, 0xCB, 0xD0],
                        },
                        domain: None,
                    },
                }),
            ),
            (
                "ptp=IEEE1588-2008:39-A7-94-FF-FE-07-CB-D0:0",
                ReferenceClock::Ptp(Ptp {
                    version: PtpVersion::Ieee1588_2008,
                    server: PtpServer::GmidDomain {
                        gmid: Eui64 {
                            bytes: [0x39, 0xA7, 0x94, 0xFF, 0xFE, 0x07, 0xCB, 0xD0],
                        },
                        domain: Some(PtpDomain::DomainNumber(0)),
                    },
                }),
            ),
            (
                "ptp=IEEE1588-2002:39-A7-94-FF-FE-07-CB-D0:testdomain123456",
                ReferenceClock::Ptp(Ptp {
                    version: PtpVersion::Ieee1588_2002,
                    server: PtpServer::GmidDomain {
                        gmid: Eui64 {
                            bytes: [0x39, 0xA7, 0x94, 0xFF, 0xFE, 0x07, 0xCB, 0xD0],
                        },
                        domain: Some(PtpDomain::DomainName {
                            name: "testdomain123456".to_string(),
                        }),
                    },
                }),
            ),
            (
                "ptp=IEEE1588-2008:39-A7-94-FF-FE-07-CB-D0:127",
                ReferenceClock::Ptp(Ptp {
                    version: PtpVersion::Ieee1588_2008,
                    server: PtpServer::GmidDomain {
                        gmid: Eui64 {
                            bytes: [0x39, 0xA7, 0x94, 0xFF, 0xFE, 0x07, 0xCB, 0xD0],
                        },
                        domain: Some(PtpDomain::DomainNumber(127)),
                    },
                }),
            ),
            ("private", ReferenceClock::Private(PrivateSource::Standard)),
            (
                "private:traceable",
                ReferenceClock::Private(PrivateSource::Traceable),
            ),
            ("gps", ReferenceClock::Gps),
            ("gal", ReferenceClock::Gal),
            ("glonass", ReferenceClock::Glonass),
        ];

        for (input, expected) in test_cases {
            println!("Testing: {input}");
            let parsed = match input.parse::<ReferenceClock>() {
                Ok(parsed) => parsed,
                Err(err) => {
                    unreachable!("Failed to parse '{}': {}", input, err);
                }
            };

            assert_eq!(parsed, expected, "Parse mismatch for: {input}");

            let roundtrip = expected.to_string();
            assert_eq!(roundtrip, input, "Round-trip failed for: {input}");
        }
    }

    #[test]
    fn test_reference_clock_invalid() {
        let invalid_cases = vec![
            (
                "",
                AttributeError::ParamNotFound {
                    param: "Clock source name".to_string(),
                    attr: "ts-refclk".to_string(),
                },
            ),
            (
                "ntp=",
                AttributeError::ParamNotFound {
                    param: "NTP server address".to_string(),
                    attr: "ts-refclk".to_string(),
                },
            ),
            (
                "ntp=:123",
                AttributeError::ParamNotFound {
                    param: "hostname in NTP server address".to_string(),
                    attr: "ts-refclk".to_string(),
                },
            ),
            (
                "ptp=:39-A7-94-FF-FE-07-CB-D0",
                AttributeError::ParamNotFound {
                    param: "PTP version".to_string(),
                    attr: "ts-refclk".to_string(),
                },
            ),
            (
                "ptp=IEEE1588-2008:",
                AttributeError::ParamNotFound {
                    param: "PTP server".to_string(),
                    attr: "ts-refclk".to_string(),
                },
            ),
            (
                "ptp=IEEE1588-2008:invalid-eui64",
                AttributeError::InvalidParamValue {
                    param: "PTP GMID".to_string(),
                    val: "invalid-eui64".to_string(),
                    attr: "ts-refclk".to_string(),
                },
            ),
            (
                "ptp=IEEE1588-2008:39-A7-94-FF-FE-07-CB-D0:tooshortname",
                AttributeError::InvalidParamValue {
                    param: "PTP domain".to_string(),
                    val: "tooshortname".to_string(),
                    attr: "ts-refclk".to_string(),
                },
            ),
            (
                "ptp=IEEE1588-2008:39-A7-94-FF-FE-07-CB-D0:128",
                AttributeError::InvalidParamValue {
                    param: "PTP domain".to_string(),
                    val: "128".to_string(),
                    attr: "ts-refclk".to_string(),
                },
            ),
        ];

        for (input, expected_err) in invalid_cases {
            println!("Testing invalid: {input}");
            assert_eq!(input.parse::<ReferenceClock>(), Err(expected_err));
        }
    }

    #[test]
    fn test_media_clock_source() {
        let test_cases = vec![
            (
                "direct",
                MediaClockSource {
                    id: None,
                    clock: MediaClock::Direct(Direct {
                        offset: None,
                        rate: None,
                    }),
                },
            ),
            (
                "direct=963214424",
                MediaClockSource {
                    id: None,
                    clock: MediaClock::Direct(Direct {
                        offset: Some(963214424),
                        rate: None,
                    }),
                },
            ),
            (
                "direct=963214424 rate=1000/1001",
                MediaClockSource {
                    id: None,
                    clock: MediaClock::Direct(Direct {
                        offset: Some(963214424),
                        rate: Some(Rate::new(1000, 1001)),
                    }),
                },
            ),
            (
                "direct rate=1000/1001",
                MediaClockSource {
                    id: None,
                    clock: MediaClock::Direct(Direct {
                        offset: None,
                        rate: Some(Rate::new(1000, 1001)),
                    }),
                },
            ),
            (
                "sender",
                MediaClockSource {
                    id: None,
                    clock: MediaClock::Sender,
                },
            ),
            (
                "IEEE1722=38-D6-6D-8E-D2-78-13-2F",
                MediaClockSource {
                    id: None,
                    clock: MediaClock::Ieee1722StreamId(Eui64 {
                        bytes: [0x38, 0xD6, 0x6D, 0x8E, 0xD2, 0x78, 0x13, 0x2F],
                    }),
                },
            ),
            (
                "id=MDA6NjA6MmI6MjA6MTI6MWY= sender",
                MediaClockSource {
                    id: Some(MediaClockId {
                        src: false,
                        tag: "MDA6NjA6MmI6MjA6MTI6MWY=".to_string(),
                    }),
                    clock: MediaClock::Sender,
                },
            ),
            (
                "id=src:MDA6NjA6MmI6MjA6MTI6MWY= direct",
                MediaClockSource {
                    id: Some(MediaClockId {
                        src: true,
                        tag: "MDA6NjA6MmI6MjA6MTI6MWY=".to_string(),
                    }),
                    clock: MediaClock::Direct(Direct {
                        offset: None,
                        rate: None,
                    }),
                },
            ),
        ];

        for (input, expected) in test_cases {
            println!("Testing: {input}");
            let parsed = match input.parse::<MediaClockSource>() {
                Ok(parsed) => parsed,
                Err(err) => {
                    unreachable!("Failed to parse '{}': {}", input, err);
                }
            };

            assert_eq!(parsed, expected, "Parse mismatch for: {input}");

            let roundtrip = expected.to_string();
            assert_eq!(roundtrip, input, "Round-trip failed for: {input}");
        }
    }

    #[test]
    fn test_media_clock_source_invalid() {
        let invalid_cases = vec![
            (
                "",
                AttributeError::ParamNotFound {
                    param: "clock source name".to_string(),
                    attr: "mediaclk".to_string(),
                },
            ),
            (
                "direct=notanumber",
                AttributeError::InvalidParamValue {
                    param: "direct (offset)".to_string(),
                    val: "notanumber".to_string(),
                    attr: "mediaclk".to_string(),
                },
            ),
            (
                "direct=count rate=100/abc",
                AttributeError::InvalidParamValue {
                    param: "direct (offset)".to_string(),
                    val: "count".to_string(),
                    attr: "mediaclk".to_string(),
                },
            ),
            (
                "IEEE1722=invalid-eui64",
                AttributeError::InvalidParamValue {
                    param: "IEEE1722 stream ID (EUI64)".to_string(),
                    val: "invalid-eui64".to_string(),
                    attr: "mediaclk".to_string(),
                },
            ),
            (
                "id= sender",
                AttributeError::ParamNotFound {
                    param: "media clock tag".to_string(),
                    attr: "mediaclk".to_string(),
                },
            ),
        ];

        for (input, expected_err) in invalid_cases {
            println!("Testing invalid: {input}");
            assert_eq!(input.parse::<MediaClockSource>(), Err(expected_err));
        }
    }

    #[test]
    fn parse_clock_signalling_attr() {
        use crate::{Ssrc, SsrcAttribute};

        let sdp = "v=0\n\
                   m=image 54111 TCP t38\n\
                   c=IN IP4 192.0.2.2\n\
                   a=ts-refclk:local\n\
                   a=mediaclk:direct=654321\n\
                   a=ssrc:1234 ts-refclk:ntp=pool.ntp.org\n\
                   a=ssrc:1234 mediaclk:direct=987654\n\
                   ";

        let medias = crate::Session::parse(sdp.as_bytes()).unwrap().medias;
        assert_eq!(
            medias[0].get_first_attribute_typed::<ReferenceClock>(),
            Some(Ok(ReferenceClock::Local))
        );
        assert_eq!(
            medias[0].get_first_attribute_typed::<MediaClockSource>(),
            Some(Ok(MediaClockSource {
                id: None,
                clock: MediaClock::Direct(Direct {
                    offset: Some(654321),
                    rate: None
                })
            })),
        );

        let ssrc_attrs = medias[0].attributes_typed::<Ssrc>();
        for ssrc_attr in ssrc_attrs {
            let ssrc_attr = ssrc_attr.unwrap();
            match ssrc_attr.attribute {
                SsrcAttribute::ReferenceClock => {
                    let ssrc_refclk = ssrc_attr.get_typed::<ReferenceClock>().unwrap();
                    assert_eq!(
                        ssrc_refclk,
                        ReferenceClock::Ntp(Ntp {
                            server: NtpServerAddr::HostPort {
                                hostname: "pool.ntp.org".to_string(),
                                port: None,
                            }
                        })
                    );
                }
                SsrcAttribute::MediaClockSource => {
                    let ssrc_mediaclk = ssrc_attr.get_typed::<MediaClockSource>().unwrap();
                    assert_eq!(
                        ssrc_mediaclk,
                        MediaClockSource {
                            id: None,
                            clock: MediaClock::Direct(Direct {
                                offset: Some(987654),
                                rate: None
                            })
                        }
                    );
                }
                other => unreachable!("{:?}", other),
            }
        }
    }

    #[test]
    fn build_clock_signalling_attr() {
        use crate::{Media, MediaType, TransportProto};

        const NTP_SERVER_ADDR: &str = "pool.ntp.org";

        let media = Media::builder(MediaType::Audio, 5000, TransportProto::RtpAvp, 96)
            .attribute(ReferenceClock::Local)
            .attribute(ReferenceClock::from_ntp_hostname(NTP_SERVER_ADDR))
            .build();

        let mut refclk_iter = media.attributes_typed::<ReferenceClock>();
        assert_eq!(refclk_iter.next(), Some(Ok(ReferenceClock::Local)));
        assert_eq!(
            refclk_iter.next(),
            Some(Ok(ReferenceClock::Ntp(Ntp {
                server: NtpServerAddr::HostPort {
                    hostname: NTP_SERVER_ADDR.to_string(),
                    port: None
                }
            })))
        );
    }
}
