// Copyright (C) 2019 Sebastian Dröge <sebastian@centricular.com>
//
// Licensed under the MIT license, see the LICENSE file or <http://opensource.org/licenses/MIT>

use super::*;

type NomError<'a> = nom::Err<nom::error::VerboseError<&'a [u8]>>;

/// Parsing errors that can be returned from [`Session::parse`].
///
/// [`Session::parse`]: struct.Session.html#method.parse
#[derive(Debug, PartialEq, Eq)]
pub enum ParserError {
    /// The given line started with an unexpected character.
    UnexpectedLine(usize, u8),
    /// The given line was not formatted correctly.
    InvalidLineFormat(usize, &'static str),
    /// The given line contained an invalid encoding at the specified field.
    InvalidFieldEncoding(usize, &'static str),
    /// The given line contained an invalid format at the specified field.
    InvalidFieldFormat(usize, &'static str),
    /// The given line was missing field data at the specified field.
    MissingField(usize, &'static str),
    /// The given line had some trailing data at the specified field.
    FieldTrailingData(usize, &'static str),
    /// The given version line did not contain a valid version number.
    InvalidVersion(usize, Vec<u8>),
    /// A second version line was found at the given line.
    MultipleVersions(usize),
    /// The SDP did not contain a version line.
    NoVersion,
    /// A second origin line was found at the given line.
    MultipleOrigins(usize),
    /// The SDP did not contain an origin line.
    NoOrigin,
    /// A second session name line was found at the given line.
    MultipleSessionNames(usize),
    /// The SDP did not contain a session name line.
    NoSessionName,
    /// A second session description line was found at the given line.
    MultipleSessionDescription(usize),
    /// A second URI line was found at the given line.
    MultipleUris(usize),
    /// A second connection line was found at the given line.
    MultipleConnections(usize),
    /// A second time zone line was found at the given line.
    MultipleTimeZones(usize),
    /// A second media title line was found at the given line.
    MultipleMediaTitles(usize),
    /// A second key line was found at the given line.
    MultipleKeys(usize),
}

impl std::error::Error for ParserError {}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        use std::convert::TryFrom;

        match *self {
            ParserError::UnexpectedLine(line, c) => {
                if let Ok(c) = char::try_from(c as u32) {
                    write!(f, "Unexpected line {} starting with '{}'", line, c)
                } else {
                    write!(f, "Unexpected line {} starting with U+{:04x}", line, c)
                }
            }
            ParserError::InvalidLineFormat(line, ref s) => {
                write!(f, "Invalid formatted line {}: \"{}\"", line, s)
            }
            ParserError::InvalidFieldEncoding(line, s) => {
                write!(f, "Invalid field encoding in line {} at {}", line, s)
            }
            ParserError::InvalidFieldFormat(line, s) => {
                write!(f, "Invalid field formatting in line {} at {}", line, s)
            }
            ParserError::MissingField(line, s) => write!(f, "Missing field {} in line {}", s, line),
            ParserError::FieldTrailingData(line, s) => {
                write!(f, "Field {} in line {} has trailing data", s, line)
            }
            ParserError::InvalidVersion(line, ref s) => write!(
                f,
                "Invalid version at line {}: {}",
                line,
                String::from_utf8_lossy(&*s)
            ),
            ParserError::MultipleVersions(line) => write!(f, "Multiple versions in line {}", line),
            ParserError::NoVersion => write!(f, "No version line"),
            ParserError::MultipleOrigins(line) => write!(f, "Multiple origins in line {}", line),
            ParserError::NoOrigin => write!(f, "No origin line"),
            ParserError::MultipleSessionNames(line) => {
                write!(f, "Multiple session-names in line {}", line)
            }
            ParserError::NoSessionName => write!(f, "No session-name line"),
            ParserError::MultipleSessionDescription(line) => {
                write!(f, "Multiple session-information in line {}", line)
            }
            ParserError::MultipleUris(line) => write!(f, "Multiple URIs in line {}", line),
            ParserError::MultipleConnections(line) => {
                write!(f, "Multiple connections in line {}", line)
            }
            ParserError::MultipleTimeZones(line) => write!(f, "Multiple zones in line {}", line),
            ParserError::MultipleMediaTitles(line) => {
                write!(f, "Multiple media titles in line {}", line)
            }
            ParserError::MultipleKeys(line) => write!(f, "Multiple keys in line {}", line),
        }
    }
}

impl Origin {
    fn parse(line: Line) -> Result<Self, ParserError> {
        use nom::{
            bytes::complete::{tag, take_until},
            sequence::terminated,
        };

        let (rem, username) = terminated(take_until(" "), tag(" "))(line.value)
            .map_err(|_: NomError| ParserError::InvalidFieldFormat(line.n, "Origin username"))?;

        let (rem, sess_id) = terminated(take_until(" "), tag(" "))(rem)
            .map_err(|_: NomError| ParserError::InvalidFieldFormat(line.n, "Origin sess-id"))?;

        let (rem, sess_version) =
            terminated(take_until(" "), tag(" "))(rem).map_err(|_: NomError| {
                ParserError::InvalidFieldFormat(line.n, "Origin sess-version")
            })?;

        let (rem, nettype) = terminated(take_until(" "), tag(" "))(rem)
            .map_err(|_: NomError| ParserError::InvalidFieldFormat(line.n, "Origin nettype"))?;

        let (unicast_address, addrtype) = terminated(take_until(" "), tag(" "))(rem)
            .map_err(|_: NomError| ParserError::InvalidFieldFormat(line.n, "Origin addrtype"))?;

        Ok(Self {
            username: if username == "-".as_bytes() {
                None
            } else {
                Some(parse_str(username, line.n, "Origin username")?)
            },
            sess_id: parse_str(sess_id, line.n, "Origin sess-id")?,
            sess_version: parse_parsable(sess_version, line.n, "Origin sess-version")?,
            nettype: parse_str(nettype, line.n, "Origin nettype")?,
            addrtype: parse_str(addrtype, line.n, "Origin addrtype")?,
            unicast_address: parse_str(unicast_address, line.n, "Origin unicast-address")?,
        })
    }
}

impl Connection {
    fn parse(line: Line) -> Result<Self, ParserError> {
        use nom::{
            bytes::complete::{tag, take_until},
            sequence::terminated,
        };

        let (rem, nettype) = terminated(take_until(" "), tag(" "))(line.value)
            .map_err(|_: NomError| ParserError::InvalidFieldFormat(line.n, "Connection nettype"))?;

        let (connection_address, addrtype) =
            terminated(take_until(" "), tag(" "))(rem).map_err(|_: NomError| {
                ParserError::InvalidFieldFormat(line.n, "Connection addrtype")
            })?;

        Ok(Self {
            nettype: parse_str(nettype, line.n, "Connection nettype")?,
            addrtype: parse_str(addrtype, line.n, "Connection addrtype")?,
            connection_address: parse_str(
                connection_address,
                line.n,
                "Connection connection-address",
            )?,
        })
    }
}

impl Bandwidth {
    fn parse(line: Line) -> Result<Self, ParserError> {
        use nom::{
            bytes::complete::{tag, take_until},
            sequence::terminated,
        };

        let (bandwidth, bwtype) = terminated(take_until(":"), tag(":"))(line.value)
            .map_err(|_: NomError| ParserError::InvalidFieldFormat(line.n, "Bandwidth bwtype"))?;

        Ok(Self {
            bwtype: parse_str(bwtype, line.n, "Bandwidth bwtype")?,
            bandwidth: parse_parsable(bandwidth, line.n, "Bandwidth bandwidth")?,
        })
    }
}

impl Time {
    fn parse(line: Line) -> Result<Time, ParserError> {
        use nom::{
            bytes::complete::{tag, take_until},
            sequence::terminated,
        };

        let (stop_time, start_time) = terminated(take_until(" "), tag(" "))(line.value)
            .map_err(|_: NomError| ParserError::InvalidFieldFormat(line.n, "Time start-time"))?;

        Ok(Self {
            start_time: parse_parsable(start_time, line.n, "Time start-time")?,
            stop_time: parse_parsable(stop_time, line.n, "Time stop-time")?,
            repeats: vec![],
        })
    }

    fn parse_with_repeats(time_line: Line, repeat_line: Vec<Line>) -> Result<Time, ParserError> {
        let time = Self::parse(time_line)?;
        let repeats = repeat_line
            .into_iter()
            .map(Repeat::parse)
            .collect::<Result<_, _>>()?;

        Ok(Time {
            start_time: time.start_time,
            stop_time: time.stop_time,
            repeats,
        })
    }
}

impl Repeat {
    fn parse(line: Line) -> Result<Repeat, ParserError> {
        use nom::{
            branch::alt,
            bytes::complete::{tag, take_until},
            combinator::{eof, rest},
            multi::many_till,
            sequence::terminated,
        };

        let (rem, repeat_interval) = terminated(take_until(" "), tag(" "))(line.value)
            .map_err(|_: NomError| ParserError::InvalidFieldFormat(line.n, "Repeat interval"))?;
        let repeat_interval = parse_typed_time(repeat_interval, line.n, "Repeat interval")? as u64;

        let (rem, active_duration) = alt((terminated(take_until(" "), tag(" ")), rest))(rem)
            .map_err(|_: NomError| ParserError::InvalidFieldFormat(line.n, "Repeat duration"))?;
        let active_duration = parse_typed_time(active_duration, line.n, "Repeat interval")? as u64;

        //many0 parser fails as soon as you use a child parser (like rest) that doesn't alter
        //input, this feels like a bug and should actually return in place and not fail
        //hence we use many_till here
        let (_, (offsets, _)) = many_till(alt((terminated(take_until(" "), tag(" ")), rest)), eof)(
            rem,
        )
        .map_err(|_: NomError| ParserError::InvalidFieldFormat(line.n, "Repeat offsets 1"))?;
        let offsets = offsets
            .into_iter()
            .map(|o| parse_typed_time(o, line.n, "Repeat offsets").map(|o| o as u64))
            .collect::<Result<_, _>>()?;

        Ok(Self {
            repeat_interval,
            active_duration,
            offsets,
        })
    }
}

impl TimeZone {
    #[allow(clippy::bind_instead_of_map)]
    fn parse(line: Line) -> Result<Vec<Self>, ParserError> {
        use nom::{
            branch::alt,
            bytes::complete::{tag, take_until},
            combinator::{map, rest},
            error::VerboseError,
            multi::many1,
            sequence::tuple,
        };

        let timezone_parser = map::<_, _, _, VerboseError<&[u8]>, _, _>(
            tuple((
                take_until(" "),
                tag(" "),
                alt((
                    map(tuple((take_until(" "), tag(" "))), |tuple| tuple.0),
                    rest,
                )),
            )),
            |(adjustment_time, _, offset)| (adjustment_time, offset),
        );
        //(_, timezones): (&[u8], Vec<(&[u8], &[u8])>)
        let (_, timezones) = many1(timezone_parser)(line.value).map_err(|_: NomError| {
            ParserError::InvalidFieldFormat(line.n, "TimeZone adjustment-time or offset")
        })?;
        let timezones: Result<Vec<Self>, ParserError> = timezones
            .into_iter()
            .map(|(adjustment_time, offset)| {
                parse_typed_time(offset, line.n, "TimeZone offset").and_then(|offset| {
                    parse_parsable(adjustment_time, line.n, "TimeZone adjustment-time").and_then(
                        |adjustment_time| {
                            Ok(Self {
                                adjustment_time,
                                offset,
                            })
                        },
                    )
                })
            })
            .collect();

        timezones
    }
}

impl Attribute {
    fn parse(line: Line) -> Result<Self, ParserError> {
        use nom::{
            branch::alt,
            bytes::complete::{tag, take_until},
            combinator::{opt, rest},
            sequence::preceded,
        };

        let (rem, attribute) = alt((take_until(":"), rest))(line.value)
            .map_err(|_: NomError| ParserError::InvalidFieldFormat(line.n, "Attribute name"))?;
        let (_, value) = opt(preceded(tag(":"), rest))(rem)
            .map_err(|_: NomError| ParserError::InvalidFieldFormat(line.n, "Attribute value"))?;

        Ok(Self {
            attribute: parse_str(attribute, line.n, "Attribute name")?,
            value: value
                .map(|v| parse_str(v, line.n, "Attribute value"))
                .transpose()?,
        })
    }
}

impl Key {
    fn parse(line: Line) -> Result<Self, ParserError> {
        use nom::{
            branch::alt,
            bytes::complete::{tag, take_until},
            combinator::{opt, rest},
            sequence::preceded,
        };

        let (rem, method) = alt((take_until(":"), rest))(line.value)
            .map_err(|_: NomError| ParserError::InvalidFieldFormat(line.n, "Key method"))?;
        let (_, encryption_key) = opt(preceded(tag(":"), rest))(rem)
            .map_err(|_: NomError| ParserError::InvalidFieldFormat(line.n, "Key encryption-key"))?;

        Ok(Self {
            method: parse_str(method, line.n, "Key method")?,
            encryption_key: encryption_key
                .map(|v| parse_str(v, line.n, "Key encryption-key"))
                .transpose()?,
        })
    }
}

impl Media {
    fn parse_all(line_number: usize, input: &[u8]) -> Result<Vec<Self>, ParserError> {
        let mut rem = input;
        let mut next_n = line_number;
        let mut medias = vec![];

        loop {
            let old_len = rem.len();
            let tuple = Self::parse_single(next_n, rem)?;
            rem = tuple.0;
            next_n = tuple.1;
            let media = tuple.2;

            if old_len == rem.len() {
                break;
            }
            medias.push(media);
            if rem.is_empty() {
                break;
            }
        }

        Ok(medias)
    }

    fn parse_single(line_number: usize, input: &[u8]) -> Result<(&[u8], usize, Self), ParserError> {
        let (rem, next_n, media) = parse_line("m=", line_number, input, "Media line")?;
        let (rem, next_n, media_info) = parse_opt_line("i=", next_n, rem, "Media info")?;
        let (rem, next_n, connections) = parse_multi0("c=", next_n, rem, "Media connection")?;
        let (rem, next_n, bandwidths) = parse_multi0("b=", next_n, rem, "Session bandwidth")?;
        let (rem, next_n, key) = parse_opt_line("k=", next_n, rem, "Session key")?;
        let (rem, next_n, attributes) = parse_multi0("a=", next_n, rem, "Session attribute")?;

        let media = Self::parse_m_line(media)?;

        let media = Media {
            media: media.media,
            port: media.port,
            num_ports: media.num_ports,
            proto: media.proto,
            fmt: media.fmt,
            media_title: media_info
                .map(|s| parse_str(s.value, s.n, "Session info"))
                .transpose()?,
            connections: connections
                .into_iter()
                .map(Connection::parse)
                .collect::<Result<_, _>>()?,
            bandwidths: bandwidths
                .into_iter()
                .map(Bandwidth::parse)
                .collect::<Result<_, _>>()?,
            key: key.map(Key::parse).transpose()?,
            attributes: attributes
                .into_iter()
                .map(Attribute::parse)
                .collect::<Result<_, _>>()?,
        };

        Ok((rem, next_n, media))
    }

    fn parse_m_line(line: Line) -> Result<Media, ParserError> {
        use nom::{
            bytes::complete::{tag, take_until},
            combinator::rest,
            error::VerboseError,
            sequence::{terminated, tuple},
        };

        let (rem, name) = terminated(take_until(" "), tag(" "))(line.value)
            .map_err(|_: NomError| ParserError::InvalidFieldFormat(line.n, "Media name"))?;

        let (rem, port) = terminated(take_until(" "), tag(" "))(rem)
            .map_err(|_: NomError| ParserError::InvalidFieldFormat(line.n, "Media port"))?;

        let (port, num_ports) =
            match tuple::<_, _, VerboseError<&[u8]>, _>((take_until("/"), tag("/"), rest))(port) {
                Ok((_, (port, _, num_ports))) => (port, Some(num_ports)),
                Err(_) => (port, None),
            };

        let (fmt, proto) = terminated(take_until(" "), tag(" "))(rem)
            .map_err(|_: NomError| ParserError::InvalidFieldFormat(line.n, "Media proto"))?;

        Ok(Media {
            media: parse_str(name, line.n, "Media name")?,
            port: parse_parsable(port, line.n, "Media port")?,
            num_ports: num_ports
                .map(|n| parse_parsable(n, line.n, "Media num_ports"))
                .transpose()?,
            proto: parse_str(proto, line.n, "Media proto")?,
            fmt: parse_str(fmt, line.n, "Media fmt")?,
            media_title: None,
            connections: Vec::new(),
            bandwidths: Vec::new(),
            key: None,
            attributes: Vec::new(),
        })
    }
}

impl Session {
    /// Parse an SDP session description from a byte slice.
    pub fn parse(input: &[u8]) -> Result<Session, ParserError> {
        let (rem, next_n) = parse_version(input)?;
        let (rem, next_n, origin) = parse_line("o=", next_n, rem, "Session originator")?;

        let (rem, next_n, session_name) = parse_line("s=", next_n, rem, "Session name")?;

        let (rem, next_n, session_info) = parse_opt_line("i=", next_n, rem, "Session info")?;

        let (rem, next_n, uri) = parse_opt_line("u=", next_n, rem, "Session uri")?;
        let (rem, next_n, emails) = parse_multi0("e=", next_n, rem, "Session email")?;
        let (rem, next_n, phones) = parse_multi0("p=", next_n, rem, "Session phone")?;
        let (rem, next_n, connection) = parse_opt_line("c=", next_n, rem, "Session connection")?;
        let (rem, next_n, bandwidths) = parse_multi0("b=", next_n, rem, "Session bandwidth")?;
        let (rem, next_n, time_with_repeats) = parse_time_with_repeats_many0(next_n, rem)?;
        let (rem, next_n, time_zone) = parse_opt_line("z=", next_n, rem, "Session timezone")?;
        let (rem, next_n, key) = parse_opt_line("k=", next_n, rem, "Session key")?;
        let (rem, next_n, attributes) = parse_multi0("a=", next_n, rem, "Session attribute")?;

        Ok(Session {
            origin: Origin::parse(origin)?,
            session_name: parse_str(session_name.value, session_name.n, "Session name")?,
            session_description: session_info
                .map(|s| parse_str(s.value, s.n, "Session info"))
                .transpose()?,
            uri: uri
                .map(|u| parse_str(u.value, u.n, "Session uri"))
                .transpose()?,
            emails: emails
                .into_iter()
                .map(|e| parse_str(e.value, e.n, "Session email"))
                .collect::<Result<_, _>>()?,
            phones: phones
                .into_iter()
                .map(|e| parse_str(e.value, e.n, "Session phone"))
                .collect::<Result<_, _>>()?,
            connection: connection.map(Connection::parse).transpose()?,
            bandwidths: bandwidths
                .into_iter()
                .map(Bandwidth::parse)
                .collect::<Result<_, _>>()?,
            times: time_with_repeats
                .into_iter()
                .map(|tuple| Time::parse_with_repeats(tuple.0, tuple.1))
                .collect::<Result<_, _>>()?,
            time_zones: time_zone
                .map(TimeZone::parse)
                .transpose()?
                .unwrap_or_else(Vec::new),
            key: key.map(Key::parse).transpose()?,
            attributes: attributes
                .into_iter()
                .map(Attribute::parse)
                .collect::<Result<_, _>>()?,
            medias: Media::parse_all(next_n, rem)?,
        })
    }
}

fn parse_version(input: &[u8]) -> Result<(&[u8], usize), ParserError> {
    let (rem, next_n, version) = parse_line("v=", 1, input, "version")?;
    if version.value != b"0" {
        Err(ParserError::InvalidVersion(1, version.value.into()))
    } else {
        Ok((rem, next_n))
    }
}

#[allow(clippy::type_complexity)]
fn parse_time_with_repeats_many0(
    line_number: usize,
    input: &[u8],
) -> Result<(&[u8], usize, Vec<(Line, Vec<Line>)>), ParserError> {
    let mut vec = vec![];
    let mut next_n = line_number;
    let mut rem = input;
    loop {
        let tuple = parse_time_with_repeats(next_n, rem)?;
        rem = tuple.0;
        next_n = tuple.1;
        let time_with_repeat = tuple.2;

        match time_with_repeat {
            Some(time_with_repeat) => vec.push(time_with_repeat),
            None => break,
        }
    }

    Ok((rem, next_n, vec))
}

#[allow(clippy::type_complexity)]
fn parse_time_with_repeats(
    line_number: usize,
    input: &[u8],
) -> Result<(&[u8], usize, Option<(Line, Vec<Line>)>), ParserError> {
    let (rem, next_n, time) = parse_opt_line("t=", line_number, input, "Session time")?;

    match time {
        Some(time) => {
            let (rem, next_n, repeat) = parse_multi0("r=", next_n, rem, "Session repeat")?;

            Ok((rem, next_n, Some((time, repeat))))
        }
        None => Ok((rem, next_n, None)),
    }
}

fn parse_multi0<'a>(
    prefix: &'static str,
    line_number: usize,
    input: &'a [u8],
    error: &'static str,
) -> Result<(&'a [u8], usize, Vec<Line<'a>>), ParserError> {
    use nom::{bytes::complete::tag, multi::many0, sequence::preceded};

    let (rem, emails) = many0(preceded(tag(prefix), take_till_line_end))(input)
        .map_err(|_: NomError| ParserError::InvalidFieldFormat(line_number, error))?;

    Ok((
        rem,
        line_number + emails.len(),
        emails
            .into_iter()
            .enumerate()
            .map(|(index, input)| (line_number + index, input).into())
            .collect::<Vec<Line>>(),
    ))
}

fn parse_opt_line<'a>(
    prefix: &'static str,
    line_number: usize,
    input: &'a [u8],
    error: &'static str,
) -> Result<(&'a [u8], usize, Option<Line<'a>>), ParserError> {
    let (rem, version) = take_opt_line(prefix, input)
        .map_err(|_: NomError| ParserError::InvalidFieldFormat(line_number, error))?;

    match version {
        Some(version) => Ok((
            rem,
            line_number + 1,
            Some(Line::from((line_number, version))),
        )),
        None => Ok((rem, line_number, None)),
    }
}

fn parse_line<'a>(
    prefix: &'static str,
    line_number: usize,
    input: &'a [u8],
    error: &'static str,
) -> Result<(&'a [u8], usize, Line<'a>), ParserError> {
    let (rem, version) = take_line(prefix, input)
        .map_err(|_: NomError| ParserError::InvalidFieldFormat(line_number, error))?;

    Ok((rem, line_number + 1, Line::from((line_number, version))))
}

fn take_line<'a>(
    prefix: &'static str,
    input: &'a [u8],
) -> nom::IResult<&'a [u8], &'a [u8], nom::error::VerboseError<&'a [u8]>> {
    use nom::{bytes::complete::tag, sequence::preceded};

    preceded(tag(prefix), take_till_line_end)(input)
}

fn take_opt_line<'a>(
    prefix: &'static str,
    input: &'a [u8],
) -> nom::IResult<&'a [u8], Option<&'a [u8]>, nom::error::VerboseError<&'a [u8]>> {
    use nom::{bytes::complete::tag, combinator::opt, sequence::preceded};

    opt(preceded(tag(prefix), take_till_line_end))(input)
}

fn take_till_line_end(input: &[u8]) -> nom::IResult<&[u8], &[u8], nom::error::VerboseError<&[u8]>> {
    use nom::{
        branch::alt,
        bytes::complete::{tag, take_until},
        sequence::terminated,
    };

    let (rem, line) = alt((
        terminated(take_until("\r\n"), tag("\r\n")),
        terminated(take_until("\r"), tag("\r")),
        terminated(take_until("\n"), tag("\n")),
    ))(input)?;

    nom::IResult::Ok((rem, line))
}

struct Line<'item> {
    /// The 1-based line number.
    n: usize,

    value: &'item [u8],
}

impl<'item> From<(usize, &'item [u8])> for Line<'item> {
    fn from(tuple: (usize, &'item [u8])) -> Self {
        Self {
            n: tuple.0,
            value: tuple.1,
        }
    }
}

fn parse_str(it: &[u8], line: usize, field: &'static str) -> Result<String, ParserError> {
    std::str::from_utf8(it)
        .map(String::from)
        .map_err(|_| ParserError::InvalidFieldEncoding(line, field))
}

fn parse_parsable<T>(it: &[u8], line: usize, field: &'static str) -> Result<T, ParserError>
where
    T: std::str::FromStr,
{
    std::str::from_utf8(it)
        .map_err(|_| ParserError::InvalidFieldEncoding(line, field))
        .and_then(|s| {
            s.parse()
                .map_err(|_| ParserError::InvalidFieldFormat(line, field))
        })
}

fn take_until_time_unit(
    input: &[u8],
) -> nom::IResult<&[u8], &[u8], nom::error::VerboseError<&[u8]>> {
    use nom::{branch::alt, bytes::complete::take_until, combinator::rest};

    alt((
        take_until("d"),
        take_until("h"),
        take_until("m"),
        take_until("s"),
        rest,
    ))(input)
}

fn time_unit_to_sec(
    input: &[u8],
    line_number: usize,
    descr: &'static str,
) -> Result<u64, ParserError> {
    match input {
        b"d" => Ok(86_400),
        b"h" => Ok(3_600),
        b"m" => Ok(60),
        b"s" => Ok(1),
        b"" => Ok(1),
        _ => Err(ParserError::InvalidFieldFormat(line_number, descr)),
    }
}

fn parse_typed_time(
    part: &[u8],
    line_number: usize,
    descr: &'static str,
) -> Result<i64, ParserError> {
    use nom::{bytes::complete::tag, combinator::opt, error::VerboseError};
    use std::convert::TryInto;

    let invalid_format_error =
        |_: nom::Err<VerboseError<&[u8]>>| ParserError::InvalidFieldFormat(line_number, descr);

    let (rem, sign) = opt(tag("-"))(part).map_err(invalid_format_error)?;
    let (time_unit, period) = take_until_time_unit(rem).map_err(invalid_format_error)?;

    let period: u64 = parse_parsable(period, line_number, descr)?;

    let offset: i64 = (time_unit_to_sec(time_unit, line_number, descr)? * period)
        .try_into()
        .map_err(|_| ParserError::InvalidFieldFormat(line_number, descr))?;

    match sign {
        Some(_) => Ok(-offset),
        None => Ok(offset),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    impl<'item> From<&'item [u8]> for Line<'item> {
        fn from(input: &'item [u8]) -> Self {
            Self { n: 1, value: input }
        }
    }

    #[test]
    fn parse_origin() {
        let line = "- 3840284955 3840284955 IN IP4 192.168.0.30"
            .as_bytes()
            .into();
        assert_eq!(
            Origin::parse(line).unwrap(),
            Origin {
                username: None,
                sess_id: "3840284955".into(),
                sess_version: 3840284955,
                nettype: "IN".into(),
                addrtype: "IP4".into(),
                unicast_address: "192.168.0.30".into(),
            }
        );

        let line = "jdoe 2890844526 2890842807 IN IP6 ::ffff:c0a8:1e"
            .as_bytes()
            .into();

        assert_eq!(
            Origin::parse(line).unwrap(),
            Origin {
                username: Some("jdoe".into()),
                sess_id: "2890844526".into(),
                sess_version: 2890842807,
                nettype: "IN".into(),
                addrtype: "IP6".into(),
                unicast_address: "::ffff:c0a8:1e".into(),
            }
        );

        let line = "jdoe 2890844526 2890842807IN IP6 ::ffff:c0a8:1e"
            .as_bytes()
            .into();
        assert!(Origin::parse(line).is_err());
    }

    #[test]
    fn parse_connection() {
        let line = "IN IP4 192.168.0.30".as_bytes().into();
        assert_eq!(
            Connection::parse(line).unwrap(),
            Connection {
                nettype: "IN".into(),
                addrtype: "IP4".into(),
                connection_address: "192.168.0.30".into()
            }
        );

        let line = "IN IP6 ::ffff:c0a8:1e".as_bytes().into();
        assert_eq!(
            Connection::parse(line).unwrap(),
            Connection {
                nettype: "IN".into(),
                addrtype: "IP6".into(),
                connection_address: "::ffff:c0a8:1e".into()
            }
        );

        let line = "IN IP6".as_bytes().into();
        assert!(Connection::parse(line).is_err());
    }

    #[test]
    fn parse_bandwidth() {
        let line = "AS:128".as_bytes().into();
        assert_eq!(
            Bandwidth::parse(line).unwrap(),
            Bandwidth {
                bwtype: "AS".into(),
                bandwidth: 128
            }
        );

        let line = "X-YZ:256".as_bytes().into();
        assert_eq!(
            Bandwidth::parse(line).unwrap(),
            Bandwidth {
                bwtype: "X-YZ".into(),
                bandwidth: 256
            }
        );

        let line = "AS 128".as_bytes().into();
        assert!(Bandwidth::parse(line).is_err());
    }

    #[test]
    fn parse_time() {
        let line = "2873397496 2873404696".as_bytes().into();
        assert_eq!(
            Time::parse(line).unwrap(),
            Time {
                start_time: 2873397496,
                stop_time: 2873404696,
                repeats: vec![]
            }
        );

        let time_line = "2873397496 2873404696".as_bytes().into();
        let repeat_line = "604800 3600 0 90000".as_bytes().into();
        assert_eq!(
            Time::parse_with_repeats(time_line, vec![repeat_line]).unwrap(),
            Time {
                start_time: 2873397496,
                stop_time: 2873404696,
                repeats: vec![Repeat {
                    repeat_interval: 604800,
                    active_duration: 3600,
                    offsets: vec![0, 90000]
                }]
            }
        );
    }

    #[test]
    fn parse_repeat() {
        let line = "604800 3600 0 90000".as_bytes().into();
        assert_eq!(
            Repeat::parse(line).unwrap(),
            Repeat {
                repeat_interval: 604800,
                active_duration: 3600,
                offsets: vec![0, 90000]
            }
        );

        let line = "7d 1h 0 25h".as_bytes().into();
        assert_eq!(
            Repeat::parse(line).unwrap(),
            Repeat {
                repeat_interval: 604800,
                active_duration: 3600,
                offsets: vec![0, 90000]
            }
        );
    }

    #[test]
    fn parse_timezones() {
        let line = "2882844526 -2h".as_bytes().into();
        assert_eq!(
            TimeZone::parse(line).unwrap(),
            vec![TimeZone {
                adjustment_time: 2882844526,
                offset: -(2 * 3_600)
            }]
        );

        let line = "2882844526 -1h 2898848070 0".as_bytes().into();
        assert_eq!(
            TimeZone::parse(line).unwrap(),
            vec![
                TimeZone {
                    adjustment_time: 2882844526,
                    offset: -3_600
                },
                TimeZone {
                    adjustment_time: 2898848070,
                    offset: 0
                }
            ]
        );

        let line = "2882844526 -2y".as_bytes().into();
        assert!(TimeZone::parse(line).is_err());
    }

    #[test]
    fn parse_attribute() {
        let line = "sendrecv".as_bytes().into();
        assert_eq!(
            Attribute::parse(line).unwrap(),
            Attribute {
                attribute: "sendrecv".into(),
                value: None
            }
        );

        let line = "rtpmap:9 G722/8000".as_bytes().into();
        assert_eq!(
            Attribute::parse(line).unwrap(),
            Attribute {
                attribute: "rtpmap".into(),
                value: Some("9 G722/8000".into())
            }
        );
    }

    #[test]
    fn parse_key() {
        let line = "prompt".as_bytes().into();
        assert_eq!(
            Key::parse(line).unwrap(),
            Key {
                method: "prompt".into(),
                encryption_key: None
            }
        );

        let line = "clear:1234".as_bytes().into();
        assert_eq!(
            Key::parse(line).unwrap(),
            Key {
                method: "clear".into(),
                encryption_key: Some("1234".into())
            }
        );
    }

    #[test]
    fn parse_media_all() {
        let data = concat!(
            "m=video 51372/2 RTP/AVP 99\r",
            "i=A Seminar on the session description protocol\r",
            "c=IN IP4 224.2.17.12/127\r",
            "b=AS:128\r",
            "k=clear:1234\r",
            "a=rtpmap:99 h263-1998/90000\r",
            "a=fingerprint:sha-256 3A:96:6D:57:B2:C2:C7:61:A0:46:3E:1C:97:39:D3:F7:0A:88:A0:B1:EC:03:FB:10:A5:5D:3A:37:AB:DD:02:AA\r"
        )
        .as_bytes();
        let medias = Media::parse_all(1, data).unwrap();
        assert_eq!(medias.first().cloned().unwrap(), Media {
            media: "video".into(),
            port: 51372,
            num_ports: Some(2),
            proto: "RTP/AVP".into(),
            fmt: "99".into(),
            media_title: Some("A Seminar on the session description protocol".into()),
            connections: vec![Connection {
                nettype: String::from("IN"),
                addrtype: String::from("IP4"),
                connection_address: String::from("224.2.17.12/127"),
            }],
            bandwidths: vec![Bandwidth {
                bwtype: String::from("AS"),
                bandwidth: 128,
            }],
            key: Some(Key {
                method: String::from("clear"),
                encryption_key: Some(String::from("1234")),
            }),
            attributes: vec![Attribute {
                attribute: String::from("rtpmap"),
                value: Some(String::from("99 h263-1998/90000"))
            }, Attribute {
                attribute: String::from("fingerprint"),
                value: Some(String::from("sha-256 3A:96:6D:57:B2:C2:C7:61:A0:46:3E:1C:97:39:D3:F7:0A:88:A0:B1:EC:03:FB:10:A5:5D:3A:37:AB:DD:02:AA"))
            }]
        });

        let data = concat!(
            "m=audio 49170 RTP/AVP 0\r",
            "m=video 51372/2 RTP/AVP 99\r",
            "i=A Seminar on the session description protocol\r",
            "c=IN IP4 224.2.17.12/127\r",
            "b=AS:128\r",
            "k=clear:1234\r",
            "a=rtpmap:99 h263-1998/90000\r",
            "a=fingerprint:sha-256 3A:96:6D:57:B2:C2:C7:61:A0:46:3E:1C:97:39:D3:F7:0A:88:A0:B1:EC:03:FB:10:A5:5D:3A:37:AB:DD:02:AA\r"
        )
        .as_bytes();
        let medias = Media::parse_all(1, data).unwrap();
        assert_eq!(
            medias.first().cloned().unwrap(),
            Media {
                media: "audio".into(),
                port: 49170,
                num_ports: None,
                proto: "RTP/AVP".into(),
                fmt: "0".into(),
                media_title: None,
                connections: vec![],
                bandwidths: vec![],
                key: None,
                attributes: vec![]
            }
        );
        assert_eq!(medias.last().cloned().unwrap(), Media {
            media: "video".into(),
            port: 51372,
            num_ports: Some(2),
            proto: "RTP/AVP".into(),
            fmt: "99".into(),
            media_title: Some("A Seminar on the session description protocol".into()),
            connections: vec![Connection {
                nettype: String::from("IN"),
                addrtype: String::from("IP4"),
                connection_address: String::from("224.2.17.12/127"),
            }],
            bandwidths: vec![Bandwidth {
                bwtype: String::from("AS"),
                bandwidth: 128,
            }],
            key: Some(Key {
                method: String::from("clear"),
                encryption_key: Some(String::from("1234")),
            }),
            attributes: vec![Attribute {
                attribute: String::from("rtpmap"),
                value: Some(String::from("99 h263-1998/90000"))
            }, Attribute {
                attribute: String::from("fingerprint"),
                value: Some(String::from("sha-256 3A:96:6D:57:B2:C2:C7:61:A0:46:3E:1C:97:39:D3:F7:0A:88:A0:B1:EC:03:FB:10:A5:5D:3A:37:AB:DD:02:AA"))
            }]
        });
    }

    #[test]
    fn parse_media_single() {
        let data = concat!(
            "m=video 51372/2 RTP/AVP 99\r",
            "i=A Seminar on the session description protocol\r",
            "c=IN IP4 224.2.17.12/127\r",
            "b=AS:128\r",
            "k=clear:1234\r",
            "a=rtpmap:99 h263-1998/90000\r",
            "a=fingerprint:sha-256 3A:96:6D:57:B2:C2:C7:61:A0:46:3E:1C:97:39:D3:F7:0A:88:A0:B1:EC:03:FB:10:A5:5D:3A:37:AB:DD:02:AA\r"
        )
        .as_bytes();
        let (rem, next_n, media) = Media::parse_single(1, data).unwrap();
        assert_eq!(media, Media {
            media: "video".into(),
            port: 51372,
            num_ports: Some(2),
            proto: "RTP/AVP".into(),
            fmt: "99".into(),
            media_title: Some("A Seminar on the session description protocol".into()),
            connections: vec![Connection {
                nettype: String::from("IN"),
                addrtype: String::from("IP4"),
                connection_address: String::from("224.2.17.12/127"),
            }],
            bandwidths: vec![Bandwidth {
                bwtype: String::from("AS"),
                bandwidth: 128,
            }],
            key: Some(Key {
                method: String::from("clear"),
                encryption_key: Some(String::from("1234")),
            }),
            attributes: vec![Attribute {
                attribute: String::from("rtpmap"),
                value: Some(String::from("99 h263-1998/90000"))
            }, Attribute {
                attribute: String::from("fingerprint"),
                value: Some(String::from("sha-256 3A:96:6D:57:B2:C2:C7:61:A0:46:3E:1C:97:39:D3:F7:0A:88:A0:B1:EC:03:FB:10:A5:5D:3A:37:AB:DD:02:AA"))
            }]
        });
        assert_eq!(rem, "".as_bytes());
        assert_eq!(next_n, 8);
    }

    #[test]
    fn parse_media_m_line() {
        let line = "audio 49170 RTP/AVP 0".as_bytes().into();
        let media = Media::parse_m_line(line).unwrap();

        assert_eq!(
            media,
            Media {
                media: "audio".into(),
                port: 49170,
                num_ports: None,
                proto: "RTP/AVP".into(),
                fmt: "0".into(),
                media_title: None,
                connections: vec![],
                bandwidths: vec![],
                key: None,
                attributes: vec![]
            }
        );

        let line = "audio 50002 RTP/AVP 96 9 0 8 97 98 99 3 101 102 103 104"
            .as_bytes()
            .into();
        let media = Media::parse_m_line(line).unwrap();
        assert_eq!(
            media,
            Media {
                media: "audio".into(),
                port: 50002,
                num_ports: None,
                proto: "RTP/AVP".into(),
                fmt: "96 9 0 8 97 98 99 3 101 102 103 104".into(),
                media_title: None,
                connections: vec![],
                bandwidths: vec![],
                key: None,
                attributes: vec![]
            }
        );

        let line = "video 49170/2 RTP/AVP 31".as_bytes().into();
        let media = Media::parse_m_line(line).unwrap();
        assert_eq!(
            media,
            Media {
                media: "video".into(),
                port: 49170,
                num_ports: Some(2),
                proto: "RTP/AVP".into(),
                fmt: "31".into(),
                media_title: None,
                connections: vec![],
                bandwidths: vec![],
                key: None,
                attributes: vec![]
            }
        );
    }

    #[test]
    fn parse_session() {
        let data = concat!(
            "v=0\r",
            "o=jdoe 2890844526 2890842807 IN IP4 10.47.16.5\r",
            "s=SDP Seminar\r",
            "i=A Seminar on the session description protocol\r",
            "u=http://www.example.com/seminars/sdp.pdf\r",
            "e=j.doe@example.com (Jane Doe)\r",
            "p=+1 617 555-6011\r",
            "c=IN IP4 224.2.17.12/127\r",
            "b=AS:128\r",
            "t=2873397496 2873404696\r",
            "r=7d 1h 0 25h\r",
            "t=0 0\r",
            "z=2882844526 -1h 2898848070 0\r",
            "k=clear:1234\r",
            "a=recvonly\r"
        );
        let media1 = "m=audio 49170 RTP/AVP 0\r";
        let media2 = concat!(
            "m=video 51372/2 RTP/AVP 99\r",
            "a=rtpmap:99 h263-1998/90000\r",
            "a=fingerprint:sha-256 3A:96:6D:57:B2:C2:C7:61:A0:46:3E:1C:97:39:D3:F7:0A:88:A0:B1:EC:03:FB:10:A5:5D:3A:37:AB:DD:02:AA\r"
        );
        let data = format!("{}{}{}", data, media1, media2);
        let session = Session::parse(&data.as_bytes()).unwrap();
        let expected_session = Session {
            origin: Origin::parse(
                "jdoe 2890844526 2890842807 IN IP4 10.47.16.5"
                    .as_bytes()
                    .into(),
            )
            .unwrap(),
            session_name: "SDP Seminar".into(),
            session_description: Some("A Seminar on the session description protocol".into()),
            uri: Some("http://www.example.com/seminars/sdp.pdf".into()),
            emails: vec!["j.doe@example.com (Jane Doe)".into()],
            phones: vec!["+1 617 555-6011".into()],
            connection: Some(
                Connection::parse("IN IP4 224.2.17.12/127".as_bytes().into()).unwrap(),
            ),
            bandwidths: vec![Bandwidth::parse("AS:128".as_bytes().into()).unwrap()],
            times: vec![
                Time::parse_with_repeats(
                    "2873397496 2873404696".as_bytes().into(),
                    vec!["7d 1h 0 25h".as_bytes().into()],
                )
                .unwrap(),
                Time::parse("0 0".as_bytes().into()).unwrap(),
            ],
            time_zones: TimeZone::parse("2882844526 -1h 2898848070 0".as_bytes().into()).unwrap(),
            key: Some(Key::parse("clear:1234".as_bytes().into()).unwrap()),
            attributes: vec![Attribute::parse("recvonly".as_bytes().into()).unwrap()],
            medias: vec![
                Media::parse_single(1, media1.as_bytes()).unwrap().2,
                Media::parse_single(1, media2.as_bytes()).unwrap().2,
            ],
        };
        assert_eq!(session, expected_session);

        let data = format!("{}{}{}\rv=1234\r", data, media1, media2);
        assert!(Session::parse(&data.as_bytes()).is_err())
    }
}
