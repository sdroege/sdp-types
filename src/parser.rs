// Copyright (C) 2019 Sebastian Dröge <sebastian@centricular.com>
//
// Licensed under the MIT license, see the LICENSE file or <http://opensource.org/licenses/MIT>

use super::*;

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
    fn parse((line, data): (usize, &[u8])) -> Result<Origin, ParserError> {
        // <username> <sess-id> <sess-version> <nettype> <addrtype> <unicast-address>
        let mut origin = data.splitn_str(6, b" ");
        let username = parse_str(&mut origin, line, "Origin username")?;
        let sess_id = parse_str(&mut origin, line, "Origin sess-id")?;
        let sess_version = parse_str_u64(&mut origin, line, "Origin sess-version")?;
        let nettype = parse_str(&mut origin, line, "Origin nettype")?;
        let addrtype = parse_str(&mut origin, line, "Origin addrtype")?;
        let unicast_address = parse_str(&mut origin, line, "Origin unicast-address")?;

        Ok(Origin {
            username: if username == "-" {
                None
            } else {
                Some(username)
            },
            sess_id,
            sess_version,
            nettype,
            addrtype,
            unicast_address,
        })
    }
}

impl Connection {
    fn parse((line, data): (usize, &[u8])) -> Result<Connection, ParserError> {
        // <nettype> <addrtype> <connection-address>
        let mut connection = data.splitn_str(3, b" ");
        let nettype = parse_str(&mut connection, line, "Connection nettype")?;
        let addrtype = parse_str(&mut connection, line, "Connection addrtype")?;
        let connection_address = parse_str(&mut connection, line, "Connection connection-address")?;

        Ok(Connection {
            nettype,
            addrtype,
            connection_address,
        })
    }
}

impl Bandwidth {
    fn parse((line, data): (usize, &[u8])) -> Result<Bandwidth, ParserError> {
        // <bwtype>:<bandwidth>
        let mut bandwidth = data.split_str(b":");
        let bwtype = parse_str(&mut bandwidth, line, "Bandwidth bwtype")?;
        let bw = parse_str_u64(&mut bandwidth, line, "Bandwidth bandwidth")?;
        if bandwidth.next().is_some() {
            return Err(ParserError::FieldTrailingData(line, "Bandwidth"));
        }

        Ok(Bandwidth {
            bwtype,
            bandwidth: bw,
        })
    }
}

impl Time {
    fn parse((line, data): (usize, &[u8])) -> Result<Time, ParserError> {
        // <start-time> <stop-time>
        let mut time = data.split_str(b" ");
        let start_time = parse_str_u64(&mut time, line, "Time start-time")?;
        let stop_time = parse_str_u64(&mut time, line, "Time stop-time")?;
        if time.next().is_some() {
            return Err(ParserError::FieldTrailingData(line, "Time"));
        }

        Ok(Time {
            start_time,
            stop_time,
            repeats: Vec::new(),
        })
    }
}

fn parse_typed_time(s: &[u8], line: usize, field: &'static str) -> Result<u64, ParserError> {
    let (num, factor) = match s
        .split_last()
        .ok_or(ParserError::InvalidFieldFormat(line, field))?
    {
        (b'd', prefix) => (prefix, 86_400),
        (b'h', prefix) => (prefix, 3_600),
        (b'm', prefix) => (prefix, 60),
        (b's', prefix) => (prefix, 1),
        (_, _) => (s, 1),
    };

    let num =
        std::str::from_utf8(num).map_err(|_| ParserError::InvalidFieldEncoding(line, field))?;
    let num = num
        .parse::<u64>()
        .map_err(|_| ParserError::InvalidFieldFormat(line, field))?;
    num.checked_mul(factor)
        .ok_or(ParserError::InvalidFieldFormat(line, field))
}

impl Repeat {
    fn parse((line, data): (usize, &[u8])) -> Result<Repeat, ParserError> {
        // <repeat interval> <active duration> <offsets from start-time>
        let mut repeat = data.split_str(b" ");
        let repeat_interval = repeat
            .next()
            .ok_or(ParserError::MissingField(line, "Repeat repeat-interval"))
            .and_then(|s| parse_typed_time(s, line, "Repeat repeat-interval"))?;
        let active_duration = repeat
            .next()
            .ok_or(ParserError::MissingField(line, "Repeat active-duration"))
            .and_then(|s| parse_typed_time(s, line, "Repeat active-duration"))?;

        let offsets = repeat
            .map(|s| parse_typed_time(s, line, "Repeat active-duration"))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Repeat {
            repeat_interval,
            active_duration,
            offsets,
        })
    }
}

impl TimeZone {
    fn parse((line, data): (usize, &[u8])) -> Result<Vec<TimeZone>, ParserError> {
        // <adjustment time> <offset> <adjustment time> <offset> ....
        let mut zones = data.split_str(b" ");

        let mut ret = Vec::new();
        loop {
            let adjustment_time = parse_str_u64(&mut zones, line, "TimeZone adjustment-time");

            let adjustment_time = match adjustment_time {
                Ok(adjustment_time) => adjustment_time,
                Err(ParserError::MissingField(..)) => break,
                Err(err) => return Err(err),
            };

            let offset = zones
                .next()
                .ok_or(ParserError::MissingField(line, "TimeZone offset"))
                .and_then(|s| {
                    use std::convert::TryInto;

                    let (sign, s) = if s.get(0) == Some(&b'-') {
                        (true, &s[1..])
                    } else {
                        (false, s)
                    };

                    parse_typed_time(s, line, "TimeZone offset")
                        .and_then(|t| {
                            t.try_into().map_err(|_| {
                                ParserError::InvalidFieldFormat(line, "TimeZone offset")
                            })
                        })
                        .map(|t: i64| if sign { -t } else { t })
                })?;

            ret.push(TimeZone {
                adjustment_time,
                offset,
            });
        }

        Ok(ret)
    }
}

impl Attribute {
    fn parse((line, data): (usize, &[u8])) -> Result<Attribute, ParserError> {
        // <attribute>:<value>
        // <attribute>
        let mut attribute = data.splitn_str(2, b":");
        let name = parse_str(&mut attribute, line, "Attribute name")?;
        let value = parse_str_opt(&mut attribute, line, "Attribute value")?;

        Ok(Attribute {
            attribute: name,
            value,
        })
    }
}

impl Key {
    fn parse((line, data): (usize, &[u8])) -> Result<Key, ParserError> {
        // <method>:<encryption key>
        // <method>
        let mut key = data.splitn_str(2, b":");
        let method = parse_str(&mut key, line, "Key method")?;
        let encryption_key = parse_str_opt(&mut key, line, "Key encryption-key")?;

        Ok(Key {
            method,
            encryption_key,
        })
    }
}

impl Media {
    fn parse_m_line((line, data): (usize, &[u8])) -> Result<Media, ParserError> {
        // <media> <port> <proto> <fmt> ...
        let mut media = data.splitn_str(4, b" ");
        let name = parse_str(&mut media, line, "Media name")?;

        let (port, num_ports) = media
            .next()
            .ok_or(ParserError::MissingField(line, "Media port"))
            .and_then(|s| str_from_utf8((line, s), "Media Port"))
            .and_then(|port| {
                let mut split = port.splitn(2, '/');
                let port = split
                    .next()
                    .ok_or(ParserError::MissingField(line, "Media port"))
                    .and_then(|port| {
                        port.parse()
                            .map_err(|_| ParserError::InvalidFieldFormat(line, "Media port"))
                    })?;

                let num_ports = split
                    .next()
                    .ok_or(ParserError::MissingField(line, "Media num-ports"))
                    .and_then(|num_ports| {
                        num_ports
                            .parse()
                            .map_err(|_| ParserError::InvalidFieldFormat(line, "Media num-ports"))
                    });

                match num_ports {
                    Ok(num_ports) => Ok((port, Some(num_ports))),
                    Err(ParserError::MissingField(..)) => Ok((port, None)),
                    Err(err) => Err(err),
                }
            })?;

        let proto = parse_str(&mut media, line, "Media proto")?;
        let fmt = parse_str(&mut media, line, "Media fmt")?;

        Ok(Media {
            media: name,
            port,
            num_ports,
            proto,
            fmt,
            media_title: None,
            connections: Vec::new(),
            bandwidths: Vec::new(),
            key: None,
            attributes: Vec::new(),
        })
    }

    fn parse<'a>(
        lines: &mut std::iter::Peekable<impl Iterator<Item = (usize, &'a [u8])>>,
    ) -> Result<Option<Media>, ParserError> {
        let media = match line_parser(lines, b'm', b"icbka")
            .next()?
            .map(Media::parse_m_line)
            .transpose()?
        {
            None => return Ok(None),
            Some(media) => media,
        };

        // Parse media information line
        // - Can exist not at all or exactly once
        // - Must be followed by "cbka" lines for the this media
        //   or "m" for the following media
        let media_title = line_parser_once(
            lines,
            b'i',
            b"cbkam",
            ParserError::MultipleMediaTitles,
            |v| str_from_utf8(v, "Media Title"),
        )?;

        // Parse connection lines
        // - Can exist not at all, once or multiple times
        // - Must be followed by "bka" lines for the this media
        //   or "m" for the following media
        let connections = line_parser(lines, b'c', b"bkam")
            .map(Connection::parse)
            .collect::<Vec<_>>()?;

        // Parse bandwidth lines:
        // - Can exist not at all, once or multiple times
        // - Must be followed by "ka" lines for the this media
        //   or "m" for the following media
        let bandwidths = line_parser(lines, b'b', b"kam")
            .map(Bandwidth::parse)
            .collect::<Vec<_>>()?;

        // Parse key line
        // - Can exist not at all or exactly once
        // - Must be followed by "a" lines for the this media
        //   or "m" for the following media
        let key = line_parser_once(lines, b'k', b"am", ParserError::MultipleKeys, Key::parse)?;

        // Parse attribute lines:
        // - Can exist not at all, once or multiple times
        // - Must be followed by "m" lines
        let attributes = line_parser(lines, b'a', b"m")
            .map(Attribute::parse)
            .collect::<Vec<_>>()?;

        Ok(Some(Media {
            media_title,
            connections,
            bandwidths,
            key,
            attributes,
            ..media
        }))
    }
}

impl Session {
    /// Parse an SDP session description from a byte slice.
    pub fn parse(data: &[u8]) -> Result<Session, ParserError> {
        // Create an iterator which returns for each line its human-readable
        // (1-based) line number and contents.
        let mut lines = data.lines().enumerate().map(|(i, bytes)| (i + 1, bytes)).peekable();

        // See RFC 4566 Section 9 for details

        // Check version line:
        // - Must only exist exactly
        // - Must be followed by "o" line
        // - Must have a value of b"0"
        line_parser_once(
            &mut lines,
            b'v',
            b"o",
            ParserError::MultipleVersions,
            |(line, version)| {
                if version != b"0" {
                    Err(ParserError::InvalidVersion(line, version.into()))
                } else {
                    Ok(())
                }
            },
        )?
        .ok_or(ParserError::NoVersion)?;

        // Parse origin line:
        // - Must only exist exactly once
        // - Must be followed by "s" line
        let origin = line_parser_once(
            &mut lines,
            b'o',
            b"s",
            ParserError::MultipleOrigins,
            Origin::parse,
        )?
        .ok_or(ParserError::NoOrigin)?;

        // Parse session name line:
        // - Must only exist exactly once
        // - Must be followed by "iuepcbt" line
        let session_name = line_parser_once(
            &mut lines,
            b's',
            b"iuepcbt",
            ParserError::MultipleSessionNames,
            |v| str_from_utf8(v, "Session Name"),
        )?
        .ok_or(ParserError::NoSessionName)?;

        // Parse session information line:
        // - Must only exist once or not at all
        // - Must be followed by "uepcbt" line
        let session_description = line_parser_once(
            &mut lines,
            b'i',
            b"uepcbt",
            ParserError::MultipleSessionDescription,
            |v| str_from_utf8(v, "Session Description"),
        )?;

        // Parse URI line:
        // - Must only exist once or not at all
        // - Must be followed by "epcbt" line
        let uri = line_parser_once(&mut lines, b'u', b"epcbt", ParserError::MultipleUris, |v| {
            str_from_utf8(v, "Uri")
        })?;

        // Parse E-Mail lines:
        // - Can exist not at all, once or multiple times
        // - Must be followed by "pcbt" line
        let emails = line_parser(&mut lines, b'e', b"pcbt")
            .map(|v| str_from_utf8(v, "E-Mail"))
            .collect::<Vec<_>>()?;

        // Parse phone number lines:
        // - Can exist not at all, once or multiple times
        // - Must be followed by "cbt" line
        let phones = line_parser(&mut lines, b'p', b"cbt")
            .map(|v| str_from_utf8(v, "Phone"))
            .collect::<Vec<_>>()?;

        // Parse connection line:
        // - Can exist not at all or exactly once per session
        // - Must be followed by "bt" line
        let connection = line_parser_once(
            &mut lines,
            b'c',
            b"bt",
            ParserError::MultipleConnections,
            Connection::parse,
        )?;

        // Parse bandwidth lines:
        // - Can exist not at all, once or multiple times
        // - Must be followed by "t" line
        let bandwidths = line_parser(&mut lines, b'b', b"t")
            .map(Bandwidth::parse)
            .collect::<Vec<_>>()?;

        // Parse time lines
        // - Must exist at least once
        // - If followed by "r" lines then these are part of the same time field
        // - Must be followed by "zam" lines
        let mut times = vec![];
        loop {
            let time = match line_parser(&mut lines, b't', b"rzkam")
                .next()?
                .map(Time::parse)
                .transpose()?
            {
                None => break,
                Some(time) => time,
            };

            // Parse repeat lines
            // - Can exist not at all, once or multiple times
            // - Must be followed by "t" line for the next time, or "zam" lines
            let repeats = line_parser(&mut lines, b'r', b"tzkam")
                .map(Repeat::parse)
                .collect::<Vec<_>>()?;

            times.push(Time { repeats, ..time });
        }

        // Parse zones line:
        // - Can exist not at all or exactly once per session
        // - Must be followed by "am" lines
        let time_zones = line_parser_once(
            &mut lines,
            b'z',
            b"kam",
            ParserError::MultipleTimeZones,
            TimeZone::parse,
        )?
        .unwrap_or_else(Vec::new);

        // Parse key line
        // - Can exist not at all or exactly once
        // - Must be followed by "a" lines for the this media
        //   or "m" for the following media
        let key = line_parser_once(
            &mut lines,
            b'k',
            b"am",
            ParserError::MultipleKeys,
            Key::parse,
        )?;

        // Parse attribute lines:
        // - Can exist not at all, once or multiple times
        // - Must be followed by "m" lines
        let attributes = line_parser(&mut lines, b'a', b"m")
            .map(Attribute::parse)
            .collect::<Vec<_>>()?;

        // Parse media lines:
        // - Can exist not at all, once or multiple times
        // - Are followed by "icbka" fields for the same media
        //   or "m" for the following media
        let mut medias = vec![];
        while let Some(media) = Media::parse(&mut lines)? {
            medias.push(media);
        }

        Ok(Session {
            origin,
            session_name,
            session_description,
            uri,
            emails,
            phones,
            connection,
            bandwidths,
            times,
            time_zones,
            key,
            attributes,
            medias,
        })
    }
}

// Field parser helpers on byte slice iterators
fn parse_str<'a>(
    it: &mut impl Iterator<Item = &'a [u8]>,
    line: usize,
    field: &'static str,
) -> Result<String, ParserError> {
    it.next()
        .ok_or(ParserError::MissingField(line, field))
        .and_then(|b| {
            std::str::from_utf8(b)
                .map(String::from)
                .map_err(|_| ParserError::InvalidFieldEncoding(line, field))
        })
}

fn parse_str_u64<'a>(
    it: &mut impl Iterator<Item = &'a [u8]>,
    line: usize,
    field: &'static str,
) -> Result<u64, ParserError> {
    it.next()
        .ok_or(ParserError::MissingField(line, field))
        .and_then(|b| {
            std::str::from_utf8(b).map_err(|_| ParserError::InvalidFieldEncoding(line, field))
        })
        .and_then(|s| {
            s.parse()
                .map_err(|_| ParserError::InvalidFieldFormat(line, field))
        })
}

fn parse_str_opt<'a>(
    it: &mut impl Iterator<Item = &'a [u8]>,
    line: usize,
    field: &'static str,
) -> Result<Option<String>, ParserError> {
    it.next()
        .map(|b| {
            std::str::from_utf8(b)
                .map(String::from)
                .map_err(|_| ParserError::InvalidFieldEncoding(line, field))
        })
        .transpose()
}

// Line parser helper for converting a byte slice to a string
fn str_from_utf8((line, s): (usize, &[u8]), field: &'static str) -> Result<String, ParserError> {
    std::str::from_utf8(s)
        .map(String::from)
        .map_err(|_| ParserError::InvalidFieldEncoding(line, field))
}

// Parsing helper iterators below
struct LineParser<'iter, 'item, I: Iterator<Item = (usize, &'item [u8])>> {
    it: &'iter mut std::iter::Peekable<I>,
    current: u8,
    expected_next: &'static [u8],
}

struct LineParserOnce<
    'iter,
    'item,
    I: Iterator<Item = (usize, &'item [u8])>,
    F: Fn(usize) -> ParserError,
> {
    it: LineParser<'iter, 'item, I>,
    error_fn: F,
}

fn line_parser<'iter, 'item, I: Iterator<Item = (usize, &'item [u8])>>(
    it: &'iter mut std::iter::Peekable<I>,
    current: u8,
    expected_next: &'static [u8],
) -> LineParser<'iter, 'item, I> {
    LineParser {
        it,
        current,
        expected_next,
    }
}

fn line_parser_once<
    'iter,
    'item,
    T,
    I: Iterator<Item = (usize, &'item [u8])>,
    F: Fn(usize) -> ParserError,
    G: Fn((usize, &[u8])) -> Result<T, ParserError>,
>(
    it: &'iter mut std::iter::Peekable<I>,
    current: u8,
    expected_next: &'static [u8],
    error_fn: F,
    func: G,
) -> Result<Option<T>, ParserError> {
    LineParserOnce {
        it: line_parser(it, current, expected_next),
        error_fn,
    }
    .next()?
    .map(func)
    .transpose()
}

impl<'iter, 'item, I: Iterator<Item = (usize, &'item [u8])>> FallibleIterator
    for LineParser<'iter, 'item, I>
{
    type Item = (usize, &'item [u8]);
    type Error = ParserError;

    fn next(&mut self) -> Result<Option<Self::Item>, Self::Error> {
        while let Some((n, line)) = self.it.peek() {
            if line.is_empty() {
                self.it.next();
                continue;
            }
            let equals = line.iter().position(|b| *b == b'=');
            let key = match equals {
                None => {
                    return Err(ParserError::InvalidLineFormat(
                        *n,
                        "Line not in key=value format",
                    ))
                }
                Some(i) if i == 1 => line[0],
                _ => {
                    return Err(ParserError::InvalidLineFormat(
                        *n,
                        "Line key not 1 character",
                    ))
                }
            };

            return if key == self.current {
                Ok(self.it.next().map(|(n, line)| (n, &line[2..])))
            } else if self.expected_next.contains(&key) {
                Ok(None)
            } else {
                Err(ParserError::UnexpectedLine(*n, key))
            };
        }
        Ok(None)
    }
}

impl<'iter, 'item, I: Iterator<Item = (usize, &'item [u8])>, F: Fn(usize) -> ParserError>
    FallibleIterator for LineParserOnce<'iter, 'item, I, F>
{
    type Item = (usize, &'item [u8]);
    type Error = ParserError;

    fn next(&mut self) -> Result<Option<Self::Item>, Self::Error> {
        let item = self.it.next()?;
        if let Some((line, _)) = self.it.next()? {
            return Err((self.error_fn)(line));
        }

        Ok(item)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_sdp() {
        let sdp = b"v=0\r
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
        let parsed = Session::parse(&sdp[..]).unwrap();
        let expected = Session {
            origin: Origin {
                username: Some("jdoe".into()),
                sess_id: "2890844526".into(),
                sess_version: 2890842807,
                nettype: "IN".into(),
                addrtype: "IP4".into(),
                unicast_address: "10.47.16.5".into(),
            },
            session_name: "SDP Seminar".into(),
            session_description: Some("A Seminar on the session description protocol".into()),
            uri: Some("http://www.example.com/seminars/sdp.pdf".into()),
            emails: vec!["j.doe@example.com (Jane Doe)".into()],
            phones: vec!["+1 617 555-6011".into()],
            connection: Some(Connection {
                nettype: "IN".into(),
                addrtype: "IP4".into(),
                connection_address: "224.2.17.12/127".into(),
            }),
            bandwidths: vec![Bandwidth {
                bwtype: "AS".into(),
                bandwidth: 128,
            }],
            times: vec![Time {
                start_time: 2873397496,
                stop_time: 2873404696,
                repeats: vec![Repeat {
                    repeat_interval: 604800,
                    active_duration: 3600,
                    offsets: vec![0, 90000],
                }],
            }],
            time_zones: vec![
                TimeZone {
                    adjustment_time: 2882844526,
                    offset: -3600,
                },
                TimeZone {
                    adjustment_time: 2898848070,
                    offset: 0,
                },
            ],
            key: Some(Key {
                method: "clear".into(),
                encryption_key: Some("1234".into()),
            }),
            attributes: vec![Attribute {
                attribute: "recvonly".into(),
                value: None,
            }],
            medias: vec![
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
                    attributes: vec![],
                },
                Media {
                    media: "video".into(),
                    port: 51372,
                    num_ports: Some(2),
                    proto: "RTP/AVP".into(),
                    fmt: "99".into(),
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
                            attribute: "fingerprint".into(),
                            value: Some("sha-256 3A:96:6D:57:B2:C2:C7:61:A0:46:3E:1C:97:39:D3:F7:0A:88:A0:B1:EC:03:FB:10:A5:5D:3A:37:AB:DD:02:AA".into()),
                        }
                    ],
                },
            ],
        };

        assert_eq!(parsed, expected);
    }

    #[test]
    fn parse_only_key() {
        Session::parse(b"v\n").unwrap_err();
    }

    #[test]
    fn unexpected_line_err() {
        match Session::parse(b"v=0\r\n*=asdf\r\n") {
            Err(ParserError::UnexpectedLine(line, c)) => {
                assert_eq!(line, 2);
                assert_eq!(c, b'*');
            }
            o => panic!("bad result: {:#?}", o),
        }
    }

    #[test]
    fn parse_sdp_real_camera() {
        let sdp = b"v=0\r
o=VSTC 3828747520 3828747520 IN IP4 192.168.1.165\r
s=streamed by the VSTARCAM RTSP server\r
e=NONE\r
c=IN IP4 0.0.0.0\r
t=0 0\r
m=video 0 RTP/AVP 96\r
b=AS:1024\r
a=control:track0\r
a=rtpmap:96 H264/90000\r
a=fmtp:96 packetization-mode=1;profile-level-id=4d001f;sprop-parameter-sets=Z00AH52oFAFum4CAgKAAAAMAIAAAAwHwgA==,aO48gA==\r
m=audio 0 RTP/AVP 8	 \r
b=AS:64\r
a=control:track1\r
a=rtpmap:8 PCMA/8000/1\r

";
        let _parsed = Session::parse(&sdp[..]).unwrap();
    }

    #[test]
    fn parse_overflowing_time() {
        assert_eq!(
            Session::parse(b"v=0\no=  0  =\x00 \ns=q\nt=0 5\nz=00 666666000079866660m "),
            Err(ParserError::InvalidFieldFormat(5, "TimeZone offset"))
        );
    }
}
