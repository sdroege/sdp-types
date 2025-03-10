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
    #[deprecated(note = "This is no longer considered an error.")]
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
    #[deprecated(note = "This is no longer considered an error.")]
    NoOrigin,
    /// A second session name line was found at the given line.
    MultipleSessionNames(usize),
    /// The SDP did not contain a session name line.
    #[deprecated(note = "This is no longer considered an error.")]
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
            #[allow(deprecated)]
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
                String::from_utf8_lossy(s)
            ),
            ParserError::MultipleVersions(line) => write!(f, "Multiple versions in line {}", line),
            ParserError::NoVersion => write!(f, "No version line"),
            ParserError::MultipleOrigins(line) => write!(f, "Multiple origins in line {}", line),
            #[allow(deprecated)]
            ParserError::NoOrigin => write!(f, "No origin line"),
            ParserError::MultipleSessionNames(line) => {
                write!(f, "Multiple session-names in line {}", line)
            }
            #[allow(deprecated)]
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
    fn parse(line: &Line) -> Result<Origin, ParserError> {
        // <username> <sess-id> <sess-version> <nettype> <addrtype> <unicast-address>
        let mut origin = line.value.splitn_str(6, b" ");
        let username = parse_str(&mut origin, line.n, "Origin username")?;
        let sess_id = parse_str(&mut origin, line.n, "Origin sess-id")?;
        let sess_version = parse_str_u64(&mut origin, line.n, "Origin sess-version")?;
        let nettype = parse_str(&mut origin, line.n, "Origin nettype")?;
        let addrtype = parse_str(&mut origin, line.n, "Origin addrtype")?;
        let unicast_address = parse_str(&mut origin, line.n, "Origin unicast-address")?;

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
    fn parse(line: &Line) -> Result<Connection, ParserError> {
        // <nettype> <addrtype> <connection-address>
        let mut connection = line.value.splitn_str(3, b" ");
        let nettype = parse_str(&mut connection, line.n, "Connection nettype")?;
        let addrtype = parse_str(&mut connection, line.n, "Connection addrtype")?;
        let connection_address =
            parse_str(&mut connection, line.n, "Connection connection-address")?;

        Ok(Connection {
            nettype,
            addrtype,
            connection_address,
        })
    }
}

impl Bandwidth {
    fn parse(line: &Line) -> Result<Bandwidth, ParserError> {
        // <bwtype>:<bandwidth>
        let mut bandwidth = line.value.split_str(b":");
        let bwtype = parse_str(&mut bandwidth, line.n, "Bandwidth bwtype")?;
        let bw = parse_str_u64(&mut bandwidth, line.n, "Bandwidth bandwidth")?;
        if bandwidth.next().is_some() {
            return Err(ParserError::FieldTrailingData(line.n, "Bandwidth"));
        }

        Ok(Bandwidth {
            bwtype,
            bandwidth: bw,
        })
    }
}

impl Time {
    fn parse(line: &Line) -> Result<Time, ParserError> {
        // <start-time> <stop-time>
        let mut time = line.value.split_str(b" ").filter(|part| !part.is_empty());
        let start_time = parse_str_u64(&mut time, line.n, "Time start-time")?;
        let stop_time = parse_str_u64(&mut time, line.n, "Time stop-time")?;
        if time.next().is_some() {
            return Err(ParserError::FieldTrailingData(line.n, "Time"));
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
    fn parse(line: &Line) -> Result<Repeat, ParserError> {
        // <repeat interval> <active duration> <offsets from start-time>
        let mut repeat = line.value.split_str(b" ").filter(|part| !part.is_empty());
        let repeat_interval = repeat
            .next()
            .ok_or(ParserError::MissingField(line.n, "Repeat repeat-interval"))
            .and_then(|s| parse_typed_time(s, line.n, "Repeat repeat-interval"))?;
        let active_duration = repeat
            .next()
            .ok_or(ParserError::MissingField(line.n, "Repeat active-duration"))
            .and_then(|s| parse_typed_time(s, line.n, "Repeat active-duration"))?;

        let offsets = repeat
            .map(|s| parse_typed_time(s, line.n, "Repeat active-duration"))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Repeat {
            repeat_interval,
            active_duration,
            offsets,
        })
    }
}

impl TimeZone {
    fn parse(line: &Line) -> Result<Vec<TimeZone>, ParserError> {
        // <adjustment time> <offset> <adjustment time> <offset> ....
        let mut zones = line.value.split_str(b" ").filter(|part| !part.is_empty());

        let mut ret = Vec::new();
        loop {
            let adjustment_time = parse_str_u64(&mut zones, line.n, "TimeZone adjustment-time");

            let adjustment_time = match adjustment_time {
                Ok(adjustment_time) => adjustment_time,
                Err(ParserError::MissingField(..)) => break,
                Err(err) => return Err(err),
            };

            let offset = zones
                .next()
                .ok_or(ParserError::MissingField(line.n, "TimeZone offset"))
                .and_then(|s| {
                    use std::convert::TryInto;

                    let (sign, s) = if s.first() == Some(&b'-') {
                        (true, &s[1..])
                    } else {
                        (false, s)
                    };

                    parse_typed_time(s, line.n, "TimeZone offset")
                        .and_then(|t| {
                            t.try_into().map_err(|_| {
                                ParserError::InvalidFieldFormat(line.n, "TimeZone offset")
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
    fn parse(line: &Line) -> Result<Attribute, ParserError> {
        // <attribute>:<value>
        // <attribute>
        let mut attribute = line.value.splitn_str(2, b":");
        let name = parse_str(&mut attribute, line.n, "Attribute name")?;
        let value = parse_str_opt(&mut attribute, line.n, "Attribute value")?;

        Ok(Attribute {
            attribute: name,
            value,
        })
    }
}

impl Key {
    fn parse(line: &Line) -> Result<Key, ParserError> {
        // <method>:<encryption key>
        // <method>
        let mut key = line.value.splitn_str(2, b":");
        let method = parse_str(&mut key, line.n, "Key method")?;
        let encryption_key = parse_str_opt(&mut key, line.n, "Key encryption-key")?;

        Ok(Key {
            method,
            encryption_key,
        })
    }
}

impl Media {
    fn parse_m_line(line: &Line) -> Result<Media, ParserError> {
        // <media> <port> <proto> <fmt> ...
        let mut media = line.value.splitn_str(4, b" ");
        let name = parse_str(&mut media, line.n, "Media name")?;

        let (port, num_ports) = media
            .next()
            .ok_or(ParserError::MissingField(line.n, "Media port"))
            .and_then(|s| str_from_utf8(line.n, s, "Media Port"))
            .and_then(|port| {
                let mut split = port.splitn(2, '/');
                let port = split
                    .next()
                    .ok_or(ParserError::MissingField(line.n, "Media port"))
                    .and_then(|port| {
                        port.parse()
                            .map_err(|_| ParserError::InvalidFieldFormat(line.n, "Media port"))
                    })?;

                let num_ports = split
                    .next()
                    .ok_or(ParserError::MissingField(line.n, "Media num-ports"))
                    .and_then(|num_ports| {
                        num_ports
                            .parse()
                            .map_err(|_| ParserError::InvalidFieldFormat(line.n, "Media num-ports"))
                    });

                match num_ports {
                    Ok(num_ports) => Ok((port, Some(num_ports))),
                    Err(ParserError::MissingField(..)) => Ok((port, None)),
                    Err(err) => Err(err),
                }
            })?;

        let proto = parse_str(&mut media, line.n, "Media proto")?;
        let fmt = parse_str(&mut media, line.n, "Media fmt")?;

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

    fn parse<'a, I: FallibleIterator<Item = Line<'a>, Error = ParserError>>(
        lines: &mut fallible_iterator::Peekable<I>,
    ) -> Result<Option<Media>, ParserError> {
        let media = match lines.next()? {
            None => return Ok(None),
            Some(line) => {
                assert_eq!(line.key, b'm');
                Media::parse_m_line(&line)?
            }
        };

        // As with Session::parse, be more permissive about order than RFC 8866.
        let mut media_title = None;
        let mut connections = vec![];
        let mut bandwidths = vec![];
        let mut key = None;
        let mut attributes = vec![];
        while matches!(lines.peek(), Ok(Some(Line { key, .. })) if *key != b'm') {
            let line = lines.next().unwrap().unwrap();

            match line.key {
                // Parse media information line
                // - Can exist not at all or exactly once
                b'i' => parse_rejecting_duplicates(
                    &mut media_title,
                    &line,
                    ParserError::MultipleMediaTitles,
                    |l| str_from_utf8(l.n, l.value, "Media Title"),
                )?,

                // Parse connection lines
                // - Can exist not at all, once or multiple times
                b'c' => connections.push(Connection::parse(&line)?),

                // Parse bandwidth lines:
                // - Can exist not at all, once or multiple times
                b'b' => bandwidths.push(Bandwidth::parse(&line)?),

                // Parse key line
                // - Can exist not at all or exactly once
                b'k' => parse_rejecting_duplicates(
                    &mut key,
                    &line,
                    ParserError::MultipleKeys,
                    Key::parse,
                )?,

                // Parse attribute lines:
                // - Can exist not at all, once or multiple times
                b'a' => attributes.push(Attribute::parse(&line)?),

                _ => (),
            }
        }

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
        let mut lines =
            LineParser(data.lines().enumerate().map(|(i, bytes)| (i + 1, bytes))).peekable();

        // Parses anything allowed by RFC 8866 Section 9 and more:
        // - be more lax about order. As in the RFC, "v=" must come first and
        //   "m=" starts the media descriptions. Other fields can come in
        //   almost any order. "r=" refers to the most recent "t=", even if it's
        //   not the most recent line.
        // - allow "o=", "t=" and "s=" lines to be missing.

        // Check version line, which we expect to come first.
        match lines.next()? {
            Some(Line {
                n,
                key: b'v',
                value,
            }) => {
                if value != b"0" {
                    return Err(ParserError::InvalidVersion(n, value.into()));
                }
            }
            _ => return Err(ParserError::NoVersion),
        }

        let mut origin = None;
        let mut session_name = None;
        let mut session_description = None;
        let mut uri = None;
        let mut emails = vec![];
        let mut phones = vec![];
        let mut connection = None;
        let mut bandwidths = vec![];
        let mut times = vec![];
        let mut time_zones = None;
        let mut attributes = vec![];
        let mut key = None;
        while matches!(lines.peek(), Ok(Some(Line { key, .. })) if *key != b'm') {
            let line = lines.next().unwrap().unwrap();
            match line.key {
                // Parse origin line:
                // - Must only exist once or not at all
                b'o' => parse_rejecting_duplicates(
                    &mut origin,
                    &line,
                    ParserError::MultipleOrigins,
                    Origin::parse,
                )?,

                // Parse session name line:
                // - Must only exist once or not at all
                b's' => parse_rejecting_duplicates(
                    &mut session_name,
                    &line,
                    ParserError::MultipleSessionNames,
                    |l| str_from_utf8(l.n, l.value, "Session Name"),
                )?,

                // Parse session information line:
                // - Must only exist once or not at all
                b'i' => parse_rejecting_duplicates(
                    &mut session_description,
                    &line,
                    ParserError::MultipleSessionDescription,
                    |l| str_from_utf8(l.n, l.value, "Session Description"),
                )?,

                // Parse URI line:
                // - Must only exist once or not at all
                b'u' => {
                    parse_rejecting_duplicates(&mut uri, &line, ParserError::MultipleUris, |l| {
                        str_from_utf8(l.n, l.value, "Uri")
                    })?
                }

                // Parse E-Mail lines:
                // - Can exist not at all, once or multiple times
                b'e' => emails.push(str_from_utf8(line.n, line.value, "E-Mail")?),

                // Parse phone number lines:
                // - Can exist not at all, once or multiple times
                b'p' => phones.push(str_from_utf8(line.n, line.value, "Phone")?),

                // Parse connection line:
                // - Can exist not at all or exactly once per session
                b'c' => parse_rejecting_duplicates(
                    &mut connection,
                    &line,
                    ParserError::MultipleConnections,
                    Connection::parse,
                )?,

                // Parse bandwidth lines:
                // - Can exist not at all, once or multiple times
                b'b' => bandwidths.push(Bandwidth::parse(&line)?),

                // Parse time lines
                // - If followed by "r" lines then these are part of the same time field
                b't' => times.push(Time::parse(&line)?),

                // Parse repeat lines
                // - Can exist not at all, once or multiple times
                b'r' => {
                    if let Some(t) = times.last_mut() {
                        t.repeats.push(Repeat::parse(&line)?);
                    }
                }

                // Parse zones line:
                // - Can exist not at all or exactly once per session
                b'z' => parse_rejecting_duplicates(
                    &mut time_zones,
                    &line,
                    ParserError::MultipleTimeZones,
                    TimeZone::parse,
                )?,

                // Parse key line
                // - Can exist not at all or exactly once
                b'k' => parse_rejecting_duplicates(
                    &mut key,
                    &line,
                    ParserError::MultipleKeys,
                    Key::parse,
                )?,

                // Parse attribute lines:
                // - Can exist not at all, once or multiple times
                b'a' => attributes.push(Attribute::parse(&line)?),

                _ => (),
            }
        }

        let origin = origin.unwrap_or_else(|| Origin {
            username: None,
            sess_id: String::new(),
            sess_version: 0,
            nettype: String::new(),
            addrtype: String::new(),
            unicast_address: String::new(),
        });
        let session_name = session_name.unwrap_or_default();

        let time_zones = time_zones.unwrap_or_default();

        // Parse media lines:
        // - Can exist not at all, once or multiple times
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

fn parse_rejecting_duplicates<
    T,
    E: Fn(usize) -> ParserError,
    P: Fn(&Line) -> Result<T, ParserError>,
>(
    value: &mut Option<T>,
    line: &Line<'_>,
    duplicate_error_fn: E,
    parser: P,
) -> Result<(), ParserError> {
    if value.is_some() {
        return Err(duplicate_error_fn(line.n));
    }
    *value = Some(parser(line)?);
    Ok(())
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
fn str_from_utf8(line: usize, s: &[u8], field: &'static str) -> Result<String, ParserError> {
    std::str::from_utf8(s)
        .map(String::from)
        .map_err(|_| ParserError::InvalidFieldEncoding(line, field))
}

struct Line<'item> {
    /// The 1-based line number.
    n: usize,

    key: u8,

    value: &'item [u8],
}

// Parsing helper iterators below
struct LineParser<'item, I: Iterator<Item = (usize, &'item [u8])>>(I);

impl<'item, I: Iterator<Item = (usize, &'item [u8])>> FallibleIterator for LineParser<'item, I> {
    type Item = Line<'item>;
    type Error = ParserError;

    fn next(&mut self) -> Result<Option<Self::Item>, Self::Error> {
        for (n, line) in &mut self.0 {
            if line.is_empty() {
                continue;
            }
            let equals = line.iter().position(|b| *b == b'=');
            let key = match equals {
                None => {
                    return Err(ParserError::InvalidLineFormat(
                        n,
                        "Line not in key=value format",
                    ))
                }
                Some(1) => line[0],
                _ => {
                    return Err(ParserError::InvalidLineFormat(
                        n,
                        "Line key not 1 character",
                    ))
                }
            };
            return Ok(Some(Line {
                n,
                key,
                value: &line[2..],
            }));
        }
        Ok(None)
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

    /// Parses SDP from a Geovision camera which (incorrectly) omits the "t="
    /// line.
    #[test]
    fn parse_sdp_geovision() {
        let sdp = b"v=0\r
o=- 1001 1 IN IP4 192.168.5.237\r
s=VCP IPC Realtime stream\r
m=video 0 RTP/AVP 105\r
c=IN IP4 192.168.5.237\r
a=control:rtsp://192.168.5.237/media/video1/video\r
a=rtpmap:105 H264/90000\r
a=fmtp:105 profile-level-id=4d4032; packetization-mode=1; sprop-parameter-sets=Z01AMpWgCoAwfiZuAgICgAAB9AAAdTBC,aO48gA==\r
a=recvonly\r
m=application 0 RTP/AVP 107\r
c=IN IP4 192.168.5.237\r
a=control:rtsp://192.168.5.237/media/video1/metadata\r
a=rtpmap:107 vnd.onvif.metadata/90000\r
a=fmtp:107 DecoderTag=h3c-v3 RTCP=0\r
a=recvonly\r
";
        let _parsed = Session::parse(&sdp[..]).unwrap();
    }

    /// Parses SDP from an Anpviz camera which (incorrectly) places an `a=`
    /// between the `c=` and `t=` lines of a session.
    #[test]
    fn parse_sdp_anpviz() {
        let sdp = b"v=0\r
o=- 1109162014219182 1109162014219192 IN IP4 x.y.z.w\r
s=RTSP/RTP stream from anjvision ipcamera\r
e=NONE\r
c=IN IP4 0.0.0.0\r
a=tool:LIVE555 Streaming Media v2011.05.25 CHAM.LI@ANJVISION.COM\r
t=0 0\r
a=range:npt=0-\r
a=control:*\r
m=video 0 RTP/AVP 96\r
a=rtpmap:96 H264/90000\r
a=control:trackID=1\r
a=fmtp:96 profile-level-id=4D401F;packetization-mode=0;sprop-parameter-sets=Z01AH5WgLASabAQ=,aO48gA==;config=00000001674d401f95a02c049a6c040000000168ee3c800000000106f02c0445c6f5000620ebc2f3f7639e48250bfcb561bb2b85dda6fe5f06cc8b887b6a915f5aa3bebfffffffffff7380\r
a=x-dimensions: 704, 576\r
a=x-framerate: 12\r
m=audio 0 RTP/AVP 0\r
a=rtpmap:0 MPEG4-GENERIC/16000/2\r
a=fmtp:0 config=1408\r
a=control:trackID=2\r
a=Media_header:MEDIAINFO=494D4B48010100000400010010710110401F000000FA000000000000000000000000000000000000;\r
a=appversion:1.0\r
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

    #[test]
    fn parse_sdp_without_origin() {
        let sdp = b"v=0\r
s=streamed by the macro-video rtsp server\r
t=0 0\r
a=control:*\r
a=range:npt=0-\r
a=x-qt-text-nam:streamed by the macro-video rtsp server\r
c=IN IP4 0.0.0.0\r
m=video 0 RTP/AVP 96\r
b=AS:500\r
a=rtpmap:96 H264/90000\r
a=fmtp:96 profile-level-id=TeAo;packetization-mode=1;sprop-parameter-sets=J03gKI1oBQBboQAAAwABAAADACgPFCKg,KO4BNJI=\r
a=control:track1\r
";
        let _parsed = Session::parse(&sdp[..]).unwrap();
    }

    #[test]
    fn parse_sdp_data_after_media() {
        let sdp = b"v=0\r
o=- 1691154453 1 IN IP4 192.168.1.100\r
i=Pagos\r
a=type:broadcast\r
s=RandD2\r
m=video 15002 RTP/AVP 97\r
a=range:npt=0-\r
a=rtpmap:97 H264/90000\r
a=fmtp:97 profile-level-id=4D4029; packetization-mode=1; sprop-parameter-sets=Z01AKZZUBQHsgA==,aO44gA==\r
a=framerate:15.000\r
a=control:rtsp://192.168.1.20/camera1.sdp\r
c=IN IP4 0.0.0.0\r
t=0 0\r
";
        let _parsed = Session::parse(&sdp[..]).unwrap();
    }

    #[test]
    fn parse_sdp_without_session_name() {
        let sdp = b"v=0\r
o=- 1109162014219182 1109162014219192 IN IP4 x.y.z.w\r
t=0 0\r
a=control:*\r
a=range:npt=0-\r
a=x-qt-text-nam:streamed by the macro-video rtsp server\r
c=IN IP4 0.0.0.0\r
m=video 0 RTP/AVP 96\r
b=AS:500\r
a=rtpmap:96 H264/90000\r
a=fmtp:96 profile-level-id=TeAo;packetization-mode=1;sprop-parameter-sets=J03gKI1oBQBboQAAAwABAAADACgPFCKg,KO4BNJI=\r
a=control:track1\r
";
        let _parsed = Session::parse(&sdp[..]).unwrap();
    }

    #[test]
    fn parse_sdp_with_trailing_spaces() {
        // Obtained from a camera integrated into Elegoo Saturn 4 Ultra
        // Notice the trailing space in "t" field
        let sdp = b"v=0\r
o=- 4922720 1 IN IP4 10.0.0.108\r
t=0 0 \r
a=control:*\r
m=video 0 RTP/AVP 96\r
a=rtpmap:96 H264/9000\r
a=control:track0\r
";
        let _parsed = Session::parse(&sdp[..]).unwrap();
    }
}
