// Copyright (C) 2019 Sebastian Dröge <sebastian@centricular.com>
//
// Licensed under the MIT license, see the LICENSE file or <http://opensource.org/licenses/MIT>

use super::*;

fn format_time<W: std::io::Write>(w: &mut W, t: u64) -> Result<(), std::io::Error> {
    if t == 0 {
        write!(w, "{t}")?;
    } else if t % 86_400 == 0 {
        write!(w, "{}d", t / 86_400)?;
    } else if t % 3_600 == 0 {
        write!(w, "{}h", t / 3_600)?;
    } else if t % 60 == 0 {
        write!(w, "{}m", t / 60)?;
    } else {
        write!(w, "{t}")?;
    }

    Ok(())
}

impl Origin {
    fn write<W: std::io::Write>(&self, w: &mut W) -> Result<(), std::io::Error> {
        writeln!(
            w,
            "o={username} {sess_id} {sess_version} {nettype} {addrtype} {unicast_address}\r",
            username = if let Some(ref username) = self.username {
                username
            } else {
                "-"
            },
            sess_id = self.sess_id,
            sess_version = self.sess_version,
            nettype = self.nettype,
            addrtype = self.addrtype,
            unicast_address = self.unicast_address,
        )
    }
}

impl Connection {
    fn write<W: std::io::Write>(&self, w: &mut W) -> Result<(), std::io::Error> {
        writeln!(
            w,
            "c={nettype} {addrtype} {connection_address}\r",
            nettype = self.nettype,
            addrtype = self.addrtype,
            connection_address = self.connection_address
        )
    }
}

impl Bandwidth {
    fn write<W: std::io::Write>(&self, w: &mut W) -> Result<(), std::io::Error> {
        writeln!(
            w,
            "b={bwtype}:{bw}\r",
            bwtype = self.bwtype,
            bw = self.bandwidth
        )
    }
}

impl Time {
    fn write<W: std::io::Write>(&self, w: &mut W) -> Result<(), std::io::Error> {
        writeln!(
            w,
            "t={start_time} {stop_time}\r",
            start_time = self.start_time,
            stop_time = self.stop_time,
        )
    }
}

impl Repeat {
    fn write<W: std::io::Write>(&self, w: &mut W) -> Result<(), std::io::Error> {
        write!(w, "r=")?;
        format_time(w, self.repeat_interval)?;
        write!(w, " ")?;
        format_time(w, self.active_duration)?;

        for offset in &self.offsets {
            write!(w, " ")?;
            format_time(w, *offset)?;
        }

        writeln!(w, "\r")
    }
}

impl TimeZone {
    fn write<W: std::io::Write>(w: &mut W, zones: &[TimeZone]) -> Result<(), std::io::Error> {
        write!(w, "z=")?;

        let mut first = true;
        for zone in zones {
            if !first {
                write!(w, " ")?;
            }

            write!(w, "{} ", zone.adjustment_time)?;
            if zone.offset < 0 {
                write!(w, "-")?;
                format_time(w, (-zone.offset) as u64)?;
            } else {
                format_time(w, zone.offset as u64)?;
            }
            first = false;
        }
        writeln!(w, "\r")
    }
}

impl Key {
    fn write<W: std::io::Write>(&self, w: &mut W) -> Result<(), std::io::Error> {
        if let Some(ref encryption_key) = self.encryption_key {
            writeln!(
                w,
                "k={method}:{encryption_key}\r",
                method = self.method,
                encryption_key = encryption_key
            )
        } else {
            writeln!(w, "k={method}\r", method = self.method)
        }
    }
}

impl Attribute {
    fn write<W: std::io::Write>(&self, w: &mut W) -> Result<(), std::io::Error> {
        if let Some(ref value) = self.value {
            writeln!(
                w,
                "a={name}:{value}\r",
                name = self.attribute,
                value = value
            )
        } else {
            writeln!(w, "a={name}\r", name = self.attribute)
        }
    }
}

impl Media {
    fn write<W: std::io::Write>(&self, w: &mut W) -> Result<(), std::io::Error> {
        write!(w, "m={media} {port}", media = self.media, port = self.port)?;

        if let Some(ref num_ports) = self.num_ports {
            write!(w, "/{num_ports}")?;
        }
        writeln!(w, " {} {}\r", self.proto, self.fmt)?;

        if let Some(ref media_title) = self.media_title {
            writeln!(w, "i={media_title}\r")?;
        }

        for connection in &self.connections {
            connection.write(w)?;
        }

        for bandwidth in &self.bandwidths {
            bandwidth.write(w)?;
        }

        if let Some(ref key) = self.key {
            key.write(w)?;
        }

        for attribute in &self.attributes {
            attribute.write(w)?;
        }

        Ok(())
    }
}

impl Session {
    /// Write the SDP session description to a `std::io::Write`.
    pub fn write<W: std::io::Write>(&self, w: &mut W) -> Result<(), std::io::Error> {
        writeln!(w, "v=0\r")?;
        self.origin.write(w)?;
        writeln!(w, "s={session_name}\r", session_name = self.session_name)?;

        if let Some(ref session_description) = self.session_description {
            writeln!(w, "i={session_description}\r")?;
        }

        if let Some(ref uri) = self.uri {
            writeln!(w, "u={uri}\r")?;
        }

        for email in &self.emails {
            writeln!(w, "e={email}\r")?;
        }

        for phone in &self.phones {
            writeln!(w, "p={phone}\r")?;
        }

        if let Some(ref connection) = self.connection {
            connection.write(w)?;
        }

        for bandwidth in &self.bandwidths {
            bandwidth.write(w)?;
        }

        if !self.times.is_empty() {
            for time in &self.times {
                time.write(w)?;

                for repeat in &time.repeats {
                    repeat.write(w)?;
                }
            }
        } else {
            writeln!(w, "t=0 0\r")?;
        }

        if !self.time_zones.is_empty() {
            TimeZone::write(w, &self.time_zones)?;
        }

        if let Some(ref key) = self.key {
            key.write(w)?;
        }

        for attribute in &self.attributes {
            attribute.write(w)?;
        }

        for media in &self.medias {
            media.write(w)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn write_sdp() {
        let sdp = Session {
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
                    attributes: vec![Attribute {
                        attribute: "rtpmap".into(),
                        value: Some("99 h263-1998/90000".into()),
                    }],
                },
            ],
        };

        let mut written = vec![];
        sdp.write(&mut written).unwrap();

        let expected = "v=0\r
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
";

        assert_eq!(String::from_utf8_lossy(&written), expected);
    }
}
