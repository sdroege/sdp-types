#![no_main]
use libfuzzer_sys::fuzz_target;
use sdp_types::Session;

// Drain the typed attribute iterator of the given session or media
// description. The results are irrelevant, this only exists to catch panics
// and other crashes in the typed attribute parsers.
macro_rules! exercise_typed_attributes {
    ($owner:expr, $($t:ty),* $(,)?) => {{
        $(
            let mut iter = $owner.attributes_typed::<$t>();
            loop {
                match iter.next() {
                    Some(Ok(_)) => {}
                    Some(Err(_)) | None => break,
                }
            }
        )*
    }};
}

// Invoke `$call!($owner, <all typed attribute types>)`.
macro_rules! for_all_typed_attributes {
    ($owner:expr, $call:ident) => {
        $call!(
            $owner,
            sdp_types::RtpMap,
            sdp_types::RtcpFb,
            sdp_types::Fmtp,
            sdp_types::ExtMap,
            sdp_types::Rtcp,
            sdp_types::Crypto,
            sdp_types::Candidate,
            sdp_types::Fingerprint,
            sdp_types::Group,
            sdp_types::Setup,
            sdp_types::Ssrc,
            sdp_types::SsrcGroup,
            sdp_types::ReferenceClock,
            sdp_types::MediaClockSource,
        );
    };
}

fuzz_target!(|data: &[u8]| {
    let session = match Session::parse(data) {
        Ok(session) => session,
        Err(_) => return,
    };

    // serialize the parsed session
    let mut written = vec![];
    session.write(&mut written).unwrap();

    for_all_typed_attributes!(session, exercise_typed_attributes);
    for media in &session.medias {
        for_all_typed_attributes!(media, exercise_typed_attributes);
    }
});
