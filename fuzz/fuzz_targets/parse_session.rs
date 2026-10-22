#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    let session = sdp_types::Session::parse(data);

    if let Ok(s) = session {
        // Serialize the parsed session and parse the result again: the written
        // output must be valid input again.
        let mut written = vec![];
        s.write(&mut written).unwrap();
        sdp_types::Session::parse(&written).unwrap();
    };
});
