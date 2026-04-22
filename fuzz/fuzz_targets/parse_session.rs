#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    let session = sdp_types::Session::parse(data);

    if let Ok (s) = session {
        // serialize the parsed session
        let mut written = vec![];
        s.write(&mut written).unwrap();
    };
});
