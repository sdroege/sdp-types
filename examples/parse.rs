use std::io::prelude::*;
use std::{env, fs, process};

fn main() -> process::ExitCode {
    let mut args = env::args();
    let _ = args.next().unwrap();
    let Some(file) = args.next() else {
        eprintln!("Usage: parse [file.sdp]");
        return process::ExitCode::FAILURE;
    };

    let mut file = match fs::File::open(file) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Failed to open file: {err}");
            return process::ExitCode::FAILURE;
        }
    };

    let mut content = Vec::new();
    match file.read_to_end(&mut content) {
        Ok(_) => {}
        Err(err) => {
            eprintln!("Failed to read file: {err}");
            return process::ExitCode::FAILURE;
        }
    };

    let sdp = match sdp_types::Session::parse(&content) {
        Ok(sdp) => sdp,
        Err(err) => {
            eprintln!("Failed to parse SDP: {err}");
            return process::ExitCode::FAILURE;
        }
    };

    println!("{sdp:#?}");

    process::ExitCode::SUCCESS
}
