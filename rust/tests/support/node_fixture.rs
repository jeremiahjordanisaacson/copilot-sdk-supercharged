#![cfg(test)]

use std::path::PathBuf;

pub fn shutdown_script() -> PathBuf {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../test/harness/stdio-shutdown-runtime.cjs")
        .canonicalize()
        .expect("canonical shutdown fixture path");
    path_for_node(path)
}

#[cfg(windows)]
pub fn path_for_node(path: PathBuf) -> PathBuf {
    use std::ffi::OsString;
    use std::os::windows::ffi::{OsStrExt, OsStringExt};

    // Match ffi::path_for_child_process: Node's CommonJS entrypoint loader
    // rejects Rust's canonical verbatim paths, including verbatim UNC paths.
    const VERBATIM_PREFIX: &[u16] = &[b'\\' as u16, b'\\' as u16, b'?' as u16, b'\\' as u16];
    const UNC_PREFIX: &[u16] = &[b'U' as u16, b'N' as u16, b'C' as u16, b'\\' as u16];

    let encoded: Vec<u16> = path.as_os_str().encode_wide().collect();
    let Some(stripped) = encoded.strip_prefix(VERBATIM_PREFIX) else {
        return path;
    };
    let normalized = if let Some(unc_path) = stripped.strip_prefix(UNC_PREFIX) {
        let mut result = vec![b'\\' as u16, b'\\' as u16];
        result.extend_from_slice(unc_path);
        result
    } else {
        stripped.to_vec()
    };
    PathBuf::from(OsString::from_wide(&normalized))
}

#[cfg(not(windows))]
fn path_for_node(path: PathBuf) -> PathBuf {
    path
}
