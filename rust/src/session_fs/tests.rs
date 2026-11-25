/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#![cfg(test)]

use super::*;

#[test]
fn fs_error_preserves_io_kind_and_message_on_wire() {
    for (io_err, kind, code) in [
        (
            std::io::Error::new(std::io::ErrorKind::NotFound, "missing.txt"),
            FsErrorKind::NotFound("missing.txt".into()),
            SessionFsErrorCode::ENOENT,
        ),
        (
            std::io::Error::other("disk full"),
            FsErrorKind::Other,
            SessionFsErrorCode::UNKNOWN,
        ),
    ] {
        let message = io_err.to_string();
        let error = FsError::from(io_err);
        assert_eq!(error.kind(), &kind);
        let wire = error.into_wire();
        assert_eq!(wire.code, code);
        assert!(wire.message.unwrap().contains(&message));
    }
}
