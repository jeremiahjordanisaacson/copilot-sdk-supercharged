/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#![cfg(test)]

use std::sync::atomic::{AtomicUsize, Ordering};

use super::*;

const ADMISSION_REJECTION: &str = "controlled auth owner declined native admission";
const CHILD_CASE: &str = "COPILOT_SDK_AUTH_OWNER_TRANSPORT_TEST";

#[test]
fn default_transport_reaches_native_auth_owner() {
    if let Ok(case) = std::env::var(CHILD_CASE) {
        tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .unwrap()
            .block_on(check_default_transport(&case));
        return;
    }

    for case in ["process-inprocess", "stdio", "unset"] {
        let mut command = std::process::Command::new(std::env::current_exe().unwrap());
        command
            .args([
                "--exact",
                "auth_owner_transport_tests::default_transport_reaches_native_auth_owner",
                "--nocapture",
            ])
            .current_dir(std::env::current_dir().unwrap())
            .env(CHILD_CASE, case)
            .env_remove(DEFAULT_CONNECTION_ENV_VAR);
        match case {
            "stdio" => {
                command.env(DEFAULT_CONNECTION_ENV_VAR, "stdio");
            }
            "process-inprocess" => {
                command.env(DEFAULT_CONNECTION_ENV_VAR, "inprocess");
            }
            _ => {}
        }
        #[cfg(windows)]
        {
            use std::os::windows::process::CommandExt;
            command.creation_flags(0x0800_0000);
        }
        let output = command.output().expect("run isolated SDK test");
        assert!(
            output.status.success(),
            "{case}: {}\nstdout:\n{}\nstderr:\n{}",
            output.status,
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr),
        );
    }
}

async fn check_default_transport(case: &str) {
    let home = tempfile::tempdir().unwrap();
    let admissions = Arc::new(AtomicUsize::new(0));
    let recorded = Arc::clone(&admissions);
    let runtime =
        RuntimeConnection::for_default(OwnedRuntimeOptions::new().with_base_directory(home.path()))
            .unwrap();
    if case == "stdio" {
        assert!(matches!(runtime, RuntimeConnection::SpawnedStdio(_)));
        return;
    }
    let RuntimeConnection::InProcess(mut inprocess) = runtime else {
        panic!("inprocess default")
    };
    inprocess.auth_owner = Some(Arc::new(move |server, connection| {
        assert_ne!(server, 0, "native startup must supply a server");
        assert_ne!(
            connection, 0,
            "native startup must supply an auth reservation"
        );
        recorded.fetch_add(1, Ordering::SeqCst);
        Err(Error::with_message(
            ErrorKind::InvalidConfig,
            ADMISSION_REJECTION,
        ))
    }));
    let native_admission = case != "stdio";
    if native_admission
        && let Some(library) = std::env::var_os("COPILOT_AUTH_OWNER_RUNTIME_LIBRARY")
    {
        let library = PathBuf::from(library);
        assert!(library.is_absolute(), "the runtime image must be explicit");
        assert!(library.is_file(), "runtime image: {}", library.display());
        inprocess.runtime_library_path = Some(library);
    }

    let error = Client::start(
        ClientOptions::new(crate::ClientMode::CodingAgent)
            .with_runtime(RuntimeConnection::InProcess(inprocess)),
    )
    .await
    .unwrap_err();
    assert!(
        matches!(error.kind(), ErrorKind::InvalidConfig),
        "{error:?}"
    );
    assert_eq!(error.to_string(), ADMISSION_REJECTION, "{case}");
    assert_eq!(
        admissions.load(Ordering::SeqCst),
        usize::from(native_admission)
    );
}
