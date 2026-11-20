#![cfg(test)]

use super::*;
use tokio::io::{AsyncBufReadExt, AsyncReadExt, AsyncWriteExt, BufReader};
use tokio::net::{UnixListener, UnixStream};

// Each process retains a separate witness socket until it exits. Unlike kill(0),
// EOF proves termination even when an orphan is still waiting to be reaped.
const WRAPPER: &str = r#"
const net = require('node:net');
const cp = require('node:child_process');
const [socket, mode] = process.argv.slice(1);
const witness = net.connect(socket, () => {
    witness.write(`${process.pid}\n`);
    cp.spawn(process.execPath, ['-e', `
        const net = require('node:net');
        const witness = net.connect(process.argv[1], () => {
            witness.write(process.pid + '\\n');
        });
        witness.on('data', () => process.exit(0));
        setInterval(() => {}, 1000);
    `, socket], {stdio: 'ignore'});
});
if (mode === 'stall') {
    setInterval(() => {}, 1000);
} else if (mode === 'tcp-error') {
    // Let both witnesses connect before closing stdout without a port announcement.
    witness.on('data', () => require('node:fs').closeSync(1));
    setInterval(() => {}, 1000);
} else {
    let input = Buffer.alloc(0);
    process.stdin.on('data', chunk => {
        input = Buffer.concat([input, chunk]);
        while (true) {
            const end = input.indexOf('\r\n\r\n');
            if (end < 0) break;
            const size = Number(/Content-Length: (\d+)/i.exec(input.subarray(0, end))[1]);
            if (input.length < end + 4 + size) break;
            const request = JSON.parse(input.subarray(end + 4, end + 4 + size));
            input = input.subarray(end + 4 + size);
            const result = request.method === 'connect'
                ? {ok: true, protocolVersion: 4, version: 'test'} : {};
            const body = JSON.stringify({jsonrpc: '2.0', id: request.id, result});
            process.stdout.write(`Content-Length: ${Buffer.byteLength(body)}\r\n\r\n${body}`);
        }
    });
    process.stdin.on('end', () => {
        witness.write('eof\n');
        if (mode === 'eof-stall') return;
        if (mode === 'clean') {
            // The test releases the descendant too, exercising an empty group.
            witness.once('data', () => process.exit(0));
        } else {
            process.exit(0);
        }
    });
}
"#;

struct Witness {
    stream: UnixStream,
    pid: u32,
    exited: bool,
}

impl Witness {
    async fn accept(listener: &UnixListener) -> Self {
        let (stream, _) = tokio::time::timeout(Duration::from_secs(10), listener.accept())
            .await
            .expect("process did not connect its exit witness")
            .unwrap();
        let mut reader = BufReader::new(stream);
        let mut line = String::new();
        tokio::time::timeout(Duration::from_secs(10), reader.read_line(&mut line))
            .await
            .expect("process did not send its PID")
            .unwrap();
        Self {
            stream: reader.into_inner(),
            pid: line.trim().parse().expect("witness PID"),
            exited: false,
        }
    }

    async fn exited(&mut self) {
        let mut remaining = Vec::new();
        let result = tokio::time::timeout(
            Duration::from_secs(5),
            self.stream.read_to_end(&mut remaining),
        )
        .await;
        assert!(result.is_ok(), "process {} survived SDK teardown", self.pid);
        result.unwrap().unwrap();
        self.exited = true;
    }

    async fn stdin_closed(&mut self) {
        let mut marker = [0; 4];
        tokio::time::timeout(Duration::from_secs(10), self.stream.read_exact(&mut marker))
            .await
            .expect("wrapper did not observe stdin EOF")
            .unwrap();
        assert_eq!(&marker, b"eof\n");
    }
}

impl Drop for Witness {
    fn drop(&mut self) {
        if !self.exited {
            // Only failed tests need fallback cleanup; the live socket pins its owner.
            let _ = std::process::Command::new("/bin/kill")
                .args(["-KILL", &self.pid.to_string()])
                .current_dir(std::env::temp_dir())
                .status();
        }
    }
}

fn options(
    temp: &Path,
    mode: &str,
    mut runtime: RuntimeConnection,
) -> (UnixListener, ClientOptions) {
    let socket = temp.join("witness.sock");
    let listener = UnixListener::bind(&socket).unwrap();
    let process = match &mut runtime {
        RuntimeConnection::SpawnedStdio(process) => process,
        RuntimeConnection::SpawnedTcp(tcp) => &mut tcp.process,
        _ => unreachable!("process lifecycle tests require a spawned runtime"),
    };
    *process = SpawnedProcessOptions::new()
        .with_program(CliProgram::Path(PathBuf::from("node")))
        .with_working_directory(temp)
        .with_args(["-e", WRAPPER, socket.to_str().unwrap(), mode]);
    let options = ClientOptions::new(ClientMode::CodingAgent).with_runtime(runtime);
    (listener, options)
}

#[tokio::test]
async fn cancelled_start_kills_wrapper_and_grandchild() {
    for transport in [
        RuntimeConnection::for_spawned_stdio(Default::default()),
        RuntimeConnection::for_spawned_tcp(Default::default()),
    ] {
        let temp = tempfile::tempdir().unwrap();
        let (listener, options) = options(temp.path(), "stall", transport);
        let mut start = Box::pin(Client::start(options));
        let (mut root, mut descendant) = tokio::select! {
            result = &mut start => panic!("startup must stall: {result:?}"),
            witnesses = async {
                (Witness::accept(&listener).await, Witness::accept(&listener).await)
            } => witnesses,
        };
        assert!(
            tokio::time::timeout(Duration::ZERO, start).await.is_err(),
            "startup should be cancelled before the handshake"
        );
        root.exited().await;
        descendant.exited().await;
    }
}

#[tokio::test]
async fn stop_kills_stubborn_grandchild() {
    teardown("stop").await;
}

#[tokio::test]
async fn force_stop_kills_stubborn_grandchild() {
    teardown("force_stop").await;
}

#[tokio::test]
async fn drop_kills_stubborn_grandchild() {
    teardown("drop").await;
}

async fn teardown(action: &str) {
    let temp = tempfile::tempdir().unwrap();
    let (listener, options) = options(
        temp.path(),
        "serve",
        RuntimeConnection::for_spawned_stdio(Default::default()),
    );
    let (client, (mut root, mut descendant)) = tokio::join!(Client::start(options), async {
        (
            Witness::accept(&listener).await,
            Witness::accept(&listener).await,
        )
    },);
    let client = client.unwrap();
    let pid = i32::try_from(client.pid().unwrap()).unwrap();
    assert_eq!(
        unsafe { libc::getpgid(pid) },
        pid,
        "CLI must lead its own group"
    );
    assert_ne!(
        pid,
        unsafe { libc::getpgrp() },
        "never contain the host's group"
    );
    let stopped = match action {
        "stop" => client.stop().await,
        "force_stop" => {
            client.force_stop();
            Ok(())
        }
        "drop" => {
            drop(client);
            Ok(())
        }
        _ => unreachable!(),
    };
    root.exited().await;
    descendant.exited().await;
    stopped.expect("graceful leader exit must not cause spurious StopErrors");
}

#[tokio::test]
async fn tcp_startup_error_kills_wrapper_and_grandchild() {
    let temp = tempfile::tempdir().unwrap();
    let (listener, options) = options(
        temp.path(),
        "tcp-error",
        RuntimeConnection::for_spawned_tcp(Default::default()),
    );
    let mut start = Box::pin(Client::start(options));
    let (mut root, mut descendant) = tokio::select! {
        result = &mut start => panic!("must wait for both witnesses: {result:?}"),
        witnesses = async {
            (Witness::accept(&listener).await, Witness::accept(&listener).await)
        } => witnesses,
    };
    root.stream.write_all(b"close stdout").await.unwrap();
    let result = tokio::time::timeout(Duration::from_secs(10), start)
        .await
        .unwrap();
    assert!(
        result.is_err(),
        "missing port announcement must fail startup"
    );
    root.exited().await;
    descendant.exited().await;
}

#[tokio::test]
async fn stop_after_clean_group_exit_succeeds() {
    let temp = tempfile::tempdir().unwrap();
    let (listener, options) = options(
        temp.path(),
        "clean",
        RuntimeConnection::for_spawned_stdio(Default::default()),
    );
    let (client, (mut root, mut descendant)) = tokio::join!(Client::start(options), async {
        (
            Witness::accept(&listener).await,
            Witness::accept(&listener).await,
        )
    });
    let client = client.unwrap();
    let (stopped, ()) = tokio::join!(client.stop(), async {
        root.stdin_closed().await;
        descendant.stream.write_all(b"exit").await.unwrap();
        descendant.exited().await;
        root.stream.write_all(b"exit").await.unwrap();
        root.exited().await;
    });
    stopped.expect("an already exited process group is not a StopErrors failure");
    client
        .stop()
        .await
        .expect("repeated stop must remain harmless");
    client.force_stop();
}

#[tokio::test]
async fn cancelled_stop_and_concurrent_force_stop_retire_group_before_reap() {
    for cancel in [false, true] {
        let temp = tempfile::tempdir().unwrap();
        let (listener, options) = options(
            temp.path(),
            "eof-stall",
            RuntimeConnection::for_spawned_stdio(Default::default()),
        );
        let (client, (mut root, mut descendant)) = tokio::join!(Client::start(options), async {
            (
                Witness::accept(&listener).await,
                Witness::accept(&listener).await,
            )
        });
        let client = client.unwrap();
        let mut stop = Box::pin(client.stop());
        tokio::select! {
            result = &mut stop => panic!("wrapper must stall after stdin EOF: {result:?}"),
            () = root.stdin_closed() => {}
        }
        if cancel {
            assert!(tokio::time::timeout(Duration::ZERO, stop).await.is_err());
        } else {
            client.force_stop();
            stop.await
                .expect("force_stop must interrupt the graceful exit wait");
        }
        root.exited().await;
        descendant.exited().await;
    }
}
