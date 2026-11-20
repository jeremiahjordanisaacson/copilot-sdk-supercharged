#![cfg(test)]

use std::os::unix::process::CommandExt;
use std::time::Duration;

use super::*;

#[tokio::test]
async fn graceful_exit_observation_keeps_the_leader_unreaped() {
    let temp = tempfile::tempdir().unwrap();
    let mut command = Command::new("/bin/sh");
    command
        .args(["-c", "exit 0"])
        .current_dir(temp.path())
        .kill_on_drop(true);
    let (child, tree) = spawn(&mut command).unwrap();
    let mut process = OwnedProcess::new(child, tree);
    let pid = process.child.id().unwrap();
    tokio::time::timeout(Duration::from_secs(5), async {
        while !process.has_exited().unwrap() {
            tokio::time::sleep(Duration::from_millis(10)).await;
        }
    })
    .await
    .unwrap();
    assert!(
        process.has_exited().unwrap(),
        "WNOWAIT must preserve waitability"
    );
    assert_eq!(process.child.id(), Some(pid));
    process.terminate_tree().unwrap();
    assert!(process.child.wait().await.unwrap().success());
}

#[tokio::test]
async fn absent_group_is_success_without_signalling_the_host() {
    let temp = tempfile::tempdir().unwrap();
    // Keep the leader PID allocated but leave its newly created group empty.
    // That exercises ESRCH without ever signalling a recycled process group.
    let host_group = unsafe { libc::getpgrp() };
    let mut command = Command::new("/bin/sh");
    command
        .args(["-c", "exec sleep 60"])
        .current_dir(temp.path())
        .kill_on_drop(true);
    // SAFETY: setpgid is async-signal-safe and the closure captures only a PID.
    unsafe {
        command.as_std_mut().pre_exec(move || {
            if libc::setpgid(0, host_group) == 0 {
                Ok(())
            } else {
                Err(io::Error::last_os_error())
            }
        });
    }
    let (mut child, tree) = spawn(&mut command).unwrap();
    let pid = i32::try_from(child.id().unwrap()).unwrap();
    assert_ne!(pid, host_group);
    assert_eq!(unsafe { libc::getpgid(pid) }, host_group);
    tree.unwrap()
        .terminate()
        .expect("ESRCH means the group already exited");
    child.kill().await.unwrap();
}

#[cfg(target_os = "macos")]
#[test]
fn failed_group_snapshot_does_not_certify_exit() {
    if std::env::var_os("COPILOT_SDK_TEST_DARWIN_FAULT").is_some() {
        tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .unwrap()
            .block_on(async {
                let temp = tempfile::tempdir().unwrap();
                let mut command = Command::new("/bin/sleep");
                command
                    .arg("60")
                    .env_remove("DYLD_INSERT_LIBRARIES")
                    .current_dir(temp.path())
                    .kill_on_drop(true);
                let (mut child, tree) = spawn(&mut command).unwrap();
                let result = tree.unwrap().terminate();
                child.kill().await.unwrap();
                assert_eq!(
                    result
                        .expect_err("an inspection failure is not proof of exit")
                        .raw_os_error(),
                    Some(libc::ENOMEM)
                );
            });
        return;
    }
    run_interposed("failed_group_snapshot_does_not_certify_exit", "0");
}

#[cfg(target_os = "macos")]
#[test]
fn group_signal_requires_quiescence() {
    if std::env::var_os("COPILOT_SDK_TEST_DARWIN_FAULT").is_none() {
        run_interposed("group_signal_requires_quiescence", "1");
        return;
    }
    signal_fault_case(true).expect("a missed group member must be drained");
}

#[cfg(target_os = "macos")]
#[test]
fn group_exit_wait_timeout_is_not_success() {
    if std::env::var_os("COPILOT_SDK_TEST_DARWIN_FAULT").is_none() {
        run_interposed("group_exit_wait_timeout_is_not_success", "2");
        return;
    }
    assert_eq!(
        signal_fault_case(false).unwrap_err().kind(),
        io::ErrorKind::TimedOut
    );
}

#[cfg(target_os = "macos")]
#[test]
fn group_permission_failure_is_not_exit_or_timeout() {
    if std::env::var_os("COPILOT_SDK_TEST_DARWIN_FAULT").is_none() {
        run_interposed("group_permission_failure_is_not_exit_or_timeout", "3");
        return;
    }
    assert_eq!(
        signal_fault_case(false).unwrap_err().kind(),
        io::ErrorKind::PermissionDenied
    );
}

#[cfg(target_os = "macos")]
#[test]
fn group_observer_failure_still_signals_descendants() {
    if std::env::var_os("COPILOT_SDK_TEST_DARWIN_FAULT").is_none() {
        run_interposed("group_observer_failure_still_signals_descendants", "4");
        return;
    }
    assert_eq!(
        signal_fault_case(true).unwrap_err().raw_os_error(),
        Some(libc::EMFILE)
    );
}

#[cfg(target_os = "macos")]
#[test]
fn group_suppressed_exit_notification_rechecks_identity() {
    if std::env::var_os("COPILOT_SDK_TEST_DARWIN_FAULT").is_none() {
        run_interposed("group_suppressed_exit_notification_rechecks_identity", "5");
        return;
    }
    signal_fault_case(true).expect("a threadless group must not time out");
}

#[cfg(target_os = "macos")]
fn signal_fault_case(expect_exit: bool) -> io::Result<()> {
    tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap()
        .block_on(async {
            use tokio::io::{AsyncBufReadExt, AsyncReadExt, BufReader};
            let temp = tempfile::tempdir().unwrap();
            let mut command = Command::new("/bin/sh");
            command
                .args(["-c", "sleep 60 & printf 'ready\\n'; wait"])
                .env_remove("DYLD_INSERT_LIBRARIES")
                .current_dir(temp.path())
                .stdout(std::process::Stdio::piped())
                .kill_on_drop(true);
            let (child, tree) = spawn(&mut command).unwrap();
            let mut fixture = SignalFixture {
                pgid: child.id().map(|pid| i32::try_from(pid).unwrap()),
                process: OwnedProcess::new(child, tree),
            };
            let mut output = BufReader::new(fixture.process.child.stdout.take().unwrap());
            let mut ready = String::new();
            tokio::time::timeout(Duration::from_secs(5), output.read_line(&mut ready))
                .await
                .unwrap()
                .unwrap();
            assert_eq!(ready, "ready\n");
            let result = fixture.process.terminate_tree();
            let mut remaining = Vec::new();
            let eof = if expect_exit {
                Some(
                    tokio::time::timeout(
                        Duration::from_secs(5),
                        output.read_to_end(&mut remaining),
                    )
                    .await,
                )
            } else {
                None
            };
            fixture.signal_cleanup();
            fixture.process.child.kill().await.unwrap();
            if let Some(eof) = eof {
                eof.expect("a descendant survived group teardown").unwrap();
            }
            result
        })
}

#[cfg(target_os = "macos")]
struct SignalFixture {
    pgid: Option<i32>,
    process: OwnedProcess,
}

#[cfg(target_os = "macos")]
impl SignalFixture {
    fn signal_cleanup(&mut self) {
        if let Some(pgid) = self.pgid.take() {
            // The unreaped root pins this private group even on assertion failure.
            unsafe { libc::kill(-pgid, libc::SIGTERM) };
        }
    }
}

#[cfg(target_os = "macos")]
impl Drop for SignalFixture {
    fn drop(&mut self) {
        self.signal_cleanup();
    }
}

#[cfg(target_os = "macos")]
fn run_interposed(test: &str, mode: &str) {
    let temp = tempfile::tempdir().unwrap();
    let source = temp.path().join("snapshot_fault.c");
    let library = temp.path().join("snapshot_fault.dylib");
    std::fs::write(
        &source,
        r#"
#include <errno.h>
#include <libproc.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/event.h>
#include <sys/proc.h>
#include <sys/proc_info.h>
#include <unistd.h>

static unsigned passes;
static unsigned waits;
static unsigned queues;
static uintptr_t watched[2];
static unsigned watch_count;
static pid_t owned_group;
static pid_t exited_member;
static int exit_queue = -1;

static void remove_watch(uintptr_t pid) {
    for (unsigned i = 0; i < watch_count; ++i) {
        if (watched[i] == pid) {
            watched[i] = watched[--watch_count];
            return;
        }
    }
}

static int process_exit_queue(void) {
    // Tokio owns the first queue; only inject faults into SDK exit observation.
    if (++queues > 1 && atoi(getenv("COPILOT_SDK_TEST_DARWIN_FAULT")) == 4) {
        fputs("injected observer EMFILE\n", stderr);
        errno = EMFILE;
        return -1;
    }
    int fd = kqueue();
    if (fd >= 0 && queues > 1) exit_queue = fd;
    return fd;
}

static int group_signal(pid_t pid, int signal) {
    if (pid < -1 && signal == SIGKILL) {
        switch (atoi(getenv("COPILOT_SDK_TEST_DARWIN_FAULT"))) {
        case 0:
        case 3:
            fputs("injected group EPERM\n", stderr);
            errno = EPERM;
            return -1;
        case 1:
            // Model a successful XNU snapshot that missed a concurrent child.
            if (passes++ == 0) {
                fputs("injected missed child\n", stderr);
                return kill(-pid, signal);
            }
            break;
        case 2:
            // An unbounded implementation eventually succeeds, failing the test.
            if (passes++ < 64) return 0;
            break;
        case 5:
            ++passes;
            owned_group = -pid;
            // Hold the SDK until a real descendant exit, then model its reaping.
            int observer = kqueue();
            struct kevent change, event;
            EV_SET(&change, exited_member, EVFILT_PROC, EV_ADD | EV_ONESHOT,
                   NOTE_EXIT, 0, NULL);
            int attached = observer >= 0 &&
                kevent(observer, &change, 1, NULL, 0, NULL) == 0;
            int signalled = kill(pid, signal);
            struct timespec timeout = { 2, 0 };
            int result = 0;
            if (attached && signalled == 0) {
                do {
                    result = kevent(observer, NULL, 0, &event, 1, &timeout);
                } while (result < 0 && errno == EINTR);
            }
            if (observer >= 0) close(observer);
            if (result == 1 && (event.fflags & NOTE_EXIT)) {
                // Force the SDK's EV_DELETE to receive ENOENT for this exit.
                EV_SET(&change, exited_member, EVFILT_PROC, EV_DELETE | EV_RECEIPT,
                       NOTE_EXIT, 0, NULL);
                struct timespec immediate = { 0, 0 };
                if (kevent(exit_queue, &change, 1, &event, 1, &immediate) == 1 &&
                    (event.flags & EV_ERROR) && event.data == 0) {
                    fputs("injected exited member before refresh\n", stderr);
                    return signalled;
                }
            }
            exited_member = 0;
            return signalled;
        }
    }
    return kill(pid, signal);
}

static int failed_snapshot(uint32_t type, uint32_t group, void *buffer, int size) {
    if (buffer != NULL && atoi(getenv("COPILOT_SDK_TEST_DARWIN_FAULT")) == 0) {
        fputs("injected snapshot ENOMEM\n", stderr);
        errno = ENOMEM;
        return 0;
    }
    int written = proc_listpids(type, group, buffer, size);
    if (buffer != NULL && written > 0 && passes == 0 &&
        atoi(getenv("COPILOT_SDK_TEST_DARWIN_FAULT")) == 1) {
        // Model a child not published until after our first group snapshot.
        int *pids = buffer;
        int kept = 0;
        for (int i = 0; i < written / (int)sizeof(int); ++i) {
            if (pids[i] == (int)group) pids[kept++] = pids[i];
        }
        return kept * (int)sizeof(int);
    }
    return written;
}

static int member_info(int pid, int flavor, uint64_t arg, void *buffer, int size) {
    int result = proc_pidinfo(pid, flavor, arg, buffer, size);
    if (atoi(getenv("COPILOT_SDK_TEST_DARWIN_FAULT")) == 5 &&
        flavor == PROC_PIDTBSDINFO &&
        result == (int)sizeof(struct proc_bsdinfo)) {
        struct proc_bsdinfo *info = buffer;
        if (passes == 0 && info->pbi_pid != info->pbi_pgid) {
            exited_member = pid;
        } else if (passes > 0 && pid == exited_member) {
            errno = ESRCH;
            return 0;
        } else if (passes > 0 && waits == 0 &&
                   info->pbi_pgid == (uint32_t)owned_group) {
            // Hold the pre-exit observation until the SDK enters its event wait.
            info->pbi_status = SRUN;
        }
    }
    return result;
}

static int exit_wait(int fd, const struct kevent *changes, int nchanges,
                     struct kevent *events, int nevents, const struct timespec *timeout) {
    if (fd == exit_queue && passes > 0 && nchanges == 0 && nevents > 0 &&
        atoi(getenv("COPILOT_SDK_TEST_DARWIN_FAULT")) == 2) {
        if (waits++ == 0) {
            fputs("injected exit wait EINTR\n", stderr);
            errno = EINTR;
            return -1;
        }
        fputs("injected exit wait timeout\n", stderr);
        return 0;
    }
    if (fd == exit_queue && passes > 0 && nchanges == 0 && nevents > 0 &&
        atoi(getenv("COPILOT_SDK_TEST_DARWIN_FAULT")) == 5) {
        while (watch_count > 0) {
            int result = kevent(fd, changes, nchanges, events, nevents, timeout);
            if (result != 1) return result;
            if (events[0].filter == EVFILT_PROC && (events[0].fflags & NOTE_EXIT)) {
                remove_watch(events[0].ident);
            }
        }
        ++waits;
        // Model a late debugger withholding notifications until after the wait.
        fputs("injected suppressed exit notifications\n", stderr);
        return 0;
    }
    int result = kevent(fd, changes, nchanges, events, nevents, timeout);
    if (fd == exit_queue && nchanges == 1 &&
        result == 1 && (events[0].flags & EV_ERROR)) {
        if ((changes[0].flags & EV_ADD) && events[0].data == 0) {
            remove_watch(changes[0].ident);
            if (watch_count == sizeof(watched) / sizeof(watched[0])) {
                errno = EOVERFLOW;
                return -1;
            }
            watched[watch_count++] = changes[0].ident;
        } else if ((changes[0].flags & EV_DELETE) &&
                   (events[0].data == 0 || events[0].data == ENOENT)) {
            remove_watch(changes[0].ident);
        }
    }
    return result;
}

__attribute__((used, section("__DATA,__interpose")))
static const struct { const void *replacement; const void *original; } faults[] = {
    { (const void *)group_signal, (const void *)kill },
    { (const void *)failed_snapshot, (const void *)proc_listpids },
    { (const void *)exit_wait, (const void *)kevent },
    { (const void *)process_exit_queue, (const void *)kqueue },
    { (const void *)member_info, (const void *)proc_pidinfo },
};
"#,
    )
    .unwrap();
    let compile = std::process::Command::new("/usr/bin/cc")
        .args(["-dynamiclib", "-o"])
        .arg(&library)
        .arg(&source)
        .current_dir(temp.path())
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "could not compile syscall fault fixture: {}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let output = std::process::Command::new(std::env::current_exe().unwrap())
        .args([
            "--exact",
            &format!("process_tree::unix_tests::{test}"),
            "--nocapture",
        ])
        .env("COPILOT_SDK_TEST_DARWIN_FAULT", mode)
        .env("DYLD_INSERT_LIBRARIES", &library)
        .current_dir(temp.path())
        .output()
        .unwrap();
    let expected_faults: &[&str] = match mode {
        "0" => &["injected snapshot ENOMEM"],
        "1" => &["injected missed child"],
        "2" => &["injected exit wait EINTR", "injected exit wait timeout"],
        "3" => &["injected group EPERM"],
        "4" => &["injected observer EMFILE"],
        "5" => &[
            "injected exited member before refresh",
            "injected suppressed exit notifications",
        ],
        _ => panic!("unknown Darwin fault mode: {mode}"),
    };
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        output.status.success()
            && String::from_utf8_lossy(&output.stdout).contains("1 passed;")
            && expected_faults.iter().all(|fault| stderr.contains(fault)),
        "isolated Darwin syscall regression failed:\n{}\n{}",
        String::from_utf8_lossy(&output.stdout),
        stderr
    );
}
