//! Descendant ownership of an SDK-spawned CLI process.
//!
//! A Job Object with `JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE` lets Windows
//! terminate the CLI when the SDK-hosting process exits abruptly, even when
//! Rust cleanup code never runs. Unix uses a private process group, terminated
//! before reaping the root so its PGID cannot be recycled during teardown.
//! Unix cleanup requires Rust destructors to run and covers descendants that
//! remain in the group, not processes that deliberately detach via setsid.
//! Darwin drains concurrent forks synchronously before retiring the group;
//! failure to establish quiescence is an error, not deferred cleanup.
//! Terminal signals to the host's foreground group do not reach this group;
//! the host must arrange cleanup when handling shutdown signals.
//! When Windows denies reopening the initial thread, a logged ordinary-spawn
//! retry uses best-effort Job assignment, without pre-assignment containment.

use std::io;

use tokio::process::Child;
#[cfg(feature = "runtime")]
use tokio::process::Command;

/// Keeps the containment guard alive until it has been terminated, before any
/// Child drop can reap the leader. All explicit waits follow terminate_tree.
pub(crate) struct OwnedProcess {
    pub(crate) child: Child,
    tree: Option<ProcessTree>,
}

impl OwnedProcess {
    #[cfg(feature = "runtime")]
    pub(crate) fn new(child: Child, tree: Option<ProcessTree>) -> Self {
        Self { child, tree }
    }

    pub(crate) fn terminate_tree(&mut self) -> io::Result<()> {
        match self.tree.take() {
            Some(tree) => tree.terminate(),
            None => Ok(()),
        }
    }

    pub(crate) fn has_exited(&mut self) -> io::Result<bool> {
        #[cfg(all(unix, feature = "runtime"))]
        {
            let Some(pid) = self.child.id() else {
                return Ok(true);
            };
            // SAFETY: siginfo_t is a plain output buffer; WNOWAIT leaves our
            // child's PID allocated until terminate_tree has retired its group.
            let mut info: libc::siginfo_t = unsafe { std::mem::zeroed() };
            let result = unsafe {
                libc::waitid(
                    libc::P_PID,
                    pid,
                    &mut info,
                    libc::WEXITED | libc::WNOHANG | libc::WNOWAIT,
                )
            };
            if result == 0 {
                return Ok(info.si_signo == libc::SIGCHLD);
            }
            let error = io::Error::last_os_error();
            if error.kind() == io::ErrorKind::Interrupted {
                return Ok(false);
            }
            if error.raw_os_error() == Some(libc::ECHILD)
                && let Some(mut tree) = self.tree.take()
            {
                // A foreign reaper has released the PID; never signal it again.
                tree.0.take();
            }
            Err(error)
        }
        #[cfg(not(all(unix, feature = "runtime")))]
        {
            self.child.try_wait().map(|status| status.is_some())
        }
    }
}

impl Drop for OwnedProcess {
    fn drop(&mut self) {
        if let Err(error) = self.terminate_tree() {
            tracing::warn!(%error, "failed to terminate CLI process tree on drop");
        }
    }
}

#[cfg(feature = "runtime")]
pub(crate) fn spawn(command: &mut Command) -> io::Result<(Child, Option<ProcessTree>)> {
    #[cfg(any(unix, windows))]
    {
        platform::spawn(command)
            .map(|(child, tree)| (child, tree.map(|tree| ProcessTree(Some(tree)))))
    }
    #[cfg(not(any(unix, windows)))]
    {
        command.spawn().map(|child| (child, None))
    }
}

pub(crate) struct ProcessTree(Option<platform::Tree>);

impl ProcessTree {
    pub(crate) fn terminate(mut self) -> io::Result<()> {
        self.0.take().expect("process tree is armed").terminate()
    }
}

impl Drop for ProcessTree {
    fn drop(&mut self) {
        if let Some(tree) = self.0.take() {
            let _ = tree.terminate();
        }
    }
}

#[cfg(not(all(any(unix, windows), feature = "runtime")))]
mod platform {
    pub(super) struct Tree;

    impl Tree {
        pub(super) fn terminate(&self) -> std::io::Result<()> {
            unreachable!("process-tree ownership requires Unix or Windows and runtime")
        }
    }
}

#[cfg(all(unix, feature = "runtime"))]
mod platform {
    use std::io;
    #[cfg(target_os = "macos")]
    use std::os::fd::{AsRawFd, FromRawFd, OwnedFd};
    use std::os::unix::process::CommandExt;
    #[cfg(target_os = "macos")]
    use std::time::{Duration, Instant};

    use tokio::process::{Child, Command};

    pub(super) struct Tree {
        pgid: i32,
    }

    #[cfg(target_os = "macos")]
    type Identity = (u32, u64, u64);
    #[cfg(target_os = "macos")]
    const PROC_FLAG_TRACED: u32 = 2;
    #[cfg(target_os = "macos")]
    const PROC_FLAG_INEXIT: u32 = 4;

    pub(super) fn spawn(command: &mut Command) -> io::Result<(Child, Option<Tree>)> {
        command.as_std_mut().process_group(0);
        let mut child = command.spawn()?;
        let pgid = child
            .id()
            .and_then(|pid| i32::try_from(pid).ok())
            .filter(|&pid| pid > 1);
        let Some(pgid) = pgid else {
            child.start_kill()?;
            return Err(io::Error::other(
                "CLI process has no valid process group ID",
            ));
        };
        Ok((child, Some(Tree { pgid })))
    }

    impl Tree {
        #[cfg(not(target_os = "macos"))]
        pub(super) fn terminate(&self) -> io::Result<()> {
            // SAFETY: process_group(0) creates a private PGID equal to the
            // owned root's PID, which remains unreaped until after this signal.
            if unsafe { libc::kill(-self.pgid, libc::SIGKILL) } == 0 {
                return Ok(());
            }
            let error = io::Error::last_os_error();
            if error.raw_os_error() == Some(libc::ESRCH) {
                return Ok(());
            }
            Err(error)
        }

        #[cfg(target_os = "macos")]
        pub(super) fn terminate(&self) -> io::Result<()> {
            self.drain().inspect_err(|_| {
                // Observation can fail before the first signal. Still attempt
                // group cleanup while the leader pins the PGID, preserving the
                // original error rather than claiming confirmed quiescence.
                unsafe { libc::kill(-self.pgid, libc::SIGKILL) };
            })
        }

        #[cfg(target_os = "macos")]
        fn drain(&self) -> io::Result<()> {
            // SAFETY: the owned, unreaped leader pins this private PGID.
            if unsafe { libc::kill(-self.pgid, 0) } != 0 {
                let error = io::Error::last_os_error();
                match error.raw_os_error() {
                    Some(libc::ESRCH) => return Ok(()),
                    Some(libc::EPERM) => {}
                    _ => return Err(error),
                }
            }
            let queue = ExitQueue::new()?;
            let mut exited = Vec::new();
            // XNU snapshots group PIDs before signalling. Watch each generation
            // before killing it, then discover children published during exit.
            for _ in 0..64 {
                let mut watched = Vec::new();
                let mut unattachable = Vec::new();
                for pid in self.members()? {
                    let Some(info) = process_info(pid)? else {
                        continue;
                    };
                    let identity = (info.pbi_pid, info.pbi_start_tvsec, info.pbi_start_tvusec);
                    if info.pbi_pgid != self.pgid as u32 || exited.contains(&identity) {
                        continue;
                    }
                    if info.pbi_status == libc::SZOMB {
                        exited.push(identity);
                        continue;
                    }
                    if info.pbi_flags & PROC_FLAG_TRACED != 0 {
                        unattachable.push(identity);
                        continue;
                    }
                    match queue.change(identity.0, libc::EV_ADD | libc::EV_ONESHOT) {
                        Ok(()) => {
                            if self.matches_live(identity)? {
                                watched.push(identity);
                            } else {
                                queue.change(identity.0, libc::EV_DELETE)?;
                                exited.push(identity);
                            }
                        }
                        // proc_find rejects INEXIT processes before their last
                        // thread has finished; ESRCH alone is not exit proof.
                        Err(error) if error.raw_os_error() == Some(libc::ESRCH) => {
                            if let Some(info) = process_info(pid)?
                                && info.pbi_pgid == self.pgid as u32
                                && (info.pbi_pid, info.pbi_start_tvsec, info.pbi_start_tvusec)
                                    == identity
                                && info.pbi_status != libc::SZOMB
                            {
                                if info.pbi_flags & (PROC_FLAG_INEXIT | PROC_FLAG_TRACED) == 0 {
                                    return Err(error);
                                }
                                unattachable.push(identity);
                                continue;
                            }
                            exited.push(identity);
                        }
                        Err(error) => return Err(error),
                    }
                }
                if watched.is_empty() && unattachable.is_empty() {
                    if self.all_members_exited(&exited)? {
                        return Ok(());
                    }
                    continue;
                }
                // SAFETY: the owned, unreaped leader pins this private PGID.
                if unsafe { libc::kill(-self.pgid, libc::SIGKILL) } != 0 {
                    let error = io::Error::last_os_error();
                    match error.raw_os_error() {
                        Some(libc::ESRCH) => return Ok(()),
                        Some(libc::EPERM) => {
                            for identity in &watched {
                                if let Some(info) = process_info(identity.0 as i32)?
                                    && info.pbi_pgid == self.pgid as u32
                                    && (info.pbi_pid, info.pbi_start_tvsec, info.pbi_start_tvusec)
                                        == *identity
                                    && info.pbi_status != libc::SZOMB
                                    && info.pbi_flags & PROC_FLAG_INEXIT == 0
                                {
                                    return Err(error);
                                }
                            }
                        }
                        _ => return Err(error),
                    }
                }
                let mut backoff = Duration::from_millis(1);
                while !watched.is_empty() || !unattachable.is_empty() {
                    self.refresh_pending(&mut watched, &mut exited, Some(&queue))?;
                    self.refresh_pending(&mut unattachable, &mut exited, None)?;
                    if watched.is_empty() && unattachable.is_empty() {
                        break;
                    }
                    let event = match queue.wait((!unattachable.is_empty()).then_some(backoff)) {
                        Ok(event) => event,
                        Err(error) if error.kind() == io::ErrorKind::TimedOut => {
                            // A debugger attached after registration can
                            // suppress NOTE_EXIT. Recheck before reporting
                            // failure; the queue is no longer used on timeout.
                            self.refresh_pending(&mut watched, &mut exited, None)?;
                            self.refresh_pending(&mut unattachable, &mut exited, None)?;
                            if !watched.is_empty() || !unattachable.is_empty() {
                                return Err(error);
                            }
                            break;
                        }
                        Err(error) => return Err(error),
                    };
                    if let Some(event) = event {
                        let notified_pid = event.ident;
                        if event.filter == libc::EVFILT_PROC
                            && event.fflags & libc::NOTE_EXIT != 0
                            && let Some(index) = watched
                                .iter()
                                .position(|identity| identity.0 as usize == notified_pid)
                        {
                            // NOTE_EXIT is posted after the last thread exits,
                            // just before SZOMB becomes visible to proc_pidinfo.
                            exited.push(watched.swap_remove(index));
                        }
                    }
                    backoff = (backoff * 2).min(Duration::from_millis(16));
                }
                if self.all_members_exited(&exited)? {
                    return Ok(());
                }
            }
            Err(io::Error::other(
                "CLI process group kept creating members for 64 generations",
            ))
        }

        #[cfg(target_os = "macos")]
        fn matches_live(&self, identity: Identity) -> io::Result<bool> {
            Ok(process_info(identity.0 as i32)?.is_some_and(|info| {
                info.pbi_pgid == self.pgid as u32
                    && (info.pbi_pid, info.pbi_start_tvsec, info.pbi_start_tvusec) == identity
                    && info.pbi_status != libc::SZOMB
            }))
        }

        #[cfg(target_os = "macos")]
        fn refresh_pending(
            &self,
            pending: &mut Vec<Identity>,
            exited: &mut Vec<Identity>,
            queue: Option<&ExitQueue>,
        ) -> io::Result<()> {
            let mut index = 0;
            while index < pending.len() {
                let identity = pending[index];
                if self.matches_live(identity)? {
                    index += 1;
                    continue;
                }
                if let Some(queue) = queue {
                    queue.change(identity.0, libc::EV_DELETE)?;
                }
                exited.push(pending.swap_remove(index));
            }
            Ok(())
        }

        #[cfg(target_os = "macos")]
        fn all_members_exited(&self, exited: &[Identity]) -> io::Result<bool> {
            // Every identity must have been proved threadless before this
            // listing, so no remaining creator can publish another child.
            for pid in self.members()? {
                if let Some(info) = process_info(pid)?
                    && info.pbi_pgid == self.pgid as u32
                    && !exited.contains(&(
                        info.pbi_pid,
                        info.pbi_start_tvsec,
                        info.pbi_start_tvusec,
                    ))
                {
                    return Ok(false);
                }
            }
            Ok(true)
        }

        #[cfg(target_os = "macos")]
        fn members(&self) -> io::Result<Vec<i32>> {
            const PROC_PGRP_ONLY: u32 = 2;
            // SAFETY: null requests the required buffer size for this group.
            let size = unsafe {
                libc::proc_listpids(PROC_PGRP_ONLY, self.pgid as u32, std::ptr::null_mut(), 0)
            };
            if size <= 0 {
                return Err(io::Error::last_os_error());
            }
            let mut pids = vec![0i32; size as usize / std::mem::size_of::<i32>()];
            // SAFETY: pids is initialized and writable for exactly size bytes.
            let written = unsafe {
                libc::proc_listpids(
                    PROC_PGRP_ONLY,
                    self.pgid as u32,
                    pids.as_mut_ptr().cast(),
                    size,
                )
            };
            if written <= 0 {
                return Err(io::Error::last_os_error());
            }
            if written == size {
                return Err(io::Error::other("CLI process group snapshot was truncated"));
            }
            pids.truncate(written as usize / std::mem::size_of::<i32>());
            Ok(pids)
        }
    }

    #[cfg(target_os = "macos")]
    struct ExitQueue {
        fd: OwnedFd,
        deadline: Instant,
    }

    #[cfg(target_os = "macos")]
    impl ExitQueue {
        fn new() -> io::Result<Self> {
            // SAFETY: kqueue creates a fresh descriptor, not inherited by fork.
            let fd = unsafe { libc::kqueue() };
            if fd < 0 {
                return Err(io::Error::last_os_error());
            }
            Ok(Self {
                // SAFETY: this is the unique owner of the valid descriptor.
                fd: unsafe { OwnedFd::from_raw_fd(fd) },
                deadline: Instant::now() + Duration::from_secs(2),
            })
        }

        fn remaining(&self) -> io::Result<Duration> {
            self.deadline
                .checked_duration_since(Instant::now())
                .ok_or_else(Self::timeout)
        }

        fn timeout() -> io::Error {
            io::Error::new(
                io::ErrorKind::TimedOut,
                "CLI process group exit wait timed out",
            )
        }

        fn change(&self, pid: u32, flags: u16) -> io::Result<()> {
            let change = libc::kevent {
                ident: pid as usize,
                filter: libc::EVFILT_PROC,
                flags: flags | libc::EV_RECEIPT,
                fflags: libc::NOTE_EXIT,
                data: 0,
                udata: std::ptr::null_mut(),
            };
            let timeout = libc::timespec {
                tv_sec: 0,
                tv_nsec: 0,
            };
            loop {
                self.remaining()?;
                // SAFETY: kevent is a plain output buffer with a valid zero value.
                let mut receipt: libc::kevent = unsafe { std::mem::zeroed() };
                let result = unsafe {
                    libc::kevent(self.fd.as_raw_fd(), &change, 1, &mut receipt, 1, &timeout)
                };
                let error = if result < 0 {
                    io::Error::last_os_error()
                } else if result == 1 && receipt.flags & libc::EV_ERROR != 0 {
                    if receipt.data == 0 {
                        return Ok(());
                    }
                    io::Error::from_raw_os_error(
                        i32::try_from(receipt.data).map_err(io::Error::other)?,
                    )
                } else {
                    return Err(io::Error::other(
                        "missing CLI process exit registration receipt",
                    ));
                };
                if error.kind() == io::ErrorKind::Interrupted {
                    continue;
                }
                if flags & libc::EV_DELETE != 0 && error.raw_os_error() == Some(libc::ENOENT) {
                    return Ok(());
                }
                return Err(error);
            }
        }

        fn wait(&self, recheck: Option<Duration>) -> io::Result<Option<libc::kevent>> {
            loop {
                let remaining = self.remaining()?;
                let duration = recheck.map_or(remaining, |interval| interval.min(remaining));
                let timeout = libc::timespec {
                    tv_sec: i64::try_from(duration.as_secs()).map_err(io::Error::other)?,
                    tv_nsec: duration.subsec_nanos().into(),
                };
                // SAFETY: event is a writable output buffer; timeout is initialized.
                let mut event: libc::kevent = unsafe { std::mem::zeroed() };
                let result = unsafe {
                    libc::kevent(
                        self.fd.as_raw_fd(),
                        std::ptr::null(),
                        0,
                        &mut event,
                        1,
                        &timeout,
                    )
                };
                if result < 0 {
                    let error = io::Error::last_os_error();
                    if error.kind() == io::ErrorKind::Interrupted {
                        continue;
                    }
                    return Err(error);
                }
                if result == 0 {
                    return if recheck.is_some() {
                        Ok(None)
                    } else {
                        Err(Self::timeout())
                    };
                }
                if event.flags & libc::EV_ERROR != 0 && event.data != 0 {
                    return Err(io::Error::from_raw_os_error(
                        i32::try_from(event.data).map_err(io::Error::other)?,
                    ));
                }
                return Ok(Some(event));
            }
        }
    }

    #[cfg(target_os = "macos")]
    fn process_info(pid: i32) -> io::Result<Option<libc::proc_bsdinfo>> {
        // SAFETY: proc_bsdinfo is a plain output buffer with a valid zero value.
        let mut info: libc::proc_bsdinfo = unsafe { std::mem::zeroed() };
        let size = i32::try_from(std::mem::size_of_val(&info)).map_err(io::Error::other)?;
        // SAFETY: info is writable for size bytes; argument 1 includes zombies.
        let read = unsafe {
            libc::proc_pidinfo(pid, libc::PROC_PIDTBSDINFO, 1, (&raw mut info).cast(), size)
        };
        if read <= 0 {
            let error = io::Error::last_os_error();
            if error.raw_os_error() == Some(libc::ESRCH) {
                return Ok(None);
            }
            return Err(error);
        }
        if read != size {
            return Err(io::Error::other(
                "incomplete CLI process group member information",
            ));
        }
        Ok(Some(info))
    }
}

#[cfg(all(windows, feature = "runtime"))]
mod platform {
    use std::mem::size_of;
    use std::os::windows::io::{AsRawHandle, FromRawHandle, OwnedHandle};
    use std::os::windows::process::CommandExt;
    use std::{io, ptr};

    use tokio::process::{Child, Command};
    use windows_sys::Win32::Foundation::{
        ERROR_ACCESS_DENIED, ERROR_NO_MORE_FILES, ERROR_NO_MORE_ITEMS, HANDLE, INVALID_HANDLE_VALUE,
    };
    use windows_sys::Win32::System::Diagnostics::ProcessSnapshotting::{
        HPSS, HPSSWALK, PSS_CAPTURE_THREADS, PSS_THREAD_ENTRY, PSS_WALK_THREADS,
        PssCaptureSnapshot, PssFreeSnapshot, PssWalkMarkerCreate, PssWalkMarkerFree,
        PssWalkSnapshot,
    };
    use windows_sys::Win32::System::Diagnostics::ToolHelp::{
        CreateToolhelp32Snapshot, TH32CS_SNAPTHREAD, THREADENTRY32, Thread32First, Thread32Next,
    };
    use windows_sys::Win32::System::JobObjects::{
        AssignProcessToJobObject, CreateJobObjectW, JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE,
        JOBOBJECT_EXTENDED_LIMIT_INFORMATION, JobObjectExtendedLimitInformation,
        SetInformationJobObject, TerminateJobObject,
    };
    use windows_sys::Win32::System::Threading::{
        CREATE_NO_WINDOW, CREATE_SUSPENDED, GetCurrentProcess, GetProcessIdOfThread, OpenThread,
        ResumeThread, THREAD_QUERY_LIMITED_INFORMATION, THREAD_SUSPEND_RESUME,
    };

    #[cfg(test)]
    use super::tests::{
        BEFORE_ASSIGNMENT, BEFORE_RESUME, FailurePoint, MARKERS, RESUME_ELAPSED, SNAPSHOTS,
        THREADS_EXAMINED, fail_at,
    };

    struct ThreadSnapshot(HPSS);

    impl Drop for ThreadSnapshot {
        fn drop(&mut self) {
            // SAFETY: this process uniquely owns the local snapshot descriptor.
            let status = unsafe { PssFreeSnapshot(GetCurrentProcess(), self.0) };
            if let Err(error) = check_status(status) {
                tracing::warn!(%error, "failed to free CLI thread snapshot");
            }
            #[cfg(test)]
            if status == 0 {
                SNAPSHOTS.set((SNAPSHOTS.get().0, SNAPSHOTS.get().1 + 1));
            }
        }
    }

    struct WalkMarker(HPSSWALK);

    impl Drop for WalkMarker {
        fn drop(&mut self) {
            // SAFETY: this value uniquely owns a valid walk marker.
            let status = unsafe { PssWalkMarkerFree(self.0) };
            if let Err(error) = check_status(status) {
                tracing::warn!(%error, "failed to free CLI thread walk marker");
            }
            #[cfg(test)]
            if status == 0 {
                MARKERS.set((MARKERS.get().0, MARKERS.get().1 + 1));
            }
        }
    }

    pub(super) struct Tree {
        job: OwnedHandle,
    }

    #[derive(Debug)]
    struct ThreadOpenError(io::Error);

    impl std::fmt::Display for ThreadOpenError {
        fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(formatter, "OpenThread failed: {}", self.0)
        }
    }

    impl std::error::Error for ThreadOpenError {
        fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
            Some(&self.0)
        }
    }

    pub(super) fn spawn(command: &mut Command) -> io::Result<(Child, Option<Tree>)> {
        // The root cannot run or create descendants before Job assignment.
        command
            .as_std_mut()
            .creation_flags(CREATE_NO_WINDOW | CREATE_SUSPENDED);
        let mut child = command.spawn()?;
        let tree = match attach(&child) {
            Ok(tree) => tree,
            Err(error) => {
                if let Err(kill_error) = terminate_suspended(&mut child) {
                    tracing::warn!(%kill_error, "failed to terminate suspended CLI child");
                }
                return Err(error);
            }
        };
        let Err(error) = resume(&child, &tree) else {
            return Ok((child, Some(tree)));
        };
        // Keep the Job alive until termination is requested. Dropping it earlier
        // races start_kill against kill-on-close. Never retry a root that can run.
        if let Err(kill_error) = terminate_suspended(&mut child) {
            tracing::warn!(%kill_error, "failed to terminate suspended CLI child");
            return Err(error);
        }
        let denied_thread_open = error
            .get_ref()
            .and_then(|source| source.downcast_ref::<ThreadOpenError>())
            .is_some_and(|source| source.0.raw_os_error() == Some(ERROR_ACCESS_DENIED as i32));
        if !denied_thread_open {
            return Err(error);
        }
        drop(child);
        tracing::warn!(
            %error,
            "CLI initial thread access denied; retrying without suspension and with \
             best-effort private Job assignment; descendants can escape before assignment"
        );
        command.as_std_mut().creation_flags(CREATE_NO_WINDOW);
        let child = command.spawn().map_err(|spawn_error| {
            io::Error::new(
                spawn_error.kind(),
                format!("ordinary CLI spawn failed after {error}: {spawn_error}"),
            )
        })?;
        #[cfg(test)]
        let assignment =
            fail_at(FailurePoint::CompatibilityAssignment).and_then(|()| tree.assign(&child));
        #[cfg(not(test))]
        let assignment = tree.assign(&child);
        match assignment {
            Ok(()) => Ok((child, Some(tree))),
            Err(assignment_error) => {
                tracing::warn!(
                    %assignment_error,
                    "ordinary CLI child could not join its private Job; descendants may survive \
                     client shutdown and processes may survive SDK host exit"
                );
                Ok((child, None))
            }
        }
    }

    pub(super) fn terminate_suspended(child: &mut Child) -> io::Result<()> {
        #[cfg(test)]
        let result = fail_at(FailurePoint::Kill).and_then(|()| child.start_kill());
        #[cfg(not(test))]
        let result = child.start_kill();
        match result {
            // TerminateProcess can deny an already-terminated process. Only an
            // observed exit on this Child's retained handle makes retry safe.
            Err(error) if error.raw_os_error() == Some(ERROR_ACCESS_DENIED as i32) => {
                match child.try_wait() {
                    Ok(Some(_)) => Ok(()),
                    Ok(None) => Err(error),
                    Err(wait_error) => {
                        tracing::warn!(%wait_error, "failed to confirm suspended CLI child exit");
                        Err(error)
                    }
                }
            }
            result => result,
        }
    }

    fn attach(child: &Child) -> io::Result<Tree> {
        #[cfg(test)]
        if let Some(hook) = BEFORE_ASSIGNMENT.with_borrow_mut(Option::take) {
            hook(child)?;
        }
        // SAFETY: null security attributes and name create a private,
        // non-inheritable Job Object.
        let raw_job = unsafe { CreateJobObjectW(ptr::null(), ptr::null()) };
        if raw_job.is_null() {
            return Err(io::Error::last_os_error());
        }
        // SAFETY: CreateJobObjectW succeeded and transfers ownership of this handle.
        let job = unsafe { OwnedHandle::from_raw_handle(raw_job) };

        let mut limits = JOBOBJECT_EXTENDED_LIMIT_INFORMATION::default();
        limits.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
        // SAFETY: `limits` has the layout required by the selected info class.
        if unsafe {
            SetInformationJobObject(
                job.as_raw_handle(),
                JobObjectExtendedLimitInformation,
                ptr::from_ref(&limits).cast(),
                size_of::<JOBOBJECT_EXTENDED_LIMIT_INFORMATION>() as u32,
            )
        } == 0
        {
            return Err(io::Error::last_os_error());
        }

        let tree = Tree { job };
        tree.assign(child)?;
        Ok(tree)
    }

    fn resume(child: &Child, _tree: &Tree) -> io::Result<()> {
        #[cfg(test)]
        if let Some(hook) = BEFORE_RESUME.with_borrow_mut(Option::take) {
            hook(child, _tree.job.as_raw_handle())?;
        }
        let process = child.raw_handle().ok_or_else(|| {
            io::Error::new(io::ErrorKind::NotFound, "CLI exited before thread resume")
        })?;
        #[cfg(test)]
        let resume_start = std::time::Instant::now();
        let result = resume_initial_thread(
            process.cast(),
            child.id().ok_or_else(|| {
                io::Error::new(io::ErrorKind::NotFound, "CLI exited before thread resume")
            })?,
        );
        #[cfg(test)]
        RESUME_ELAPSED.set(resume_start.elapsed());
        result
    }

    pub(super) fn check_status(status: u32) -> io::Result<()> {
        if status == 0 {
            Ok(())
        } else {
            Err(io::Error::from_raw_os_error(status as i32))
        }
    }

    pub(super) fn resume_initial_thread(process: HANDLE, pid: u32) -> io::Result<()> {
        #[cfg(test)]
        THREADS_EXAMINED.set(0);
        let thread_id = match find_initial_thread_pss(process, pid) {
            Ok(thread_id) => thread_id,
            Err(error) => {
                tracing::warn!(
                    %error,
                    "CLI process-scoped thread lookup failed; falling back to Toolhelp"
                );
                find_initial_thread_toolhelp(pid)?
            }
        };
        let thread = open_initial_thread(thread_id)
            .map_err(|error| io::Error::new(error.kind(), ThreadOpenError(error)))?;
        resume_owned_thread(thread.as_raw_handle(), pid)
    }

    fn open_initial_thread(thread_id: u32) -> io::Result<OwnedHandle> {
        #[cfg(test)]
        fail_at(FailurePoint::OpenThread)?;
        // SAFETY: the lookup selected a thread belonging to the suspended root.
        // The opened handle's ownership is independently checked before resume.
        let raw_thread = unsafe {
            OpenThread(
                THREAD_SUSPEND_RESUME | THREAD_QUERY_LIMITED_INFORMATION,
                0,
                thread_id,
            )
        };
        if raw_thread.is_null() {
            return Err(io::Error::last_os_error());
        }
        // SAFETY: OpenThread succeeded and transfers ownership of this handle.
        Ok(unsafe { OwnedHandle::from_raw_handle(raw_thread) })
    }

    pub(super) fn find_initial_thread_toolhelp(pid: u32) -> io::Result<u32> {
        #[cfg(test)]
        fail_at(FailurePoint::Toolhelp)?;
        // SAFETY: this read-only system snapshot does not execute any thread.
        let raw = unsafe { CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0) };
        if raw == INVALID_HANDLE_VALUE {
            return Err(io::Error::last_os_error());
        }
        // SAFETY: CreateToolhelp32Snapshot succeeded and transfers ownership of this handle.
        let snapshot = unsafe { OwnedHandle::from_raw_handle(raw) };
        let mut entry = THREADENTRY32 {
            dwSize: size_of::<THREADENTRY32>() as u32,
            ..Default::default()
        };
        // SAFETY: entry has the required layout and the snapshot remains owned.
        let mut found = unsafe { Thread32First(snapshot.as_raw_handle(), &mut entry) };
        while found != 0 {
            if entry.th32OwnerProcessID == pid {
                return Ok(entry.th32ThreadID);
            }
            entry.dwSize = size_of::<THREADENTRY32>() as u32;
            // SAFETY: entry and snapshot remain valid throughout the walk.
            found = unsafe { Thread32Next(snapshot.as_raw_handle(), &mut entry) };
        }
        let error = io::Error::last_os_error();
        if error.raw_os_error() == Some(ERROR_NO_MORE_FILES as i32) {
            Err(io::Error::new(
                io::ErrorKind::NotFound,
                "CLI initial thread was not found in the Toolhelp snapshot",
            ))
        } else {
            Err(error)
        }
    }

    fn find_initial_thread_pss(process: HANDLE, pid: u32) -> io::Result<u32> {
        #[cfg(test)]
        fail_at(FailurePoint::Capture)?;
        let mut snapshot = ptr::null_mut();
        // SAFETY: the suspended child owns the live process handle throughout
        // capture and resume. Only its thread information is captured.
        check_status(unsafe {
            PssCaptureSnapshot(process, PSS_CAPTURE_THREADS, 0, &mut snapshot)
        })?;
        let snapshot = ThreadSnapshot(snapshot);
        #[cfg(test)]
        SNAPSHOTS.set((SNAPSHOTS.get().0 + 1, SNAPSHOTS.get().1));

        #[cfg(test)]
        fail_at(FailurePoint::Marker)?;
        let mut marker = ptr::null_mut();
        // SAFETY: Windows initializes the marker with its default allocator.
        check_status(unsafe { PssWalkMarkerCreate(ptr::null(), &mut marker) })?;
        let marker = WalkMarker(marker);
        #[cfg(test)]
        MARKERS.set((MARKERS.get().0 + 1, MARKERS.get().1));

        loop {
            #[cfg(test)]
            fail_at(FailurePoint::Walk)?;
            let mut entry = PSS_THREAD_ENTRY::default();
            // SAFETY: the aligned entry has the layout and size required by
            // PSS_WALK_THREADS; both snapshot and marker remain owned.
            let status = unsafe {
                PssWalkSnapshot(
                    snapshot.0,
                    PSS_WALK_THREADS,
                    marker.0,
                    ptr::from_mut(&mut entry).cast(),
                    size_of::<PSS_THREAD_ENTRY>() as u32,
                )
            };
            if status == ERROR_NO_MORE_ITEMS {
                return Err(io::Error::new(
                    io::ErrorKind::NotFound,
                    "CLI initial thread was not found in its process snapshot",
                ));
            }
            check_status(status)?;
            #[cfg(test)]
            THREADS_EXAMINED.set(THREADS_EXAMINED.get() + 1);
            if entry.ProcessId == pid {
                return Ok(entry.ThreadId);
            }
        }
    }

    pub(super) fn resume_owned_thread(thread: HANDLE, pid: u32) -> io::Result<()> {
        #[cfg(test)]
        fail_at(FailurePoint::QueryThread)?;
        // SAFETY: the caller owns this thread handle with limited-query rights.
        let owner = unsafe { GetProcessIdOfThread(thread) };
        if owner == 0 {
            return Err(io::Error::last_os_error());
        }
        if owner != pid {
            return Err(io::Error::new(
                io::ErrorKind::PermissionDenied,
                "CLI initial thread no longer belongs to the spawned process",
            ));
        }
        #[cfg(test)]
        fail_at(FailurePoint::Resume)?;
        // SAFETY: the open handle's ownership was verified, and the root's
        // initial thread remains suspended until Job assignment completes.
        if unsafe { ResumeThread(thread) } == u32::MAX {
            return Err(io::Error::last_os_error());
        }
        Ok(())
    }

    impl Tree {
        fn assign(&self, child: &Child) -> io::Result<()> {
            let process = child.raw_handle().ok_or_else(|| {
                io::Error::new(
                    io::ErrorKind::NotFound,
                    "CLI exited before Job Object assignment",
                )
            })?;
            // SAFETY: the Job and child handles remain valid. Normal startup
            // assigns before resume; compatibility startup is best-effort.
            if unsafe { AssignProcessToJobObject(self.job.as_raw_handle(), process.cast()) } == 0 {
                Err(io::Error::last_os_error())
            } else {
                Ok(())
            }
        }

        pub(super) fn terminate(&self) -> io::Result<()> {
            // SAFETY: the handle is a live Job Object owned by this value.
            if unsafe { TerminateJobObject(self.job.as_raw_handle(), 1) } != 0 {
                Ok(())
            } else {
                Err(io::Error::last_os_error())
            }
        }
    }
}

#[cfg(all(test, windows, feature = "runtime"))]
mod tests;

#[cfg(all(test, unix, feature = "runtime"))]
mod unix_tests;
