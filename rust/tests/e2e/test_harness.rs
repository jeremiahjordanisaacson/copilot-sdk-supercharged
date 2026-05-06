// Copyright (c) Microsoft Corporation. All rights reserved.

//! Test harness that manages a replaying CAPI proxy for E2E tests.
//!
//! Spawns the shared test harness server (`test/harness/server.ts`) and
//! provides methods to configure and stop the proxy.

use std::collections::HashMap;
use std::env;
use std::io::BufRead;
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};

/// A handle to the replaying CAPI proxy process.
pub struct TestHarness {
    process: Option<Child>,
    proxy_url: Option<String>,
    repo_root: PathBuf,
}

impl TestHarness {
    /// Compute the repo root from the Cargo manifest directory.
    /// `rust/` is one level below the repo root.
    fn find_repo_root() -> PathBuf {
        let manifest_dir = env::var("CARGO_MANIFEST_DIR")
            .map(PathBuf::from)
            .unwrap_or_else(|_| env::current_dir().expect("cannot determine cwd"));

        // If we're in the `rust` directory, go up one level
        if manifest_dir.file_name().map(|n| n == "rust").unwrap_or(false) {
            manifest_dir.parent().unwrap().to_path_buf()
        } else {
            manifest_dir
        }
    }

    /// Creates a new, un-started harness.
    pub fn new() -> Self {
        Self {
            process: None,
            proxy_url: None,
            repo_root: Self::find_repo_root(),
        }
    }

    /// Returns the absolute path to the repo root.
    pub fn repo_root(&self) -> &Path {
        &self.repo_root
    }

    /// Returns the absolute path to the snapshots directory.
    pub fn snapshots_dir(&self) -> PathBuf {
        self.repo_root.join("test").join("snapshots")
    }

    /// Returns the proxy URL, or panics if not started.
    pub fn proxy_url(&self) -> &str {
        self.proxy_url.as_deref().expect("proxy not started")
    }

    /// Starts the replaying proxy server.
    pub fn start(&mut self) {
        if self.proxy_url.is_some() {
            return;
        }

        let server_path = self.repo_root.join("test").join("harness").join("server.ts");
        let harness_dir = server_path.parent().unwrap();

        let is_windows = cfg!(target_os = "windows");
        let mut cmd = if is_windows {
            let mut c = Command::new("cmd");
            c.args(["/c", "npx", "tsx", &server_path.to_string_lossy()]);
            c
        } else {
            let mut c = Command::new("npx");
            c.args(["tsx", &server_path.to_string_lossy()]);
            c
        };

        cmd.current_dir(harness_dir)
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit());

        let mut child = cmd.spawn().expect("failed to spawn proxy server");

        // Read the first line to get the listening URL
        let stdout = child.stdout.take().expect("no stdout from proxy");
        let mut reader = std::io::BufReader::new(stdout);
        let mut line = String::new();
        reader
            .read_line(&mut line)
            .expect("failed to read proxy output");

        let re = regex::Regex::new(r"Listening: (http://[^\s]+)").unwrap();
        let caps = re.captures(line.trim()).unwrap_or_else(|| {
            child.kill().ok();
            panic!("unexpected proxy output: {line}");
        });
        let url = caps.get(1).unwrap().as_str().to_string();

        self.proxy_url = Some(url);
        self.process = Some(child);
    }

    /// Configures the proxy for a specific test snapshot.
    pub fn configure(&self, snapshot_path: &str, work_dir: &str) {
        let url = self.proxy_url.as_deref().expect("proxy not started");
        let body = serde_json::json!({
            "filePath": snapshot_path,
            "workDir": work_dir,
        });

        let client = reqwest::blocking::Client::new();
        let resp = client
            .post(format!("{url}/config"))
            .json(&body)
            .send()
            .expect("failed to send config request");

        assert!(
            resp.status().is_success(),
            "proxy config failed with status {}",
            resp.status()
        );
    }

    /// Stops the proxy server gracefully.
    pub fn stop(&mut self) {
        if let Some(ref url) = self.proxy_url {
            let _ = reqwest::blocking::Client::new()
                .post(format!("{url}/stop?skipWritingCache=true"))
                .send();
        }

        if let Some(ref mut child) = self.process {
            let _ = child.kill();
            let _ = child.wait();
        }

        self.process = None;
        self.proxy_url = None;
    }

    /// Returns environment variables for an isolated test run.
    pub fn test_env(&self, work_dir: &str, home_dir: &str) -> HashMap<String, String> {
        let mut env: HashMap<String, String> = env::vars().collect();
        env.insert(
            "COPILOT_API_URL".into(),
            self.proxy_url().to_string(),
        );
        env.insert("COPILOT_HOME".into(), home_dir.to_string());
        env.insert("XDG_CONFIG_HOME".into(), home_dir.to_string());
        env.insert("XDG_STATE_HOME".into(), home_dir.to_string());
        env.insert("GH_TOKEN".into(), "fake-token-for-e2e-tests".to_string());
        env.insert("GITHUB_TOKEN".into(), "fake-token-for-e2e-tests".to_string());
        env
    }

    /// Returns the CLI path for tests.
    pub fn cli_path(&self) -> String {
        if let Ok(p) = env::var("COPILOT_CLI_PATH") {
            if Path::new(&p).exists() {
                return p;
            }
        }

        let cli = self
            .repo_root
            .join("nodejs")
            .join("node_modules")
            .join("@github")
            .join("copilot")
            .join("index.js");

        if cli.exists() {
            return cli.to_string_lossy().into_owned();
        }

        panic!("CLI not found. Set COPILOT_CLI_PATH or run 'npm install' in the nodejs directory.");
    }
}

impl Drop for TestHarness {
    fn drop(&mut self) {
        self.stop();
    }
}
