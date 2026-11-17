// Copyright (c) Microsoft Corporation. All rights reserved.

package copilot_test

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"net"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
	"time"

	copilot "github.com/microsoft/copilot-sdk/go/v2"
)

func TestSpawnedJavaScriptUsesChildPath(t *testing.T) {
	node, err := exec.LookPath("node")
	if err != nil {
		t.Fatal("process lookup fixture requires Node.js:", err)
	}
	fixture, err := os.ReadFile("../test/harness/stdio-shutdown-runtime.cjs")
	if err != nil {
		t.Fatal(err)
	}
	directory := t.TempDir()
	script := filepath.Join(directory, "runtime.js")
	if err := os.WriteFile(script, fixture, 0o600); err != nil {
		t.Fatal(err)
	}
	emptyPath := t.TempDir()
	nodePath := filepath.Dir(node)
	type pathCase struct {
		name             string
		parentPath       string
		childPath        string
		workingDirectory string
		wantErr          error
	}
	cases := []pathCase{
		{name: "child PATH supplies node", parentPath: emptyPath, childPath: nodePath},
		{name: "child PATH excludes node", parentPath: nodePath, childPath: emptyPath, wantErr: exec.ErrNotFound},
	}
	childCwd := t.TempDir()
	childBin, err := os.MkdirTemp(childCwd, "bin-")
	if err != nil {
		t.Fatal(err)
	}
	nodeName := "node"
	if runtime.GOOS == "windows" {
		nodeName += ".exe"
	}
	if err := os.WriteFile(filepath.Join(childBin, nodeName), nil, 0o700); err != nil {
		t.Fatal(err)
	}
	childRelativePath := filepath.Base(childBin) + string(os.PathListSeparator) + nodePath
	cases = append(cases,
		pathCase{name: "child relative match cannot be bypassed", parentPath: emptyPath, childPath: childRelativePath, workingDirectory: childCwd, wantErr: exec.ErrDot},
		pathCase{name: "missing child relative match allows later absolute entry", parentPath: emptyPath, childPath: childRelativePath, workingDirectory: t.TempDir()},
	)
	if runtime.GOOS != "windows" {
		root := t.TempDir()
		project := filepath.Join(root, "physical", "project")
		tools := filepath.Join(root, "physical", "tools")
		aliases := filepath.Join(root, "aliases")
		for _, path := range []string{project, tools, aliases} {
			if err := os.MkdirAll(path, 0o700); err != nil {
				t.Fatal(err)
			}
		}
		alias := filepath.Join(aliases, "current")
		if err := os.Symlink(project, alias); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(filepath.Join(tools, "node"), nil, 0o700); err != nil {
			t.Fatal(err)
		}
		cases = append(cases, pathCase{
			name: "symlinked child cwd preserves parent traversal", parentPath: emptyPath,
			childPath:        "../tools" + string(os.PathListSeparator) + nodePath,
			workingDirectory: alias, wantErr: exec.ErrDot,
		})
	}
	if runtime.GOOS == "windows" {
		relativeBin, err := os.MkdirTemp(".", ".sdk-node-path-")
		if err != nil {
			t.Fatal(err)
		}
		t.Cleanup(func() {
			if err := os.RemoveAll(relativeBin); err != nil {
				t.Error(err)
			}
		})
		nodeBytes, err := os.ReadFile(node)
		if err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(filepath.Join(relativeBin, "node.exe"), nodeBytes, 0o700); err != nil {
			t.Fatal(err)
		}
		absoluteBin, err := filepath.Abs(relativeBin)
		if err != nil {
			t.Fatal(err)
		}
		cases = append(cases,
			pathCase{name: "absolute PATH confirms the same executable", parentPath: emptyPath, childPath: relativeBin + ";" + absoluteBin},
			pathCase{name: "absolute PATH cannot replace a different relative executable", parentPath: emptyPath, childPath: relativeBin + ";" + nodePath, wantErr: exec.ErrDot},
		)
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			t.Setenv("PATH", tc.parentPath)
			marker := filepath.Join(t.TempDir(), "telemetry.jsonl")
			client := copilot.NewClient(&copilot.ClientOptions{
				Mode: copilot.ModeCodingAgent,
				Runtime: copilot.SpawnedStdioConnection{
					Path:             script,
					Args:             []string{marker, "stop", filepath.Join(t.TempDir(), "runtime.pid")},
					Env:              append(os.Environ(), "PATH="+tc.childPath),
					WorkingDirectory: tc.workingDirectory,
					BaseDirectory:    t.TempDir(),
				},
				GitHub: copilot.ClientGitHubOptions{UseLoggedInUser: copilot.Bool(false)},
			})
			t.Cleanup(client.ForceStop)
			ctx, cancel := context.WithTimeout(t.Context(), 10*time.Second)
			defer cancel()
			err := client.Start(ctx)
			if tc.wantErr != nil {
				if !errors.Is(err, tc.wantErr) {
					t.Fatalf("expected child PATH lookup error %v, got %v", tc.wantErr, err)
				}
				return
			}
			if err != nil {
				t.Fatal("Start failed with node on child PATH:", err)
			}
			if err := client.Stop(); err != nil {
				t.Fatal("Stop failed:", err)
			}
			if contents, err := os.ReadFile(marker); err != nil || string(contents) != "{\"type\":\"span\"}\n" {
				t.Fatalf("child did not complete shutdown: %q (error: %v)", contents, err)
			}
		})
	}
}

func TestSpawnedWindowsDriveRelativePath(t *testing.T) {
	if runtime.GOOS != "windows" {
		t.Skip("Windows drive-relative PATH lookup")
	}
	directory := t.TempDir()
	executable, err := os.CreateTemp(directory, "copilot-drive-relative-*.exe")
	if err != nil {
		t.Fatal(err)
	}
	if err := executable.Close(); err != nil {
		t.Fatal(err)
	}
	t.Chdir(directory)
	client := copilot.NewClient(&copilot.ClientOptions{
		Mode: copilot.ModeCodingAgent,
		Runtime: copilot.SpawnedStdioConnection{
			Path: filepath.Base(executable.Name()),
			Env:  []string{"PATH=" + filepath.VolumeName(directory), "PATHEXT=.EXE"},
		},
	})
	t.Cleanup(client.ForceStop)
	ctx, cancel := context.WithTimeout(t.Context(), 10*time.Second)
	defer cancel()
	if err := client.Start(ctx); !errors.Is(err, exec.ErrDot) {
		t.Fatalf("expected drive-relative executable rejection %v, got %v", exec.ErrDot, err)
	}
}

func TestOwnedStdioShutdown(t *testing.T) {
	node, err := exec.LookPath("node")
	if err != nil {
		t.Fatal("shutdown fixture requires Node.js:", err)
	}
	fixture, err := filepath.Abs("../test/harness/stdio-shutdown-runtime.cjs")
	if err != nil {
		t.Fatal(err)
	}

	for _, mode := range []string{"stop", "force", "fallback", "start-failure"} {
		t.Run(mode, func(t *testing.T) {
			directory := t.TempDir()
			marker := filepath.Join(directory, "telemetry.jsonl")
			pidFile := filepath.Join(directory, "runtime.pid")
			client := copilot.NewClient(&copilot.ClientOptions{Mode: copilot.ModeCodingAgent, GitHub: copilot.ClientGitHubOptions{UseLoggedInUser: copilot.Bool(false)}, Runtime: copilot.SpawnedStdioConnection{
				Path: node,
				Args: []string{fixture, marker, mode, pidFile},
			},
			})
			t.Cleanup(client.ForceStop)

			ctx, cancel := context.WithTimeout(t.Context(), 10*time.Second)
			defer cancel()
			err := client.Start(ctx)
			if mode == "start-failure" {
				if err == nil || !strings.Contains(err.Error(), "protocol version") {
					t.Fatalf("expected protocol version failure, got %v", err)
				}
			} else {
				if err != nil {
					t.Fatal("Start failed:", err)
				}
				started := time.Now()
				if mode == "force" {
					runShutdownWithWatchdog(t, func() error {
						client.ForceStop()
						return nil
					})
					if elapsed := time.Since(started); elapsed >= 10*time.Second {
						t.Fatalf("ForceStop waited for graceful timeout: %s", elapsed)
					}
				} else {
					runShutdownWithWatchdog(t, client.Stop)
				}
				if mode == "fallback" && time.Since(started) < 10*time.Second {
					t.Fatal("Stop did not allow the full graceful exit timeout")
				}
			}

			// Force-stop and failed startup kill without waiting for the child to be reaped.
			exitWait := "0"
			if mode == "force" || mode == "start-failure" {
				exitWait = "5000"
			}
			assertShutdownChildExited(t, node, pidFile, exitWait)
			contents, err := os.ReadFile(marker)
			if mode == "force" || mode == "start-failure" {
				if !os.IsNotExist(err) {
					t.Fatalf("forced termination unexpectedly finalized telemetry: %q (error: %v)", contents, err)
				}
			} else if err != nil || string(contents) != "{\"type\":\"span\"}\n" {
				t.Fatalf("Stop returned without EOF cleanup: %q (error: %v)", contents, err)
			}
			runShutdownWithWatchdog(t, client.Stop)
		})
	}
}

func runShutdownWithWatchdog(t *testing.T, stop func() error) {
	t.Helper()
	done := make(chan error, 1)
	go func() { done <- stop() }()
	select {
	case err := <-done:
		if err != nil {
			t.Fatal("shutdown failed:", err)
		}
	case <-time.After(40 * time.Second):
		// Cover shutdown RPC, graceful exit, and forced reap budgets, plus scheduling slack.
		t.Fatal("shutdown exceeded all cleanup budgets")
	}
}

func assertShutdownChildExited(t *testing.T, node, pidFile, waitMillis string) {
	t.Helper()
	ctx, cancel := context.WithTimeout(t.Context(), 10*time.Second)
	defer cancel()
	// Node's process probe is portable, unlike os.Process.Signal(0) on Windows.
	cmd := exec.CommandContext(ctx, node, "-e", `
const fs = require("node:fs");
const pid = Number(fs.readFileSync(process.argv[1], "utf8"));
const deadline = Date.now() + Number(process.argv[2]);
function check() {
    try {
        process.kill(pid, 0);
    } catch (error) {
        if (error.code === "ESRCH") return;
        throw error;
    }
    if (Date.now() >= deadline) throw new Error("Child still running");
    setTimeout(check, 25);
}
check();
`, pidFile, waitMillis)
	if output, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("child process did not exit: %v\n%s", err, output)
	}
}

// Stopping a client attached to an external server closes the client's own TCP
// connection. That close is expected, so shutdown must not report it on stdout
// or through the standard logger. The shutdown runs in a child process so the
// assertion sees exactly what it wrote to the process streams.
func TestExternalServerShutdownWritesNothing(t *testing.T) {
	const childKey = "COPILOT_SDK_TEST_EXTERNAL_SHUTDOWN"
	const marker = "external-server-shutdown"
	mode := os.Getenv(childKey)
	if mode == "" {
		for _, mode := range []string{"stop", "force"} {
			t.Run(mode, func(t *testing.T) {
				cmd := exec.CommandContext(t.Context(), os.Args[0], "-test.run=^TestExternalServerShutdownWritesNothing$")
				cmd.Env = append(os.Environ(), childKey+"="+mode)
				var stdout, stderr bytes.Buffer
				cmd.Stdout, cmd.Stderr = &stdout, &stderr
				if err := cmd.Run(); err != nil {
					t.Fatalf("shutdown child: %v\nstdout:\n%s\nstderr:\n%s", err, &stdout, &stderr)
				}
				for _, stream := range []struct {
					name   string
					output string
				}{{"stdout", stdout.String()}, {"stderr", stderr.String()}} {
					parts := strings.Split(stream.output, marker+"\n")
					if len(parts) != 3 {
						t.Fatalf("%s lacks the shutdown markers:\n%s", stream.name, stream.output)
					}
					if parts[1] != "" {
						t.Errorf("%s shutdown wrote to %s:\n%s", mode, stream.name, parts[1])
					}
				}
			})
		}
		return
	}

	peer := newResponseRetirementPeer(t, 0)
	client := copilot.NewClient(&copilot.ClientOptions{Mode: copilot.ModeCodingAgent, Runtime: copilot.TCPConnection{Host: "127.0.0.1", Port: peer.listener.Addr().(*net.TCPAddr).Port}})
	ctx, cancel := context.WithTimeout(t.Context(), 10*time.Second)
	defer cancel()
	if err := client.Start(ctx); err != nil {
		t.Fatal("start:", err)
	}

	fmt.Fprintln(os.Stdout, marker)
	fmt.Fprintln(os.Stderr, marker)
	if mode == "force" {
		client.ForceStop()
	} else if err := client.Stop(); err != nil {
		t.Error("stop:", err)
	}
	fmt.Fprintln(os.Stdout, marker)
	fmt.Fprintln(os.Stderr, marker)
}
