//go:build copilot_no_inprocess || (!darwin && !linux && !windows) || (!amd64 && !arm64)

package testharness

func waitForInProcessCleanup() error {
	return nil
}

// PrepareForProcessWait is a no-op when the in-process runtime is unavailable.
func PrepareForProcessWait() {}

// ProtectProcessWait is a no-op when the in-process runtime is unavailable.
func ProtectProcessWait() func() {
	return func() {}
}
