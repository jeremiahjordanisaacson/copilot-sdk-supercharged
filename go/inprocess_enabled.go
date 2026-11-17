//go:build !copilot_no_inprocess && (darwin || linux || windows) && (amd64 || arm64)

package copilot

import "github.com/microsoft/copilot-sdk/go/v2/internal/ffihost"

const inProcessAvailable = true

func createInProcessHost(runtimePath, cliEntrypoint string, config inProcessHostConfig) (inProcessHost, error) {
	return ffihost.Create(runtimePath, cliEntrypoint, config.Environment, config.Args)
}
