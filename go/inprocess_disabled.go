//go:build copilot_no_inprocess || (!darwin && !linux && !windows) || (!amd64 && !arm64)

package copilot

import "errors"

const inProcessAvailable = false

var errInProcessUnavailable = errors.New("in-process transport unavailable")

func createInProcessHost(string, string, inProcessHostConfig) (inProcessHost, error) {
	return nil, errInProcessUnavailable
}
