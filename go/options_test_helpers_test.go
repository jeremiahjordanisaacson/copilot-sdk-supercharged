package copilot

import "encoding/json"

// sessionWireGroup reads an optional request group without conflating an absent
// leaf with an explicit false, empty collection, or null leaf.
func sessionWireGroup[T any](fields map[string]T, name string) map[string]T {
	data, _ := json.Marshal(fields[name])
	var group map[string]T
	_ = json.Unmarshal(data, &group)
	return group
}
