package main

import (
	"context"
	"fmt"

	"github.com/microsoft/copilot-sdk/go/v2/rpc"
)

type commandLister interface {
	List(context.Context) (*rpc.CommandList, error)
}

var _ commandLister = (*rpc.ServerCommandsAPI)(nil)
var _ func(*rpc.ServerCommandsAPI, context.Context) (*rpc.CommandList, error) = (*rpc.ServerCommandsAPI).List

type sessionCommandLister interface {
	List(context.Context, *rpc.SessionCommandsListRequest) (*rpc.CommandList, error)
}

var _ sessionCommandLister = (*rpc.CommandsAPI)(nil)
var _ func(*rpc.CommandsAPI, context.Context, *rpc.SessionCommandsListRequest) (*rpc.CommandList, error) = (*rpc.CommandsAPI).List
var _ func(*rpc.CommandsAPI, context.Context, *rpc.SessionCommandsListRequest) (*rpc.CommandListWithMetadata, error) = (*rpc.CommandsAPI).ListWithMetadata

func main() {
	api := rpc.NewServerRPC(nil).Commands
	var list func(context.Context) (*rpc.CommandList, error) = api.List
	sessionAPI := rpc.NewSessionRPC(nil, "consumer-session").Commands
	var sessionList func(context.Context, *rpc.SessionCommandsListRequest) (*rpc.CommandList, error) = sessionAPI.List
	var sessionListWithMetadata func(context.Context, *rpc.SessionCommandsListRequest) (*rpc.CommandListWithMetadata, error) = sessionAPI.ListWithMetadata
	command := rpc.SlashCommandInfo{[]string{"md"}, true, "Choose a model", nil, nil, rpc.SlashCommandKindBuiltin, "model", nil}
	if command.Name != "model" || list == nil || sessionList == nil || sessionListWithMetadata == nil {
		panic("legacy command consumer changed")
	}
	fmt.Println("External command interfaces, function values, and positional literals work.")
}
