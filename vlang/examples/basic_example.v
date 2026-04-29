// V SDK basic example -- interactive chat with Copilot CLI
//
// Build:  v run examples/basic_example.v
// Or:     v -o chat examples/basic_example.v && ./chat

import copilot
import os
import readline

fn main() {
	// Create client -- connect to an existing server or spawn the CLI
	mut opts := copilot.CopilotClientOptions{}
	cli_url := os.getenv('COPILOT_CLI_URL')
	if cli_url.len > 0 {
		opts.cli_url = cli_url
	}

	mut client := copilot.new_client(opts)
	client.start() or {
		eprintln('Failed to start client: ${err}')
		exit(1)
	}
	defer {
		client.stop() or {}
	}

	// Define a simple tool
	mut params := map[string]copilot.ToolParam{}
	params['expression'] = copilot.ToolParam{
		param_type: 'string'
		description: 'A math expression to evaluate'
		required: true
	}
	calc_tool := copilot.define_tool('calculator', 'Evaluate a math expression', params,
		fn (args map[string]string) !string {
		expr := args['expression'] or { return error('missing expression') }
		return '{"result": "computed: ${expr}"}'
	})

	// Create a session
	config := copilot.SessionConfig{
		model: 'gpt-4'
		streaming: true
		tools: [calc_tool]
		on_permission_request: copilot.approve_all
	}

	mut session := client.create_session(config) or {
		eprintln('Failed to create session: ${err}')
		exit(1)
	}
	defer {
		session.disconnect() or {}
	}

	// Register event handler for streaming output
	session.on(fn (ev copilot.SessionEvent) {
		match ev.event_type {
			'assistant.message_delta' {
				print(ev.raw_data)
			}
			'assistant.message' {
				println('\n---')
			}
			else {}
		}
	})

	println('Copilot V SDK Chat (type "quit" to exit)')
	println('=========================================')

	// REPL loop
	mut reader := readline.Readline{}
	for {
		line := reader.read_line('> ') or { break }
		input := line.trim_space()
		if input == 'quit' || input == 'exit' {
			break
		}
		if input.len == 0 {
			continue
		}

		session.send(copilot.SendOptions{ prompt: input }) or {
			eprintln('Send error: ${err}')
		}
	}

	println('Goodbye!')
}
