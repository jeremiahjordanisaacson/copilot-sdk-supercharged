# Advanced v2.0 Features

Recipes for the advanced features introduced in v2.0 of the Copilot SDK.

## Per-Session Authentication

Scope auth tokens per session for multi-tenant applications:

```rust
use copilot_sdk::{CopilotClient, SessionConfig};

let client = CopilotClient::new()?;

let config = SessionConfig {
    github_token: Some("ghu_tenant_abc_token_here".to_string()),
    ..Default::default()
};

let session = client.create_session(&config).await?;
// This session authenticates as a specific tenant user
session.send("Hello from tenant ABC").await?;
```

## Session Idle Timeout

Auto-cleanup inactive sessions with a configurable timeout:

```rust
let config = SessionConfig {
    session_idle_timeout_seconds: Some(600), // 10 minutes
    ..Default::default()
};

let session = client.create_session(&config).await?;
// Session automatically closes after 600 seconds of inactivity
```

## SessionFs (Session Filesystem)

Provide a session-scoped filesystem with 10 I/O operations (readFile, writeFile, appendFile, exists, stat, mkdir, readdir, readdirWithTypes, rm, rename):

```rust
use copilot_sdk::{SessionFs, FsProvider, DirEntry, FileStat};
use async_trait::async_trait;

struct MyFsProvider {
    base_path: PathBuf,
}

#[async_trait]
impl FsProvider for MyFsProvider {
    async fn read_file(&self, path: &str) -> Result<Vec<u8>> {
        tokio::fs::read(self.base_path.join(path)).await.map_err(Into::into)
    }

    async fn write_file(&self, path: &str, content: &[u8]) -> Result<()> {
        tokio::fs::write(self.base_path.join(path), content).await.map_err(Into::into)
    }

    async fn append_file(&self, path: &str, content: &[u8]) -> Result<()> {
        let mut file = tokio::fs::OpenOptions::new()
            .append(true)
            .open(self.base_path.join(path))
            .await?;
        file.write_all(content).await.map_err(Into::into)
    }

    async fn exists(&self, path: &str) -> Result<bool> {
        Ok(self.base_path.join(path).exists())
    }

    async fn stat(&self, path: &str) -> Result<FileStat> {
        let meta = tokio::fs::metadata(self.base_path.join(path)).await?;
        Ok(FileStat {
            size: meta.len(),
            is_directory: meta.is_dir(),
        })
    }

    async fn mkdir(&self, path: &str) -> Result<()> {
        tokio::fs::create_dir_all(self.base_path.join(path)).await.map_err(Into::into)
    }

    async fn readdir(&self, path: &str) -> Result<Vec<String>> {
        let mut entries = Vec::new();
        let mut dir = tokio::fs::read_dir(self.base_path.join(path)).await?;
        while let Some(entry) = dir.next_entry().await? {
            entries.push(entry.file_name().to_string_lossy().to_string());
        }
        Ok(entries)
    }

    async fn readdir_with_types(&self, path: &str) -> Result<Vec<DirEntry>> {
        let mut entries = Vec::new();
        let mut dir = tokio::fs::read_dir(self.base_path.join(path)).await?;
        while let Some(entry) = dir.next_entry().await? {
            let file_type = entry.file_type().await?;
            entries.push(DirEntry {
                name: entry.file_name().to_string_lossy().to_string(),
                is_directory: file_type.is_dir(),
            });
        }
        Ok(entries)
    }

    async fn rm(&self, path: &str) -> Result<()> {
        let full = self.base_path.join(path);
        if full.is_dir() {
            tokio::fs::remove_dir_all(full).await?;
        } else {
            tokio::fs::remove_file(full).await?;
        }
        Ok(())
    }

    async fn rename(&self, old_path: &str, new_path: &str) -> Result<()> {
        tokio::fs::rename(self.base_path.join(old_path), self.base_path.join(new_path))
            .await
            .map_err(Into::into)
    }
}

let config = SessionConfig {
    session_fs: Some(Box::new(MyFsProvider {
        base_path: PathBuf::from("/workspace"),
    })),
    ..Default::default()
};

let session = client.create_session(&config).await?;
```

## Commands and UI Elicitation

Register slash commands and handle interactive prompts/dialogs:

```rust
use copilot_sdk::{Command, ElicitationRequest};

let commands = vec![
    Command::new("deploy", "Deploy the current project"),
    Command::new("test", "Run the test suite"),
];

let config = SessionConfig {
    commands: Some(commands),
    on_elicitation_request: Some(Box::new(|request: ElicitationRequest| {
        Box::pin(async move {
            let mut response = std::collections::HashMap::new();
            response.insert("environment".to_string(), serde_json::json!("staging"));
            response.insert("confirmed".to_string(), serde_json::json!(true));
            Ok(response)
        })
    })),
    ..Default::default()
};

let session = client.create_session(&config).await?;
```

## System Prompt Customization

Fine-grained control with append, replace, and customize modes:

### Replace mode

```rust
let config = SessionConfig {
    system_prompt: Some("You are a Rust code review assistant. Focus on safety and performance.".to_string()),
    system_prompt_mode: Some("replace".to_string()),
    ..Default::default()
};

let session = client.create_session(&config).await?;
```

### Customize mode with sections

```rust
use copilot_sdk::SystemPromptSection;

let sections = vec![
    SystemPromptSection::new("role", "You are an expert Rust developer."),
    SystemPromptSection::new("constraints", "Always suggest safe, zero-cost abstractions."),
    SystemPromptSection::new("output", "Format responses as Markdown with code blocks."),
];

let config = SessionConfig {
    system_prompt_sections: Some(sections),
    system_prompt_mode: Some("customize".to_string()),
    ..Default::default()
};

let session = client.create_session(&config).await?;
```

## Per-Agent Skills

Preload skill content into agent context at startup:

```rust
let config = SessionConfig {
    skill_directories: Some(vec![
        "/workspace/.copilot/skills".to_string(),
        "/workspace/custom-skills".to_string(),
    ]),
    disabled_skills: Some(vec!["deprecated-skill".to_string()]),
    ..Default::default()
};

let session = client.create_session(&config).await?;
```

## Per-Agent Tool Visibility

Control tool access per agent with excludedTools:

```rust
use copilot_sdk::AgentConfig;

let default_agent = AgentConfig {
    excluded_tools: Some(vec![
        "shell_exec".to_string(),
        "file_delete".to_string(),
        "network_request".to_string(),
    ]),
    ..Default::default()
};

let config = SessionConfig {
    default_agent: Some(default_agent),
    ..Default::default()
};

let session = client.create_session(&config).await?;
```

## Runtime Request Headers

Send custom HTTP headers per message turn:

```rust
use copilot_sdk::SendOptions;
use std::collections::HashMap;

let mut headers = HashMap::new();
headers.insert("X-Request-Id".to_string(), uuid::Uuid::new_v4().to_string());
headers.insert("X-Tenant-Id".to_string(), "tenant-abc-123".to_string());

let options = SendOptions {
    request_headers: Some(headers),
    ..Default::default()
};

session.send_with_options("Analyze this code", &options).await?;
```

## Model Capabilities Override

Deep-merge overrides for model feature flags:

```rust
let capabilities = serde_json::json!({
    "streaming": true,
    "functionCalling": true,
    "vision": false,
    "maxTokens": 8192
});

let config = SessionConfig {
    model_capabilities: Some(capabilities),
    ..Default::default()
};

let session = client.create_session(&config).await?;
```

## Config Discovery

Auto-detect MCP servers and skill directories from workspace:

```rust
let config = SessionConfig {
    enable_config_discovery: Some(true),
    ..Default::default()
};

let session = client.create_session(&config).await?;
// MCP servers and skills from .copilot/ in the workspace are loaded automatically
```

## Sub-Agent Streaming Events

Control streaming event forwarding from sub-agents:

```rust
let config = SessionConfig {
    include_sub_agent_streaming_events: Some(true),
    ..Default::default()
};

let session = client.create_session(&config).await?;

session.on_event("assistant.message_delta", |event| {
    // Receives deltas from both the primary agent and sub-agents
    print!("{}", event.delta);
});
```

## Session Metadata

Retrieve session metadata via RPC:

```rust
let session = client.create_session(&config).await?;
session.send("Hello").await?;

let metadata = session.get_session_metadata().await?;
println!("Session ID: {}", metadata.session_id);
println!("Created at: {}", metadata.created_at);
println!("Turn count: {}", metadata.turn_count);
```

## MCP Server Configuration

Configure MCP servers with stdio and HTTP transport types:

```rust
use copilot_sdk::McpServerConfig;

let mcp_servers = vec![
    // Stdio transport
    McpServerConfig {
        name: "local-tools".to_string(),
        transport_type: "stdio".to_string(),
        command: Some("node".to_string()),
        args: Some(vec!["./mcp-server/index.js".to_string()]),
        ..Default::default()
    },
    // HTTP transport
    McpServerConfig {
        name: "remote-tools".to_string(),
        transport_type: "http".to_string(),
        url: Some("https://mcp.example.com/api".to_string()),
        headers: Some(HashMap::from([
            ("Authorization".to_string(), format!("Bearer {}", token)),
        ])),
        ..Default::default()
    },
];

let config = SessionConfig {
    mcp_servers: Some(mcp_servers),
    ..Default::default()
};

let session = client.create_session(&config).await?;
```

## Image Generation

Configure response format and image options:

```rust
let config = SessionConfig {
    response_format: Some("image".to_string()),
    ..Default::default()
};

let session = client.create_session(&config).await?;

let response = session.send("Generate a UML class diagram for a user auth system").await?;
// Response contains image data
if let Some(image_data) = response.image_data {
    tokio::fs::write("diagram.png", &image_data).await?;
}
```
