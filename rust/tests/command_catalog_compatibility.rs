#![cfg(test)]

use copilot_sdk::rpc::{CommandList, SlashCommandInfo, SlashCommandKind};
use copilot_sdk::session::Session;
use copilot_sdk::{Client, Error};

#[test]
fn command_catalog_preserves_exhaustive_literals_and_patterns() {
    let command = SlashCommandInfo {
        aliases: None,
        allow_during_agent_execution: true,
        description: "Select a model".into(),
        experimental: None,
        input: None,
        kind: SlashCommandKind::Builtin,
        name: "model".into(),
        schedulable: None,
    };
    let CommandList { commands } = CommandList {
        commands: vec![command],
    };
    for command in commands {
        let SlashCommandInfo {
            aliases: _,
            allow_during_agent_execution: _,
            description: _,
            experimental: _,
            input: _,
            kind: _,
            name,
            schedulable: _,
        } = command;
        assert_eq!(name, "model");
    }
}

async fn _command_catalog_entry_points_compile(
    client: &Client,
    session: &Session,
) -> Result<(), Error> {
    use copilot_sdk::rpc::CommandListWithMetadata;

    let _: CommandList = client.rpc().commands().list().await?;
    let _: CommandList = session.rpc().commands().list().await?;
    let _: CommandList = session
        .rpc()
        .commands()
        .list_with_params(Default::default())
        .await?;
    let _: CommandListWithMetadata = client
        .rpc()
        .commands()
        .list_with_params(Default::default())
        .await?;
    let _: CommandListWithMetadata = client.rpc().commands().list_with_metadata().await?;
    let _: CommandListWithMetadata = session.rpc().commands().list_with_metadata().await?;
    let _: CommandListWithMetadata = session
        .rpc()
        .commands()
        .list_with_metadata_with_params(Default::default())
        .await?;
    Ok(())
}
