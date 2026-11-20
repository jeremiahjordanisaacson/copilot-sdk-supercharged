#![cfg(test)]
#![allow(clippy::unwrap_used)]

use copilot_sdk::rpc::{
    DiscoveredMcpServer, McpDiscoverOptions, McpDiscoverRequest, McpDiscoverResult,
    McpServerSource, Plugin, PluginList, PluginListResult, PluginsListRequest,
};
use copilot_sdk::session::Session;
use copilot_sdk::{Client, Error};
use serde_json::json;

async fn _legacy_discover(
    client: &Client,
    params: McpDiscoverRequest,
) -> Result<McpDiscoverResult, Error> {
    client.rpc().mcp().discover(params).await
}

async fn _configured_discover(
    client: &Client,
    params: McpDiscoverOptions,
) -> Result<McpDiscoverResult, Error> {
    client.rpc().mcp().discover_with_options(params).await
}

async fn _legacy_plugins(client: &Client) -> Result<PluginListResult, Error> {
    client.rpc().plugins().list().await
}

async fn _invocation_plugins(
    client: &Client,
    params: PluginsListRequest,
) -> Result<PluginListResult, Error> {
    client.rpc().plugins().list_with_params(params).await
}

async fn _session_plugins_with_verified_source(session: &Session) -> Result<PluginList, Error> {
    session.rpc().plugins().list().await
}

#[test]
fn discovery_results_support_exhaustive_literals_and_patterns() {
    let server = DiscoveredMcpServer {
        configuration: None,
        effective_source: None,
        enabled: true,
        name: "published".to_owned(),
        source: McpServerSource::User,
        source_plugin: None,
        source_plugin_version: None,
        r#type: None,
    };
    let result = McpDiscoverResult {
        diagnostics: None,
        servers: vec![server],
    };
    assert_eq!(
        serde_json::to_value(&result).unwrap(),
        json!({"servers": [{"name": "published", "source": "user", "enabled": true}]}),
    );
    let McpDiscoverResult {
        servers,
        diagnostics: _,
    } = result;
    let DiscoveredMcpServer {
        configuration: _,
        effective_source: _,
        enabled,
        name,
        source: _,
        source_plugin: _,
        source_plugin_version: _,
        r#type: _,
    } = servers.into_iter().next().unwrap();
    assert!(enabled);
    assert_eq!(name, "published");
    let result = PluginList {
        plugins: vec![Plugin {
            assignment: None,
            direct_source_id: None,
            enabled: true,
            id: None,
            installed: None,
            installed_from: None,
            managed: None,
            managed_desired_enabled: None,
            marketplace: "market".to_owned(),
            name: "published".to_owned(),
            root_path: None,
            source: None,
            team: None,
            verified_source: None,
            version: None,
        }],
    };
    assert_eq!(
        serde_json::to_value(&result).unwrap(),
        json!({"plugins": [{"name": "published", "marketplace": "market", "enabled": true}]})
    );
    let PluginList { plugins } = result;
    let Plugin {
        assignment: _,
        direct_source_id: _,
        enabled,
        id: _,
        installed: _,
        installed_from: _,
        managed: _,
        managed_desired_enabled: _,
        marketplace,
        name,
        root_path: _,
        source: _,
        team: _,
        verified_source: _,
        version: _,
    } = plugins.into_iter().next().unwrap();
    assert!(enabled);
    assert_eq!(marketplace, "market");
    assert_eq!(name, "published");
    let result = PluginListResult {
        plugins: vec![],
        warnings: None,
    };
    assert_eq!(
        serde_json::to_value(&result).unwrap(),
        json!({"plugins": []})
    );
    let PluginListResult {
        plugins,
        warnings: _,
    } = result;
    assert!(plugins.is_empty());
}

#[test]
fn invocation_results_retain_configuration_diagnostics_and_verified_source() {
    let wire = json!({
        "servers": [{
            "name": "configured",
            "source": "user",
            "enabled": true,
            "configuration": {"env": {"TOKEN": "owned-secret"}},
        }],
        "diagnostics": [],
    });
    let extended: McpDiscoverResult = serde_json::from_value(wire.clone()).unwrap();
    assert_eq!(
        extended.servers[0].configuration.as_ref().unwrap()["env"]["TOKEN"],
        "owned-secret",
    );
    assert_eq!(extended.diagnostics.unwrap().len(), 0);
    let canonical: McpDiscoverResult = serde_json::from_value(wire.clone()).unwrap();
    assert_eq!(serde_json::to_value(canonical).unwrap(), wire,);

    let wire = json!({
        "plugins": [{
            "name": "pinned",
            "marketplace": "",
            "enabled": true,
            "verifiedSource": {
                "repository": "owner/plugin",
                "revision": "0123456789012345678901234567890123456789",
            },
        }],
        "warnings": ["owned warning"],
    });
    let extended: PluginListResult = serde_json::from_value(wire.clone()).unwrap();
    assert_eq!(extended.warnings.unwrap(), vec!["owned warning"]);
    assert_eq!(
        extended.plugins[0]
            .verified_source
            .as_ref()
            .unwrap()
            .repository,
        "owner/plugin",
    );
    let session_plugins: PluginList = serde_json::from_value(wire.clone()).unwrap();
    assert_eq!(
        session_plugins.plugins[0]
            .verified_source
            .as_ref()
            .unwrap()
            .revision,
        "0123456789012345678901234567890123456789",
    );
    let canonical: PluginList = serde_json::from_value(wire.clone()).unwrap();
    assert_eq!(
        serde_json::to_value(canonical).unwrap(),
        json!({"plugins": wire["plugins"]}),
    );
    let canonical: PluginListResult = serde_json::from_value(wire.clone()).unwrap();
    assert_eq!(serde_json::to_value(canonical).unwrap(), wire);
}
