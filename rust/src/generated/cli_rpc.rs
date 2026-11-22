//! Auto-generated typed JSON-RPC namespace — do not edit manually.
//!
//! Generated from `api.schema.json` by `scripts/codegen/rust.ts`. The
//! [`ClientRpc`] and [`SessionRpc`] view structs let callers reach every
//! protocol method through a typed namespace tree, so wire method names
//! and request/response shapes live in exactly one place — this file.

#![allow(missing_docs)]
#![allow(clippy::too_many_arguments)]
#![allow(deprecated)]
#![allow(dead_code)]

use super::cli_api_types::{rpc_methods, *};
use crate::session::Session;
use crate::{Client, Error};

/// Session method paths for in-process host adapters.
#[doc(hidden)]
pub const SESSION_METHOD_PATHS: &[(&str, &[&str])] = &[];

/// Typed view over the [`Client`]'s server-level RPC namespace.
#[derive(Clone, Copy)]
pub struct ClientRpc<'a> {
    pub(crate) client: &'a Client,
}

impl<'a> ClientRpc<'a> {
    /// `acp.*` sub-namespace.
    pub fn acp(&self) -> ClientRpcAcp<'a> {
        ClientRpcAcp {
            client: self.client,
        }
    }

    /// `agents.*` sub-namespace.
    pub fn agents(&self) -> ClientRpcAgents<'a> {
        ClientRpcAgents {
            client: self.client,
        }
    }

    /// `ahp.*` sub-namespace.
    pub fn ahp(&self) -> ClientRpcAhp<'a> {
        ClientRpcAhp {
            client: self.client,
        }
    }

    /// `cafeMetrics.*` sub-namespace.
    pub fn cafe_metrics(&self) -> ClientRpcCafeMetrics<'a> {
        ClientRpcCafeMetrics {
            client: self.client,
        }
    }

    /// `mcpSource.*` sub-namespace.
    pub fn mcp_source(&self) -> ClientRpcMcpSource<'a> {
        ClientRpcMcpSource {
            client: self.client,
        }
    }

    /// `providersSource.*` sub-namespace.
    pub fn providers_source(&self) -> ClientRpcProvidersSource<'a> {
        ClientRpcProvidersSource {
            client: self.client,
        }
    }

    /// `session.*` sub-namespace.
    pub fn session(&self) -> ClientRpcSession<'a> {
        ClientRpcSession {
            client: self.client,
        }
    }
}

/// `acp.*` RPCs.
#[derive(Clone, Copy)]
pub struct ClientRpcAcp<'a> {
    pub(crate) client: &'a Client,
}

impl<'a> ClientRpcAcp<'a> {
    /// Serves ACP through the original trusted SDK host's engine until its transport ends. Closes ACP sessions before completion. Refuses other peers and unrelated engines.
    ///
    /// Wire method: `acp.serve`.
    ///
    /// # Parameters
    ///
    /// * `params` - ACP transport configuration for an existing in-process SDK engine.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn serve(&self, params: AcpServeRequest) -> Result<(), Error> {
        let wire_params = serde_json::to_value(params)?;
        let _: serde::de::IgnoredAny = self
            .client
            .call_typed(rpc_methods::ACP_SERVE, Some(wire_params))
            .await?;
        Ok(())
    }

    /// Stops the original trusted SDK host's ACP transport and waits for its sessions to close.
    ///
    /// Wire method: `acp.shutdown`.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn shutdown(&self) -> Result<(), Error> {
        let wire_params = serde_json::json!({});
        let _: serde::de::IgnoredAny = self
            .client
            .call_typed(rpc_methods::ACP_SHUTDOWN, Some(wire_params))
            .await?;
        Ok(())
    }
}

/// `agents.*` RPCs.
#[derive(Clone, Copy)]
pub struct ClientRpcAgents<'a> {
    pub(crate) client: &'a Client,
}

impl<'a> ClientRpcAgents<'a> {
    /// Lists the agents this runtime ships, by name. A consumer separating shipped agents from ones the user or a plugin authored should compare against these names rather than against `AgentInfo.source`: an authored agent may carry the `builtin` source while not being one of these, and the runtime treats the two as separate questions. `disableableNames` is the subset a user may turn off, which a client needs to decide whether to offer a toggle. `yamlBasedNames` is the subset backed by a shipped YAML definition, which a client needs before asking the runtime to load one.
    ///
    /// Wire method: `agents.getBuiltins`.
    ///
    /// # Returns
    ///
    /// The agents this runtime ships, named so a consumer can tell them apart from authored ones.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn get_builtins(&self) -> Result<AgentsGetBuiltinsResult, Error> {
        let wire_params = serde_json::json!({});
        self.client
            .call_typed(rpc_methods::AGENTS_GETBUILTINS, Some(wire_params))
            .await
    }

    /// Lists the shipped agents a client should offer right now, filtered by the feature flags it passes. `getBuiltins` names every agent the runtime knows about; some of those are gated, so a client rendering a picker wants this narrower list together with the description to show beside each name.
    ///
    /// Wire method: `agents.getAvailableBuiltins`.
    ///
    /// # Parameters
    ///
    /// * `params` - The feature flags to evaluate shipped agents against.
    ///
    /// # Returns
    ///
    /// The shipped agents available under the requested flags.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn get_available_builtins(
        &self,
        params: AgentsGetAvailableBuiltinsRequest,
    ) -> Result<AgentsGetAvailableBuiltinsResult, Error> {
        let wire_params = serde_json::to_value(params)?;
        self.client
            .call_typed(rpc_methods::AGENTS_GETAVAILABLEBUILTINS, Some(wire_params))
            .await
    }

    /// Loads one shipped agent's YAML definition, for a client that needs what the agent declares rather than only its name. `getBuiltins` reports which names have a definition to load: a name outside its `yamlBasedNames` is special-cased in code and has none. The definition crosses as its own JSON rather than as contract-typed fields, because the runtime parses it with the agent schema's tolerant shape and re-typing it here would drop the keys that shape accepts and this one does not. The projected `__nativeCustomAgent` view the runtime derives is included, so a caller reading the declared model and a caller rendering the agent see the same definition.
    ///
    /// Wire method: `agents.getBuiltinDefinition`.
    ///
    /// # Parameters
    ///
    /// * `params` - The shipped agent whose definition to load.
    ///
    /// # Returns
    ///
    /// One shipped agent's definition.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn get_builtin_definition(
        &self,
        params: AgentsGetBuiltinDefinitionRequest,
    ) -> Result<AgentsGetBuiltinDefinitionResult, Error> {
        let wire_params = serde_json::to_value(params)?;
        self.client
            .call_typed(rpc_methods::AGENTS_GETBUILTINDEFINITION, Some(wire_params))
            .await
    }

    /// Resolves the model a custom agent asks for against the models actually available, and answers both the model to switch to and the warning a user should see when the agent's preference cannot be met. A custom agent may name several acceptable models in preference order, so the decision is a match rather than a lookup, and an agent whose preference is unavailable is a normal outcome that produces a warning rather than an error. A host must call this rather than pick the first available name itself, because the preference order and the wording of the warning are what keep one installation's agent selection the same as another's.
    ///
    /// Wire method: `agents.customAgentInitialModelDecision`.
    ///
    /// # Parameters
    ///
    /// * `params` - The models a custom agent asks for, and the models actually available.
    ///
    /// # Returns
    ///
    /// The model to switch to, and the warning to show when the agent's preference could not be met.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn custom_agent_initial_model_decision(
        &self,
        params: AgentsCustomAgentInitialModelDecisionParams,
    ) -> Result<AgentsCustomAgentInitialModelDecisionResult, Error> {
        let wire_params = serde_json::to_value(params)?;
        self.client
            .call_typed(
                rpc_methods::AGENTS_CUSTOMAGENTINITIALMODELDECISION,
                Some(wire_params),
            )
            .await
    }
}

/// `ahp.*` RPCs.
#[derive(Clone, Copy)]
pub struct ClientRpcAhp<'a> {
    pub(crate) client: &'a Client,
}

impl<'a> ClientRpcAhp<'a> {
    /// `ahp.connectionLogs.*` sub-namespace.
    pub fn connection_logs(&self) -> ClientRpcAhpConnectionLogs<'a> {
        ClientRpcAhpConnectionLogs {
            client: self.client,
        }
    }

    /// `ahp.connections.*` sub-namespace.
    pub fn connections(&self) -> ClientRpcAhpConnections<'a> {
        ClientRpcAhpConnections {
            client: self.client,
        }
    }
}

/// `ahp.connectionLogs.*` RPCs.
#[derive(Clone, Copy)]
pub struct ClientRpcAhpConnectionLogs<'a> {
    pub(crate) client: &'a Client,
}

impl<'a> ClientRpcAhpConnectionLogs<'a> {
    /// Opens a redacted diagnostic log owned by this admitted SDK connection, before any relay authentication starts.
    ///
    /// Wire method: `ahp.connectionLogs.open`.
    ///
    /// # Parameters
    ///
    /// * `params` - Opens a diagnostic log before outgoing authentication starts.
    ///
    /// # Returns
    ///
    /// Identifies a diagnostic log owned by the originating SDK connection.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn open(
        &self,
        params: AhpConnectionLogOpenRequest,
    ) -> Result<AhpConnectionLogRequest, Error> {
        let wire_params = serde_json::to_value(params)?;
        self.client
            .call_typed(rpc_methods::AHP_CONNECTIONLOGS_OPEN, Some(wire_params))
            .await
    }

    /// Appends diagnostic text through the runtime's live secret filter to an owned connection log.
    ///
    /// Wire method: `ahp.connectionLogs.write`.
    ///
    /// # Parameters
    ///
    /// * `params` - Appends a redacted diagnostic message to a connection log.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn write(&self, params: AhpConnectionLogWriteRequest) -> Result<(), Error> {
        let wire_params = serde_json::to_value(params)?;
        let _: serde::de::IgnoredAny = self
            .client
            .call_typed(rpc_methods::AHP_CONNECTIONLOGS_WRITE, Some(wire_params))
            .await?;
        Ok(())
    }

    /// Waits for an owned connection log's admitted writes to reach its file.
    ///
    /// Wire method: `ahp.connectionLogs.flush`.
    ///
    /// # Parameters
    ///
    /// * `params` - Identifies a diagnostic log owned by the originating SDK connection.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn flush(&self, params: AhpConnectionLogRequest) -> Result<(), Error> {
        let wire_params = serde_json::to_value(params)?;
        let _: serde::de::IgnoredAny = self
            .client
            .call_typed(rpc_methods::AHP_CONNECTIONLOGS_FLUSH, Some(wire_params))
            .await?;
        Ok(())
    }

    /// Flushes and closes an owned connection log. The log lifetime is independent of relay authentication and retries.
    ///
    /// Wire method: `ahp.connectionLogs.close`.
    ///
    /// # Parameters
    ///
    /// * `params` - Identifies a diagnostic log owned by the originating SDK connection.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn close(&self, params: AhpConnectionLogRequest) -> Result<(), Error> {
        let wire_params = serde_json::to_value(params)?;
        let _: serde::de::IgnoredAny = self
            .client
            .call_typed(rpc_methods::AHP_CONNECTIONLOGS_CLOSE, Some(wire_params))
            .await?;
        Ok(())
    }
}

/// `ahp.connections.*` RPCs.
#[derive(Clone, Copy)]
pub struct ClientRpcAhpConnections<'a> {
    pub(crate) client: &'a Client,
}

impl<'a> ClientRpcAhpConnections<'a> {
    /// Creates a session at the remote AHP source and returns its refreshed catalog state. This does not create a local SDK attachment.
    ///
    /// Wire method: `ahp.connections.createSession`.
    ///
    /// # Parameters
    ///
    /// * `params` - Creates a session at the remote source, not a local SDK attachment.
    ///
    /// # Returns
    ///
    /// A session created at the remote source and its refreshed connection snapshot.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn create_session(
        &self,
        params: AhpConnectionCreateSessionRequest,
    ) -> Result<AhpConnectionCreateSessionResult, Error> {
        let wire_params = serde_json::to_value(params)?;
        self.client
            .call_typed(
                rpc_methods::AHP_CONNECTIONS_CREATESESSION,
                Some(wire_params),
            )
            .await
    }

    /// Selects a source catalog session without subscribing to its complete state. A null summary means no session matched.
    ///
    /// Wire method: `ahp.connections.selectSession`.
    ///
    /// # Parameters
    ///
    /// * `params` - Selects a remote source session without opening a session subscription.
    ///
    /// # Returns
    ///
    /// The selected source catalog row.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn select_session(
        &self,
        params: AhpConnectionSelectSessionRequest,
    ) -> Result<AhpConnectionSelectSessionResult, Error> {
        let wire_params = serde_json::to_value(params)?;
        self.client
            .call_typed(
                rpc_methods::AHP_CONNECTIONS_SELECTSESSION,
                Some(wire_params),
            )
            .await
    }

    /// Disposes a session at the remote source for every attached client. This is not a local SDK detach.
    ///
    /// Wire method: `ahp.connections.disposeSession`.
    ///
    /// # Parameters
    ///
    /// * `params` - Disposes a remote source session for every attached client.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn dispose_session(
        &self,
        params: AhpConnectionDisposeSessionRequest,
    ) -> Result<(), Error> {
        let wire_params = serde_json::to_value(params)?;
        let _: serde::de::IgnoredAny = self
            .client
            .call_typed(
                rpc_methods::AHP_CONNECTIONS_DISPOSESESSION,
                Some(wire_params),
            )
            .await?;
        Ok(())
    }

    /// Creates an inert outgoing AHP connection owned by this admitted SDK connection. Uses its existing original auth owner and never accepts a setup bearer.
    ///
    /// Wire method: `ahp.connections.create`.
    ///
    /// # Parameters
    ///
    /// * `params` - Creates an inert outgoing connection under the caller's existing runtime auth owner.
    ///
    /// # Returns
    ///
    /// The connection's current protocol snapshot and negotiated client identity.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn create(
        &self,
        params: AhpConnectionCreateRequest,
    ) -> Result<AhpConnectionStateResult, Error> {
        let wire_params = serde_json::to_value(params)?;
        self.client
            .call_typed(rpc_methods::AHP_CONNECTIONS_CREATE, Some(wire_params))
            .await
    }

    /// Attaches a runtime-owned SDK session facade to a session on the caller's outgoing AHP connection. Session state, history, approvals, and turn lifetimes stay in the runtime.
    ///
    /// Wire method: `ahp.connections.attachSession`.
    ///
    /// # Parameters
    ///
    /// * `params` - Attaches a runtime session facade to an existing AHP session.
    ///
    /// # Returns
    ///
    /// A registered runtime session and its independent attachment lease.
    /// Read session.remote.readProjection before using host state.
    /// The first read waits for subscription readiness and reports initialization errors.
    /// Release the lease if initialization fails.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn attach_session(
        &self,
        params: AhpSessionAttachRequest,
    ) -> Result<AhpSessionAttachResult, Error> {
        let wire_params = serde_json::to_value(params)?;
        self.client
            .call_typed(
                rpc_methods::AHP_CONNECTIONS_ATTACHSESSION,
                Some(wire_params),
            )
            .await
    }

    /// Releases the caller's AHP session facade, settles pending requests, and releases its subscription without disposing the host session.
    ///
    /// Wire method: `ahp.connections.detachSession`.
    ///
    /// # Parameters
    ///
    /// * `params` - Releases this caller's runtime facade without disposing the host's session.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn detach_session(&self, params: AhpSessionDetachRequest) -> Result<(), Error> {
        let wire_params = serde_json::to_value(params)?;
        let _: serde::de::IgnoredAny = self
            .client
            .call_typed(
                rpc_methods::AHP_CONNECTIONS_DETACHSESSION,
                Some(wire_params),
            )
            .await?;
        Ok(())
    }

    /// Connects and initializes an owned outgoing AHP connection. Connection errors remain in the returned state.
    ///
    /// Wire method: `ahp.connections.connect`.
    ///
    /// # Parameters
    ///
    /// * `params` - Selects an outgoing connection owned by this SDK connection.
    ///
    /// # Returns
    ///
    /// The connection's current protocol snapshot and negotiated client identity.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn connect(
        &self,
        params: AhpConnectionRequest,
    ) -> Result<AhpConnectionStateResult, Error> {
        let wire_params = serde_json::to_value(params)?;
        self.client
            .call_typed(rpc_methods::AHP_CONNECTIONS_CONNECT, Some(wire_params))
            .await
    }

    /// Reconnects the same outgoing client and restores its active subscriptions.
    ///
    /// Wire method: `ahp.connections.reconnect`.
    ///
    /// # Parameters
    ///
    /// * `params` - Selects an outgoing connection owned by this SDK connection.
    ///
    /// # Returns
    ///
    /// The connection's current protocol snapshot and negotiated client identity.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn reconnect(
        &self,
        params: AhpConnectionRequest,
    ) -> Result<AhpConnectionStateResult, Error> {
        let wire_params = serde_json::to_value(params)?;
        self.client
            .call_typed(rpc_methods::AHP_CONNECTIONS_RECONNECT, Some(wire_params))
            .await
    }

    /// Refreshes the outgoing connection's session catalog.
    ///
    /// Wire method: `ahp.connections.refresh`.
    ///
    /// # Parameters
    ///
    /// * `params` - Selects an outgoing connection owned by this SDK connection.
    ///
    /// # Returns
    ///
    /// The connection's current protocol snapshot and negotiated client identity.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn refresh(
        &self,
        params: AhpConnectionRequest,
    ) -> Result<AhpConnectionStateResult, Error> {
        let wire_params = serde_json::to_value(params)?;
        self.client
            .call_typed(rpc_methods::AHP_CONNECTIONS_REFRESH, Some(wire_params))
            .await
    }

    /// Authenticates pending protected resources with this connection's renewable account-bound credential. Reports failures per resource.
    ///
    /// Wire method: `ahp.connections.authenticate`.
    ///
    /// # Parameters
    ///
    /// * `params` - Authenticates pending protected resources with the connection's renewable credential source.
    ///
    /// # Returns
    ///
    /// Authentication outcome for each protected resource.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn authenticate(
        &self,
        params: AhpConnectionAuthenticateRequest,
    ) -> Result<AhpConnectionsAuthenticateResult, Error> {
        let wire_params = serde_json::to_value(params)?;
        self.client
            .call_typed(rpc_methods::AHP_CONNECTIONS_AUTHENTICATE, Some(wire_params))
            .await
    }

    /// Waits for the next connection-owned event batch. Only one reader may wait at a time. Disposal releases the waiter.
    ///
    /// Wire method: `ahp.connections.readEvents`.
    ///
    /// # Parameters
    ///
    /// * `params` - Selects an outgoing connection owned by this SDK connection.
    ///
    /// # Returns
    ///
    /// Events admitted for this connection, in protocol order.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn read_events(
        &self,
        params: AhpConnectionRequest,
    ) -> Result<AhpConnectionReadEventsResult, Error> {
        let wire_params = serde_json::to_value(params)?;
        self.client
            .call_typed(rpc_methods::AHP_CONNECTIONS_READEVENTS, Some(wire_params))
            .await
    }

    /// Disposes an owned outgoing connection, cancels its protocol work, and releases its subscriptions.
    ///
    /// Wire method: `ahp.connections.dispose`.
    ///
    /// # Parameters
    ///
    /// * `params` - Selects an outgoing connection owned by this SDK connection.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn dispose(&self, params: AhpConnectionRequest) -> Result<(), Error> {
        let wire_params = serde_json::to_value(params)?;
        let _: serde::de::IgnoredAny = self
            .client
            .call_typed(rpc_methods::AHP_CONNECTIONS_DISPOSE, Some(wire_params))
            .await?;
        Ok(())
    }
}

/// `cafeMetrics.*` RPCs.
#[derive(Clone, Copy)]
pub struct ClientRpcCafeMetrics<'a> {
    pub(crate) client: &'a Client,
}

impl<'a> ClientRpcCafeMetrics<'a> {
    /// Starts anonymous CAFE metrics for this process. The runtime selects the endpoint from COPILOT_CAFE_METRICS_URL or this connection's original auth owner. Offline and unsupported platforms refuse startup. Release eligibility must resolve before samples are sent. Requires the private trusted CLI host capability.
    ///
    /// Wire method: `cafeMetrics.start`.
    ///
    /// # Parameters
    ///
    /// * `params` - Versions and provider configuration for CAFE.
    ///
    /// # Returns
    ///
    /// CAFE startup result.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn start(
        &self,
        params: CafeMetricsStartRequest,
    ) -> Result<CafeMetricsStartResult, Error> {
        let wire_params = serde_json::to_value(params)?;
        self.client
            .call_typed(rpc_methods::CAFEMETRICS_START, Some(wire_params))
            .await
    }

    /// Resolves release eligibility once from the immutable release lookup shared with auto-update. Requires the private trusted CLI host capability.
    ///
    /// Wire method: `cafeMetrics.resolveAdmission`.
    ///
    /// # Parameters
    ///
    /// * `params` - Immutable release lookup result.
    ///
    /// # Returns
    ///
    /// Admission result.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn resolve_admission(
        &self,
        params: CafeMetricsResolveAdmissionRequest,
    ) -> Result<CafeMetricsResolveAdmissionResult, Error> {
        let wire_params = serde_json::to_value(params)?;
        self.client
            .call_typed(rpc_methods::CAFEMETRICS_RESOLVEADMISSION, Some(wire_params))
            .await
    }

    /// Records only the first interactive CLI readiness observation in this process. Invalid durations produce no sample. Requires the private trusted CLI host capability.
    ///
    /// Wire method: `cafeMetrics.recordCliReady`.
    ///
    /// # Parameters
    ///
    /// * `params` - Existing initial interactive CLI readiness observation.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn record_cli_ready(
        &self,
        params: CafeMetricsRecordCliReadyRequest,
    ) -> Result<(), Error> {
        let wire_params = serde_json::to_value(params)?;
        let _: serde::de::IgnoredAny = self
            .client
            .call_typed(rpc_methods::CAFEMETRICS_RECORDCLIREADY, Some(wire_params))
            .await?;
        Ok(())
    }

    /// Records one terminal resume result. Does nothing before startup. Requires the private trusted CLI host capability.
    ///
    /// Wire method: `cafeMetrics.recordResumeResult`.
    ///
    /// # Parameters
    ///
    /// * `params` - Terminal result of one resume attempt.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn record_resume_result(
        &self,
        params: CafeMetricsRecordResumeResultRequest,
    ) -> Result<(), Error> {
        let wire_params = serde_json::to_value(params)?;
        let _: serde::de::IgnoredAny = self
            .client
            .call_typed(
                rpc_methods::CAFEMETRICS_RECORDRESUMERESULT,
                Some(wire_params),
            )
            .await?;
        Ok(())
    }

    /// Stops recording and drains within one 1.5-second deadline. flushOnly sends at most one batch without stopping recording. Requires the private trusted CLI host capability.
    ///
    /// Wire method: `cafeMetrics.shutdown`.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn shutdown(&self) -> Result<(), Error> {
        let wire_params = serde_json::json!({});
        let _: serde::de::IgnoredAny = self
            .client
            .call_typed(rpc_methods::CAFEMETRICS_SHUTDOWN, Some(wire_params))
            .await?;
        Ok(())
    }

    /// Stops recording and drains within one 1.5-second deadline. flushOnly sends at most one batch without stopping recording. Requires the private trusted CLI host capability.
    ///
    /// Wire method: `cafeMetrics.shutdown`.
    ///
    /// # Parameters
    ///
    /// * `params` - Preliminary flush or final disposal.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn shutdown_with_params(
        &self,
        params: CafeMetricsShutdownRequest,
    ) -> Result<(), Error> {
        let wire_params = serde_json::to_value(params)?;
        let _: serde::de::IgnoredAny = self
            .client
            .call_typed(rpc_methods::CAFEMETRICS_SHUTDOWN, Some(wire_params))
            .await?;
        Ok(())
    }
}

/// `mcpSource.*` RPCs.
#[derive(Clone, Copy)]
pub struct ClientRpcMcpSource<'a> {
    pub(crate) client: &'a Client,
}

impl<'a> ClientRpcMcpSource<'a> {
    /// Validates explicit account-authenticated MCP approvals and retains opaque grants in the original engine before any session opens. Requires the private CLI host capability.
    ///
    /// Wire method: `mcpSource.prepareGitHubAuth`.
    ///
    /// # Parameters
    ///
    /// * `params` - Explicit host approval for account-authenticated MCP endpoints.
    ///
    /// # Returns
    ///
    /// Configuration after the original engine retains approved account-authenticated servers.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn prepare_git_hub_auth(
        &self,
        params: McpGitHubAuthPrepareRequest,
    ) -> Result<McpGitHubAuthPrepareResult, Error> {
        let wire_params = serde_json::to_value(params)?;
        self.client
            .call_typed(rpc_methods::MCPSOURCE_PREPAREGITHUBAUTH, Some(wire_params))
            .await
    }
}

/// `providersSource.*` RPCs.
#[derive(Clone, Copy)]
pub struct ClientRpcProvidersSource<'a> {
    pub(crate) client: &'a Client,
}

impl<'a> ClientRpcProvidersSource<'a> {
    /// Forwards a saved-model addition over the original authenticated local attachment to its owning server. Requires private CLI-host admission and fails if that attachment is no longer active; never saves to the controller as a fallback.
    ///
    /// Wire method: `providersSource.addModel`.
    ///
    /// # Parameters
    ///
    /// * `params` - Saves through the exact authenticated local attachment that discovered a model.
    ///
    /// # Returns
    ///
    /// Result of adding a model to the saved provider configuration.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn add_model(
        &self,
        params: AttachedProviderConfigurationAddModelRequest,
    ) -> Result<SavedProviderConfigurationAddModelResult, Error> {
        let wire_params = serde_json::to_value(params)?;
        self.client
            .call_typed(rpc_methods::PROVIDERSSOURCE_ADDMODEL, Some(wire_params))
            .await
    }
}

/// `session.*` RPCs.
#[derive(Clone, Copy)]
pub struct ClientRpcSession<'a> {
    pub(crate) client: &'a Client,
}

impl<'a> ClientRpcSession<'a> {
    /// `session.cafeMetrics.*` sub-namespace.
    pub fn cafe_metrics(&self) -> ClientRpcSessionCafeMetrics<'a> {
        ClientRpcSessionCafeMetrics {
            client: self.client,
        }
    }

    /// `session.canvasSource.*` sub-namespace.
    pub fn canvas_source(&self) -> ClientRpcSessionCanvasSource<'a> {
        ClientRpcSessionCanvasSource {
            client: self.client,
        }
    }

    /// `session.eventLog.*` sub-namespace.
    pub fn event_log(&self) -> ClientRpcSessionEventLog<'a> {
        ClientRpcSessionEventLog {
            client: self.client,
        }
    }

    /// `session.mcp.*` sub-namespace.
    pub fn mcp(&self) -> ClientRpcSessionMcp<'a> {
        ClientRpcSessionMcp {
            client: self.client,
        }
    }

    /// `session.mcpSource.*` sub-namespace.
    pub fn mcp_source(&self) -> ClientRpcSessionMcpSource<'a> {
        ClientRpcSessionMcpSource {
            client: self.client,
        }
    }

    /// `session.remote.*` sub-namespace.
    pub fn remote(&self) -> ClientRpcSessionRemote<'a> {
        ClientRpcSessionRemote {
            client: self.client,
        }
    }

    /// `session.sandbox.*` sub-namespace.
    pub fn sandbox(&self) -> ClientRpcSessionSandbox<'a> {
        ClientRpcSessionSandbox {
            client: self.client,
        }
    }
}

/// `session.cafeMetrics.*` RPCs.
#[derive(Clone, Copy)]
pub struct ClientRpcSessionCafeMetrics<'a> {
    pub(crate) client: &'a Client,
}

impl<'a> ClientRpcSessionCafeMetrics<'a> {
    /// Records live model-call events, including subagents, and root compaction events. Replaces any earlier recorder. Requires the private trusted CLI host capability.
    ///
    /// Wire method: `session.cafeMetrics.attach`.
    ///
    /// # Parameters
    ///
    /// * `params` - Identifies the target session.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn attach(&self, params: SessionCafeMetricsAttachParams) -> Result<(), Error> {
        let wire_params = serde_json::to_value(params)?;
        let _: serde::de::IgnoredAny = self
            .client
            .call_typed(rpc_methods::SESSION_CAFEMETRICS_ATTACH, Some(wire_params))
            .await?;
        Ok(())
    }

    /// Stops recording this session. Does nothing if it has no recorder. Requires the private trusted CLI host capability.
    ///
    /// Wire method: `session.cafeMetrics.detach`.
    ///
    /// # Parameters
    ///
    /// * `params` - Identifies the target session.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn detach(&self, params: SessionCafeMetricsDetachParams) -> Result<(), Error> {
        let wire_params = serde_json::to_value(params)?;
        let _: serde::de::IgnoredAny = self
            .client
            .call_typed(rpc_methods::SESSION_CAFEMETRICS_DETACH, Some(wire_params))
            .await?;
        Ok(())
    }
}

/// `session.canvasSource.*` RPCs.
#[derive(Clone, Copy)]
pub struct ClientRpcSessionCanvasSource<'a> {
    pub(crate) client: &'a Client,
}

impl<'a> ClientRpcSessionCanvasSource<'a> {
    /// Closes a canvas through its original session source. Requires the privately admitted in-process CLI host.
    ///
    /// Wire method: `session.canvasSource.close`.
    ///
    /// # Parameters
    ///
    /// * `params` - Closes a canvas through its original owned or attached session namespace.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn close(&self, params: CanvasSourceCloseRequest) -> Result<(), Error> {
        let wire_params = serde_json::to_value(params)?;
        let _: serde::de::IgnoredAny = self
            .client
            .call_typed(rpc_methods::SESSION_CANVASSOURCE_CLOSE, Some(wire_params))
            .await?;
        Ok(())
    }
}

/// `session.eventLog.*` RPCs.
#[derive(Clone, Copy)]
pub struct ClientRpcSessionEventLog<'a> {
    pub(crate) client: &'a Client,
}

impl<'a> ClientRpcSessionEventLog<'a> {
    /// Publishes an existing event type for an admitted controller. The runtime validates, timestamps, orders, projects, and stores the event.
    ///
    /// Wire method: `session.eventLog.publish`.
    ///
    /// # Parameters
    ///
    /// * `params` - Publishes a controller event through the runtime-owned session log.
    ///
    /// # Returns
    ///
    /// Runtime acceptance of a controller event.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn publish(
        &self,
        params: SessionEventLogPublishParams,
    ) -> Result<SessionEventPublishResult, Error> {
        let wire_params = serde_json::to_value(params)?;
        self.client
            .call_typed(rpc_methods::SESSION_EVENTLOG_PUBLISH, Some(wire_params))
            .await
    }
}

/// `session.mcp.*` RPCs.
#[derive(Clone, Copy)]
pub struct ClientRpcSessionMcp<'a> {
    pub(crate) client: &'a Client,
}

impl<'a> ClientRpcSessionMcp<'a> {
    /// `mcp.externalClient.*` sub-namespace.
    pub fn external_client(&self) -> ClientRpcSessionMcpExternalClient<'a> {
        ClientRpcSessionMcpExternalClient {
            client: self.client,
        }
    }

    /// `mcp.oauth.*` sub-namespace.
    pub fn oauth(&self) -> ClientRpcSessionMcpOauth<'a> {
        ClientRpcSessionMcpOauth {
            client: self.client,
        }
    }

    /// Configures the built-in GitHub MCP server for the session's current auth context.
    ///
    /// Wire method: `session.mcp.configureGitHub`.
    ///
    /// # Parameters
    ///
    /// * `params` - Credential-free authentication identity used to configure GitHub MCP.
    ///
    /// # Returns
    ///
    /// Result of configuring GitHub MCP.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn configure_git_hub(
        &self,
        params: McpConfigureGitHubRequest,
    ) -> Result<McpConfigureGitHubResult, Error> {
        let wire_params = serde_json::to_value(params)?;
        self.client
            .call_typed(rpc_methods::SESSION_MCP_CONFIGUREGITHUB, Some(wire_params))
            .await
    }
}

/// `mcp.externalClient.*` RPCs.
#[derive(Clone, Copy)]
pub struct ClientRpcSessionMcpExternalClient<'a> {
    pub(crate) client: &'a Client,
}

impl<'a> ClientRpcSessionMcpExternalClient<'a> {
    /// Private trusted CLI transport for a caller-owned live MCP client. Exchanges serializable advertisements, correlated tool and protocol callbacks, progress, reverse requests, cancellation, and catalog notifications. The caller retains the client and transport.
    ///
    /// Wire method: `session.mcp.externalClient.exchange`.
    ///
    /// # Parameters
    ///
    /// * `params` - Object envelope for one trusted live-client transport operation.
    ///
    /// # Returns
    ///
    /// Operation-specific reply or a complete live-client callback frame.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn exchange(
        &self,
        params: SessionMcpExternalClientExchangeParams,
    ) -> Result<McpExternalClientResult, Error> {
        let wire_params = serde_json::to_value(params)?;
        self.client
            .call_typed(
                rpc_methods::SESSION_MCP_EXTERNALCLIENT_EXCHANGE,
                Some(wire_params),
            )
            .await
    }
}

/// `mcp.oauth.*` RPCs.
#[derive(Clone, Copy)]
pub struct ClientRpcSessionMcpOauth<'a> {
    pub(crate) client: &'a Client,
}

impl<'a> ClientRpcSessionMcpOauth<'a> {
    /// Reserves a host-driven OAuth flow identity before authentication or cancellation.
    ///
    /// Wire method: `session.mcp.oauth.prepareFlow`.
    ///
    /// # Parameters
    ///
    /// * `params` - Correlation identity reserved before an effectful authentication request.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn prepare_flow(
        &self,
        params: SessionMcpOauthPrepareFlowParams,
    ) -> Result<(), Error> {
        let wire_params = serde_json::to_value(params)?;
        let _: serde::de::IgnoredAny = self
            .client
            .call_typed(
                rpc_methods::SESSION_MCP_OAUTH_PREPAREFLOW,
                Some(wire_params),
            )
            .await?;
        Ok(())
    }

    /// Cancels an exact host-driven OAuth flow on its original session requester.
    ///
    /// Wire method: `session.mcp.oauth.cancelFlow`.
    ///
    /// # Parameters
    ///
    /// * `params` - Correlation identity reserved before an effectful authentication request.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn cancel_flow(&self, params: SessionMcpOauthCancelFlowParams) -> Result<(), Error> {
        let wire_params = serde_json::to_value(params)?;
        let _: serde::de::IgnoredAny = self
            .client
            .call_typed(rpc_methods::SESSION_MCP_OAUTH_CANCELFLOW, Some(wire_params))
            .await?;
        Ok(())
    }

    /// Authenticates an arbitrary MCP URL through the original session credential owner, with correlated host progress and authorization callbacks.
    ///
    /// Wire method: `session.mcp.oauth.authenticate`.
    ///
    /// # Parameters
    ///
    /// * `params` - Full host-driven authentication for an arbitrary, possibly unsaved, MCP URL.
    ///
    /// # Returns
    ///
    /// Token projection for the existing host-delegated provider.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn authenticate(
        &self,
        params: SessionMcpOauthAuthenticateParams,
    ) -> Result<McpOauthAuthenticateResult, Error> {
        let wire_params = serde_json::to_value(params)?;
        self.client
            .call_typed(
                rpc_methods::SESSION_MCP_OAUTH_AUTHENTICATE,
                Some(wire_params),
            )
            .await
    }

    /// Checks whether the selected MCP credential owner has a valid access token.
    ///
    /// Wire method: `session.mcp.oauth.hasValidTokens`.
    ///
    /// # Parameters
    ///
    /// * `params` - A narrow token-validity query against the chosen credential owner.
    ///
    /// # Returns
    ///
    /// Whether the credential operation succeeded.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn has_valid_tokens(
        &self,
        params: SessionMcpOauthHasValidTokensParams,
    ) -> Result<SessionMcpOauthHasValidTokensResult, Error> {
        let wire_params = serde_json::to_value(params)?;
        self.client
            .call_typed(
                rpc_methods::SESSION_MCP_OAUTH_HASVALIDTOKENS,
                Some(wire_params),
            )
            .await
    }

    /// Attempts silent MCP token refresh without opening a browser.
    ///
    /// Wire method: `session.mcp.oauth.tryRefreshTokens`.
    ///
    /// # Parameters
    ///
    /// * `params` - A narrow token-validity query against the chosen credential owner.
    ///
    /// # Returns
    ///
    /// Whether the credential operation succeeded.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn try_refresh_tokens(
        &self,
        params: SessionMcpOauthTryRefreshTokensParams,
    ) -> Result<SessionMcpOauthTryRefreshTokensResult, Error> {
        let wire_params = serde_json::to_value(params)?;
        self.client
            .call_typed(
                rpc_methods::SESSION_MCP_OAUTH_TRYREFRESHTOKENS,
                Some(wire_params),
            )
            .await
    }

    /// Stores a confidential MCP client's static secret in its selected credential owner.
    ///
    /// Wire method: `session.mcp.oauth.saveStaticClientSecret`.
    ///
    /// # Parameters
    ///
    /// * `params` - Persist a confidential client's static secret without exposing token-store CRUD.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn save_static_client_secret(
        &self,
        params: SessionMcpOauthSaveStaticClientSecretParams,
    ) -> Result<(), Error> {
        let wire_params = serde_json::to_value(params)?;
        let _: serde::de::IgnoredAny = self
            .client
            .call_typed(
                rpc_methods::SESSION_MCP_OAUTH_SAVESTATICCLIENTSECRET,
                Some(wire_params),
            )
            .await?;
        Ok(())
    }

    /// Releases an explicit host-owned MCP OAuth store on its original session requester.
    ///
    /// Wire method: `session.mcp.oauth.disposeStore`.
    ///
    /// # Parameters
    ///
    /// * `params` - Dispose an explicit host store without altering the session authorizer's owner.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn dispose_store(
        &self,
        params: SessionMcpOauthDisposeStoreParams,
    ) -> Result<(), Error> {
        let wire_params = serde_json::to_value(params)?;
        let _: serde::de::IgnoredAny = self
            .client
            .call_typed(
                rpc_methods::SESSION_MCP_OAUTH_DISPOSESTORE,
                Some(wire_params),
            )
            .await?;
        Ok(())
    }

    /// Completes a tokenless initialize handshake for an unsaved remote MCP URL, preserving session egress and the refusing authentication challenge.
    ///
    /// Wire method: `session.mcp.oauth.testConnection`.
    ///
    /// # Parameters
    ///
    /// * `params` - Tokenless initialize handshake for a possibly unsaved remote MCP server.
    ///
    /// # Returns
    ///
    /// Outcome of the tokenless handshake, with the original challenge when available.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn test_connection(
        &self,
        params: SessionMcpOauthTestConnectionParams,
    ) -> Result<McpOauthTestConnectionResult, Error> {
        let wire_params = serde_json::to_value(params)?;
        self.client
            .call_typed(
                rpc_methods::SESSION_MCP_OAUTH_TESTCONNECTION,
                Some(wire_params),
            )
            .await
    }

    /// Derives an MCP credential key through the selected session account and store owner.
    ///
    /// Wire method: `session.mcp.oauth.storeKey`.
    ///
    /// # Parameters
    ///
    /// * `params` - Derive a credential key through the selected account and store owner.
    ///
    /// # Returns
    ///
    /// The selected owner's OAuth credential key.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn store_key(
        &self,
        params: SessionMcpOauthStoreKeyParams,
    ) -> Result<SessionMcpOauthStoreKeyResult, Error> {
        let wire_params = serde_json::to_value(params)?;
        self.client
            .call_typed(rpc_methods::SESSION_MCP_OAUTH_STOREKEY, Some(wire_params))
            .await
    }
}

/// `session.mcpSource.*` RPCs.
#[derive(Clone, Copy)]
pub struct ClientRpcSessionMcpSource<'a> {
    pub(crate) client: &'a Client,
}

impl<'a> ClientRpcSessionMcpSource<'a> {
    /// Manages the original trusted CLI host's live IDE client on the canonical session graph. Requires the private host capability and original session ownership; remote peers cannot confer first-party trust.
    ///
    /// Wire method: `session.mcpSource.ideClient`.
    ///
    /// # Parameters
    ///
    /// * `params` - Original-host request for a session-owned IDE bridge.
    ///
    /// # Returns
    ///
    /// Result of an original-host IDE operation.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn ide_client(
        &self,
        params: SessionMcpSourceIdeClientParams,
    ) -> Result<McpIdeClientResult, Error> {
        let wire_params = serde_json::to_value(params)?;
        self.client
            .call_typed(rpc_methods::SESSION_MCPSOURCE_IDECLIENT, Some(wire_params))
            .await
    }
}

/// `session.remote.*` RPCs.
#[derive(Clone, Copy)]
pub struct ClientRpcSessionRemote<'a> {
    pub(crate) client: &'a Client,
}

impl<'a> ClientRpcSessionRemote<'a> {
    /// Replaces an owned session subscription in release and restore phases. Release returns a generation so the caller can check cancellation before restore. Restore requires the same lease and generation and waits for the authoritative snapshot to apply.
    ///
    /// Wire method: `session.remote.reattach`.
    ///
    /// # Parameters
    ///
    /// * `params` - Request for one phase of replacing an attached session's transport subscription.
    ///
    /// # Returns
    ///
    /// Runtime generation that fences the two subscription replacement phases.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn reattach(
        &self,
        params: SessionRemoteReattachParams,
    ) -> Result<AhpSessionLeaseResult, Error> {
        let wire_params = serde_json::to_value(params)?;
        self.client
            .call_typed(rpc_methods::SESSION_REMOTE_REATTACH, Some(wire_params))
            .await
    }

    /// Reads this caller's read-only AHP collaboration and pending-request projection, waiting after an unchanged revision.
    ///
    /// Wire method: `session.remote.readProjection`.
    ///
    /// # Parameters
    ///
    /// * `params` - Reads the host-owned collaboration and pending-request projection.
    ///
    /// # Returns
    ///
    /// A read-only host projection for clients with synchronous render paths.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn read_projection(
        &self,
        params: SessionRemoteReadProjectionParams,
    ) -> Result<AhpSessionProjectionResult, Error> {
        let wire_params = serde_json::to_value(params)?;
        self.client
            .call_typed(
                rpc_methods::SESSION_REMOTE_READPROJECTION,
                Some(wire_params),
            )
            .await
    }

    /// Submits one message through its originating trusted CLI host's request-owned admission callback. The runtime owns preparation, routing, and the admitted write.
    ///
    /// Wire method: `session.remote.sendWithAdmission`.
    ///
    /// # Parameters
    ///
    /// * `params` - One trusted CLI host's message and its invocation-local admission identity.
    ///
    /// # Returns
    ///
    /// Result of sending a user message
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn send_with_admission(
        &self,
        params: SessionRemoteSendWithAdmissionParams,
    ) -> Result<SendResult, Error> {
        let wire_params = serde_json::to_value(params)?;
        self.client
            .call_typed(
                rpc_methods::SESSION_REMOTE_SENDWITHADMISSION,
                Some(wire_params),
            )
            .await
    }

    /// Publishes composer or view state through an owned AHP session. The runtime coalesces updates and owns retries.
    ///
    /// Wire method: `session.remote.publishState`.
    ///
    /// # Parameters
    ///
    /// * `params` - Publishes this client's collaboration state through the AHP host.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn publish_state(
        &self,
        params: SessionRemotePublishStateParams,
    ) -> Result<(), Error> {
        let wire_params = serde_json::to_value(params)?;
        let _: serde::de::IgnoredAny = self
            .client
            .call_typed(rpc_methods::SESSION_REMOTE_PUBLISHSTATE, Some(wire_params))
            .await?;
        Ok(())
    }
}

/// `session.sandbox.*` RPCs.
#[derive(Clone, Copy)]
pub struct ClientRpcSessionSandbox<'a> {
    pub(crate) client: &'a Client,
}

impl<'a> ClientRpcSessionSandbox<'a> {
    /// Invokes a closed sandbox operation through the session's original authenticated attachment or AHP owner. Requires a trusted in-process host and a matching runtime-owned session binding. Never falls back to the local presentation session.
    ///
    /// Wire method: `session.sandbox.invokeOwned`.
    ///
    /// # Parameters
    ///
    /// * `params` - Request for an admitted host to use its original remote sandbox owner.
    ///
    /// # Returns
    ///
    /// Existing result of the sandbox operation on its original transport owner.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases. Pin both the
    /// SDK and CLI versions if your code depends on it.
    ///
    /// </div>
    pub async fn invoke_owned(
        &self,
        params: SessionSandboxInvokeOwnedParams,
    ) -> Result<SandboxInvokeOwnedResult, Error> {
        let wire_params = serde_json::to_value(params)?;
        self.client
            .call_typed(rpc_methods::SESSION_SANDBOX_INVOKEOWNED, Some(wire_params))
            .await
    }
}

/// Typed view over a [`Session`]'s RPC namespace.
#[derive(Clone, Copy)]
pub(crate) struct SessionRpc<'a> {
    pub(crate) session: &'a Session,
}

impl<'a> SessionRpc<'a> {}
