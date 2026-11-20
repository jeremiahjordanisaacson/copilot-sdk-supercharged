//! Repository-private construction for the native CLI, not a published SDK mode.

use crate::{Client, ClientOptions, Error, ErrorKind, RuntimeConnection};

/// Install an owner-scoped timing consumer during the native CLI's connection setup.
///
/// The consumer performs only in-memory initialization; release admission and endpoint
/// resolution remain asynchronous. Ordinary SDK builds do not compile this entry point.
pub fn initialize_session_timing(
    client: &Client,
    config: &serde_json::Value,
) -> Result<bool, Error> {
    let _span = tracing::debug_span!("session.timing.cli_timing_initialize").entered();
    let config = serde_json::to_vec(config)
        .map_err(|error| Error::with_message(ErrorKind::InvalidConfig, error.to_string()))?;
    let host = client.inner.ffi_host.lock();
    let host = host.as_ref().ok_or_else(|| {
        Error::with_message(
            ErrorKind::InvalidConfig,
            "CLI timing initialization requires an active in-process connection",
        )
    })?;
    host.initialize_session_timing(&config)
}

/// Start the native CLI's connection after normal SDK setup.
///
/// The caller checks image identity before the SDK captures CLI authority.
/// Ordinary SDK builds do not compile this entry point.
pub async fn start<E>(
    options: ClientOptions,
    validate_image: impl FnOnce(&Client) -> Result<(), E>,
) -> Result<Client, E>
where
    E: From<Error>,
{
    if !matches!(
        &options.runtime,
        Some(RuntimeConnection::InProcess(_) | RuntimeConnection::AttachedInProcess(_))
    ) {
        return Err(Error::with_message(
            ErrorKind::InvalidConfig,
            "native CLI construction requires the in-process transport",
        )
        .into());
    }
    let client = Client::start(options).await.map_err(E::from)?;
    finish_start(client, validate_image)
}

fn finish_start<E>(
    client: Client,
    validate_image: impl FnOnce(&Client) -> Result<(), E>,
) -> Result<Client, E>
where
    E: From<Error>,
{
    let _span = tracing::debug_span!("session.timing.cli_host_admission").entered();
    let admission = validate_image(&client).and_then(|()| {
        let host = client.inner.ffi_host.lock();
        let host = host.as_ref().ok_or_else(|| {
            E::from(Error::with_message(
                ErrorKind::InvalidConfig,
                "native CLI construction requires an active in-process connection",
            ))
        })?;
        host.admit_cli_host().map_err(E::from)
    });
    if let Err(error) = admission {
        client.force_stop();
        return Err(error);
    }
    Ok(client)
}

#[cfg(test)]
mod tests;
