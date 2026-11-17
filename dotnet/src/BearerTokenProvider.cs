/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

using System.Diagnostics.CodeAnalysis;

namespace Microsoft.Copilot;

/// <summary>
/// Arguments passed to a bearer-token callback (the <c>BearerTokenProvider</c> property
/// on <see cref="ProviderConfig"/> / <see cref="NamedProviderConfig"/>) when the
/// runtime needs a fresh bearer token for a BYOK provider.
/// </summary>
/// <remarks>
/// Part of the experimental managed-identity / bearer-token-provider surface and
/// may change or be removed in future SDK or CLI releases.
/// </remarks>
[Experimental(Diagnostics.Experimental)]
public sealed class ProviderTokenArgs
{
    /// <summary>
    /// Configured <see cref="NamedProviderConfig.Name"/> of the connection needing a token.
    /// </summary>
    /// <remarks>
    /// The callback closes over its own token scope/audience; the runtime is
    /// provider-agnostic and forwards only the provider name.
    /// </remarks>
    public required string ProviderName { get; init; }

    /// <summary>
    /// Id of the session that triggered this token request. A client-level
    /// shared callback registered for many sessions can use this to resolve the
    /// owning session and scope token acquisition or caching per session.
    /// </summary>
    public required string SessionId { get; init; }
}
