/**
 * SDK protocol version spoken by the Solidity SDK.
 *
 * Copilot CLI 1.0.113 and newer report protocol 4 on `connect` / `ping`;
 * older CLIs are rejected at startup.
 */
export const SDK_PROTOCOL_VERSION = 4;

/**
 * Verify the protocol version reported by the server.
 *
 * @throws Error when the server reports no version or a version other than
 *     {@link SDK_PROTOCOL_VERSION}
 */
export function assertSdkProtocolVersion(serverVersion: number | undefined | null): void {
    if (serverVersion === undefined || serverVersion === null) {
        throw new Error(
            `SDK protocol version mismatch: SDK supports version ${SDK_PROTOCOL_VERSION}, ` +
                "but server does not report a protocol version. " +
                "Please update your SDK or server to ensure compatibility.",
        );
    }
    if (serverVersion !== SDK_PROTOCOL_VERSION) {
        throw new Error(
            `SDK protocol version mismatch: SDK supports version ${SDK_PROTOCOL_VERSION}, ` +
                `but server reports version ${serverVersion}. ` +
                "Please update your SDK or server to ensure compatibility.",
        );
    }
}
