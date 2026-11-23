/// SDK protocol version for the Copilot D SDK.
///
/// The Copilot CLI reports its protocol version as a JSON number in the
/// `connect` / `ping` response (`protocolVersion`). This SDK only talks to a
/// server that reports exactly this version (Copilot CLI 1.0.113 or newer).
module copilot.sdk_protocol_version;

/// The SDK protocol version this SDK speaks.
enum int SdkProtocolVersion = 4;

/// Returns the SDK protocol version this SDK speaks.
int getSdkProtocolVersion() @safe pure nothrow @nogc
{
    return SdkProtocolVersion;
}
