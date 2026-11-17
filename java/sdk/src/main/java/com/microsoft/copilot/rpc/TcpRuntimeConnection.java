/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot.rpc;

import java.net.InetAddress;
import java.net.UnknownHostException;

/**
 * Connects to an already-running runtime over plaintext TCP without owning its
 * lifetime. Construct with {@link RuntimeConnection#forTcp(String, int)}.
 *
 * @since 1.0.0
 */
public final class TcpRuntimeConnection extends RuntimeConnection {

    private final String host;
    private final int port;
    private String connectionToken;

    TcpRuntimeConnection(String host, int port) {
        if (host == null || host.isEmpty()
                || host.chars().anyMatch(c -> Character.isWhitespace(c) || "/\\?#@[]".indexOf(c) >= 0)) {
            throw new IllegalArgumentException("host must be a hostname or IP address without a scheme, port, or path");
        }
        if (host.indexOf(':') >= 0) {
            String address = host;
            int zone = host.indexOf('%');
            if (zone >= 0) {
                if (zone == host.length() - 1 || host.indexOf('%', zone + 1) >= 0) {
                    throw new IllegalArgumentException("host must be an unbracketed IPv6 address");
                }
                address = host.substring(0, zone);
            }
            try {
                // Brackets force numeric parsing; named zones must not require a local
                // interface yet.
                InetAddress.getByName("[" + address + "]");
            } catch (UnknownHostException e) {
                throw new IllegalArgumentException("host must be an unbracketed IPv6 address", e);
            }
        }
        if (port < 1 || port > 65535) {
            throw new IllegalArgumentException("port must be from 1 through 65535");
        }
        this.host = host;
        this.port = port;
    }

    /**
     * Returns the hostname or IP address of the runtime.
     *
     * @return the host, without a scheme, port, path, or IPv6 brackets
     */
    public String getHost() {
        return host;
    }

    /**
     * Returns the TCP port of the runtime.
     *
     * @return the port, from 1 through 65535
     */
    public int getPort() {
        return port;
    }

    /**
     * Returns the shared secret used to authenticate the connection.
     *
     * @return the token, or {@code null} if the runtime does not require one
     */
    public String getConnectionToken() {
        return connectionToken;
    }

    /**
     * Sets the shared secret used to authenticate the connection.
     *
     * @param connectionToken
     *            the token, or {@code null} if the runtime does not require one
     * @return this instance for method chaining
     */
    public TcpRuntimeConnection setConnectionToken(String connectionToken) {
        this.connectionToken = connectionToken;
        return this;
    }
}
