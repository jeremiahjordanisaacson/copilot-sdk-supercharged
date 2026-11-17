# Copyright (c) Microsoft Corporation. All rights reserved.

from unittest.mock import Mock, patch

import pytest

from copilot import CopilotClient, RuntimeConnection


class TestTcpConnection:
    @pytest.mark.parametrize("host", ["localhost", "127.0.0.1", "::1"])
    def test_attach_with_host_and_port(self, host):
        client = CopilotClient(mode="coding-agent", runtime=RuntimeConnection.for_tcp(host, 9000))
        assert client.runtime_port == 9000
        assert client._actual_host == host
        assert client._is_external_server

    @pytest.mark.parametrize(
        "host",
        [
            "",
            " ",
            "http://localhost",
            "https://localhost",
            "localhost:9000",
            "localhost/path",
            "[::1]",
        ],
    )
    def test_reject_invalid_host_before_connecting(self, host):
        with pytest.raises(
            ValueError,
            match="host must be a hostname or IP address",
        ):
            CopilotClient(
                mode="coding-agent",
                runtime=RuntimeConnection.for_tcp(host, 9000),
            )

    @pytest.mark.parametrize("port", [0, -1, 65536, 1.5, True, "9000"])
    def test_reject_invalid_port(self, port):
        with pytest.raises(ValueError, match="port must be an integer from 1 through 65535"):
            CopilotClient(mode="coding-agent", runtime=RuntimeConnection.for_tcp("localhost", port))

    @pytest.mark.asyncio
    async def test_connect_via_tcp_uses_family_independent_resolution(self):
        client = CopilotClient(mode="coding-agent", runtime=RuntimeConnection.for_tcp("::1", 9000))
        fake_socket = Mock()
        fake_socket.makefile.return_value = Mock()

        with patch("socket.create_connection", return_value=fake_socket) as create_connection:
            await client._connect_via_tcp()

        create_connection.assert_called_once_with(("::1", 9000), timeout=10)
        client._process.terminate()
