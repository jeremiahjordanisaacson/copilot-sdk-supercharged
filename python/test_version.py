"""Verify the importable package reports this fork's distribution version."""

import os
import subprocess
import sys
from pathlib import Path


def test_version_uses_microsoft_distribution_metadata(tmp_path: Path):
    # This fork publishes as copilot-sdk-supercharged; other distributions sharing the
    # `copilot` import package must not supply the version.
    for name, version in [("copilot-sdk-supercharged", "1.2.3"), ("microsoft-copilot-sdk", "9.9.9")]:
        metadata = tmp_path / f"{name.replace('-', '_')}-{version}.dist-info"
        metadata.mkdir()
        (metadata / "METADATA").write_text(
            f"Metadata-Version: 2.4\nName: {name}\nVersion: {version}\n",
            encoding="utf-8",
        )

    result = subprocess.run(
        [sys.executable, "-c", "import copilot; print(copilot.__version__)"],
        cwd=tmp_path,
        env={**os.environ, "PYTHONPATH": str(Path(__file__).parent)},
        capture_output=True,
        text=True,
        check=True,
    )

    assert result.stdout.strip() == "1.2.3"
