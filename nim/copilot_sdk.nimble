# Package
version       = "2.0.0"
author        = "GitHub"
description   = "GitHub Copilot SDK for Nim - build AI-powered applications using the Copilot CLI"
license       = "MIT"
srcDir        = "src"

# Dependencies
requires "nim >= 2.0.0"

# Tasks
task test, "Run unit tests":
  exec "nim c -r tests/test_client.nim"

task example, "Run the basic example":
  exec "nim c -r examples/basic_example.nim"
