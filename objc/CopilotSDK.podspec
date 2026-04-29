Pod::Spec.new do |s|
  s.name         = "CopilotObjCSDK"
  s.version      = "0.1.0"
  s.summary      = "Objective-C SDK for the GitHub Copilot CLI"
  s.description  = <<-DESC
    The Copilot Objective-C SDK provides a native Objective-C interface
    for communicating with the GitHub Copilot CLI server via JSON-RPC 2.0
    over stdio. Create sessions, send messages, define custom tools,
    and handle streaming events with idiomatic Objective-C patterns.
  DESC

  s.homepage     = "https://github.com/github/copilot-sdk"
  s.license      = { :type => "MIT", :file => "../LICENSE" }
  s.author       = { "GitHub" => "copilot@github.com" }

  s.ios.deployment_target     = "16.0"
  s.osx.deployment_target     = "13.0"
  s.tvos.deployment_target    = "16.0"
  s.watchos.deployment_target = "9.0"

  s.source       = { :git => "https://github.com/github/copilot-sdk.git", :tag => s.version.to_s }

  s.source_files  = "src/**/*.{h,m}"
  s.public_header_files = "src/**/*.h"

  s.frameworks = "Foundation"
  s.requires_arc = true
end
