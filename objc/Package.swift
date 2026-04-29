// swift-tools-version: 5.9
// ----------------------------------------------------------------------------------------------------
//  Copyright (c) Microsoft Corporation. All rights reserved.
// ----------------------------------------------------------------------------------------------------

import PackageDescription

let package = Package(
    name: "CopilotObjCSDK",
    platforms: [
        .macOS(.v13),
        .iOS(.v16),
        .tvOS(.v16),
        .watchOS(.v9),
    ],
    products: [
        .library(
            name: "CopilotObjCSDK",
            targets: ["CopilotObjCSDK"]
        ),
    ],
    targets: [
        .target(
            name: "CopilotObjCSDK",
            path: "src",
            publicHeadersPath: ".",
            cSettings: [
                .headerSearchPath("."),
            ]
        ),
        .testTarget(
            name: "CopilotObjCSDKTests",
            dependencies: ["CopilotObjCSDK"],
            path: "tests"
        ),
    ]
)
