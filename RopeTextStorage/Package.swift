// swift-tools-version:5.1
//
// Copyright (c) 2020, 2021 David Young.  All rights reserved.
//
import PackageDescription

let package = Package(
	name: "RopeTextStorage",
	platforms: [.macOS(.v10_11)],
	products: [
		.library(name: "RopeTextStorage", targets: ["RopeTextStorage"]),
	],
	dependencies: [.package(path: "/Users/dyoung/rope/Rope")],
	targets: [
		.target(name: "RopeTextStorage", dependencies: ["Rope"]),
		.testTarget(name: "RopeTextStorageTests",
		            dependencies: ["RopeTextStorage", "Rope"])
	]
)
