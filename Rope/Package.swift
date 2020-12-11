// swift-tools-version:5.1
//
// Copyright (c) 2019, 2020 David Young.  All rights reserved.
//
import PackageDescription

let package = Package(
	name: "Rope",
	platforms: [.macOS(.v10_11)],
	products: [
		.library(name: "Rope", targets: ["Rope"]),
	],
	targets: [
		.target(name: "Rope"),
		.testTarget(name: "RopeTests", dependencies: ["Rope"])
	]
)
