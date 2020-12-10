// swift-tools-version:5.1
import PackageDescription

let package = Package(
	name: "RopeTextStorage",
	platforms: [.macOS(.v10_11)],
	products: [
		.library(name: "RopeTextStorage", targets: ["RopeTextStorage"]),
	],
	dependencies: [.package(path: "../Rope")],
	targets: [
		.target(name: "RopeTextStorage", dependencies: ["Rope"]),
		.testTarget(name: "RopeTextStorageTests",
		            dependencies: ["RopeTextStorage", "Rope"])
	]
)
