//
// Copyright (c) 2019, 2020, 2021 David Young.  All rights reserved.
//

/* Protocol for recipients of messages that tell which unit Ranges were
 * replaced and by how many units they changed in length.
 */
public protocol RopeOffsetDelegate {
	func ropeDidChange(on: Range<Int>, changeInLength: Int)
	func ropeAttributesDidChange(on: Range<Int>)
}

public extension Rope {
	typealias TypeErasedOffsetDelegate = AnyRopeOffsetDelegate
}

public struct AnyRopeOffsetDelegate : RopeOffsetDelegate {
	public typealias DidChange = (Range<Int>, Int) -> Void
	public typealias AttributesDidChange = (Range<Int>) -> Void
	let didChange: DidChange
	let attributesDidChange: AttributesDidChange
	public init(didChange: @escaping DidChange,
	            attributesDidChange: @escaping AttributesDidChange) {
		self.didChange = didChange
		self.attributesDidChange = attributesDidChange
	}
	public func ropeDidChange(on range: Range<Int>, changeInLength: Int){
		didChange(range, changeInLength)
	}
	public func ropeAttributesDidChange(on range: Range<Int>) {
		attributesDidChange(range)
	}
}
