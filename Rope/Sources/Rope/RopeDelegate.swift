//
// Copyright (c) 2019, 2020, 2021 David Young.  All rights reserved.
//

/* Protocol for recipients of messages that tell which unit Ranges were
 * replaced and by how many units they changed in length.
 */
public protocol RopeDelegate {
	func ropeDidChange(on: Range<Int>, changeInLength: Int)
	func ropeAttributesDidChange(on: Range<Int>)
}

public struct AnyRopeDelegate<C : Content> : RopeDelegate {
	public typealias DidChange = (Range<Int>, Int) -> ()
	public typealias AttributesDidChange = (Range<Int>) -> ()
	let didChange: DidChange
	let attributesDidChange: AttributesDidChange
	public init(didChange: @escaping DidChange,
	            attributesDidChange: @escaping AttributesDidChange) {
		self.didChange = didChange
		self.attributesDidChange = attributesDidChange
	}
	public func ropeDidChange(on range: Range<Int>,
	    changeInLength delta: Int) {
		return didChange(range, delta)
	}
	public func ropeAttributesDidChange(on range: Range<Int>) {
		attributesDidChange(range)
	}
}
