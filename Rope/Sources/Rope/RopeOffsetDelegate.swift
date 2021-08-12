/* Protocol for recipients of messages that tell which Offset Ranges were
 * replaced and by how many Offset units they changed in length.
 */
public protocol RopeOffsetDelegate {
	associatedtype Offset : Comparable
	func ropeDidChange(on: Range<Offset>, changeInLength: Int)
	func ropeAttributesDidChange(on: Range<Offset>)
}

public extension Rope {
	typealias TypeErasedOffsetDelegate = AnyRopeOffsetDelegate<Offset>
}

public struct AnyRopeOffsetDelegate<O : Comparable> : RopeOffsetDelegate {
	public typealias Offset = O
	typealias DidChange = (Range<Offset>, Int) -> Void
	typealias AttributesDidChange = (Range<Offset>) -> Void
	let didChange: DidChange
	let attributesDidChange: AttributesDidChange
	public func ropeDidChange(on range: Range<Offset>, changeInLength: Int){
		didChange(range, changeInLength)
	}
	public func ropeAttributesDidChange(on range: Range<Offset>) {
		attributesDidChange(range)
	}
}
