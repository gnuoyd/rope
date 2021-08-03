/* Protocol for recipients of messages that tell which Offset Ranges were
 * replaced and by how many Offset units they changed in length.
 */
public protocol RopeOffsetDelegate : AnyObject {
	associatedtype Offset : Comparable
	func ropeDidChange(on: Range<Offset>, changeInLength: Int)
	func ropeAttributesDidChange(on: Range<Offset>)
}
