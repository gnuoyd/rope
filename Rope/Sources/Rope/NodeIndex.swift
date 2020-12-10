import Foundation   // for NSRange

public struct NodeIndex : Comparable {
	public init(utf16Offset offset: Int) {
		self.utf16Offset = offset
	}
	public let utf16Offset: Int
}

extension NodeIndex {
	public static let start: NodeIndex = NodeIndex(utf16Offset: 0)
}

extension NodeIndex {
	public static func utf16Range(_ range: NSRange) -> Range<NodeIndex> {
		let lower = NodeIndex(utf16Offset: range.location)
		let upper = NodeIndex(utf16Offset: NSMaxRange(range))
		return lower..<upper
	}
	public static func utf16Range(_ range: Range<Int>) -> Range<NodeIndex> {
		let lower = NodeIndex(utf16Offset: range.lowerBound)
		let upper = NodeIndex(utf16Offset: range.upperBound)
		return lower..<upper
	}
	public static func utf16RangeTo(_ upperBound: Int) -> Range<NodeIndex> {
		let upper = NodeIndex(utf16Offset: upperBound)
		return NodeIndex.start..<upper
	}
}

extension Range where Bound == NodeIndex {
	public var utf16NSRange: NSRange {
		let lower = lowerBound.utf16Offset
		let upper = upperBound.utf16Offset
		return NSMakeRange(lower, upper - lower)
	}
}

func +(_ l: NodeIndex, _ r: NodeIndex) -> NodeIndex {
	return NodeIndex(utf16Offset: l.utf16Offset + r.utf16Offset)
}

public func ==(_ l: NodeIndex, _ r: NodeIndex) -> Bool {
	return l.utf16Offset == r.utf16Offset
}

public func <(_ l: NodeIndex, _ r: NodeIndex) -> Bool {
	return l.utf16Offset < r.utf16Offset
}

func -(_ l: NodeIndex, _ r: NodeIndex) -> NodeIndex {
	return NodeIndex(utf16Offset: l.utf16Offset - r.utf16Offset)
}

func max(_ l: NodeIndex, _ r: NodeIndex) -> NodeIndex {
	return (l <= r) ? r : l
}

func min(_ l: NodeIndex, _ r: NodeIndex) -> NodeIndex {
	return (l <= r) ? l : r
}

