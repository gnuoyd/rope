public struct NodeIndex : Comparable {
	public init(utf16Offset offset: Int) {
		self.utf16Offset = offset
	}
	public let utf16Offset: Int
}

extension NodeIndex {
	public static let start: NodeIndex = NodeIndex(utf16Offset: 0)
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
	if l <= r {
		return NodeIndex.start
	}
	return NodeIndex(utf16Offset: l.utf16Offset - r.utf16Offset)
}

func max(_ l: NodeIndex, _ r: NodeIndex) -> NodeIndex {
	return (l <= r) ? r : l
}

func min(_ l: NodeIndex, _ r: NodeIndex) -> NodeIndex {
	return (l <= r) ? l : r
}

