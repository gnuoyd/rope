class Holder : Hashable {
	var which: ObjectIdentifier {
		return ObjectIdentifier(self)
	}
	static func ==(_ l: Holder, _ r: Holder) -> Bool {
		return l.which == r.which
	}
	func hash(into hasher: inout Hasher) {
		hasher.combine(which)
	}
}

/*
extension Weak : Equatable where Weak.Element : Equatable {
	public static func ==(_ l: Weak, _ r: Weak) -> Bool {
		switch (l.object, r.object) {
		case let (lo?, ro?):
			return lo == ro
		case (nil, nil):
			return true
		default:
			return false
		}
	}
}
*/

/*
extension Node {
	public func inserting(cursor: Handle, at: NodeIndex) -> Node {
	}
	public func setting(attributes: Attributes, on: Handle) -> Node {
	}
	public func removing(cursor: Handle) -> Node {
	}
	public scootLeft(cursor: Handle) throws {
		// TBD
	}
	public stepLeft(cursor: Handle) throws {
		switch self {
		// [l, |[r]]
		case .concat(let l, _, _, leftCursor(cursor, let r)):
			
		case .concat(let l, 
		case .leaf(let s)
		}
	}
}
*/

/*
protocol TextContainer {
	associatedtype Index
	var startIndex: Index { get }
	var endIndex: Index { get }
	func element(at i: Index) -> Character
	func substring(from: Index, to: Index, depth: Int) -> Self
	func appending(_: Self) -> Self
	func deleting(from start: Index, to end: Index) -> Self
	func inserting(text: Self, at insertionPt: Index) -> Self
}
*/

