import Foundation

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

public class Handle : Hashable {
	var which: ObjectIdentifier {
		return ObjectIdentifier(self)
	}
	init() {
	}
	public static func ==(_ l: Handle, _ r: Handle) -> Bool {
		return l.which == r.which
	}
	public func hash(into hasher: inout Hasher) {
		hasher.combine(which)
	}
}

public typealias Attributes = [NSAttributedString.Key : Any]

public protocol Content : RangeReplaceableCollection {
	associatedtype SubSequence
	associatedtype Element
	subscript(_ range: Range<Int>) -> Self? { get }
	subscript(_ i: Int) -> Element { get }
	var isEmpty: Bool { get }
	static var empty: Self { get }
	var length: Int { get }
	static func +<Other>(_ l: Self, _ r: Other) -> Self where Other : Sequence, Character == Other.Element
	init(_: SubSequence)
	init(repeating: Element, count: Int)
/*
	var startIndex: Int { get }
	var endIndex: Int { get }
*/
}

extension Content {
	var headAndTail: (Self, Self)? {
		if startIndex == endIndex {
			return nil
		}
		return (Self.init(self[..<self.index(after: startIndex)]), Self.init(dropFirst(1)))
	}
}

public struct RopeIndex : Comparable {
	public let characters: Int
	let opened: Int
	let closed: Int
	let cursors: Int
}

extension RopeIndex {
	public init(characters: Int, cursors: Int = 0) {
		self.characters = characters
		self.opened = 0
		self.closed = 0
		self.cursors = cursors
	}
	public init(cursors: Int) {
		self.characters = 0
		self.opened = 0
		self.closed = 0
		self.cursors = cursors
	}
	public init(opened: Int, closed: Int) {
		self.characters = 0
		self.opened = opened
		self.closed = closed
		self.cursors = 0
	}
	public init(containers: Int) {
		self.characters = 0
		self.opened = containers
		self.closed = containers
		self.cursors = 0
	}
	public static let zero: RopeIndex =
	    RopeIndex(characters: 0, opened: 0, closed: 0, cursors: 0)
}

/*
public struct Weak<O : AnyObject> {
	public typealias Element = O
	private weak var _object: O?
	public var object: O? {
		return _object
	}
	public init(_ r: O) {
		_object = r
	}
}
*/

/* A Node directly encodes the presence of cursors because it is
 * possible for a cursor to move up and down the hierarchy of text
 * containers without changing between-character positions.  A cursor
 * can appear at the position left of the first character or right of
 * the last character in a Node.  A cursor can also appear in an empty
 * Node.
 */
public indirect enum Node<C : Content> {
public typealias Content = C
public typealias Element = C.Element
case cursor(Handle, Attributes)
case index(Handle)
case container(Handle, Node)
case concat(Node, RopeIndex, UInt, Set<Handle>, Node,
    RopeIndex)
case leaf(Attributes, C)
case empty
}

extension Substring : Content {
	public typealias Element = Character
	public subscript(_ range: Range<Int>) -> Substring? {
		let clampedStartOffset: Int = (range.startIndex > self.length)
		    ? self.length
		    : range.startIndex
		let startOffset: Int = (clampedStartOffset < 0)
		    ? 0
		    : clampedStartOffset
		let start = self.index(startIndex, offsetBy: startOffset)
		let clampedEndOffset: Int = (range.endIndex < 0)
		    ? 0
		    : range.endIndex
		let endOffset: Int = (clampedEndOffset > self.length)
		    ? self.length
		    : clampedEndOffset
		let end = self.index(startIndex, offsetBy: endOffset)
		return self[start..<end]
	}
	public subscript(_ offset: Int) -> Element {
		let i = self.index(startIndex, offsetBy: offset)
		return self[i]
	}
	public static var empty: Substring { return "" }
	public var length: Int {
		return distance(from: startIndex, to: endIndex)
	}
}

enum Step<C: Content> {
case absent
case step(Node<C>)
case stepOut(Node<C>)
case noStep(Node<C>)
}

extension Node {
	func rightStepping(index i: Handle, to j: Handle, sibling r: Node) -> Step<C> {
		switch rightStepping(index: i, to: j) {
		case .step(let newl):
			return .step(Node(left: newl, right: r))
		case .stepOut(let newl):
			let indexr = Node(left: .index(i), right: r)
			switch indexr.rightStepping(index: i, to: j) {
			case .step(let newr):
				return .step(Node(left: newl,
				    right: newr))
			case .stepOut(let newr):
				return .stepOut(Node(left: newl, right: newr))
			case .absent:
				return .absent
			}
		case .absent:
			return .absent
		}
	}
	func rightStepping(index i: Handle, to j: Handle) -> Step<C> {
		switch self {
/*
case cursor(Handle, Attributes)
case index(Handle)
case container(Handle, Node)
case concat(Node, RopeIndex, UInt, Set<Handle>, Node,
    RopeIndex)
case leaf(Attributes, C)
case empty
*/
		case .index(i):
			return .stepOut(self)
		case .cursor(_, _), .index(_), .leaf(_, _), .empty:
			return .absent
		case .container(let h, let n):
			switch n.rightStepping(index: i, to: j) {
			case .stepOut(let newn):
				return .step(Node(left: .container(h, newn),
				                      right: .index(i)))
			case .step(let newn):
				return .step(.container(h, newn))
			case .absent:
				return .absent
			}
		/* stepping over a cursor is not a full step */
		case .concat(.index(i), _, _, _, .cursor(_, _), _):
			return .noStep(self)
		/* this is not a full step; return noStep? */
		case .concat(.index(i), _, _, _, .index(_), _):
			return .noStep(self)
		/* a step into a container is a full step */
		case .concat(.index(i), _, _, _, .container(let h, let n), _):
			/* Take a small step *into* the container. */
			return .step(Node(left: .index(i),
			                  right: .container(h,
					      Node(left: .index(j), right: n))))
		case .concat(.index(i), _, _, _, .leaf(let a, let s), _):
			switch s.headAndTail {
			case (let head, let tail)? where tail.isEmpty:
				return .step(
				    Node(left: .index(i),
				         right: Node(left: .leaf(a, head),
				                     right: .index(i))))
			case (let head, let tail)?:
				let jtail = Node(left: .index(j),
				                 right: .leaf(a, tail))
				return .step(
				    Node(left: .index(i),
				         right: Node(left: .leaf(a, head),
					             right: jtail)))
			default:
				/* XXX This shouldn't happen. */
				return .noStep(.empty)
			}
		case .concat(.index(i), _, _, _, .empty, _):
			return .noStep(.empty)
		case .concat(let l, _, _, _, .index(i), _):
			return .noStep(self)
		case .concat(let l, _, _, _, let r, _):
			assert(handles.contains(i) != r.handles.contains(i))
			if l.handles.contains(i) {
				return l.rightStepping(index: i, to: j,
				    sibling: r)
			}
			switch r.rightStepping(index: i, to: j) {
			case .step(let newr):
				return .step(Node(left: l, right: newr))
			case .stepOut(let newr):
				return .stepOut(Node(left: l, right: newr))
			case .absent:
				return .absent
			}
		}
	}
}

/* Use cases:
 *
 * Get/set/add/remove attributes on characters.
 *
 * Get/set/add/remove attributes on a cursor.
 *
 * Enclose a range in a container.  The range must be
 * well-formed: must begin and end inside the same container. 
 *
 * Insert a cursor between characters or nested between containers.
 *
 * Remove a cursor.
 *
 * "Step" a cursor left or right by a character.
 *
 * "Scoot" a cursor left or right by a container boundary.
 *
 * "Scoot" a cursor left or right by a cursor?
 *
 * Replace a cursor by a container; apply the cursor's attributes to
 * the container's content.
 *
 * Insert some text left of a cursor; apply the cursor's attributes
 * to the text.
 */
class Rope<C : Content> {
	public typealias Content = C
	var top: Node<C>
	typealias Index = Handle
	public var startIndex: Handle
	public var endIndex: Handle

	init() {
		startIndex = Handle()
		endIndex = Handle()
		top = Node(left: .index(startIndex),
			       right: .index(endIndex))
	}
	func index(after i: Index) -> Index {
		return i	// XXXXXXXXXXXXXXXXXXXXXX
	}
}

extension Node {
/*
	public func inserting(cursor: Handle, at: RopeIndex) -> Node {
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
*/
}

extension Node {
	// TBD introduce a property for all Handles but the
	// index Handles?
	public var handles: Set<Handle> {
		switch self {
		case .index(var handle):
			if isKnownUniquelyReferenced(&handle) {
				return []
			}
			return [handle]
		case .cursor(let handle, _):
			return [handle]
		case .container(let handle, let rope):
			let handles: Set<Handle> = [handle]
			return handles.union(rope.handles)
		case .concat(_, _, _, let handles, _, _):
			return handles
		case .leaf(_, _), .empty:
			return []
		}
	}
}
/*
extension Rope : ExpressibleByStringLiteral, ExpressibleByExtendedGraphemeClusterLiteral where Rope.Content : ExpressibleByStringLiteral {
	public init(stringLiteral s: S) {
		top = Node<Content>(text: s)
	}
}
*/

public protocol Initializable {
	associatedtype Initializer
	init(_ initial: Initializer)
}

extension Substring : Initializable {
	public typealias Initializer = String
}

extension Node {
	public init(left: Node<C>, right: Node<C>) {
		self = .concat(left, left.endIndex,
		               1 + max(left.depth, right.depth),
			       left.handles.union(right.handles), right, left.endIndex + right.endIndex)
	}
	public init<T>(text t: T) where C : Initializable, C.Initializer == T, T : Collection {
		if t.isEmpty {
			self = Node<C>.empty
		} else {
			self = Node<C>.leaf([:], C(t))
		}
	}
}

public class LeafIterator<C : Content> : IteratorProtocol {
	var stack: [Node<C>]

	public init(for rope: Node<C>) {
		self.stack = [rope]
	}
	public func next() -> Node<C>? {
		guard var top = stack.popLast() else {
			return nil
		}
		while true {
			switch top {
			case .concat(let l, _, _, _, let r, _):
				stack.append(r)
				top = l
			case .leaf(_, _):
				return top
			case .empty, .cursor(_, _), .container(_, _), .index(_):
				return top
			}
		}
	}
}

public struct LeafSequence<C : Content> : Sequence {
	var rope: Node<C>
	public init(of rope: Node<C>) {
		self.rope = rope
	}
	public func makeIterator() -> LeafIterator<C> {
		return LeafIterator(for: rope)
	}
}

extension Node : CustomDebugStringConvertible {
	public var debugDescription: String {
		switch self {
		case .index(_):
			return "⬦"
		case .cursor(_, _):
			return "|"
		case .container(_, let rope):
			return rope.debugDescription
		case .concat(let l, _, _, _, let r, _):
			return "[\(l), \(r)]"
		case .leaf(_, let s):
			return "\"\(s)\""
		case .empty:
			return "\"\""
		}
	}
}

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

func +(_ l: RopeIndex, _ r: RopeIndex) -> RopeIndex {
	return RopeIndex(characters: l.characters + r.characters, opened: l.opened + r.opened, closed: l.closed + r.closed, cursors: l.cursors + r.cursors)
}

public protocol RopeIndexish : Comparable {                          
	static func indexish(for: RopeIndex) -> Self                            
	static var zero: Self { get }
	static var stepIn: Self { get }
	static var stepOut: Self { get }
	static func -(_ l: Self, _ r: Self) -> Self
	static func +(_ l: Self, _ r: Self) -> Self
	static func adapt<T>(range: Range<Self>, for: T) -> Range<Int> where T : Content
}                                                                               

public extension Node {
	typealias Index = RopeIndex
	public var leaves: LeafSequence<Content> {
		return LeafSequence(of: self)
	}
	public var depth: UInt {
		switch self {
		case .leaf(_), .cursor(_, _), .empty, .index(_):
			return 0
		case .container(_, let rope):
			return rope.depth
		case .concat(_, _, let depth, _, _, _):
			return depth
		}
	}
	public var content: C {
		switch self {
		case .cursor(_, _), .empty, .index(_):
			return C.empty
		case .container(_, let rope):
			return rope.content
		case .leaf(_, let s):
			return s
		case .concat(let l, _, _, _, let r, _):
			return l.content + r.content
		}
	}
	public var startIndex: RopeIndex {
		return RopeIndex.zero
	}
	public var midIndex: Index {
		switch self {
		case .container(_, let rope):
			return rope.midIndex
		case .concat(_, let idx, _, _, _, _):
			return idx
		case .leaf(_), .empty, .cursor(_, _), .index(_):
			return self.endIndex
		}
	}
	public var endIndex: RopeIndex {
		switch self {
		case Node<C>.concat(_, _, _, _, _, let idx):
			return idx
		case .container(_, let rope):
			return rope.endIndex + RopeIndex(containers: 1)
		case .leaf(_, let s):
			return RopeIndex(characters: s.distance(from: s.startIndex, to: s.endIndex))
		case .empty, .index(_):
			return RopeIndex.zero
		case .cursor(_, _):
			return RopeIndex(cursors: 1)
		}
	}
	public func element(at i: RopeIndex) -> Element {
		switch self {
		case .leaf(_, let s):
			let c: Element = s[s.index(s.startIndex, offsetBy: i.characters)]
			return c
		case .concat(let ropel, let idx, _, _, let roper, _):
			if i < idx {
				return ropel.element(at: i)
			} else {
				return roper.element(at: i - idx)
			}
		case .cursor(_, _), .empty, .index(_):
			fatalError("In \(#function), no element \(i)")
		case .container(_, let rope):
			return rope.element(at: i)
		}
	}
	public func appending(_ rope: Node) -> Node {
		switch (self, rope) {
		case (.empty, _):
			return rope
		case (_, .empty):
			return self
		case (.concat(let l, _, _, _,
		              .leaf(let pat, let p), _),
		      .leaf(let qat, let q)) where pat.isEmpty && qat.isEmpty:
			return Node(left: l, right: .leaf([:], p + q))
		case (.leaf(let pat, let p), .leaf(let qat, let q)) where
		    pat.isEmpty && qat.isEmpty:
			return .leaf([:], p + q)
		default:
			return Node(left: self, right: rope)
		}
	}
	public var balanced: Bool {
		return endIndex.characters >= fibonacci(index: depth + 2)
	}
	public func rebalanced() -> Node<C> {
		switch self {
		case .empty, .cursor(_, _), .leaf(_, _):
			return self
		case .container(let handles, let rope):
			return .container(handles, rope.rebalanced())
		default:
			break
		}
		var slot: [Node?] = []
		let totlen = endIndex
		for fn in Fibonacci(from: 2) {
			if fn > totlen.characters {
				break
			}
			slot.append(nil)
		}
		for node in leaves {
			var rope: Node = node
			var n: Int = slot.count
			for (i, fip3) in Fibonacci(from: 3).enumerated() {
				if let left = slot[slot.count - i - 1] {
					rope = Node(left: left,
					                right: rope)
					slot[slot.count - i - 1] = nil
				}
				if fip3 >= rope.endIndex.characters {
					n = i
					break
				}
			}
			slot[slot.count - n - 1] = rope
		}
		return slot.reduce(.empty,
		    { (accum: Node, opt: Node?) -> Node in
			switch (accum, opt) {
			case (_, nil):
				return accum
			case (.empty, let next?):
				return next
			case (_, let next?):
				return Node(left: accum, right: next)
			}
		})
	}
	public func subrope<T : RopeIndexish>(from: T, to: T, depth: Int = 0) -> Node<C> {
		assert(T.zero <= from)
		let endIndex = T.indexish(for: self.endIndex)
		assert(to <= endIndex)
		// print("enter\(" " * depth) substring \(from):\(to) " +
		//  "on \(self)")
		switch self {
		case .index(_):
			assert(from == to)
			return .empty
		case .empty:
			assert(from == to)
			return self
		case .cursor(_, _):
			if T.zero == from && to == endIndex {
				return self
			}
			return .empty
		case .container(let handle, let rope):
			/* Position left of the container's
			 * "left parenthesis."
			 */
			if to < T.stepIn {
				return .empty
			}
			/* Position right of the container's
			 * "right parenthesis."
			 */
			if T.stepIn + T.stepOut + endIndex < from {
				return .empty
			}
			return .container(handle, rope.subrope(
			    from: max(T.zero, from - T.stepIn),
			    to: min(endIndex - T.stepIn, to - T.stepIn),
			    depth: depth + 1))
		case .concat(let ropel, let _idx, _, _, let roper, _):
			let idx = T.indexish(for: _idx)
			if from == to {
				return .empty
			}
			var l, r: Node
			if from == T.zero && idx <= to {
				l = ropel
			} else if idx <= from {
				l = .empty
			} else {
				l = ropel.subrope(
					from: from,
					to: min(idx, to),
					depth: depth + 1)
			}

			if from <= idx && endIndex <= to {
				r = roper
			} else if to <= idx {
				r = .empty
			} else {
				r = roper.subrope(
					from: max(T.zero, from - idx),
					to: min(endIndex - idx, to - idx),
					depth: depth + 1)
			}
			return l.appending(r)
		case .leaf(let attrs, let s):
			guard let subs = s[T.adapt(range: from..<to, for: s)]
			    else {
				return .empty
			}
			return .leaf(attrs, subs)
		}
	}
	public func subrope(from: RopeIndex, to: RopeIndex, depth: Int = 0) -> Node<C> {
		assert(RopeIndex.zero <= from)
		let endIndex = self.endIndex
		assert(to <= endIndex)
		// print("enter\(" " * depth) substring \(from):\(to) " +
		//  "on \(self)")
		switch self {
		case .index(_):
			assert(from == to)
			return .empty
		case .empty, .cursor(_, _):
			assert(from == to)
			return self
		case .container(_, let rope):
			return rope.subrope(from: from, to: to, depth: depth)
		case .concat(let ropel, let idx, _, _, let roper, _):
			if from == to {
				return .empty
			}
			var l, r: Node
			if from == RopeIndex.zero && idx <= to {
				l = ropel
			} else if idx <= from {
				l = .empty
			} else {
				l = ropel.subrope(
					from: from,
					to: min(idx, to),
					depth: depth + 1)
			}

			if from <= idx && endIndex <= to {
				r = roper
			} else if to <= idx {
				r = .empty
			} else {
				r = roper.subrope(
					from: max(RopeIndex.zero, from - idx),
					to: min(endIndex - idx, to - idx),
					depth: depth + 1)
			}
			return l.appending(r)
		case let .leaf(attrs, s):
			guard let subs = s[from.characters..<to.characters]
			    else {
				return .empty
			}
			return .leaf(attrs, subs)
		}
	}
	public func deleting(from start: Index, to end: Index) -> Node {
		return subrope(from: RopeIndex.zero, to: start).appending(
		    subrope(from: end, to: endIndex))
	}
	public func inserting(cursor handle: Handle, attributes: Attributes, at i: Index) -> Node {
		let cursor: Node = .cursor(handle, attributes)
		return subrope(from: RopeIndex.zero, to: i).appending(cursor).appending(
		    subrope(from: i, to: endIndex))
	}
	public func inserting(text rope: Node, at insertionPt: Index) -> Node {
		return subrope(from: RopeIndex.zero, to: insertionPt).appending(rope).appending(
		    subrope(from: insertionPt, to: endIndex))
	}
}

public func ==(_ l: RopeIndex, _ r: RopeIndex) -> Bool {
	return l.characters == r.characters && l.opened == r.opened && r.closed == l.closed && l.cursors == r.cursors
}

public func <(_ l: RopeIndex, _ r: RopeIndex) -> Bool {
	if l.characters < r.characters {
		return true
	} else if l.characters > r.characters {
		return false
	}
	if l.opened < r.opened {
		return true
	} else if l.opened > r.opened {
		return false
	}
	if l.closed < r.closed {
		return true
	} else if l.closed > r.closed {
		return false
	}
	return l.cursors < r.cursors
}

func -(_ l: RopeIndex, _ r: RopeIndex) -> RopeIndex {
	if l <= r {
		return RopeIndex.zero
	}
	if l.characters > r.characters {
		return RopeIndex(characters: l.characters - r.characters,
		    cursors: l.cursors)
	}
	if l.opened > r.opened {
		return RopeIndex(opened: l.opened - r.opened,
		    closed: l.closed - min(l.closed, r.opened))
	}
	return RopeIndex(characters: l.characters - r.characters)
}

func max(_ l: RopeIndex, _ r: RopeIndex) -> RopeIndex {
	return (l <= r) ? r : l
}

func min(_ l: RopeIndex, _ r: RopeIndex) -> RopeIndex {
	return (l <= r) ? l : r
}

