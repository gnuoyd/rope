import Foundation

public class Handle : Hashable {
	public typealias Id = UInt64
	static var nextId: Id = 0
	private var _id: Id
	public var id: Id { return _id }
	public init() {
		_id = Handle.nextId
		Handle.nextId = Handle.nextId + 1
	}
	public static func ==(_ l: Handle, _ r: Handle) -> Bool {
		return l._id == r._id
	}
	public func hash(into hasher: inout Hasher) {
		hasher.combine(_id)
	}
//	deinit {
//		print("b-bye")
//	}
}

public typealias Attributes = [NSAttributedString.Key : Any]

public protocol Initializable {
	associatedtype Initializer
	init(_ initial: Initializer)
}

public protocol Content : Initializable, StringProtocol {
	associatedtype SubSequence
	associatedtype Element
	subscript(r: Range<String.Index>) -> Self { get }
	var isEmpty: Bool { get }
	static var empty: Self { get }
	static var unit: Element { get }
	var length: Int { get }
	static func +<Other>(_ l: Self, _ r: Other) -> Self where Other : Sequence, Character == Other.Element
	init(_: SubSequence)
	init(repeating: Element, count: Int)
	init(unit: Element)
}

extension Content {
	var headAndTail: (Self, Self)? {
		if startIndex == endIndex {
			return nil
		}
		return (Self.init(self[..<self.index(after: startIndex)]), Self.init(dropFirst(1)))
	}
}

public struct NodeIndex : Comparable {
	public init(utf16Offset offset: Int) {
		self.utf16Offset = offset
	}
	public let utf16Offset: Int
}

extension NodeIndex {
	public static let start: NodeIndex = NodeIndex(utf16Offset: 0)
}

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
case index(Weak<Handle>)
case container(Handle, Node)
case concat(Node, NodeIndex, UInt, Set<Handle.Id>, Node, NodeIndex)
case leaf(Attributes, C)
case empty
}

public func == <C>(_ l: Node<C>, _ r: Node<C>) -> Bool {
	switch (l, r) {
	case (.cursor(let lHandle, _),
	      .cursor(let rHandle, _)):
		// XXX doesn't match attributes
		return lHandle == rHandle
	case (.index(let lWeakHandle), .index(let rWeakHandle)):
		return lWeakHandle.get() == rWeakHandle.get()
	case (.container(let lHandle, let lNode),
	      .container(let rHandle, let rNode)):
		return lHandle == rHandle && lNode == rNode
	case (.concat(let lNode1, _, _, _, let lNode2, _),
	      .concat(let rNode1, _, _, _, let rNode2, _)):
		return lNode1 == rNode1 && lNode2 == rNode2
	case (.leaf(_, let lContent), .leaf(_, let rContent)):
		// XXX doesn't match attributes
		return lContent == rContent
	case (.empty, .empty):
		return true
	default:
		return false
	}
}

extension Substring : Content {
        public init(unit: Character) {
                self.init(repeating: unit, count: 1)
        }
        
	public typealias Element = Character
	public static var unit: Character { return Character("U") }
	public static var empty: Substring { return "" }
	public var length: Int {
		return distance(from: startIndex, to: endIndex)
	}
}

/*
 * Result of looking up an element of a Node
 */
public enum ElementResult<C : Content> {
case absent
case inchOut
case step(Node<C>)
}

/*
 * Result of taking a step in a Node
 */
public enum Step<C: Content> {
case absent		/* The location to step from could not be
			 * found.  TBD: throw an Error, instead?
			 */
case step(Node<C>)	/* A full step occurred, resulting in the
			 * associated Node
			 */
case inchOut		/* A partial step occurred: stepping over a
		 	* content-free Node case cursor, empty, or index
		 	*/
case stepOut		/* A full upward step occurred: stepping over
			 * a container boundary, or stepping over the last
			 * UTF-16 element of a leaf
			 */
}

extension Node {
	public func insertingIndex(_ j: Handle, oneStepAfter i: Handle,
	    sibling r: Node) -> Step<C> {
		let result = insertingIndex(j, oneStepAfter: i)
		switch result {
		case .step(let newl):
			return .step(Node(left: newl, right: r))
		case .stepOut:
			return .step(Node(left: self,
			    right: Node(left: Node(holder: j), right: r)))
		case .inchOut:
			switch r.afterStepInsertingIndex(j) {
			case .step(let newr):
				return .step(Node(left: self, right: newr))
			case let result:
				return result
			}
		case .absent:
			return .absent
		}
	}
	public func afterStepInsertingIndex(_ j: Handle) -> Step<C> {
		switch self {
		/* A step over a cursor, index, or empty string is NOT
		 * a full step.
		 */
		case .cursor(_, _), .empty, .index(_):
			return .inchOut
		/* A step into a container is a full step. */
		case .container(let h, let n):
			return .step(.container(h,
			                        Node(left: Node(holder: j),
						     right: n)))
		case .leaf(let attrs, let content):
			switch content.headAndTail {
			case (_, let tail)? where tail.isEmpty:
				return .stepOut
			case (let head, let tail)?:
				let jtail = Node(left: Node(holder: j),
				                 right: .leaf(attrs, tail))
				return .step(Node(left: .leaf(attrs, head),
				            right: jtail))
			default:
				/* XXX Empty leaves shouldn't exist. */
				return .inchOut
			}
		/* A step into a concatenation is NOT a full step. */
		case .concat(let l, _, _, _, let r, _):
			switch l.afterStepInsertingIndex(j) {
			case .step(let newl):
				return .step(Node(left: newl, right: r))
			case .stepOut:
				return .step(
				    Node(left: l,
				         right: Node(left: Node(holder: j),
				                     right: r)))
			case .inchOut, .absent:
				break
			}
			switch r.afterStepInsertingIndex(j) {
			case .step(let newr):
				return .step(Node(left: l, right: newr))
			case let result:
				return result
			}
		}
	}
	public func insertingIndex(_ j: Handle, oneStepAfter i: Handle)
	    -> Step<C> {
		switch self {
		case .index(let w) where w.get() == i:
			return .inchOut
		case .cursor(_, _), .index(_), .leaf(_, _), .empty:
			return .absent
		case .container(let h, let n):
			switch n.insertingIndex(j, oneStepAfter: i) {
			case .inchOut:
				return .stepOut
			case .stepOut:
				return .step(.container(h,
				    Node(left: n, right: Node(holder: j))))
			case .step(let newn):
				return .step(.container(h, newn))
			case .absent:
				return .absent
			}
		case .concat(.index(let w), _, _, _, let r, _) where
		    w.get() == i:
			switch r.afterStepInsertingIndex(j) {
			case .step(let newr):
				return .step(Node(left: Node(holder: i),
				                  right: newr))
			case let result:
				return result
			}
		case .concat(let l, _, _, _, let r, _):
			let id = i.id
			switch (l.hids.contains(id), r.hids.contains(id)) {
			case (false, false):
				return .absent
			case (true, true):
				assert(l.hids.contains(id) !=
				       r.hids.contains(id))
				return .inchOut
			case (true, false):
				return l.insertingIndex(j, oneStepAfter: i,
				    sibling: r)
			case (false, true):
				let result = r.insertingIndex(j,
				    oneStepAfter: i)
				if case .step(let newr) = result {
					return .step(Node(left: l, right: newr))
				}
				return result
			}
		}
	}
	/* TBD extract `performing` from `insertingIndex(_:,oneStepAfter:)`
	 * and element(at:) ?
	 */
	public func firstElement() -> ElementResult<C> {
		switch self {
		case .cursor(_, _), .index(_), .empty:
			/* No match: the element is not on this span. */
			return .absent
		case .leaf(let attrs, let content):
			switch content.headAndTail {
			case (let head, _)?:
                                return .step(.leaf(attrs, head))
			default:
				return .inchOut
			}
		case .container(let h, _):
			return .step(.empty)
		case .concat(let l, _, _, _, let r, _):
			return l.firstElementUsingSibling(r)
		}
	}
	public func firstElementUsingSibling(_ r: Node) ->
	    ElementResult<C> {
		switch firstElement() {
		case .inchOut:
			switch r.firstElement() {
			case .step(let newr):
				return .step(newr)
			case let result:
				return result
			}
		case let result:
			return result
		}
	}
	public func element(at i: Handle, sibling r: Node) ->
	    ElementResult<C> {
		switch element(at: i) {
		case .inchOut:
			return r.firstElement()
		case let result:
			return result
		}
	}
	/* TBD extract `performing` from `insertingIndex(_:,oneStepAfter:)`
	 * and element(at:) ?
	 */
	public func element(at i: Handle) -> ElementResult<C> {
		switch self {
		case .index(let w) where w.get() == i:
			/* The index matches: inch out so that the caller
			 * returns some element right of the index.
			 */
			return .inchOut
		case .cursor(_, _), .empty, .index(_), .leaf(_, _):
			/* No match: the element is not on this span. */
			return .absent
		case .container(let h, let n):
			switch n.element(at: i) {
			case .inchOut:
				return .step(.container(h, .empty))
			case .step(let newn):
				return .step(.container(h, newn))
			case .absent:
				return .absent
			}
		case .concat(.index(let w), _, _, _, let r, _) where
		    w.get() == i:
			return r.firstElement()
		case .concat(let l, _, _, _, let r, _):
			let id = i.id
			switch (l.hids.contains(id), r.hids.contains(id)) {
			case (false, false):
				return .absent
			case (true, true):
				fatalError("No index can be in two spans")
			case (true, false):
				return l.element(at: i, sibling: r)
			case (false, true):
				return r.element(at: i)
			}
		}
	}
}

public enum RopeIndex<C : Content> : Comparable {
case start(of: Rope<C>)
case end(of: Rope<C>)
case interior(of: Rope<C>, at: UInt64, index: UInt64, handle: Handle)
}

extension RopeIndex {
        public var owner: Rope<C> {
		switch self {
		case .start(let r), .end(let r), .interior(let r, _, _, _):
			return r
		}
	}
}

enum RopeNoSuchElement : Error {
case onInterior
case atStart
case atEnd
}

enum RopeIndexComparisonError : Error {
case MismatchedOwners
}

public func == <C>(_ l: RopeIndex<C>, _ r: RopeIndex<C>) -> Bool {
	do {
		return try l.equals(r)
	} catch {
		fatalError("Not comparable")
	}
}

public func < <C>(_ l: RopeIndex<C>, _ r: RopeIndex<C>) -> Bool {
	do {
		return try l.isLessThan(r)
	} catch {
		fatalError("Not comparable")
	}
}

extension RopeIndex {
        public func equals<C>(_ other: RopeIndex<C>) throws -> Bool {
		guard self.owner === other.owner else {
			throw RopeIndexComparisonError.MismatchedOwners
		}
		switch (self, other) {
		case (.start(_), .start(_)), (.end(_), .end(_)):
			return true
		case (.interior(_, _, _, let h), .interior(_, _, _, let j))
		    where h == j:
			return true
		default:
			return false
		}
	}
	public func isLessThan<C>( _ other: RopeIndex<C>) throws -> Bool {
		guard self.owner === other.owner else {
			throw RopeIndexComparisonError.MismatchedOwners
		}
		switch (self, other) {
		case (.start(_), .start(_)):
			return false
		case (.start(_), _):
			return true
		case (.end(_), .end(_)):
			return false
		case (_, .end(_)):
			return true
		case (.interior(_, let g1, _, let h1),
		      .interior(_, let g2, _, let h2)) where g1 != g2:
			return self.owner.containsIndex(h1, before: h2)
		case (.interior(_, _, let m, _), .interior(_, _, let n, _))
		    where m < n:
			return true
		default:
			return false
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
public class Rope<C : Content> : Collection {
	public typealias Content = C
	public typealias Element = Node<C>
	public typealias Index = RopeIndex<C>
	public var top: Node<C>
	public var generation: UInt64 = 0
	public var startIndex: Index {
		if top.startIndex == top.endIndex {
                        return .end(of: self)
		}
                return .start(of: self)
	}
        public var endIndex: Index { return .end(of: self) }

	public init() {
		top = .empty
	}
	public var node: Node<C> {
		get {
			return top
		}
		set {
			top = newValue
		}
	}
	public func containsIndex(_ h1: Handle, before h2: Handle) -> Bool {
		return top.containsIndex(h1, before: h2)
	}
	public init<T>(content t: T) where C : Initializable,
	    C.Initializer == T, T : Collection {
		top = Node(content: t)
	}
	public func index(after i: Index) -> Index {
		guard i.owner === self else {
			fatalError("Mismatched owner")
		}
		switch i {
		case .start(_):
			let h = Handle()
			guard case
			    .step(let n) = top.afterStepInsertingIndex(h) else {
                                return .end(of: self)
			}
			top = n
                        return .interior(of: self, at: generation,
			                 index: 0, handle: h)
		case .end(_):
			fatalError("No index after .endIndex")
                case .interior(_, _, let m, let h):
			let j = Handle()
			switch top.insertingIndex(j, oneStepAfter: h) {
			case .inchOut:
				fatalError(
				    ".interior(\(m), \(h)) already at end?")
			case .absent:
				fatalError(
				    ".interior(\(m), \(h)) is absent")
			case .stepOut:
                                return .end(of: self)
			case .step(let node):
				top = node
                                return .interior(of: self,
				                 at: generation,
						 index: m + 1,
						 handle: j)
			}
		}
	}

	public subscript(i: Index) -> Iterator.Element {
//		get { return .leaf([:], Content.init(unit: C.unit)) }
		get {
			do {
				return try element(at: i)
			} catch {
				fatalError("No such element")
			}
		}
        }
        public func element(at i: Index) throws -> Iterator.Element {
		switch i {
		case .start(_):
			guard case .step(let node) = top.firstElement()
			    else {
				throw RopeNoSuchElement.atStart
			}
			return node
		case .interior(_, _, _, let h):
			let result = top.element(at: h)
			guard case .step(let node) = result else {
				throw RopeNoSuchElement.onInterior
			}
			return node
		case .end(_):
			throw RopeNoSuchElement.atEnd
		}
        }
	public func insert(_ elt: Element, at i: Index) {
		guard self === i.owner else {
			fatalError("Invalid index")
		}
		if case .empty = elt {
			fatalError("You may not insert .empty")
		}
		switch i {
		case .start(_):
                        top = Node<C>(left: elt, right: top)
		case .end(_):
                        top = Node(left: top, right: elt)
		case .interior(_, _, _, let h):
			top = top.inserting(elt, at: h)
		}
	}
}

extension Node {
	public func inserting(_ elt: Node, at target: Handle) -> Node {
		switch self {
		case .index(let w):
			guard let handle = w.get(), handle == target else {
				fatalError("Invalid index")
			}
                        return elt
		case .cursor(_, _):
			fatalError("Invalid index")
		case .container(let h, let r):
			return Node(handle: h, node: r.inserting(elt, at: target))
		case .concat(let l, _, _, _, let r, _):
			if l.containsIndex(target) {
				return Node(left: l.inserting(elt, at: target),
				            right: r)
			} else if r.containsIndex(target) {
				return Node(left: l,
				            right: r.inserting(elt, at: target))
			} else {
				fatalError("Invalid index")
			}
		case .leaf(_, _), .empty:
			fatalError("Invalid index")
		}
	}
}

extension Node {
	// TBD introduce a property for all Handles but the
	// index Handles?
	public var hids: Set<Handle.Id> {
		switch self {
		case .index(let w):
			guard let handle = w.get() else {
				return []
			}
			return [handle.id]
		case .cursor(let handle, _):
			return [handle.id]
		case .container(let handle, let rope):
			let hids: Set<Handle.Id> = [handle.id]
			return hids.union(rope.hids)
		case .concat(_, _, _, let hids, _, _):
			return hids
		case .leaf(_, _), .empty:
			return []
		}
	}
	public func containsIndex(_ target: Handle) -> Bool {
		switch self {
		case .index(let w):
			guard let handle = w.get() else {
				return false
			}
			return handle == target
		case .cursor(target, _):
			return true
		case .container(_, let rope):
			return rope.containsIndex(target)
		case .concat(_, _, _, let hids, _, _):
			return hids.contains(target.id)
		case .leaf(_, _), .empty:
			return false
                case .cursor(_, _):
                        return false
                }
	}
	public func containsIndex(_ h1: Handle, before h2: Handle) -> Bool {
		switch self {
		case .index(_):
			fatalError("Cannot order handles on an .index(_)")
		case .cursor(_, _):
			fatalError("Cannot order handles on a .cursor(_)")
		case .container(_, let rope):
			return rope.containsIndex(h1, before: h2)
		case .concat(let l, _, _, let hids, let r, _):
                        guard hids.contains(h1.id) && hids.contains(h2.id) else {
                                return false
                        }
			return l.containsIndex(h1, before: h2) ||
			    (l.containsIndex(h1) && r.containsIndex(h2)) ||
			    r.containsIndex(h1, before: h2)
		case .leaf(_, _):
			fatalError("Cannot order handles on a .leaf(_, _)")
		case .empty:
			fatalError("Cannot order handles on an .empty")
		}
	}
}

/*
extension Rope : ExpressibleByStringLiteral,
    ExpressibleByExtendedGraphemeClusterLiteral where
    Rope.Content : ExpressibleByStringLiteral {
	public init(stringLiteral s: S) {
		top = Node<Content>(content: s)
	}
}
*/

extension Substring : Initializable {
	public typealias Initializer = String
}

extension Node {
	public init(handle h: Handle, node n: Node<C>) {
		self = .container(h, n)
	}
	public init(holder: Handle) {
		self = .index(Weak(holder))
	}
	public init(left: Node<C>, right: Node<C>) {
		self = .concat(left, left.endIndex,
		               1 + max(left.depth, right.depth),
			       left.hids.union(right.hids), right,
			       left.endIndex + right.endIndex)
	}
	public init(content c: C) {
		if c.isEmpty {
			self = Node<C>.empty
		} else {
			self = Node<C>.leaf([:], c)
		}
	}
	public init<I>(content i: I) where C : Initializable,
	    C.Initializer == I, I : Collection {
		self.init(content: C(i))
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
		case .index(let w) where w.get() != nil:
			return "⬦"
		case .index(_):
			return "."
		case .cursor(_, _):
			return "|"
		case .container(_, let rope):
			return "(\(rope.debugDescription))"
		case .concat(let l, _, _, _, let r, _):
			return "[\(l) \(r)]"
		case .leaf(_, let s):
			return "\"\(s)\""
		case .empty:
			return "\"\""
		}
	}
}

func +(_ l: NodeIndex, _ r: NodeIndex) -> NodeIndex {
	return NodeIndex(utf16Offset: l.utf16Offset + r.utf16Offset)
}

public struct Weak<O : AnyObject> {
	typealias Reference = O
	private var _f: () -> O?
	public var get: () -> O? { return _f }
	init(_ o: O) {
		_f = { [weak o] in o }
	}
}

public extension Node {
	typealias Index = NodeIndex
	var leaves: LeafSequence<Content> {
		return LeafSequence(of: self)
	}
	var depth: UInt {
		switch self {
		case .leaf(_), .cursor(_, _), .empty, .index(_):
			return 0
		case .container(_, let rope):
			return rope.depth
		case .concat(_, _, let depth, _, _, _):
			return depth
		}
	}
	var content: C {
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
	var startIndex: NodeIndex {
		return NodeIndex.start
	}
	var midIndex: Index {
		switch self {
		case .container(_, let rope):
			return rope.midIndex
		case .concat(_, let idx, _, _, _, _):
			return idx
		case .leaf(_), .empty, .cursor(_, _), .index(_):
			return self.endIndex
		}
	}
	var endIndex: NodeIndex {
		switch self {
		case Node<C>.concat(_, _, _, _, _, let idx):
			return idx
		case .container(_, let rope):
			return rope.endIndex
		case .leaf(_, let s):
			let endOffset = s.endIndex.utf16Offset(in: s)
			let startOffset = s.startIndex.utf16Offset(in: s)
			return NodeIndex(utf16Offset: endOffset - startOffset)
		case .empty, .index(_):
			return NodeIndex.start
		case .cursor(_, _):
			return NodeIndex.start
		}
	}
	func element(at i: NodeIndex) -> Element {
		switch self {
		case .leaf(_, let s):
			let idx =
			    String.Index(utf16Offset: i.utf16Offset, in: s)
			let c: Element = s[idx]
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
	func appending(_ rope: Node) -> Node {
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
	var balanced: Bool {
		return endIndex.utf16Offset >= fibonacci(index: depth + 2)
	}
	func cleaned() -> Node<C>? {
		switch self {
		case .empty, .cursor(_, _), .leaf(_, _):
			return self
		case .container(let h, let n):
			guard let nn = n.cleaned() else {
				return nil
			}
			return .container(h, nn)
		case .index(let w):
			guard let handle = w.get() else {
				return nil
			}
			return Node(holder: handle)
		case .concat(let l, _, _, _, let r, _):
			guard let nl = l.cleaned() else {
				return r.cleaned()
			}
			guard let nr = r.cleaned() else {
				return nil
			}
			return Node(left: nl, right: nr)
		}
	}
	func rebalanced() -> Node<C> {
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
			if fn > totlen.utf16Offset {
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
				if fip3 >= rope.endIndex.utf16Offset {
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
	func subrope(from: NodeIndex, to: NodeIndex, depth: Int = 0)
	    -> Node<C> {
		assert(NodeIndex.start <= from)
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
			if from == NodeIndex.start && idx <= to {
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
					from: max(NodeIndex.start, from - idx),
					to: min(endIndex - idx, to - idx),
					depth: depth + 1)
			}
			return l.appending(r)
		case let .leaf(attrs, s):
			let i = String.Index(utf16Offset: from.utf16Offset,
			    in: s)
			let j = String.Index(utf16Offset: to.utf16Offset, in: s)
			if i >= j {
				return .empty
			}
			return .leaf(attrs, s[i..<j])
		}
	}
	func deleting(from start: Index, to end: Index) -> Node {
		return subrope(from: NodeIndex.start, to: start).appending(
		    subrope(from: end, to: endIndex))
	}
	func insertingCursor(_ handle: Handle, attributes: Attributes,
	    at i: Index) -> Node {
		let cursor: Node = .cursor(handle, attributes)
		return subrope(from: NodeIndex.start, to: i).appending(
		    cursor).appending(subrope(from: i, to: endIndex))
	}
	func insertingContent(_ rope: Node, at i: Index) -> Node {
		return subrope(from: NodeIndex.start, to: i).appending(
		    rope).appending(subrope(from: i, to: endIndex))
	}
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

