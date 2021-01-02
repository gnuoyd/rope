//
// Copyright (c) 2019, 2020 David Young.  All rights reserved.
//
import Foundation

public typealias Attributes = [NSAttributedString.Key : Any]

public class ExtentController<C : Content> : Handle {
	func subrope(of interior: Node<C>, from: NodeIndex, to: NodeIndex,
	    depth: Int = 0) -> Node<C> {
		return .extent(self, interior.subrope(from: from,
		    to: to, depth: depth))
	}
}

/* A Node directly encodes the presence of cursors because it is
 * possible for a cursor to move up and down the hierarchy of text
 * extents without changing between-character positions.  A cursor
 * can appear at the position left of the first character or right of
 * the last character in a Node.  A cursor can also appear in a
 * Node that contains no characters.
 */
public indirect enum Node<C : Content> {
public typealias Content = C
public typealias Element = C.Element
case cursor(Handle, Attributes)
case index(Weak<Handle>)
case extent(ExtentController<C>, Node)
case concat(Node, NodeIndex, UInt, Set<Handle.Id>, Node, NodeIndex)
case leaf(Attributes, C)
case empty
}

/*
 * Result of taking a step in a Node.  A full step moves up or down
 * the extent hierarchy or across a UTF-16 element.
 */
public enum Step<C: Content> {
case absent		/* The location to step from could not be
			 * found.  TBD: throw an Error, instead?
			 */
case step(Node<C>)	/* A full step occurred, resulting in the
			 * associated Node
			 */
case inchOut		/* A partial step occurred: stepping over a
			 * content-free Node `n` such as .cursor, .empty,
			 * or .index.
			 *
			 * The full step must be completed on the `n`'s
			 * parent.
			 */
case stepOut		/* A full upward step occurred: stepping over
			 * the boundary of extent `n`, or stepping over
			 * the last UTF-16 element of a leaf, `n`.
			 *
			 * The full step lands on `n`'s parent.
			 */
}

/*
 * Result of looking up an element of a Node
 */
public enum ElementResult<C : Content> {
case absent
case inchOut
case step(Node<C>)
}

public func == <C>(_ l: Node<C>, _ r: Node<C>) -> Bool {
	switch (l, r) {
	case (.cursor(let lHandle, _),
	      .cursor(let rHandle, _)):
		// XXX doesn't match attributes
		return lHandle == rHandle
	case (.index(let lWeakHandle), .index(let rWeakHandle)):
		return lWeakHandle.get() == rWeakHandle.get()
	case (.extent(let lCtlr, let lNode),
	      .extent(let rCtlr, let rNode)):
		return lCtlr == rCtlr && lNode == rNode
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

public extension Node {
	func insertingIndex(_ j: Handle, oneStepAfter i: Handle,
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
	func afterStepInsertingIndex(_ j: Handle) -> Step<C> {
		switch self {
		/* A step over a cursor, index, or empty string is NOT
		 * a full step.
		 */
		case .cursor(_, _), .empty, .index(_):
			return .inchOut
		/* A step into an extent is a full step. */
		case .extent(let ctlr, let n):
			return .step(.extent(ctlr,
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
	func insertingIndex(_ j: Handle, oneStepAfter i: Handle)
	    -> Step<C> {
		switch self {
		case .index(let w) where w.get() == i:
			return .inchOut
		case .cursor(_, _), .index(_), .leaf(_, _), .empty:
			return .absent
		case .extent(let ctlr, let n):
			switch n.insertingIndex(j, oneStepAfter: i) {
			case .inchOut:
				return .stepOut
			case .stepOut:
				return .step(.extent(ctlr,
				    Node(left: n, right: Node(holder: j))))
			case .step(let newn):
				return .step(.extent(ctlr, newn))
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
				switch result {
				case .step(let newr):
					return .step(Node(left: l, right: newr))
				default:
					return result
				}
			}
		}
	}
	/* TBD extract `performing` from `insertingIndex(_:,oneStepAfter:)`
	 * and element(at:) ?
	 */
	func firstElement() -> ElementResult<C> {
		switch self {
		case .cursor(_, _), .index(_), .empty:
			/* No match: the element is not on this span. */
			return .inchOut
		case .leaf(let attrs, let content):
			switch content.headAndTail {
			case (let head, _)?:
				return .step(.leaf(attrs, head))
			default:
				return .inchOut
			}
		case .extent(_, _):
			return .step(.empty)
		case .concat(let l, _, _, _, let r, _):
			return l.firstElementUsingSibling(r)
		}
	}
	func firstElementUsingSibling(_ r: Node) ->
	    ElementResult<C> {
		switch firstElement() {
		case .inchOut:
			return r.firstElement()
		case let result:
			return result
		}
	}
	func element(at i: Handle, sibling r: Node) ->
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
	func element(at i: Handle) -> ElementResult<C> {
		switch self {
		case .index(let w) where w.get() == i:
			/* The index matches: inch out so that the caller
			 * returns some element right of the index.
			 */
			return .inchOut
		case .cursor(_, _), .empty, .index(_), .leaf(_, _):
			/* No match: the element is not on this span. */
			return .absent
		case .extent(let ctlr, let n):
			switch n.element(at: i) {
			case .inchOut:
				return .step(.extent(ctlr, .empty))
			case .step(let newn):
				return .step(.extent(ctlr, newn))
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

public extension Node {
	func attributes(at i: NodeIndex, base: NodeIndex)
	    -> (Attributes, Range<NodeIndex>) {
		guard case .leaf(let attrs, _) = self, i < endIndex else {
			fatalError("Index out of bounds")
		}
		return (attrs, base..<base + endIndex)
	}
	func attributes(at i: NodeIndex) -> (Attributes, Range<NodeIndex>) {
		return apply({ (node: Node, i: NodeIndex, base: NodeIndex) in node.attributes(at: i, base: base) }, at: i)
	}
	func applying(_ fn: (Node) -> Node, range: Range<NodeIndex>) -> Node {
		let l = subrope(from: NodeIndex.start, to: range.lowerBound)
		let m = subrope(from: range.lowerBound, to: range.upperBound)
		let r = subrope(from: range.upperBound, to: endIndex)
		return l.appending(fn(m)).appending(r)
	}
	func settingAttributes(_ attrs: Attributes) -> Node {
		switch self {
		case .cursor(let h, _):
			return .cursor(h, attrs)
		case .extent(let ctlr, let n):
			return .extent(ctlr, n.settingAttributes(attrs))
		case .concat(let l, _, _, _, let r, _):
			return Node(left: l.settingAttributes(attrs),
			    right: r.settingAttributes(attrs))
		case .leaf(_, let content):
			return .leaf(attrs, content)
		case .empty, .index(_):
			return self
		}
	}
	func clearingAttributes() -> Node {
		switch self {
		case .cursor(let h, _):
			return .cursor(h, [:])
		case .extent(let ctlr, let n):
			return .extent(ctlr, n.clearingAttributes())
		case .concat(let l, _, _, _, let r, _):
			return Node(left: l.clearingAttributes(),
			    right: r.clearingAttributes())
		case .leaf(_, let content):
			return .leaf([:], content)
		case .empty, .index(_):
			return self
		}
	}
	func addingAttributes(_ nattrs: Attributes) -> Node {
		switch self {
		case .cursor(let h, var attrs):
			attrs.merge(nattrs) { (_, new) in new }
			return .cursor(h, attrs)
		case .extent(let ctlr, let n):
			return .extent(ctlr, n.addingAttributes(nattrs))
		case .concat(let l, _, _, _, let r, _):
			return Node(left: l.addingAttributes(nattrs),
			    right: r.addingAttributes(nattrs))
		case .leaf(var attrs, let content):
			attrs.merge(nattrs) { (_, new) in new }
			return .leaf(attrs, content)
		case .empty, .index(_):
			return self
		}
	}
	func settingAttributes(_ attrs: Attributes, range: Range<NodeIndex>)
	    -> Node {
		return applying({ node in node.settingAttributes(attrs) },
		    range: range)
	}
	func clearingAttributes(range: Range<NodeIndex>)
	    -> Node {
		return applying({ node in node.clearingAttributes() },
		    range: range)
	}
	func addingAttributes(_ attrs: Attributes, range: Range<NodeIndex>)
	    -> Node {
		return applying({ node in node.addingAttributes(attrs) },
		    range: range)
	}
}

public extension Node {
	func inserting(_ elt: Node, at target: Handle) -> Node {
		switch self {
		case .index(let w):
			guard let handle = w.get(), handle == target else {
				fatalError("Invalid index")
			}
			return elt
		case .cursor(_, _):
			fatalError("Invalid index")
		case .extent(let ctlr, let r):
			/* EXT let the controller handle the request */
			return Node(controller: ctlr,
			            node: r.inserting(elt, at: target))
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

public extension Node {
	// TBD introduce a property for all Handles but the
	// index Handles?
	var hids: Set<Handle.Id> {
		switch self {
		case .index(let w):
			guard let handle = w.get() else {
				return []
			}
			return [handle.id]
		case .cursor(let handle, _):
			return [handle.id]
		case .extent(let ctlr, let rope):
			let hids: Set<Handle.Id> = [ctlr.id]
			return hids.union(rope.hids)
		case .concat(_, _, _, let hids, _, _):
			return hids
		case .leaf(_, _), .empty:
			return []
		}
	}
	func containsIndex(_ target: Handle) -> Bool {
		switch self {
		case .index(let w):
			guard let handle = w.get() else {
				return false
			}
			return handle == target
		case .cursor(target, _):
			return true
		case .extent(_, let rope):
			return rope.containsIndex(target)
		case .concat(_, _, _, let hids, _, _):
			return hids.contains(target.id)
		case .leaf(_, _), .empty:
			return false
		case .cursor(_, _):
			return false
		}
	}
	func containsIndex(_ h1: Handle, before h2: Handle) -> Bool {
		switch self {
		case .index(_):
			fatalError("Cannot order handles on an .index(_)")
		case .cursor(_, _):
			fatalError("Cannot order handles on a .cursor(_)")
		case .extent(_, let rope):
			return rope.containsIndex(h1, before: h2)
		case .concat(let l, _, _, let hids, let r, _):
			guard hids.contains(h1.id) && hids.contains(h2.id)
			    else {
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

public extension Node {
	init(controller ctlr: ExtentController<C>, node n: Node<C>) {
		self = .extent(ctlr, n)
	}
	init(holder: Handle) {
		self = .index(Weak(holder))
	}
	init(left: Node<C>, right: Node<C>) {
		self = .concat(left, left.endIndex,
		               1 + max(left.depth, right.depth),
			       left.hids.union(right.hids), right,
			       left.endIndex + right.endIndex)
	}
	init(content c: C, attributes attrs: Attributes = [:]) {
		if c.isEmpty {
			self = Node<C>.empty
		} else {
			self = Node<C>.leaf(attrs, c)
		}
	}
	init<I>(content i: I) where C : Initializable,
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
			case .empty, .cursor(_, _), .extent(_, _), .index(_):
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
		case .extent(_, let rope):
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

public extension Node {
	typealias Index = NodeIndex
	var leaves: LeafSequence<Content> {
		return LeafSequence(of: self)
	}
	var depth: UInt {
		switch self {
		case .leaf(_, _), .cursor(_, _), .empty, .index(_):
			return 0
		case .extent(_, let rope):
			return rope.depth
		case .concat(_, _, let depth, _, _, _):
			return depth
		}
	}
	var content: C {
		switch self {
		case .cursor(_, _), .empty, .index(_):
			return C.empty
		case .extent(_, let rope):
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
		case .extent(_, let rope):
			return rope.midIndex
		case .concat(_, let idx, _, _, _, _):
			return idx
		case .leaf(_, _), .empty, .cursor(_, _), .index(_):
			return self.endIndex
		}
	}
	var endIndex: NodeIndex {
		switch self {
		case Node<C>.concat(_, _, _, _, _, let idx):
			return idx
		case .extent(_, let rope):
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
	var length: Int {
		return endIndex.utf16Offset - startIndex.utf16Offset
	}
	func apply<R>(_ fn: (Node, NodeIndex, NodeIndex) -> R,
	    at i: NodeIndex, base: NodeIndex = NodeIndex.start) -> R {
		switch self {
		case .leaf(_, _), .cursor(_, _), .empty, .index(_):
			return fn(self, i, base)
		case .concat(let ropel, let idx, _, _, let roper, _):
			if i < idx {
				return ropel.apply(fn, at: i, base: base)
			} else {
				return roper.apply(fn, at: i - idx,
				    base: base + idx)
			}
		case .extent(_, let rope):
			return rope.apply(fn, at: i, base: base)
		}
	}
	func utf16(at i: NodeIndex) -> C.UTF16View.Element {
		func utf16(_ node: Node, at i: NodeIndex, base: NodeIndex)
		    -> C.UTF16View.Element {
			guard case .leaf(_, let s) = node else {
				fatalError("In \(#function), no utf16 \(i)")
			}
			let sidx = C.Index(utf16Offset: i.utf16Offset, in: s)
			return s.utf16[sidx]
		}
		return apply(utf16, at: i)
	}
	func handlesLeading(to i0: NodeIndex) -> [Handle] {
		var path: [Handle] = []
		var i = i0
		var next = self
		while true {
			switch next {
			case .leaf(_, _), .cursor(_, _), .empty, .index(_):
				return path
			case .concat(let ropel, let idx, _, _, let roper, _):
				if i < idx {
					next = ropel
				} else {
					i = i - idx
					next = roper
				}
			case .extent(let ctlr, let rope):
				path.append(ctlr)
				next = rope
			}
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
		case .extent(_, let rope):
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
	// Return this Node with all of the expired indices removed. 
	func cleaned() -> Node<C>? {
		switch self {
		case .empty, .cursor(_, _), .leaf(_, _):
			return self
		case .extent(let ctlr, let n):
			guard let nn = n.cleaned() else {
				return nil
			}
			return .extent(ctlr, nn)
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
	// Return a copy of this Rope with its balance restored.
	func rebalanced() -> Node<C> {
		switch self {
		case .empty, .cursor(_, _), .leaf(_, _):
			return self
		case .extent(let ctlr, let rope):
			return .extent(ctlr, rope.rebalanced())
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
		/* EXT Need to take care subdividing extents.  Perhaps subrope
		 * should be an ExtentController method?
		 *
		 * XXX Today, this code simply drops the extent from the
		 * subrope!
		 */
		case .extent(let ctlr, let rope):
			return ctlr.subrope(of: rope, from: from, to: to,
			    depth: depth)
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
	subscript(range: Range<Index>) -> Content {
		return subrope(from: range.lowerBound,
			to: range.upperBound).content
	}
	func replacing(range: Range<Index>, with c: Content) -> Node {
		let l = subrope(from: NodeIndex.start, to: range.lowerBound)
		let r = subrope(from: range.upperBound, to: endIndex)
		return l.appending(Node(content: c)).appending(r)
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
