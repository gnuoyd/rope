//
// Copyright (c) 2019, 2020 David Young.  All rights reserved.
//
import Foundation

public typealias Attributes = [NSAttributedString.Key : Any]

infix operator ~: ComparisonPrecedence
infix operator !~: ComparisonPrecedence

extension Dictionary where Key == NSAttributedString.Key, Value == Any {
	static func ~(_ lhs: Self, _ rhs: Self) -> Bool {
		/* All empty attributed strings are equal, so compare two
		 * non-empty strings containing the same text to see if
		 * the attributes differ or not.
		 */
		let l = NSAttributedString(string: "x", attributes: lhs)
		let r = NSAttributedString(string: "x", attributes: rhs)
		return l.isEqual(to: r)
	}
}

func *(_ s: String, _ times: Int) -> String {
	if times < 0 {
		return String(s.reversed()) * -times
	}
	if times == 0 {
		return ""
	}
	if times.isMultiple(of: 2) {
		let t = s * (times / 2)
		return t + t
	}
	return s + s * (times - 1)
}

extension Rope.ExtentController {
	func subrope(of content: Rope.Node, from: Rope.Node.Offset,
	    tightly: Bool, depth: Int = 0) -> Rope.Node {
		let subcontent = content.subrope(from: from, depth: depth)
		return .extent(self, subcontent)
	}
	func subrope(of content: Rope.Node, to: Rope.Node.Offset,
	    tightly: Bool, depth: Int = 0) -> Rope.Node {
		let subcontent = content.subrope(to: to, depth: depth)
		return .extent(self, subcontent)
	}
	func subrope(of content: Rope.Node, after boundary: Rope.Index,
	    depth: Int = 0) -> Rope.Node? {
		guard let subcontent = content.subrope(after: boundary,
		    depth: depth) else {
			return nil
		}
		return .extent(self, subcontent)
	}
	func subrope(of content: Rope.Node, to: Rope.Index, depth: Int = 0)
	    -> Rope.Node? {
		guard let subcontent = content.subrope(to: to, depth: depth)
		    else {
			return nil
		}
		return .extent(self, subcontent)
	}
	func node(_ content: Rope.Node, inserting elt: Rope.Node,
	    at target: Handle) -> Rope.Node? {
		guard let subcontent = content.inserting(elt, at: target) else {
			return nil
		}
		return .extent(self, subcontent)
	}
}

extension Rope.Node {
	/*
	 * Result of taking a step in a Node.  A full step moves up or down
	 * the extent hierarchy or across a UTF-16 element.
	 */
	public enum Step {
	case absent		/* The location to step from could not be
				 * found.  TBD: throw an Error, instead?
				 */
	case step(Rope.Node)	/* A full step occurred, resulting in the
				 * associated Node
				 */
	case inchOut		/* A partial step occurred: stepping over a
				 * content-free Node `n` such as .cursor,
				 * .empty, or .index.
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
}

/*
 * Result of looking up an element of a Node
 */
public extension Rope.Node {
	enum ElementResult {
	case absent
	case inchOut
	case step(Rope.Node)
	}
}

/* Return true iff `lhs` is equal to `rhs`, ignoring embedded indices,
 * text attributes, and the *number* of leaves.  The *text* of leaves must
 * the same.
 */
extension Rope.Node where Content == Substring {
	public static func ~(_ lhs: Self, _ rhs: Self) -> Bool {
		let lleaves = lhs.leaves.makeIterator(),
		    rleaves = rhs.leaves.makeIterator()
		var lresidue: Self? = nil, rresidue: Self? = nil
		while true {
			switch (lresidue ?? lleaves.next(),
				rresidue ?? rleaves.next()){
			case (nil, nil):
				return true
			case (.index(_), let r), (.empty, let r):
				lresidue = nil
				rresidue = r
			case (let l, .index(_)), (let l, .empty):
				lresidue = l
				rresidue = nil
			case (.cursor(let l, _), .cursor(let r, _)):
				if l != r {
					return false
				}
				lresidue = nil
				rresidue = nil
			case (.extent(let lctlr, let l),
			      .extent(let rctlr, let r)):
				if lctlr !== rctlr {
					return false
				}
				if l !~ r {
					return false
				}
				lresidue = nil
				rresidue = nil
			case (.leaf(let lattr, let l), .leaf(let rattr, let r)):
				if l == r {
					lresidue = nil
					rresidue = nil
					continue
				}
				if l.hasPrefix(r) {
					lresidue =
					    Self(content: l.dropFirst(r.count),
					         attributes: lattr)
					rresidue = nil
					continue
				}
				if r.hasPrefix(l) {
					lresidue = nil
					rresidue =
					    Self(content: r.dropFirst(l.count),
					         attributes: rattr)
					continue
				}
				return false
			default:
				return false
			}
		}
	}

	/* Return true iff `lhs ~ rhs` is false. */
	public static func !~(_ lhs: Self, _ rhs: Self) -> Bool {
		return !(lhs ~ rhs)
	}
}

extension Rope.Node {
	public static func == (_ l: Self, _ r: Self) -> Bool {
		switch (l, r) {
		case (.cursor(let lHandle, let lattrs),
		      .cursor(let rHandle, let rattrs)):
			return lHandle == rHandle && lattrs ~ rattrs
		case (.index(let lWeakHandle), .index(let rWeakHandle)):
			return lWeakHandle.get() == rWeakHandle.get()
		case (.extent(let lCtlr, let lNode),
		      .extent(let rCtlr, let rNode)):
			return lCtlr == rCtlr && lNode == rNode
		case (.concat(let lNode1, _, _, _, let lNode2, _),
		      .concat(let rNode1, _, _, _, let rNode2, _)):
			return lNode1 == rNode1 && lNode2 == rNode2
		case (.leaf(let lattrs, let lContent), .leaf(let rattrs, let rContent)):
			return lContent == rContent && lattrs ~ rattrs
		case (.empty, .empty):
			return true
		default:
			return false
		}
	}
}

public extension Rope.Node {
	enum Side {
	case left
	case right
	}
	enum DirectedStep {
	case rightStep
	case leftStep
	}
	func inserting(_ j: Handle, one step: DirectedStep, after i: Handle,
	    sibling: Rope.Node) -> Step {
		let result = inserting(j, one: step, after: i)
		switch (result, step) {
		case (.step(let newl), .rightStep):
			return .step(.nodes(newl, sibling))
		case (.step(let newr), .leftStep):
			return .step(.nodes(sibling, newr))
		case (.stepOut, .rightStep):
			return .step(.nodes(self, Rope.Node(holder: j),
			                    sibling))
		case (.stepOut, .leftStep):
			return .step(.nodes(sibling, Rope.Node(holder: j),
			                    self))
		case (.inchOut, .rightStep):
			switch sibling.inserting(j, after: .rightStep) {
			case .step(let newr):
				return .step(.nodes(self, newr))
			case let result:
				return result
			}
		case (.inchOut, .leftStep):
			switch sibling.inserting(j, after: .leftStep) {
			case .step(let newl):
				return .step(.nodes(newl, self))
			case let result:
				return result
			}
		case (.absent, .rightStep):
			return .absent
		case (.absent, .leftStep):
			return .absent
		}
	}
	func inserting(_ j: Handle, after step: DirectedStep) -> Step {
		switch (self, step) {
		/* A step over a cursor, index, or empty string is NOT
		 * a full step.
		 */
		case (.cursor(_, _), _), (.empty, _), (.index(_), _):
			return .inchOut
		/* A step into an extent is a full step. */
		case (.extent(let ctlr, let n), .rightStep):
			// *(...) -> (*...)
			return .step(.extent(under: ctlr, Rope.Node(holder: j),
			                            n))
		case (.extent(let ctlr, let n), .leftStep):
			// (...)* -> (...*)
			return .step(.extent(under: ctlr, n,
			                            Rope.Node(holder: j)))
		case (.leaf(let attrs, let content), .rightStep):
			switch content.firstAndRest {
			case (_, let rest)? where rest.isEmpty:
				return .stepOut
			case (let first, let rest)?:
				return .step(.nodes(.leaf(attrs, first),
				                    Rope.Node(holder: j),
						    .leaf(attrs, rest)))
			default:
				/* XXX Empty leaves shouldn't exist. */
				return .inchOut
			}
		case (.leaf(let attrs, let content), .leftStep):
			switch content.restAndLast {
			case (let rest, _)? where rest.isEmpty:
				return .stepOut
			case (let rest, let last)?:
				return .step(.nodes(.leaf(attrs, rest),
				                    Rope.Node(holder: j),
						    .leaf(attrs, last)))
			default:
				/* XXX Empty leaves shouldn't exist. */
				return .inchOut
			}
		/* A step into a concatenation is NOT a full step. */
		case (.concat(let l, _, _, _, let r, _), .rightStep):
			switch l.inserting(j, after: .rightStep) {
			case .step(let newl):
				return .step(.nodes(newl, r))
			case .stepOut:
				return .step(.nodes(l, Rope.Node(holder: j), r))
			case .inchOut, .absent:
				break
			}
			switch r.inserting(j, after: .rightStep) {
			case .step(let newr):
				return .step(.nodes(l, newr))
			case let result:
				return result
			}
		/* A step into a concatenation is NOT a full step. */
		case (.concat(let l, _, _, _, let r, _), .leftStep):
			switch r.inserting(j, after: .leftStep) {
			case .step(let newr):
				return .step(.nodes(l, newr))
			case .stepOut:
				return .step(.nodes(l, Rope.Node(holder: j), r))
			case .inchOut, .absent:
				break
			}
			switch l.inserting(j, after: .leftStep) {
			case .step(let newl):
				return .step(.nodes(newl, r))
			case let result:
				return result
			}
		}
	}
	func inserting(index h: Handle, abutting side: Side,
	    of offset: Offset) -> Rope.Node {
		switch self {
		case .cursor(_, _), .index(_), .empty:
			assert(offset == 0)
			switch side {
			case .left:
				return Rope.Node(holder: h).appending(self)
			case .right:
				return self.appending(Rope.Node(holder: h))
			}
		case .leaf(let attrs, let content):
			let idx = String.Index(utf16Offset: offset.utf16Offset,
			    in: content)
			let l = content.prefix(upTo: idx)
			let r = content.suffix(from: idx)
			return Rope.Node(content: C.init(l), attributes: attrs)
			    .appending(Rope.Node(holder: h))
			    .appending(Rope.Node(content: C.init(r),
			        attributes: attrs))
		case .extent(let ctlr, let n):
			return Rope.Node(controller: ctlr,
			    node: n.inserting(index: h, abutting: side,
			        of: offset))
		case .concat(let l, let middle, _, _, let r, _):
			if offset < middle {
				return l.inserting(index: h, abutting: side,
				    of: offset).appending(r)
			}
			if middle < offset {
				return l.appending(r.inserting(index: h,
				    abutting: side, of: offset - middle))
			}
			switch side {
			case .left:
				return l.inserting(index: h, abutting: side,
				    of: offset).appending(r)
			case .right:
				return l.appending(r.inserting(index: h,
				    abutting: side, of: offset - middle))
			}
		}
	}
	func inserting(index h: Handle, at place: Offset) -> Rope.Node {
		switch self {
		case .cursor(_, _), .index(_), .empty:
			assert(place == 0)
			return self.appending(Rope.Node(holder: h))
		case .leaf(let attrs, let content):
			let idx = String.Index(utf16Offset: place.utf16Offset,
			    in: content)
			let l = content.prefix(upTo: idx)
			let r = content.suffix(from: idx)
			return Rope.Node(content: C.init(l), attributes: attrs)
			    .appending(Rope.Node(holder: h))
			    .appending(Rope.Node(content: C.init(r),
			        attributes: attrs))
		case .extent(let ctlr, let n):
			return Rope.Node(controller: ctlr,
			    node: n.inserting(index: h, at: place))
		case .concat(let l, let idx, _, _, let r, _) where place < idx:
			return l.inserting(index: h, at: place).appending(r)
		case .concat(let l, let idx, _, _, let r, _):
			return l.appending(
			    r.inserting(index: h, at: place - idx))
		}
	}
	func inserting(_ j: Handle, one step: DirectedStep, after i: Handle)
	    -> Step {
		switch (self, step) {
		case (.index(let w), _) where w.get() == i:
			return .inchOut
		case (.cursor(_, _), _), (.index(_), _),
		     (.leaf(_, _), _), (.empty, _):
			return .absent
		case (.extent(let ctlr, let n), _):
			switch (n.inserting(j, one: step, after: i), step) {
			case (.inchOut, _):
				return .stepOut
			case (.stepOut, .rightStep):
				return .step(.extent(under: ctlr,
				    n, Rope.Node(holder: j)))
			case (.stepOut, .leftStep):
				return .step(.extent(under: ctlr,
				    Rope.Node(holder: j), n))
			case (.step(let newn), _):
				return .step(.extent(ctlr, newn))
			case (.absent, _):
				return .absent
			}
		case (.concat(.index(let w), _, _, _, let r, _), .rightStep)
		    where w.get() == i:
			switch r.inserting(j, after: .rightStep) {
			case .step(let newr):
				return .step(.nodes(Rope.Node(holder: i), newr))
			case let result:
				return result
			}
		case (.concat(let l, _, _, _, .index(let w), _), .leftStep)
		    where w.get() == i:
			switch l.inserting(j, after: .leftStep) {
			case .step(let newl):
				return .step(.nodes(newl, Rope.Node(holder: i)))
			case let result:
				return result
			}
		case (.concat(let l, _, _, _, let r, _), _):
			let id = i.id
			switch (l.hids.contains(id), r.hids.contains(id),
				step) {
			case (false, false, _):
				return .absent
			case (true, true, _):
				assert(l.hids.contains(id) !=
				       r.hids.contains(id))
				return .inchOut
			case (true, false, .rightStep):
				return l.inserting(j, one: .rightStep, after: i,
				    sibling: r)
			case (false, true, .rightStep):
				let result = r.inserting(j, one: .rightStep,
				    after: i)
				switch result {
				case .step(let newr):
					return .step(.nodes(l, newr))
				default:
					return result
				}
			case (false, true, .leftStep):
				return r.inserting(j, one: .leftStep, after: i,
				    sibling: l)
			case (true, false, .leftStep):
				let result = l.inserting(j, one: .leftStep,
				    after: i)
				switch result {
				case .step(let newl):
					return .step(.nodes(newl, r))
				default:
					return result
				}
			}
		}
	}
	/* TBD extract `performing` from `inserting(_:,one:,after:)`
	 * and element(at:) ?
	 */
	func firstElement() -> ElementResult {
		switch self {
		case .cursor(_, _), .index(_), .empty:
			/* No match: the element is not on this span. */
			return .inchOut
		case .leaf(let attrs, let content):
			switch content.firstAndRest {
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
	func firstElementUsingSibling(_ r: Rope.Node) -> ElementResult {
		switch firstElement() {
		case .inchOut:
			return r.firstElement()
		case let result:
			return result
		}
	}
	func element(at i: Handle, sibling r: Rope.Node) -> ElementResult {
		switch element(at: i) {
		case .inchOut:
			return r.firstElement()
		case let result:
			return result
		}
	}
	/* TBD extract `performing` from `inserting(_:,one:,after:)`
	 * and element(at:) ?
	 */
	func element(at i: Handle) -> ElementResult {
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

public extension Rope.Node {
	func attributes(at i: Offset, base: Offset)
	    -> (Attributes, Range<Offset>) {
		guard case .leaf(let attrs, _) = self,
		    0 <= i, i < endIndex else {
			fatalError("Index out of bounds")
		}
		return (attrs, base..<base + endIndex)
	}
	func attributes(at i: Offset) -> (Attributes, Range<Offset>) {
		return transforming(at: i) {
			(node: Self, i: Offset, base: Offset) in
				node.attributes(at: i, base: base)
		}
	}
	func transforming(range: Range<Rope.Index>, with fn: (Self) -> Self)
	    -> Self? {
		guard let l = subrope(to: range.lowerBound) else {
			return nil
		}
		guard let m = subrope(after: range.lowerBound,
		                      to: range.upperBound) else {
			return nil
		}
		guard let r = subrope(after: range.upperBound) else {
			return nil
		}
		return l.appending(fn(m)).appending(r)
	}
	func transforming(range: Range<Offset>, with fn: (Self) -> Self)
	    -> Self {
		let l = subrope(from: 0, to: range.lowerBound)
		let m = subrope(from: range.lowerBound, to: range.upperBound)
		let r = subrope(from: range.upperBound, to: endIndex)
		return l.appending(fn(m)).appending(r)
	}
	func settingAttributes(_ attrs: Attributes) -> Self {
		switch self {
		case .cursor(let h, _):
			return .cursor(h, attrs)
		case .extent(let ctlr, let n):
			return .extent(ctlr, n.settingAttributes(attrs))
		case .concat(let l, _, _, _, let r, _):
			return .nodes(l.settingAttributes(attrs),
			              r.settingAttributes(attrs))
		case .leaf(_, let content):
			return .leaf(attrs, content)
		case .empty, .index(_):
			return self
		}
	}
	func clearingAttributes() -> Self {
		switch self {
		case .cursor(let h, _):
			return .cursor(h, [:])
		case .extent(let ctlr, let n):
			return .extent(ctlr, n.clearingAttributes())
		case .concat(let l, _, _, _, let r, _):
			return .nodes(l.clearingAttributes(),
			              r.clearingAttributes())
		case .leaf(_, let content):
			return .leaf([:], content)
		case .empty, .index(_):
			return self
		}
	}
	func addingAttributes(_ nattrs: Attributes) -> Self {
		switch self {
		case .cursor(let h, var attrs):
			attrs.merge(nattrs) { (_, new) in new }
			return .cursor(h, attrs)
		case .extent(let ctlr, let n):
			return .extent(ctlr, n.addingAttributes(nattrs))
		case .concat(let l, _, _, _, let r, _):
			return Self(left: l.addingAttributes(nattrs),
			    right: r.addingAttributes(nattrs))
		case .leaf(var attrs, let content):
			attrs.merge(nattrs) { (_, new) in new }
			return .leaf(attrs, content)
		case .empty, .index(_):
			return self
		}
	}
	func settingAttributes(_ attrs: Attributes, range r: Range<Rope.Index>)
	    -> Self? {
		return transforming(range: r) { node in
			node.settingAttributes(attrs)
		}
	}
	func settingAttributes(_ attrs: Attributes, range: Range<Offset>)
	    -> Self {
		return transforming(range: range) { node in
			node.settingAttributes(attrs)
		}
	}
	func clearingAttributes(range: Range<Offset>) -> Self {
		return transforming(range: range) { node in
			node.clearingAttributes()
		}
	}
	func addingAttributes(_ attrs: Attributes, range: Range<Offset>)
	    -> Self {
		return transforming(range: range) { node in
			node.addingAttributes(attrs)
		}
		    
	}
}

public extension Rope.Node {
	func inserting(_ elt: Self, at target: Handle) -> Self? {
		switch self {
		case .index(let w):
			guard let handle = w.get(), handle == target else {
				return nil
			}
			return elt
		case .cursor(_, _):
			return nil
		case .extent(let ctlr, let r):
			return ctlr.node(r, inserting: elt, at: target)
		case .concat(let l, _, _, _, let r, _):
			if l.contains(target) {
				guard let newl = l.inserting(elt, at: target)
				    else {
					return nil
				}
				return Self(left: newl, right: r)
			} else if r.contains(target) {
				guard let newr = r.inserting(elt, at: target)
				    else {
					return nil
				}
				return Self(left: l, right: newr)
			} else {
				return nil
			}
		case .leaf(_, _), .empty:
			return nil
		}
	}
}

public extension Rope.Node {
	// TBD introduce a property for all Handles but the
	// index Handles?
	var hids: HandleSet {
		switch self {
		case .index(let w):
			guard let handle = w.get() else {
				return []
			}
			return [handle.id]
		case .cursor(let handle, _):
			return [handle.id]
		case .extent(let ctlr, let rope):
			return rope.hids.union([ctlr.id])
		case .concat(_, _, _, let hids, _, _):
			return hids
		case .leaf(_, _), .empty:
			return []
		}
	}
	func contains(_ target: Handle) -> Bool {
		switch self {
		case .index(let w):
			guard let handle = w.get() else {
				return false
			}
			return handle == target
		case .cursor(target, _):
			return true
		case .extent(_, let rope):
			return rope.contains(target)
		case .concat(_, _, _, let hids, _, _):
			return hids.contains(target.id)
		case .leaf(_, _), .empty:
			return false
		case .cursor(_, _):
			return false
		}
	}
	func indices(follow target: Handle) -> Bool? {
		switch self {
		case .index(let w):
			if w.get() != target {
				return nil
			}
			return false
		case .extent(_, let rope):
			return rope.contains(target) ? true : nil
		case .concat(let l, let midx, _, _, let r, let w):
			switch l.indices(follow: target) {
			case nil:
				return r.indices(follow: target)
			case true?:
				return true
			case false?:
				return r.hids.extentCount > 0 ||
				       midx != w.utf16Offset
			}
		case .cursor(_, _), .leaf(_, _), .empty:
			return nil
		}
	}
	func indices(precede target: Handle) -> Bool? {
		switch self {
		case .index(let w):
			if w.get() != target {
				return nil
			}
			return false
		case .extent(_, let rope):
			return rope.contains(target) ? true : nil
		case .concat(let l, let midx, _, _, let r, _):
			switch r.indices(precede: target) {
			case nil:
				return l.indices(precede: target)
			case true?:
				return true
			case false?:
				return l.hids.extentCount > 0 || 0 != midx
			}
		case .cursor(_, _), .leaf(_, _), .empty:
			return nil
		}
	}
	func index(_ h1: Handle, precedes h2: Handle) -> Bool? {
		switch self {
		case .index(_):
			return nil
		case .cursor(_, _):
			return nil
		case .extent(_, let rope):
			return rope.index(h1, precedes: h2)
		case .concat(let l, _, _, let hids, let r, _):
			/* I'm not sure if short-circuiting here actually
			 * saves us much work.  Benchmark and see?
			 */
			guard hids.contains(h1.id) && hids.contains(h2.id)
			    else {
				return nil
			}
			if let ordered = l.index(h1, precedes: h2) {
				return ordered
			}
			if let ordered = r.index(h1, precedes: h2) {
				return ordered
			}
			guard let follow = l.indices(follow: h1),
			      let precede = r.indices(precede: h2) else {
				if l.contains(h2) && r.contains(h1) {
					return false
				}
				return nil
			}
			return follow || precede
		case .leaf(_, _):
			return nil
		case .empty:
			return nil
		}
	}
}

public extension Rope.Node {
	init(controller ctlr: Rope.ExtentController, node n: Self) {
		self = .extent(ctlr, n)
	}
	init(holder: Handle) {
		self = .index(Weak(holder))
	}
	private init(left: Self, right: Self) {
		switch (left, right) {
		case (_, .index(let w)) where w.get() == nil:
			self = left
		case (.index(let w), _) where w.get() == nil:
			self = right
		case (_, .empty):
			self = left
		case (.empty, _):
			self = right
		default:
			self = .concat(left, left.endIndex,
				       1 + max(left.depth, right.depth),
				       left.hids.union(right.hids), right,
				       left.dimensions + right.dimensions)
		}
	}
	init(content c: C, attributes attrs: Attributes = [:]) {
		if c.isEmpty {
			self = Self.empty
		} else {
			self = Self.leaf(attrs, c)
		}
	}
	init<I>(content i: I) where C : Initializable,
	    C.Initializer == I, I : Collection {
		self.init(content: C(i))
	}
}

extension Rope.Node {
	public class LeafIterator : IteratorProtocol {
		var stack: [Rope.Node]

		public init(for node: Rope.Node) {
			self.stack = [node]
		}
		public func next() -> Rope.Node? {
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
				case .empty, .cursor(_, _), .extent(_, _),
				     .index(_):
					return top
				}
			}
		}
	}

	public struct LeafSequence : Sequence {
		var node: Rope.Node
		public init(of node: Rope.Node) {
			self.node = node
		}
		public func makeIterator() -> LeafIterator {
			return LeafIterator(for: node)
		}
	}
}

extension Rope.Node : CustomDebugStringConvertible {
	public var debugDescription: String {
		switch self {
		case .index(let w) where w.get() != nil:
			return "⬦\(w.get()!.id)⬦"
		case .index(_):
			return "⬦⬦"
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

public extension Rope.Node {
	static func extent(under controller: Rope.ExtentController,
	    _ content: Self...) -> Self {
		return Self(controller: controller, node: tree(from: content))
	}
	static func extent(under controller: Rope.ExtentController,
	    with content: [Self]) -> Self {
		return Self(controller: controller, node: tree(from: content))
	}
	static func tree(from content: [Self]) -> Self {
		content.reduce(.empty) { (l: Self, r: Self) in
		    Self(left: l, right: r)
		}
	}
	static func nodes(_ content: Self...) -> Self {
		return tree(from: content)
	}
	static func text(_ content: C, attributes attrs: Attributes = [:])
	    -> Self {
		return Self(content: content, attributes: attrs)
	}
}

public extension Rope.Node {
	var leaves: LeafSequence {
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
	var startIndex: Offset {
		return 0
	}
	var midIndex: Offset {
		switch self {
		case .extent(_, let rope):
			return rope.midIndex
		case .concat(_, let idx, _, _, _, _):
			return idx
		case .leaf(_, _), .empty, .cursor(_, _), .index(_):
			return self.endIndex
		}
	}
	var dimensions: Dimensions {
		switch self {
		case Self.concat(_, _, _, _, _, let dims):
			return dims
		case .extent(_, let rope):
			return rope.dimensions + Dimensions(jots: 2)
		case .leaf(_, let s):
			let endOffset = s.endIndex.utf16Offset(in: s)
			let startOffset = s.startIndex.utf16Offset(in: s)
			return Dimensions(
			    utf16Offset: Offset(of: endOffset - startOffset))
		case .empty:
			return Dimensions.zero
		case .cursor(_, _), .index(_):
			return Dimensions(jots: 1)
		}
	}
	var halfPerimeter: Int {
		return dimensions.halfPerimeter
	}
	var endIndex: Offset {
		switch self {
		case Self.concat(_, _, _, _, _, let w):
			return w.utf16Offset
		case .extent(_, let rope):
			return rope.endIndex
		case .leaf(_, let s):
			let endOffset = s.endIndex.utf16Offset(in: s)
			let startOffset = s.startIndex.utf16Offset(in: s)
			return Offset(of: endOffset - startOffset)
		case .empty, .index(_), .cursor(_, _):
			return 0
		}
	}
	var length: Int {
		return endIndex.utf16Offset - startIndex.utf16Offset
	}
	func transforming<R>(at i: Offset, base: Offset = 0,
	    with fn: (Self, Offset, Offset) -> R) -> R {
		switch self {
		case .leaf(_, _), .cursor(_, _), .empty, .index(_):
			return fn(self, i, base)
		case .concat(let ropel, let idx, _, _, let roper, _):
			if i < idx {
				return ropel.transforming(at: i, base: base,
				    with: fn)
			} else {
				return roper.transforming(at: i - idx,
				    base: base + idx, with: fn)
			}
		case .extent(_, let rope):
			return rope.transforming(at: i, base: base, with: fn)
		}
	}
	func utf16(at i: Offset) -> C.UTF16View.Element {
		func utf16(_ node: Self, at i: Offset, base: Offset)
		    -> C.UTF16View.Element {
			guard case .leaf(_, let s) = node else {
				fatalError("In \(#function), no utf16 \(i)")
			}
			let sidx = C.Index(utf16Offset: i.utf16Offset, in: s)
			return s.utf16[sidx]
		}
		return transforming(at: i, with: utf16)
	}
	func extents(enclosing i0: Offset) -> [Rope.ExtentController] {
		var path: [Rope.ExtentController] = []
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
	func extents(enclosing i: Rope.Index,
	                      in controllers: [Rope.ExtentController] = [])
	    -> [Rope.ExtentController]? {
		switch (i, self) {
		case (.start(_), _), (.end(_), _):
			return controllers
		case (.interior(_, let h), .index(let w)) where w.get() == h:
			return controllers
		case (.interior(_, let h), .concat(let l, _, _, _, let r, _))
		     where self.contains(h):
		     	return l.extents(enclosing: i, in: controllers) ??
		     	       r.extents(enclosing: i, in: controllers)
		case (.interior(_, _), .extent(let ctlr, let content)):
			return content.extents(enclosing: i,
			    in: controllers + [ctlr])
		default:
			return nil
		}
	}
	func extentsOpening(at i: Rope.Index,
	                    in controllers: [Rope.ExtentController] = [])
	    -> [Rope.ExtentController]? {
		switch (self, i) {
		case (_, .end(_)):
			return []
		case (_, .start(_)):
			return []
		case (.extent(let ctlr, let content), _):
			return content.extentsOpening(at: i,
			                              in: controllers + [ctlr])
		case (.index(let w), .interior(_, let h)) where w.get() == h:
			return controllers
		case (.concat(let l, let midx, _, let hids, let r, _),
		      .interior(_, let h)):
		        guard hids.contains(h.id) else {
				return nil
			}
			if let c = l.extentsOpening(at: i, in: controllers) {
				return c
			}
			/* If there are characters left of `r`, or any
			 * extents open left of `r`, then the controllers
			 * we have seen on our way down, `controllers`, do
			 * not open at `i`. Rather, they open at an index
			 * on the left.  So leave them out of the list.
			 */
			guard 0 == midx && l.hids.extentCount == 0 else {
				return r.extentsOpening(at: i)
			}
			return r.extentsOpening(at: i, in: controllers)
		default:
			return nil
		}
	}
	func extentsClosing(at i: Rope.Index,
	                    in controllers: [Rope.ExtentController] = [])
	    -> [Rope.ExtentController]? {
		switch (self, i) {
		case (_, .end(_)):
			return []
		case (_, .start(_)):
			return []
		case (.extent(let ctlr, let content), _):
			return content.extentsClosing(at: i,
			                              in: controllers + [ctlr])
		case (.index(let w), .interior(_, let h)) where w.get() == h:
			return controllers
		case (.concat(let l, let midx, _, let hids, let r, let w),
		      .interior(_, let h)):
		        guard hids.contains(h.id) else {
				return nil
			}
			if let c = r.extentsClosing(at: i, in: controllers) {
				return c
			}
			/* If there are characters right of `l`, or any
			 * extents open right of `l`, then the controllers
			 * we have seen on our way down, `controllers`, do
			 * not close at `i`. Rather, they close at an index
			 * on the right.  So leave them out of the list.
			 */
			guard midx == w.utf16Offset &&
			      r.hids.extentCount == 0 else {
				return l.extentsClosing(at: i)
			}
			return l.extentsClosing(at: i, in: controllers)
		default:
			return nil
		}
	}
	func element(at i: Offset) -> Element {
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
	func appending(_ rope: Self) -> Self {
		switch (self, rope) {
		case (.index(let w), _) where w.get() == nil:
			return rope
		case (_, .index(let w)) where w.get() == nil:
			return self
		case (.empty, _):
			return rope
		case (_, .empty):
			return self
		case (.concat(let l, _, _, _,
		              .concat(let r, _, _, _,
			              .index(let w), _), _), _) where
		    w.get() == nil:
			return l.appending(r.appending(rope))
		case (.concat(let l, _, _, _, .index(let w), _), _) where
		    w.get() == nil:
			return l.appending(rope)
		case (.concat(let l, _, _, _,
		              .leaf(let pat, let p), _),
		      .leaf(let qat, let q)) where pat ~ qat:
			return .nodes(l, .leaf(pat, p + q))
		case (.leaf(let pat, let p), .leaf(let qat, let q)) where
		    pat ~ qat:
			return .leaf(pat, p + q)
		default:
			return .nodes(self, rope)
		}
	}
	var balanced: Bool {
		return halfPerimeter >= fibonacci(index: depth + 2)
	}
	// Return this Node with all of the expired indices removed. 
	func cleaned() -> Self? {
		switch self {
		case .empty, .cursor(_, _), .leaf(_, _):
			return self
		case .extent(let ctlr, let n):
			let nn = n.cleaned()
			return .extent(ctlr, nn ?? .empty)
		case .index(let w) where w.get() == nil:
			return nil
		case .index(_):
			return self
		case .concat(let l, _, _, _, let r, _):
			guard let nl = l.cleaned() else {
				return r.cleaned()
			}
			guard let nr = r.cleaned() else {
				return nl
			}
			return .nodes(nl, nr)
		}
	}
	// Return a copy of this Rope with its balance restored.
	func rebalanced() -> Self {
		switch self {
		case .empty, .cursor(_, _), .index(_), .leaf(_, _):
			return self
		case .extent(let ctlr, let rope):
			return .extent(ctlr, rope.rebalanced())
		default:
			break
		}
		var slot: [Self?] = []
		let totlen = halfPerimeter
		for fn in Fibonacci(from: 2) {
			if fn > totlen {
				break
			}
			slot.append(nil)
		}
		for node in leaves {
			var tree: Self = node
			var n: Int = slot.count
			for (i, fip3) in Fibonacci(from: 3).enumerated() {
				if let left = slot[slot.count - i - 1] {
					tree = .nodes(left, tree)
					slot[slot.count - i - 1] = nil
				}
				if fip3 >= tree.halfPerimeter {
					n = i
					break
				}
			}
			slot[slot.count - n - 1] = tree
		}
		return slot.reduce(.empty,
		    { (accum: Self, opt: Self?) -> Self in
			switch (accum, opt) {
			case (_, nil):
				return accum
			case (_, let next?):
				return .nodes(accum, next)
			}
		})
	}
	/* Return the subrope that starts at `from`, the UTF16 offset
	 * from the beginning of `self`, with `rightSibling` appended.
	 *
	 * In the subrope, *exclude* all indices, cursors, and other nodes
	 * that do not affect the UTF16 offset and that are adjacent to
	 * `from`, if `tightly` is true.  Otherwise, *include* those nodes.
	 *
	 * `depth` is just an (unused) diagnostic variable that increases
	 * at every level of subrope(from:rightSibling:depth:) recursion.
	 */
	func subrope(from: Offset, tightly: Bool = false,
	    rightSibling: Self = .empty, depth: Int = 0) -> Self {
		assert(0 <= from)
		switch self {
		case .empty, .cursor(_, _), .index(_):
			assert(0 == from)
			return tightly ? rightSibling
			               : self.appending(rightSibling)
		case .extent(let ctlr, let rope):
			let subextent = ctlr.subrope(of: rope, from: from,
			    tightly: tightly, depth: depth + 1)
			return subextent.appending(rightSibling)
		case .concat(let l, let idx, _, _, let r, _):
			if idx < from || tightly && idx == from {
				return r.subrope(
					from: max(0, from - idx),
					tightly: tightly,
					rightSibling: rightSibling,
					depth: depth + 1)
			}
			return l.subrope(from: from, tightly: tightly,
			    rightSibling: r.appending(rightSibling),
			    depth: depth + 1)
		case let .leaf(attrs, s):
			let i = String.Index(utf16Offset: from.utf16Offset,
			    in: s)
			if i == s.utf16.endIndex {
				return rightSibling
			}
			let subleaf: Self =
			    .leaf(attrs, Content(s.suffix(from: i)))
			return subleaf.appending(rightSibling)
		}
	}
	/* Append to `leftSibling` and return the subrope that ends at `to`,
	 * the UTF16 offset from the beginning of `self`.
	 *
	 * In the subrope, *exclude* all indices, cursors, and other nodes
	 * adjacent to `to` that do not increase the UTF16 offset, if `tightly`
	 * is true.  Otherwise, *include* those nodes.
	 *
	 * `depth` is just an (unused) diagnostic variable that increases
	 * at every level of subrope(from:rightSibling:depth:) recursion.
	 */
	func subrope(leftSibling: Self = .empty, to: Offset,
	    tightly: Bool = false, depth: Int = 0) -> Self {
		let endIndex = self.endIndex
		assert(to <= endIndex)
		switch self {
		case .empty, .cursor(_, _), .index(_):
			return tightly ? leftSibling
			               : leftSibling.appending(self)
		case .extent(let ctlr, let rope):
			let subextent = ctlr.subrope(of: rope, to: to,
			    tightly: tightly, depth: depth + 1)
			return leftSibling.appending(subextent)
		case .concat(let l, let idx, _, _, let r, _):
			if to < idx || tightly && to == idx {
				return l.subrope(leftSibling: leftSibling,
				    to: to, tightly: tightly, depth: depth + 1)
			}
			return r.subrope(
				leftSibling: leftSibling.appending(l),
				to: min(endIndex - idx, to - idx),
				tightly: tightly, depth: depth + 1)
		case let .leaf(attrs, s):
			let i = String.Index(utf16Offset: to.utf16Offset, in: s)
			if i == s.utf16.startIndex {
				return leftSibling
			}
			return leftSibling.appending(
			    .leaf(attrs, Self.Content(s.prefix(upTo: i))))
		}
	}
	func subrope(from: Offset, to: Offset) -> Self {
		return subrope(to: to).subrope(from: from)
	}
	func subrope(after boundary: Rope.Index, rightSibling: Self = .empty,
	    depth: Int = 0) -> Self? {
		switch (self, boundary) {
		case (_, .end(_)):
			return .empty
		case (_, .start(_)):
			return self.appending(rightSibling)
		case (.extent(let ctlr, let content), _):
			guard let subextent = ctlr.subrope(of: content,
			    after: boundary, depth: depth) else {
				return rightSibling.subrope(after: boundary,
				    depth: depth)
			}
			return subextent.appending(rightSibling)
		case (.index(let w), .interior(_, let h)) where w.get() == h:
			return rightSibling
		case (.concat(let l, _, _, _, let r, _), .interior(_, let h))
		    where self.contains(h):
			guard let match = l.subrope(after: boundary,
			    rightSibling: r.appending(rightSibling),
			    depth: depth + 1) else {
				return r.subrope(after: boundary,
				    rightSibling: rightSibling,
				    depth: depth + 1)
			}
			return match
		case (.cursor(_, _), _), (.empty, _), (.index(_), _),
		     (.leaf(_, _), _) where rightSibling == .empty:
			return nil
		default:
			return rightSibling.subrope(after: boundary, depth:
			    depth)
		}
	}
	func subrope(leftSibling: Self = .empty, to: Rope.Index,
	    depth: Int = 0) -> Self? {
		/*
		Swift.print("enter \(" " * depth)\(#function) " +
		            "leftSibling \(leftSibling) self \(self) " +
			    "to \(to)", terminator: ": ")
		*/
		switch (self, to) {
		case (_, .start(_)):
			return .empty
		case (_, .end(_)):
			return leftSibling.appending(self)
		case (.extent(let ctlr, let content), _):
			guard let subextent = ctlr.subrope(of: content, to: to,
			    depth: depth + 1) else {
				return leftSibling.subrope(to: to,
				                           depth: depth + 1)
			}
			return leftSibling.appending(subextent)
		case (.index(let w), .interior(_, let h)) where w.get() == h:
			return leftSibling
		case (.concat(let l, _, _, _, let r, _),
		      .interior(_, let h)) where self.contains(h):
			guard let match = r.subrope(
			    leftSibling: leftSibling.appending(l),
			    to: to, depth: depth + 1) else {
				return l.subrope(leftSibling: leftSibling,
				    to: to, depth: depth + 1)
			}
			return match
		case (.cursor(_, _), _), (.empty, _), (.index(_), _),
		     (.leaf(_, _), _) where leftSibling == .empty:
			return nil
		default:
			return leftSibling.subrope(to: to, depth: depth + 1)
		}
	}
	/*
	func deleting(from start: Offset, to end: Offset) -> Self {
		return subrope(from: 0, to: start).appending(
		    subrope(from: end, to: endIndex))
	}
	*/
	func compactMap(_ filter: (Self) -> Self?) -> Self? {
		switch self {
		case .extent(let ctlr, let content):
			let filtered = content.compactMap(filter) ?? .empty
			return Self(controller: ctlr, node: filtered)
		case .concat(let l, _, _, _, let r, _):
			switch (l.compactMap(filter), r.compactMap(filter)) {
			case (nil, nil):
				return nil
			case (let node?, nil), (nil, let node?):
				return node
			case (let newl?, let newr?):
				return .nodes(newl, newr)
			}
		case let node:
			return filter(node)
		}
	}
	func subrope(after l: Rope.Index, to r: Rope.Index, depth: Int = 0)
	    -> Self? {
		guard let suffix = subrope(after: l, depth: depth) else {
			return nil
		}
		return suffix.subrope(to: r, depth: depth)
	}
	subscript(range: Range<Rope.Index>) -> Content {
		return subrope(after: range.lowerBound,
		               to: range.upperBound)?.content ?? Content.empty
	}
	/* XXX this will split extents!  Needs to find affected extents,
	 * split, perform replacement/deletion on each affected extent.
	 */
	func replacing(_ range: Range<Rope.Index>, with c: Content) -> Self? {
		guard let l = subrope(to: range.lowerBound) else {
			return nil
		}
		guard let r = subrope(after: range.upperBound) else {
			return nil
		}
		return l.appending(Self(content: c)).appending(r)
	}
	func replacing(_ range: Range<Offset>, with c: Content) -> Self {
		let l = subrope(to: range.lowerBound)
		let r = subrope(from: range.upperBound)
		return l.appending(Self(content: c)).appending(r)
	}
	func inserting(cursor handle: Handle, attributes: Attributes,
	    at i: Offset) -> Self {
		let cursor: Self = .cursor(handle, attributes)
		return subrope(from: 0, to: i).appending(
		    cursor).appending(subrope(from: i, to: endIndex))
	}
	func inserting(content node: Self, at i: Offset) -> Self {
		if case .empty = node {
			return self
		}
		return subrope(from: 0, to: i).appending(
		    node).appending(subrope(from: i, to: endIndex))
	}
}
