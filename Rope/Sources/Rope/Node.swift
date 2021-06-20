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

extension Rope {
	class ReadonlyExtentController : ExtentController {
		override func transformingAttributes(
		    on range: Range<Rope.Index>, in content: Rope.Node,
		    with fn: (Attributes) -> Attributes) throws -> Rope.Node {
			throw Rope.Node.NodeError.readonlyExtent
		}
		override func replacing(_ range: Range<Rope.Index>,
		    in content: Rope.Node,
		    with replacement: Rope.Node.Content) throws -> Rope.Node {
			throw Rope.Node.NodeError.readonlyExtent
		}
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
extension Rope.Node where C.Element : Equatable {
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
				if l.starts(with: r) {
					lresidue =
					    Self(content: l.dropFirst(r.count),
					         attributes: lattr)
					rresidue = nil
					continue
				}
				if r.starts(with: l) {
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

extension Rope.Node : Equatable {
	public static func == (_ l: Self, _ r: Self) -> Bool {
		switch (l, r) {
		case (.cursor(let ll, let la),
		      .cursor(let rl, let ra)):
			return ll == rl && la ~ ra
		case (.index(let lwl), .index(let rwl)):
			return lwl.get() == rwl.get()
		case (.extent(let lc, let ln),
		      .extent(let rc, let rn)):
			return lc == rc && ln == rn
		case (.concat(let ln1, _, _, _, let ln2, _),
		      .concat(let rn1, _, _, _, let rn2, _)):
			return ln1 == rn1 && ln2 == rn2
		case (.leaf(let la, let lc), .leaf(let ra, let rc)):
			return lc == rc && la ~ ra
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
	func inserting(_ j: Label, one step: DirectedStep, after i: Label,
	    sibling: Rope.Node) -> Step {
		let result = inserting(j, one: step, after: i)
		switch (result, step) {
		case (.step(let newl), .rightStep):
			return .step(.nodes(newl, sibling))
		case (.step(let newr), .leftStep):
			return .step(.nodes(sibling, newr))
		case (.stepOut, .rightStep):
			return .step(.nodes(self, .index(label: j), sibling))
		case (.stepOut, .leftStep):
			return .step(.nodes(sibling, .index(label: j),
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
	func inserting(_ j: Label, after step: DirectedStep) -> Step {
		switch (self, step) {
		/* A step over a cursor, index, or empty string is NOT
		 * a full step.
		 */
		case (.cursor(_, _), _), (.empty, _), (.index(_), _):
			return .inchOut
		/* A step into an extent is a full step. */
		case (.extent(let ctlr, let n), .rightStep):
			// *(...) -> (*...)
			return .step(.extent(under: ctlr, .index(label: j),
			                            n))
		case (.extent(let ctlr, let n), .leftStep):
			// (...)* -> (...*)
			return .step(.extent(under: ctlr, n,
			                            .index(label: j)))
		case (.leaf(let attrs, let content), .rightStep):
			switch content.firstAndRest {
			case (_, let rest)? where rest.isEmpty:
				return .stepOut
			case (let first, let rest)?:
				return .step(.nodes(.leaf(attrs, first),
				                    .index(label: j),
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
				                    .index(label: j),
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
				return .step(.nodes(l, .index(label: j), r))
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
				return .step(.nodes(l, .index(label: j), r))
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
	func inserting(index h: Label, abutting side: Side,
	    of offset: Offset) -> Rope.Node {
		switch self {
		case .cursor(_, _), .index(_), .empty:
			assert(offset == 0)
			switch side {
			case .left:
				return Self.index(label: h).appending(self)
			case .right:
				return self.appending(.index(label: h))
			}
		case .leaf(let attrs, let content):
			let idx = C.Index(unitOffset: offset.unitOffset,
			    in: content)
			let l = content.prefix(upTo: idx)
			let r = content.suffix(from: idx)
			return Rope.Node(content: C.init(l), attributes: attrs)
			    .appending(.index(label: h))
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
	func inserting(index h: Label, at place: Offset) -> Rope.Node {
		switch self {
		case .cursor(_, _), .index(_), .empty:
			assert(place == 0)
			return self.appending(.index(label: h))
		case .leaf(let attrs, let content):
			let idx = C.Index(unitOffset: place.unitOffset,
			    in: content)
			let l = content.prefix(upTo: idx)
			let r = content.suffix(from: idx)
			return Rope.Node(content: C.init(l), attributes: attrs)
			    .appending(.index(label: h))
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
	func inserting(_ j: Label, one step: DirectedStep, after i: Label)
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
				    n, .index(label: j)))
			case (.stepOut, .leftStep):
				return .step(.extent(under: ctlr,
				    .index(label: j), n))
			case (.step(let newn), _):
				return .step(.extent(ctlr, newn))
			case (.absent, _):
				return .absent
			}
		case (.concat(.index(let w), _, _, _, let r, _), .rightStep)
		    where w.get() == i:
			switch r.inserting(j, after: .rightStep) {
			case .step(let newr):
				return .step(.nodes(.index(label: i), newr))
			case let result:
				return result
			}
		case (.concat(let l, _, _, _, .index(let w), _), .leftStep)
		    where w.get() == i:
			switch l.inserting(j, after: .leftStep) {
			case .step(let newl):
				return .step(.nodes(newl, .index(label: i)))
			case let result:
				return result
			}
		case (.concat(let l, _, _, _, let r, _), _):
			let id = i.id
			switch (l.labels.contains(id), r.labels.contains(id),
				step) {
			case (false, false, _):
				return .absent
			case (true, true, _):
				assert(l.labels.contains(id) !=
				       r.labels.contains(id))
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
	func element(at i: Label, sibling r: Rope.Node) -> ElementResult {
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
	func element(at i: Label) -> ElementResult {
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
			switch (l.labels.contains(id), r.labels.contains(id)) {
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
	enum NodeError : Error {
	case unexpectedExtent
	case expectedExtent
	case readonlyExtent
	case invalidExtentContent
	case indexNotFound
	case extentNotFound
	}
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
	/*
	func transforming(range: Range<Rope.Index>, with fn: (Self) -> Self)
	    -> Self? {
		guard let l = subrope(upTo: range.lowerBound) else {
			return nil
		}
		guard let m = subrope(after: range.lowerBound,
		                      upTo: range.upperBound) else {
			return nil
		}
		guard let r = subrope(after: range.upperBound) else {
			return nil
		}
		return l.appending(fn(m)).appending(r)
	}
	*/
	/* A naive version of `transformingAttributes(on:with:)` splits
	 * extents.  This version finds affected extents, splits before
	 * and after each extent, and performs `fn` on each affected extent.
	 */
	func transformingAttributes(on range: Range<Rope.Index>,
	    with fn: (Attributes) -> Attributes) throws -> Self {
		let owner = range.lowerBound.owner
		switch (try extentsEnclosing(range.lowerBound).first,
			try extentsEnclosing(range.upperBound).first) {
		case (nil, nil):
			/* Deal with an empty `range` where the lowerBound
			 * is right of the upperBound---it can happen---but
			 * nevertheless the bounds are equal.
			 */
			if range.isEmpty {
				// TBD perform an alternate, "inserting"
				// transformation
				return self
			}
			/* Important: don't discard any embedded indices at
			 * `range` boundaries!  Instead, use
			 * splitting(after:) and splitting(before:) to
			 * preserve embedded indices for reuse.
			 */
			let (head, rest) =
			    try splitting(after: range.lowerBound)
			let (middle, tail) =
			    try rest.splitting(before: range.upperBound)
			/* Important: perform .transformingAttributes() on
			 * extents in the range so that they get an
			 * opportunity to cancel if they are read-only.
			 */
			switch middle.segmentingAtAnyExtent() {
			case (_, nil, _):
				return head.appending(
				    try middle.transformingAttributes(with: fn)).appending(tail)
			case (let l, .extent(let ctlr, let m), let r):
				Swift.print("\(#function): segmented at extent")
				let entirety = owner.startIndex..<owner.endIndex
				let newl = try l.transformingAttributes(
				    on: entirety, with: fn)
				let newm = try ctlr.transformingAttributes(
				    on: entirety, in: m, with: fn)
				let newr = try r.transformingAttributes(
				    on: entirety, with: fn)
				return head.appending(newl).appending(
				    newm).appending(newr).appending(tail)
			case (_, _?, _):
				// m wasn't an extent for some reason
				// TBD change return type of
				// segmentingAtAnyExtent to avoid this
				// impossible error case
				throw NodeError.expectedExtent
			}
		case (let loExt?, let hiExt?) where loExt == hiExt:
			/* Deal with an empty `range` where the lowerBound
			 * is right of the upperBound---it can happen---but
			 * nevertheless the bounds are equal.
			 */
			if range.isEmpty {
				// TBD perform an alternate, "inserting"
				// transformation
				return self
			}
			guard case (let l, .extent(let ctlr, let m), let r) =
			    try segmenting(atExtent: loExt) else {
				throw NodeError.expectedExtent
			}
			let newm = try ctlr.transformingAttributes(
			    on: range, in: m, with: fn)
			return l.appending(newm).appending(r)
		case (let loExt?, _):
			guard case (let l, .extent(let ctlr, let m), let r) =
			    try segmenting(atExtent: loExt) else {
				throw NodeError.expectedExtent
			}
			let newm = try ctlr.transformingAttributes(
			    on: range.lowerBound..<owner.endIndex, in: m,
			    with: fn)
			let newr = try r.transformingAttributes(
			    on: owner.startIndex..<range.upperBound,
			    with: fn)
			return l.appending(newm).appending(newr)
		case (nil, let hiExt?):
			guard case (let l, .extent(let ctlr, let m), let r) =
			    try segmenting(atExtent: hiExt) else {
				throw NodeError.expectedExtent
			}
			let newl = try l.transformingAttributes(
			    on: range.lowerBound..<owner.endIndex,
			    with: fn)
			let newm = try ctlr.transformingAttributes(
			    on: owner.startIndex..<range.upperBound, in: m,
			    with: fn)
			return newl.appending(newm).appending(r)
		}
	}
	func transformingAttributes(with fn: (Attributes) -> Attributes)
	    throws -> Self {
		switch self {
		case .cursor(let h, let attrs):
			return .cursor(h, fn(attrs))
		case .extent(_, _):
			throw NodeError.unexpectedExtent
		case .concat(let l, _, _, _, let r, _):
			return .nodes(try l.transformingAttributes(with: fn),
			              try r.transformingAttributes(with: fn))
		case .leaf(let attrs, let content):
			return .leaf(fn(attrs), content)
		case .empty, .index(_):
			return self
		}
	}
	/*
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
	func addingAttributes(_ attrs: Attributes, range: Range<Rope.Index>)
	    -> Self? {
		return transforming(range) { node in
			node.addingAttributes(attrs)
		}
	}
	*/
	func settingAttributes(_ attrs: Attributes, range: Range<Rope.Index>)
	    throws -> Self {
		return try transformingAttributes(on: range) { _ in attrs }
	}
	func clearingAttributes(on r: Range<Rope.Index>) throws -> Self {
		return try transformingAttributes(on: r) { _ in [:] }
	}
}

public extension Rope.Node {
	func inserting(_ elt: Self, on side: Side, of target: Rope.Index)
	    throws -> Self {
		switch target {
		case .start(_):
			return .nodes(elt, self)
		case .end(_):
			return .nodes(self, elt)
		case .interior(_, let l):
			return try inserting(elt, on: side, of: l)
		}
	}
	func inserting(_ elt: Self, on side: Side, of target: Label)
	    throws -> Self {
		switch self {
		case .index(let w):
			guard let label = w.get(), label == target else {
				throw NodeError.indexNotFound
			}
			switch side {
			case .left:
				return .nodes(elt, self)
			case .right:
				return .nodes(self, elt)
			}
		case .cursor(_, _):
			throw NodeError.indexNotFound
		case .extent(let ctlr, let r):
			return try ctlr.node(r, inserting: elt, on: side,
			    of: target)
		case .concat(let l, _, _, _, let r, _):
			if l.contains(target) {
				let newl =
				    try l.inserting(elt, on: side, of: target)
				return Self(left: newl, right: r)
			} else if r.contains(target) {
				let newr =
				    try r.inserting(elt, on: side, of: target)
				return Self(left: l, right: newr)
			} else {
				throw NodeError.indexNotFound
			}
		case .leaf(_, _), .empty:
			throw NodeError.indexNotFound
		}
	}
}

public extension Rope.Node {
	// TBD introduce a property for all Labels but the
	// index Labels?
	var labels: LabelSet {
		switch self {
		case .index(let w):
			guard let label = w.get() else {
				return []
			}
			return [label.id]
		case .cursor(let label, _):
			return [label.id]
		case .extent(let ctlr, let rope):
			return rope.labels.union([ctlr.id])
		case .concat(_, _, _, let labels, _, _):
			return labels
		case .leaf(_, _), .empty:
			return []
		}
	}
	func contains(_ target: Label) -> Bool {
		switch self {
		case .index(let w):
			guard let label = w.get() else {
				return false
			}
			return label == target
		case .cursor(target, _):
			return true
		case .extent(_, let rope):
			return rope.contains(target)
		case .concat(_, _, _, let labels, _, _):
			return labels.contains(target.id)
		case .leaf(_, _), .empty:
			return false
		case .cursor(_, _):
			return false
		}
	}
	func steps(follow target: Label) -> Bool? {
		switch self {
		case .index(let w):
			if w.get() != target {
				return nil
			}
			return false
		case .extent(_, let rope):
			return rope.contains(target) ? true : nil
		case .concat(let l, let midx, _, _, let r, let w):
			switch l.steps(follow: target) {
			case nil:
				return r.steps(follow: target)
			case true?:
				return true
			case false?:
				return r.labels.extentCount > 0 ||
				       midx != w.unitOffset
			}
		case .cursor(_, _), .leaf(_, _), .empty:
			return nil
		}
	}
	func steps(precede target: Label) -> Bool? {
		switch self {
		case .index(let w):
			if w.get() != target {
				return nil
			}
			return false
		case .extent(_, let rope):
			return rope.contains(target) ? true : nil
		case .concat(let l, let midx, _, _, let r, _):
			switch r.steps(precede: target) {
			case nil:
				return l.steps(precede: target)
			case true?:
				return true
			case false?:
				return l.labels.extentCount > 0 || 0 != midx
			}
		case .cursor(_, _), .leaf(_, _), .empty:
			return nil
		}
	}
	func step(_ h1: Label, precedes h2: Label) -> Bool? {
		switch self {
		case .index(_):
			return nil
		case .cursor(_, _):
			return nil
		case .extent(_, let rope):
			return rope.step(h1, precedes: h2)
		case .concat(let l, _, _, let labels, let r, _):
			/* I'm not sure if short-circuiting here actually
			 * saves us much work.  Benchmark and see?
			 */
			guard labels.contains(h1.id) && labels.contains(h2.id)
			    else {
				return nil
			}
			if let ordered = l.step(h1, precedes: h2) {
				return ordered
			}
			if let ordered = r.step(h1, precedes: h2) {
				return ordered
			}
			guard let follow = l.steps(follow: h1),
			      let precede = r.steps(precede: h2) else {
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
	init(label: Label) {
		self = .index(Weak(label))
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
				       left.labels.union(right.labels), right,
				       left.dimensions + right.dimensions)
		}
	}
	init<S>(content s: S, attributes attrs: Attributes = [:]) where S : Sequence, C.Element == S.Element {
		let c = C(s)
		if c.isEmpty {
			self = Self.empty
		} else {
			self = Self.leaf(attrs, c)
		}
	}
	init(content c: C, attributes attrs: Attributes = [:]) {
		if c.isEmpty {
			self = Self.empty
		} else {
			self = Self.leaf(attrs, c)
		}
	}
	init<I>(content i: I) where C.SubSequence == I {
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
	static func index(label l: Label) -> Self {
		return Self(label: l)
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
			let endOffset = s.endIndex.unitOffset(in: s)
			let startOffset = s.startIndex.unitOffset(in: s)
			return Dimensions(
			    unitOffset: Offset(of: endOffset - startOffset))
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
			return w.unitOffset
		case .extent(_, let rope):
			return rope.endIndex
		case .leaf(_, let s):
			let endOffset = s.endIndex.unitOffset(in: s)
			let startOffset = s.startIndex.unitOffset(in: s)
			return Offset(of: endOffset - startOffset)
		case .empty, .index(_), .cursor(_, _):
			return 0
		}
	}
	var length: Int {
		return endIndex.unitOffset - startIndex.unitOffset
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
	func unit(at i: Offset) -> C.UnitView.Element {
		func unit(_ node: Self, at i: Offset, base: Offset)
		    -> C.UnitView.Element {
			guard case .leaf(_, let s) = node else {
				fatalError("In \(#function), no unit \(i)")
			}
			let sidx = C.Index(unitOffset: i.unitOffset, in: s)
			return s.units[sidx]
		}
		return transforming(at: i, with: unit)
	}
	func extentsEnclosing(_ i0: Offset) -> [Rope.ExtentController] {
		var path: [Rope.ExtentController] = []
		var i = i0
		var next = self
		while true {
			switch next {
			case .leaf(_, _), .cursor(_, _), .empty, .index(_):
				return path
			case .concat(let l, let idx, _, _, let r, _):
				if i < idx {
					next = l
				} else {
					i = i - idx
					next = r
				}
			case .extent(let ctlr, let content):
				path.append(ctlr)
				next = content
			}
		}
	}
	func extentsEnclosing(_ i: Rope.Index,
	                      under controllers: [Rope.ExtentController] = [])
	    throws -> [Rope.ExtentController] {
		switch (i, self) {
		case (.start(_), _), (.end(_), _):
			return controllers
		case (.interior(_, let h), .index(let w)) where w.get() == h:
			return controllers
		case (.interior(_, let h), .concat(let l, _, _, _, let r, _))
		    where self.contains(h):
		    	do {
				return try l.extentsEnclosing(i,
				    under: controllers)
			} catch NodeError.indexNotFound {
			       return try r.extentsEnclosing(i,
			           under: controllers)
			}
		case (.interior(_, _), .extent(let ctlr, let content)):
			return try content.extentsEnclosing(i,
			    under: controllers + [ctlr])
		default:
			throw NodeError.indexNotFound
		}
	}
	func extentsOpening(at i: Rope.Index,
	                    in controllers: [Rope.ExtentController] = [])
	    throws -> [Rope.ExtentController] {
		switch (self, i) {
		case (_, .end(_)):
			return []
		case (_, .start(_)):
			return []
		case (.extent(let ctlr, let content), _):
			return try content.extentsOpening(at: i,
			                              in: controllers + [ctlr])
		case (.index(let w), .interior(_, let h)) where w.get() == h:
			return controllers
		case (.concat(let l, let midx, _, let labels, let r, _),
		      .interior(_, let h)) where labels.contains(h.id):
			do {
				return try l.extentsOpening(at: i,
				    in: controllers)
			} catch NodeError.indexNotFound {
				/* If there are characters left of `r`, or any
				 * extents open left of `r`, then the
				 * controllers we have seen on our way down,
				 * `controllers`, do not open at `i`. Rather,
				 * they open at an index on the left.  So
				 * leave them out of the list.
				 */
				guard 0 == midx && l.labels.extentCount == 0
				    else {
					return try r.extentsOpening(at: i)
				}
				return try r.extentsOpening(at: i,
				    in: controllers)
			}
		default:
			throw NodeError.indexNotFound
		}
	}
	func extentsClosing(at i: Rope.Index,
	                    in controllers: [Rope.ExtentController] = [])
	    throws -> [Rope.ExtentController] {
		switch (self, i) {
		case (_, .end(_)):
			return []
		case (_, .start(_)):
			return []
		case (.extent(let ctlr, let content), _):
			return try content.extentsClosing(at: i,
			                              in: controllers + [ctlr])
		case (.index(let w), .interior(_, let h)) where w.get() == h:
			return controllers
		case (.concat(let l, let midx, _, let labels, let r, let w),
		      .interior(_, let h)) where labels.contains(h.id):
			do {
				return try r.extentsClosing(at: i,
				    in: controllers)
			} catch NodeError.indexNotFound {
				/* If there are characters right of `l`, or any
				 * extents open right of `l`, then the
				 * controllers we have seen on our way down,
				 * `controllers`, do not close at `i`. Rather,
				 * they close at an index on the right.  So
				 * leave them out of the list.
				 */
				guard midx == w.unitOffset &&
				      r.labels.extentCount == 0 else {
					return try l.extentsClosing(at: i)
				}
				return try l.extentsClosing(at: i,
				    in: controllers)
			}
		default:
			throw NodeError.indexNotFound
		}
	}
	func element(at i: Offset) -> Element {
		switch self {
		case .leaf(_, let s):
			let idx = C.Index(unitOffset: i.unitOffset, in: s)
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
	/* Return the subrope that starts at `from`, the unit offset
	 * from the beginning of `self`, with `rightSibling` appended.
	 *
	 * In the subrope, *exclude* all indices, cursors, and other nodes
	 * that do not affect the unit offset and that are adjacent to
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
			let i = C.Index(unitOffset: from.unitOffset, in: s)
			if i == s.units.endIndex {
				return rightSibling
			}
			let subleaf: Self =
			    .leaf(attrs, Content(s.suffix(from: i)))
			return subleaf.appending(rightSibling)
		}
	}
	/* Append to `leftSibling` and return the subrope that ends at `to`,
	 * the unit offset from the beginning of `self`.
	 *
	 * In the subrope, *exclude* all indices, cursors, and other nodes
	 * adjacent to `to` that do not increase the unit offset, if `tightly`
	 * is true.  Otherwise, *include* those nodes.
	 *
	 * `depth` is just an (unused) diagnostic variable that increases
	 * at every level of subrope(from:rightSibling:depth:) recursion.
	 */
	func subrope(leftSibling: Self = .empty, upTo boundary: Offset,
	    tightly: Bool = false, depth: Int = 0) -> Self {
		let endIndex = self.endIndex
		assert(boundary <= endIndex)
		switch self {
		case .empty, .cursor(_, _), .index(_):
			return tightly ? leftSibling
			               : leftSibling.appending(self)
		case .extent(let ctlr, let rope):
			let subextent = ctlr.subrope(of: rope, upTo: boundary,
			    tightly: tightly, depth: depth + 1)
			return leftSibling.appending(subextent)
		case .concat(let l, let idx, _, _, let r, _):
			if boundary < idx || tightly && boundary == idx {
				return l.subrope(leftSibling: leftSibling,
				    upTo: boundary, tightly: tightly,
				    depth: depth + 1)
			}
			return r.subrope(
				leftSibling: leftSibling.appending(l),
				upTo: min(endIndex - idx, boundary - idx),
				tightly: tightly, depth: depth + 1)
		case let .leaf(attrs, s):
			let i = C.Index(unitOffset: boundary.unitOffset,
			    in: s)
			if i == s.units.startIndex {
				return leftSibling
			}
			return leftSibling.appending(
			    .leaf(attrs, Self.Content(s.prefix(upTo: i))))
		}
	}
	func subrope(from l: Offset, upTo r: Offset) -> Self {
		return subrope(upTo: r).subrope(from: l)
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
	func splitting(leftSibling: Self = .empty, at boundary: Rope.Index,
	    indexOn side: Side, rightSibling: Self = .empty, depth: Int = 0)
	    throws -> (Self, Self) {
		switch (self, boundary) {
		case (_, .start(_)):
			return (leftSibling, self.appending(rightSibling))
		case (_, .end(_)):
			return (leftSibling.appending(self), rightSibling)
		case (.index(let w), .interior(_, let h)) where w.get() == h:
			if side == .right {
				return (leftSibling,
				        self.appending(rightSibling))
			}
			return (leftSibling.appending(self), rightSibling)
		case (.concat(let l, _, _, _, let r, _),
		      .interior(_, let h)) where self.contains(h):
			do {
				return try r.splitting(
				    leftSibling: leftSibling.appending(l),
				    at: boundary,
				    indexOn: side,
				    rightSibling: rightSibling,
				    depth: depth + 1)
			} catch {
			    return try l.splitting(leftSibling: leftSibling,
			           at: boundary,
				   indexOn: side,
				   rightSibling: r.appending(rightSibling),
				   depth: depth + 1)
			}
		case (.cursor(_, _), _), (.empty, _), (.index(_), _),
		     (.leaf(_, _), _) where rightSibling == .empty:
			throw NodeError.indexNotFound
		default:
			return try rightSibling.splitting(leftSibling:
			    leftSibling.appending(self), at: boundary,
			    indexOn: side, depth: depth + 1)
		}
	}
	func splitting(before boundary: Rope.Index) throws -> (Self, Self) {
		return try splitting(at: boundary, indexOn: .right)
	}
	func splitting(after boundary: Rope.Index) throws -> (Self, Self) {
		return try splitting(at: boundary, indexOn: .left)
	}
	func subrope(leftSibling: Self = .empty, upTo boundary: Rope.Index,
	    depth: Int = 0) -> Self? {
		/*
		Swift.print("enter \(" " * depth)\(#function) " +
		            "leftSibling \(leftSibling) self \(self) " +
			    "upTo \(boundary)", terminator: ": ")
		*/
		switch (self, boundary) {
		case (_, .start(_)):
			return .empty
		case (_, .end(_)):
			return leftSibling.appending(self)
		case (.extent(let ctlr, let content), _):
			guard let subextent = ctlr.subrope(of: content,
			    upTo: boundary, depth: depth + 1) else {
				return leftSibling.subrope(upTo: boundary,
				                           depth: depth + 1)
			}
			return leftSibling.appending(subextent)
		case (.index(let w), .interior(_, let h)) where w.get() == h:
			return leftSibling
		case (.concat(let l, _, _, _, let r, _),
		      .interior(_, let h)) where self.contains(h):
			guard let match = r.subrope(
			    leftSibling: leftSibling.appending(l),
			    upTo: boundary, depth: depth + 1) else {
				return l.subrope(leftSibling: leftSibling,
				    upTo: boundary, depth: depth + 1)
			}
			return match
		case (.cursor(_, _), _), (.empty, _), (.index(_), _),
		     (.leaf(_, _), _) where leftSibling == .empty:
			return nil
		default:
			return leftSibling.subrope(upTo: boundary,
			    depth: depth + 1)
		}
	}
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
	func subrope(after l: Rope.Index, upTo r: Rope.Index, depth: Int = 0)
	    -> Self? {
		guard let suffix = subrope(after: l, depth: depth) else {
			return nil
		}
		return suffix.subrope(upTo: r, depth: depth)
	}
	subscript(range: Range<Rope.Index>) -> Content {
		return subrope(after: range.lowerBound,
		               upTo: range.upperBound)?.content ?? Content.empty
	}
	/* A naive version of `replacing(_:with:)` splits extents.  This
	 * version finds affected extents, splits before and after each
	 * extent, and performs replacement/deletion on each affected extent.
	 */
	func replacing(_ range: Range<Rope.Index>, with replacement: Content)
	    throws -> Self {
		let owner = range.lowerBound.owner
		switch (try extentsEnclosing(range.lowerBound).first,
			try extentsEnclosing(range.upperBound).first) {
		case (nil, nil):
			/* Deal with an empty `range` where the lowerBound
			 * is right of the upperBound in the rope---it can
			 * happen---but nevertheless the bounds alias
			 * because there are no indexable locations
			 * between: no extent boundaries, no non-empty leaves.
			 *
			 * XXX Danger!  This may leave upperBound on the
			 * left side of lowerBound.  In that case, need to
			 * use the range upperBound..<lowerBound for the
			 * undo/redo record.
			 */
			if range.isEmpty {
				/* Undo: to do, insert .text(...) bracketed by 
				 * two new indices because `range` is empty;
				 * to undo, replace between indices
				 * with `Content.empty`.
				 *
				 * Something like `let inserted = try
				 * inserting(..., on: .right, of: range.lowerBound)`
				 */
				return try inserting(.text(replacement),
				    on: .right, of: range.lowerBound)
			}
			/* Important: don't discard any embedded indices at
			 * `range` boundaries!  Instead, use
			 * splitting(after:) and splitting(before:) to
			 * preserve embedded indices for reuse.
			 */
			let (head, rest) =
			    try splitting(after: range.lowerBound)
			let (middle, tail) =
			    try rest.splitting(before: range.upperBound)
			/* Important: perform .replacing() on extents in the
			 * range so that they get an opportunity to cancel if
			 * they are read-only.
			 */
			switch middle.segmentingAtAnyExtent() {
			case (_, nil, _):
				/* Undo: insert .text(...); replace `range`
				 * with `middle` to undo.
				 */
				return head.appending(
				    .text(replacement)).appending(tail)
			/* By our contract with `segmentingAtAnyExtent`, the
			 * `middle` subrope left of the .extent is
			 * .extent-free, so we do not need to test for
			 * a read-only .extent's "veto."  We replace it
			 * entirely by `replacement`, so there is no need to
			 * bind it.  Hence the `_` pattern.
			 */
			case (_, .extent(let ctlr, let m), let r):
				Swift.print("\(#function): segmented at extent")
				/* We have to try to replace using the
				 * controller so that a read-only extent can
				 * "veto" the replacement with .empty by
				 * throwing.
				 */
				let _ = try ctlr.replacing(
				    owner.startIndex..<owner.endIndex,
				    in: m, with: Content.empty)
				let rReplaced = try r.replacing(
				    owner.startIndex..<owner.endIndex,
				    with: Content.empty)
				/* Undo: replace `range` with `middle`.
				 * XXX Interiorize `range`.
				 */
				return head.appending(
				    .text(replacement)).appending(
				    rReplaced).appending(tail)
			case (_, _?, _):
				throw NodeError.expectedExtent
			}
		case (let loExt?, let hiExt?) where loExt == hiExt:
			/* Deal with an empty `range` where the lowerBound
			 * is right of the upperBound---it can happen---but
			 * nevertheless the bounds are equal.
			 */
			if range.isEmpty {
				/* Undo: insert .text(...) bracketed by 
				 * two new indices because `range` is empty;
				 * replace between indices
				 * with `Content.empty` to undo.
				 */
				return try inserting(.text(replacement),
				    on: .right, of: range.lowerBound)
			}
			guard case (let l, .extent(let ctlr, let m), let r) =
			    try segmenting(atExtent: loExt) else {
				throw NodeError.expectedExtent
			}
			/* Undo: .replacing(...) adds an undo record. */
			let mReplaced = try ctlr.replacing(range, in: m,
			    with: replacement)
			return l.appending(mReplaced).appending(r)
		case (let loExt?, _):
			guard case (let l, .extent(let ctlr, let m), let r) =
			    try segmenting(atExtent: loExt) else {
				throw NodeError.expectedExtent
			}
			/* Undo: .replacing(...) adds an undo record. */
			let mReplaced = try ctlr.replacing(
			    range.lowerBound..<owner.endIndex, in: m,
			    with: replacement)
			/* Undo: .replacing(...) adds an undo record. */
			let rTrimmed = try r.replacing(
			    owner.startIndex..<range.upperBound,
			    with: Content.empty)
			return l.appending(mReplaced).appending(rTrimmed)
		case (nil, let hiExt?):
			guard case (let l, .extent(let ctlr, let m), let r) =
			    try segmenting(atExtent: hiExt) else {
				throw NodeError.expectedExtent
			}
			/* Undo: .replacing(...) adds an undo record. */
			let lReplaced = try l.replacing(
			    range.lowerBound..<owner.endIndex,
			    with: replacement)
			/* Undo: .replacing(...) adds an undo record. */
			let mTrimmed = try ctlr.replacing(
			    owner.startIndex..<range.upperBound, in: m,
			    with: Content.empty)
			return lReplaced.appending(mTrimmed).appending(r)
		}
	}
	func segmenting(atExtent target: Rope.ExtentController,
	    leftSibling: Self = .empty) throws -> (Self, Self, Self) {
		switch self {
		case .extent(target, _):
			return (leftSibling, self, .empty)
		case .concat(let l, _, _, _, let r, _):
			if case let (head, extent, tail)? =
			    try? l.segmenting(atExtent: target) {
				return (leftSibling.appending(head),
					extent, tail.appending(r))
			} else {
				return try r.segmenting(atExtent: target,
				    leftSibling: leftSibling.appending(l))
			}
		case .cursor(_, _), .empty, .extent(_, _), .index(_),
		     .leaf(_, _):
			throw NodeError.extentNotFound
		}
	}
	func segmentingAtAnyExtent(leftSibling: Self = .empty)
	    -> (Self, Self?, Self) {
		switch self {
		case .cursor(_, _), .leaf(_, _), .index(_), .empty:
			return (leftSibling.appending(self), nil, .empty)
		case .extent(_, _):
			return (leftSibling, self, .empty)
		case .concat(let l, _, _, _, let r, _):
			if case let (head, extent?, tail) =
			    l.segmentingAtAnyExtent() {
				return (leftSibling.appending(head),
					extent, tail.appending(r))
			} else {
				return r.segmentingAtAnyExtent(
				    leftSibling: leftSibling.appending(l))
			}
		}
	}
	func indexingFirstExtent(label: Label, leftSibling: Self = .empty)
	    -> Self? {
		switch self {
		case .cursor(_, _), .leaf(_, _), .index(_), .empty:
			return nil
		case .extent(_, _):
			return leftSibling.appending(
			    .index(label: label)).appending(self)
		case .concat(let l, _, _, _, let r, _):
			if let inserted = l.indexingFirstExtent(
			    label: label, leftSibling: leftSibling) {
				return inserted.appending(r)
			}
			return r.indexingFirstExtent(label: label,
			    leftSibling: leftSibling.appending(l))
		}
	}
	func indexingFirstExtent(after index: Rope.Index,
	    label: Label) throws -> Self? {
		/* If extents enclose `index`, then we are about to split
		 * an extent.  That mustn't happen.  Instead, the operation
		 * that the caller would like to perform
		 * (delete, insert, replace, ...) should be forwarded to the
		 * first enclosing extent.  Return nil to prevent a split
		 * from occurring.
		 */
		guard try extentsEnclosing(index).isEmpty else {
			return nil
		}
		switch index {
		case .interior(_, let h):
			guard let l = subrope(upTo: index),
			      let r = subrope(after: index) else {
				return nil
			}
			return r.indexingFirstExtent(label: label,
				leftSibling: l.appending(.index(label: h)))
		case .start(_):
			// left of .start(_) is .empty
			return indexingFirstExtent(label: label)
		case .end(_):
			// right of .end(_) is .empty
			return self.appending(.index(label: label))
		}
	}
	/* TBD tighten up cursor placement?  Check if any nodes are
	 * excluded or doubly-included?
	 */
	func inserting(cursor label: Label, attributes: Attributes,
	    at i: Offset) -> Self {
		let cursor: Self = .cursor(label, attributes)
		return subrope(from: 0, upTo: i).appending(
		    cursor).appending(subrope(from: i, upTo: endIndex))
	}
	/* TBD Remove? Risky subrope use... */
	func inserting(content node: Self, at i: Offset) -> Self {
		if case .empty = node {
			return self
		}
		return subrope(from: 0, upTo: i).appending(
		    node).appending(subrope(from: i, upTo: endIndex))
	}
	func transformingExtent(withLabel target: Label,
	    with f: (_: Rope.ExtentController, _: Self) -> Self) -> Self? {
		switch self {
		case .cursor(_, _), .empty, .index(_), .leaf(_, _):
			return nil
		case .extent(let ctlr, let content) where ctlr == target:
			return f(ctlr, content)
		case .extent(let ctlr, let n):
			guard let newn = n.transformingExtent(withLabel: target,
			    with: f) else {
				return nil
			}
			return .extent(under: ctlr, newn)
		case .concat(let l, _, _, let labels, let r, _):
			guard labels.contains(target.id) else {
				return nil
			}
			if let newl = l.transformingExtent(
			    withLabel: target, with: f) {
				return .nodes(newl, r)
			} else if let newr = r.transformingExtent(
			    withLabel: target, with: f) {
				return .nodes(l, newr)
			} else {
				return nil
			}
		}
	}
	func insertingFirstIndex(_ label: Label, inExtent target: Label)
	    -> Self? {
		return transformingExtent(withLabel: target) {
		    (ctlr, content) in
		        .extent(under: ctlr, .index(label: label), content)
		}
	}
	func insertingLastIndex(_ label: Label, inExtent target: Label)
	    -> Self? {
		return transformingExtent(withLabel: target) {
		    (ctlr, content) in
			.extent(under: ctlr, content, .index(label: label))
		}
	}
	func insertingIndex(_ label: Label, afterExtent target: Label)
	    -> Self? {
		return transformingExtent(withLabel: target) {
		    (ctlr, content) in
			.nodes(.extent(under: ctlr, content),
			       .index(label: label))
		}
	}
	func insertingIndex(_ label: Label, beforeExtent target: Label)
	    -> Self? {
		return transformingExtent(withLabel: target) {
		    (ctlr, content) in
			.nodes(.index(label: label),
			       .extent(under: ctlr, content))
		}
	}
}

public extension Rope.Node {
	func extractUnits(from start: Offset, upTo end: Offset,
	    filling buffer: inout UnsafeMutablePointer<C.Unit>) {
		switch self {
		case .concat(let l, let idx, _, _, let r, _):
			if start < idx {
				l.extractUnits(from: start,
				    upTo: min(end, idx),
				    filling: &buffer)
			}
			if idx < end {
				r.extractUnits(from: max(start, idx) - idx,
				    upTo: end - idx,
				    filling: &buffer)
			}
		case .leaf(_, let s):
			guard case true? =
			    (s.units.withContiguousStorageIfAvailable {
				guard let base = $0.baseAddress else {
					return false
				}
				let length = end.unitOffset - start.unitOffset
				buffer.initialize(from: base + start.unitOffset,
				    count: length)
				buffer += length
				return true
			} as Bool?) else {
				let units = s.units
				guard let sidx = units.index(units.startIndex,
				        offsetBy: start.unitOffset,
				        limitedBy: units.endIndex),
				    let eidx = units.index(units.startIndex,
				        offsetBy: end.unitOffset,
					limitedBy: units.endIndex)
				    else {
					fatalError("In \(#function), " +
					    "no units range \(start)..<\(end)")
				}
				for u in units[sidx..<eidx] {
					buffer.initialize(to: u)
					buffer += 1
				}
				return
			}
		case .extent(_, let content):
			return content.extractUnits(from: start, upTo: end,
			    filling: &buffer)
		case .cursor(_, _), .empty, .index(_):
			return
		}
	}
	func extractContent(from start: Offset, upTo end: Offset)
	    -> C.SubSequence {
		switch self {
		case .concat(let l, let idx, _, _, let r, _):
			var c = C.empty
			if start < idx {
				c += l.extractContent(from: start,
				    upTo: min(end, idx))
			}
			if idx < end {
				c += r.extractContent(
				    from: max(start, idx) - idx,
				    upTo: end - idx)
			}
			return c[...]
		case .leaf(_, let s):
			guard let sidx = s.units.index(s.units.startIndex,
			    offsetBy: start.unitOffset,
			    limitedBy: s.units.endIndex),
			    let eidx = s.units.index(s.units.startIndex,
			    offsetBy: end.unitOffset,
			    limitedBy: s.units.endIndex) else {
				fatalError("In \(#function), " +
				    "no units range \(start)..<\(end)")
			}
			return s[sidx..<eidx]
		case .extent(_, let content):
			return content.extractContent(from: start, upTo: end)
		case .cursor(_, _), .empty, .index(_):
			return C.empty[...]
		}
	}
	func extractContent(from start: Offset, upTo end: Offset,
	    filling c: inout C) {
		switch self {
		case .concat(let l, let idx, _, _, let r, _):
			if start < idx {
				l.extractContent(from: start,
				    upTo: min(end, idx), filling: &c)
			}
			if idx < end {
				r.extractContent(
				    from: max(start, idx) - idx,
				    upTo: end - idx, filling: &c)
			}
		case .leaf(_, let s):
			guard let sidx = s.units.index(s.units.startIndex,
			    offsetBy: start.unitOffset,
			    limitedBy: s.units.endIndex),
			    let eidx = s.units.index(s.units.startIndex,
			    offsetBy: end.unitOffset,
			    limitedBy: s.units.endIndex) else {
				fatalError("In \(#function), " +
				    "no units range \(start)..<\(end)")
			}
			c += s[sidx..<eidx]
		case .extent(_, let content):
			content.extractContent(from: start, upTo: end,
			    filling: &c)
		case .cursor(_, _), .empty, .index(_):
			return
		}
	}
}

public extension Rope.Node {
        /* "Reify" an "abstract" range---that is, a range with any bound
	 * on the logical "start" or "end" `Index` of a `Rope`---and construct
	 * a subtree containing the reified indices.
	 *
	 * Rewrite one bound, both or neither bounds of `range`,
         * replacing lower bound `.start(of: rope)` with `.interior(of:
         * rope, label: l)` replacing upper bound `.end(of: rope)`
         * with `.interior(of: rope, label: r)` for new `Label`s `l`
         * and `r`.
	 *
	 * Simultaneously construct a new `Node` subtree by
         * augmenting `self`: prepend `.index(l)` if a `.start` bound
         * is replaced, and append `.index(r)` if a `.end` bound is
         * replaced.  Return the subtree thus constructed.
	 */
	func addingBoundaryLabels(reifying range: Range<Rope<C>.Index>,
	    lower outLower: inout Rope<C>.Index,
	    upper outUpper: inout Rope<C>.Index) -> Self {
		switch (range.lowerBound, range.upperBound) {
		case (.start(let owner), .end(_)):
			let l = Label()
			let r = Label()
			outLower = .interior(of: owner, label: l)
			outUpper = .interior(of: owner, label: r)
			return Rope<C>.Node.index(label: l).appending(
			    self).appending(.index(label: r))
		case (.start(let owner), let upper):
			let l = Label()
			outLower = .interior(of: owner, label: l)
			outUpper = upper
			return Rope<C>.Node.index(label: l).appending(self)
		case (let lower, .end(let owner)):
			let r = Label()
			outLower = lower
			outUpper = .interior(of: owner, label: r)
			return self.appending(.index(label: r))
		case (let lower, let upper):
			outLower = lower
			outUpper = upper
			return self
		}
	}
}
