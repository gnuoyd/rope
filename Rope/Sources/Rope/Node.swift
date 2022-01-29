//
// Copyright (c) 2019, 2020, 2021 David Young.  All rights reserved.
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

extension Rope.Node {
	public typealias Unit = C.Unit
}

extension Rope.Node {
	/*
	 * Result of taking a step in a Node.  A full step moves up or down
	 * the zone hierarchy or across a UTF-16 element.
	 */
	public enum Step {
	case absent		/* The location to step from could not be
				 * found.  TBD: throw an Error, instead?
				 */
	case step(Rope.Node)	/* A full step occurred, resulting in the
				 * associated Node
				 */
	case inchOut		/* A partial step occurred: stepping over a
				 * content-free Node `n` such as .empty or
				 * .index.
				 *
				 * The full step must be completed on the `n`'s
				 * parent.
				 */
	case stepOut		/* A full upward step occurred: stepping over
				 * the boundary of zone `n`, or stepping over
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
			case (.zone((let lctlr, let lprops), let l),
			      .zone((let rctlr, let rprops), let r)):
				if lctlr !== rctlr {
					return false
				}
				if lprops != rprops {
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
		case (.index(let lwl), .index(let rwl)):
			return lwl.get() == rwl.get()
		case (.zone((let lc, let lp), let ln),
		      .zone((let rc, let rp), let rn)):
			return lc == rc && lp == rp && ln == rn
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
		/* A step over an index or empty string is NOT a
		 * full step.
		 */
		case (.empty, _), (.index(_), _):
			return .inchOut
		/* A step into a zone is a full step. */
		case (.zone((let ctlr, let props), let n), .rightStep):
			// *(...) -> (*...)
			return .step(.zone(under: ctlr, properties: props,
			    .index(label: j), n))
		case (.zone((let ctlr, let props), let n), .leftStep):
			// (...)* -> (...*)
			return .step(.zone(under: ctlr, properties: props, n,
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
	func inserting(_ label: Label, abutting side: Side,
	    of offset: Int, on axis: KeyPath<Dimensions, Int>)
	    -> Rope.Node {
		let boundary = Dimensions(boundaries: 1)
		switch self {
		case .index(_), .empty:
			assert(offset == 0)
			switch side {
			case .left:
				return Self.index(label: label).appending(self)
			case .right:
				return self.appending(.index(label: label))
			}
		case .leaf(let attrs, let content):
			let idx = C.Index(unitOffset: offset, in: content)
			let l = content.prefix(upTo: idx)
			let r = content.suffix(from: idx)
			return Rope.Node(content: C.init(l), attributes: attrs)
			    .appending(.index(label: label))
			    .appending(Rope.Node(content: C.init(r),
			        attributes: attrs))
		case .zone(_, _) where offset < boundary[keyPath: axis]:
			return Self.index(label: label).appending(self)
		case .zone((let ctlr, let props), let n)
		    where offset < (boundary + n.dimensions + boundary)[keyPath: axis]:
			assert(offset >= boundary[keyPath: axis])
			return Rope.Node(controller: ctlr, properties: props,
			    node: n.inserting(label, abutting: side,
			        of: offset - boundary[keyPath: axis],
				on: axis))
		case .zone(_, _):
			return self.appending(Self.index(label: label))
		case .concat(let l, let mid, _, _, let r, _):
			if offset < mid[keyPath: axis] {
				return l.inserting(label, abutting: side,
				    of: offset, on: axis).appending(r)
			}
			if mid[keyPath: axis] < offset {
				return l.appending(r.inserting(label,
				    abutting: side,
				    of: offset - mid[keyPath: axis],
				    on: axis))
			}
			switch side {
			case .left:
				return l.inserting(label, abutting: side,
				    of: offset, on: axis).appending(r)
			case .right:
				return l.appending(r.inserting(label,
				    abutting: side,
				    of: offset - mid[keyPath: axis],
				    on: axis))
			}
		}
	}
	func inserting(_ j: Label, one step: DirectedStep, after i: Label)
	    -> Step {
		switch (self, step) {
		case (.index(let w), _) where w.get() == i:
			return .inchOut
		case (.index(_), _), (.leaf(_, _), _), (.empty, _):
			return .absent
		case (.zone((let ctlr, let props), let n), _):
			switch (n.inserting(j, one: step, after: i), step) {
			case (.inchOut, _):
				return .stepOut
			case (.stepOut, .rightStep):
				return .step(.zone(under: ctlr,
				    properties: props, n, .index(label: j)))
			case (.stepOut, .leftStep):
				return .step(.zone(under: ctlr,
				    properties: props, .index(label: j), n))
			case (.step(let newn), _):
				return .step(.zone(under: ctlr,
				    properties: props, newn))
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
			switch (l.labelSet.contains(id),
				r.labelSet.contains(id), step) {
			case (false, false, _):
				return .absent
			case (true, true, _):
				assert(l.labelSet.contains(id) !=
				       r.labelSet.contains(id))
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
		case .index(_), .empty:
			/* No match: the element is not on this span. */
			return .inchOut
		case .leaf(let attrs, let content):
			switch content.firstAndRest {
			case (let head, _)?:
				return .step(.leaf(attrs, head))
			default:
				return .inchOut
			}
		case .zone(_, _):
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
		case .empty, .index(_), .leaf(_, _):
			/* No match: the element is not on this span. */
			return .absent
		case .zone(let z, let n):
			switch n.element(at: i) {
			case .inchOut:
				return .step(.zone(z, .empty))
			case .step(let newn):
				return .step(.zone(z, newn))
			case .absent:
				return .absent
			}
		case .concat(.index(let w), _, _, _, let r, _) where
		    w.get() == i:
			return r.firstElement()
		case .concat(let l, _, _, _, let r, _):
			let id = i.id
			switch (l.labelSet.contains(id),
				r.labelSet.contains(id)) {
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
	case unexpectedZone
	case readonlyZone
	case invalidZoneContent
	case indexNotFound
	case zoneNotFound
	case indicesCrossZones
	case indicesOutOfOrder
	}
	func attributes(at i: Int, on axis: KeyPath<Dimensions, Int>,
	    defaults: Rope.BoundaryAttributes? = nil)
	    -> (Attributes, Range<Int>) {
		let boundary = Dimensions(boundaries: 1)
		let (n, residue, range) = retrieveNode(at: i, on: axis)
		switch n {
		case .zone((_, let props), _)
		    where residue < boundary[keyPath: axis]:
			let zattrs = props.attributes.open
			let openAttrs = zattrs.merging(defaults?.open ?? [:]) {
			    (z, d) in z
			}
			return (openAttrs,
			        range.lowerBound..<(range.lowerBound +
				                    boundary[keyPath: axis]))
		case .zone((_, let props), _):
			let zattrs = props.attributes.close
			let closeAttrs = zattrs.merging(defaults?.close ?? [:]){
			    (z, d) in z
			}
			return (closeAttrs,
			        (range.upperBound -
				 boundary[keyPath: axis])..<range.upperBound)
		case .leaf(let attrs, _) where
		    0 <= residue && residue < n.dimensions[keyPath: axis]:
			return (attrs, range)
		default:
			fatalError("Index out of bounds")
		}
	}
	/* A naive version of `transformingAttributes(after:upTo:with:)` splits
	 * zones.  This version finds affected zones, splits before
	 * and after each zone, and performs `fn` on each affected zone.
	 */
	func transformingAttributes(after lowerBound: Label,
	    upTo upperBound: Label,
	    with fn: (Attributes) -> Attributes) throws -> Self {
		switch (try zonesEnclosing(lowerBound).first,
			try zonesEnclosing(upperBound).first) {
		case (nil, nil):
			/* Deal with an empty range where the lowerBound
			 * is right of the upperBound---it can happen---but
			 * nevertheless the bounds are equal.
			 */
			if try label(lowerBound, aliases: upperBound) {
				// TBD perform an alternate, "inserting"
				// transformation if this routine is ever
				// adapted to perform generic transformations.
				// This is fine, for now.
				return self
			}
			/* Important: don't discard any embedded indices at
			 * boundaries!  Instead, use
			 * splitting(after:) and splitting(before:) to
			 * preserve embedded indices for reuse.
			 */
			let (head, rest) =
			    try splitting(after: lowerBound)
			let (middle, tail) =
			    try rest.splitting(before: upperBound)
			/* Important: perform .transformingAttributes() on
			 * zones in the range so that they get an
			 * opportunity to cancel if they are read-only.
			 */
			switch middle.segmentingAtAnyZone() {
			case (_, nil, _):
				return head.appending(
				    try middle.transformingAttributes(
				        with: fn)).appending(tail)
			case (let l, (let ctlr, let props, let m)?, let r):
				Swift.print("\(#function): segmented at zone")
				let newl = try l.withFreshBoundaries {
				    (left, right, node) in
				        try node.transformingAttributes(
					    after: left, upTo: right, with: fn)
				}
				let newm = try m.withFreshBoundaries {
				    (left, right, node) in
				        try ctlr.transformingAttributes(
					    after: left, upTo: right, in: node,
					    properties: props,
					    andBoundaries: .both, with: fn)
				}
				let newr = try r.withFreshBoundaries {
				    (left, right, node) in
				        try node.transformingAttributes(
					    after: left, upTo: right, with: fn)
				}
				return head.appending(newl).appending(
				    newm).appending(newr).appending(tail)
			}
		case (let loExt?, let hiExt?) where loExt == hiExt:
			let (l, (ctlr, props, m), r) =
			    try segmenting(atZone: loExt)
			let newm = try ctlr.transformingAttributes(
			    after: lowerBound, upTo: upperBound, in: m,
			    properties: props, with: fn)
			return l.appending(newm).appending(r)
		case (let loExt?, _):
			let (l, (ctlr, props, m), r) =
			    try segmenting(atZone: loExt)
			let newm = try m.withFreshRightBoundary {
			    (upper, node) in
				try ctlr.transformingAttributes(
				    after: lowerBound, upTo: upper, in: node,
				    properties: props, andBoundaries: .right,
				    with: fn)
			}
			let newr = try r.withFreshLeftBoundary {
			    (lower, node) in
			        try node.transformingAttributes(after: lower,
				    upTo: upperBound, with: fn)
			}
			return l.appending(newm).appending(newr)
		case (nil, let hiExt?):
			let (l, (ctlr, props, m), r) =
			    try segmenting(atZone: hiExt)
			let newl = try l.withFreshRightBoundary {
			    (upper, node) in
			        try node.transformingAttributes(
				    after: lowerBound, upTo: upper, with: fn)
			}
			let newm = try m.withFreshLeftBoundary {
			    (lower, node) in
			        try ctlr.transformingAttributes(
				    after: lower, upTo: upperBound, in: node,
				    properties: props, andBoundaries: .left,
				    with: fn)
			}
			return newl.appending(newm).appending(r)
		}
	}
	func transformingAttributes(with fn: (Attributes) -> Attributes)
	    throws -> Self {
		switch self {
		case .zone(_, _):
			throw NodeError.unexpectedZone
		case .concat(let l, _, _, _, let r, _):
			return .nodes(try l.transformingAttributes(with: fn),
			              try r.transformingAttributes(with: fn))
		case .leaf(let attrs, let content):
			return .leaf(fn(attrs), content)
		case .empty, .index(_):
			return self
		}
	}
	func settingAttributes(_ attrs: Attributes, range: Range<Rope.Index>)
	    throws -> Self {
		return try transformingAttributes(
		    after: range.lowerBound.label,
		    upTo: range.upperBound.label) { _ in attrs }
	}
	func clearingAttributes(on range: Range<Rope.Index>) throws -> Self {
		return try transformingAttributes(after: range.lowerBound.label,
		    upTo: range.upperBound.label) { _ in [:] }
	}
}

public class ChangeList<Target> {
	public typealias Change =
	    (Target, ChangeList) throws -> Target
	private var items: [Change]
	public required init() {
		items = []
	}
	public func record(_ item: @escaping Change) {
		items.append(item)
	}
	public func append(_ changes: ChangeList) {
		items = items + changes.items
	}
	public func play(withTarget targetIn: Target) throws
	    -> (Target, ChangeList) {
		let undoList = Self()
		var target = targetIn
		while let item = items.popLast() {
			target = try item(target, undoList)
		}
		return (target, undoList)
	}
}

public extension Rope.Node {
	enum Interval {
	case jot
	case step
	}
	// TBD introduce a property for all Labels but the
	// index Labels?
	var labelSet: LabelSet {
		switch self {
		case .index(let w):
			guard let label = w.get() else {
				return []
			}
			return [label.id]
		case .zone((let ctlr, _), let rope):
			return rope.labelSet.union([ctlr.id])
		case .concat(_, _, _, let set, _, _):
			return set
		case .leaf(_, _), .empty:
			return []
		}
	}
	func contains(_ target: Label) -> Bool {
		switch self {
		case .index(let w) where w.get() == target:
			return true
		case .index(_):
			return false
		case .zone(_, let n):
			return n.contains(target)
		case .concat(_, _, _, let set, _, _):
			return set.contains(target.id)
		case .leaf(_, _), .empty:
			return false
		}
	}
	func any(_ ival: Interval, follows target: Label) throws -> Bool {
		switch self {
		case .index(let w) where w.get() == target:
			return false
		case .zone(_, let n) where n.contains(target):
			return true
		case .concat(let l, let mid, _, _, let r, let w):
			do {
				return try l.any(ival, follows: target) ||
				    mid.steps != w.steps ||
				    (ival == .jot &&
				     r.rightmostIndexLabel() != nil)
			} catch {
				return try r.any(ival, follows: target)
			}
		case .empty, .zone(_, _), .index(_),
		     .leaf(_, _):
			throw NodeError.indexNotFound
		}
	}
	func leftmostIndexLabel() -> Label? {
		switch self {
		case .index(let w):
			return w.get()
		case .zone(_, let n):
			return n.leftmostIndexLabel()
		case .concat(let l, _, _, _, let r, _):
			return l.leftmostIndexLabel() ?? r.leftmostIndexLabel()
		case .empty, .leaf(_, _):
			return nil
		}
	}
	func rightmostIndexLabel() -> Label? {
		switch self {
		case .index(let w):
			return w.get()
		case .zone(_, let n):
			return n.rightmostIndexLabel()
		case .concat(let l, _, _, _, let r, _):
			return r.rightmostIndexLabel() ??
			       l.rightmostIndexLabel()
		case .empty, .leaf(_, _):
			return nil
		}
	}
	func any(_ ival: Interval, precedes target: Label) throws -> Bool {
		switch self {
		case .index(let w) where w.get() == target:
			return false
		case .zone(_, let rope) where rope.contains(target):
			return true
		case .concat(let l, let mid, _, _, let r, _):
			do {
				return
				    try r.any(ival, precedes: target) ||
				    0 != mid.steps ||
				    (ival == .jot &&
				     l.leftmostIndexLabel() != nil)
			} catch {
				return try l.any(ival, precedes: target)
			}
		case .empty, .zone(_, _), .index(_),
		     .leaf(_, _):
			throw NodeError.indexNotFound
		}
	}
	func label(_ h1: Label, precedes h2: Label, by ival: Interval)
	    throws -> Bool {
		switch self {
		case .zone(_, let rope):
			return try rope.label(h1, precedes: h2, by: ival)
		case .concat(let l, _, _, _, let r, _):
			if l.contains(h2) && r.contains(h1) {
				return false
			}
			if l.contains(h1) && r.contains(h2) {
				return try ival == .jot ||
				           l.any(ival, follows: h1) ||
				           r.any(ival, precedes: h2)
			}
			if let ordered = try? l.label(h1, precedes: h2,
			    by: ival) {
				return ordered
			}
			if let ordered = try? r.label(h1, precedes: h2,
			    by: ival) {
				return ordered
			}
			throw NodeError.indexNotFound
		case .index(let w) where w.get() == h1 && h1 == h2:
			return false
		case .empty, .index(_), .leaf(_, _):
			throw NodeError.indexNotFound
		}
	}
}

public extension Rope.Node {
	init(controller ctlr: Rope.ZoneController,
	    properties props: Rope.ZoneProperties, node n: Self) {
		self = .zone((ctlr, props), n)
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
			self = .concat(left, left.dimensions,
				       1 + max(left.depth, right.depth),
				       left.labelSet.union(right.labelSet),
				       right,
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
				case .empty, .zone(_, _),
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
		case .zone(_, let rope):
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
	static func zone(under controller: Rope.ZoneController,
	    properties props: Rope.ZoneProperties = Rope.ZoneProperties(),
	    _ content: Self...) -> Self {
		return Self(controller: controller, properties: props,
		    node: tree(from: content))
	}
	static func zone(under controller: Rope.ZoneController,
	    properties props: Rope.ZoneProperties,
	    with content: [Self]) -> Self {
		return Self(controller: controller, properties: props,
		    node: tree(from: content))
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
	func withFreshBoundaries<T>(_ f: (Label, Label, Rope.Node) throws -> T)
	    rethrows -> T {
		let label: (l: Label, r: Label) = (Label(), Label())
		return try f(label.l, label.r,
		    .nodes(.index(label: label.l), self,
		           .index(label: label.r)))
	}
	func withFreshBoundaries<T>(_ f: (Label, Label, Rope.Node) -> T)
	    -> T {
		let label: (l: Label, r: Label) = (Label(), Label())
		return f(label.l, label.r,
		    .nodes(.index(label: label.l), self,
		           .index(label: label.r)))
	}
	func withFreshLeftBoundary<T>(_ f: (Label, Rope.Node) throws -> T)
	    rethrows -> T {
		let label = Label()
		return try f(label, .nodes(.index(label: label), self))
	}
	func withFreshLeftBoundary<T>(_ f: (Label, Rope.Node) -> T) -> T {
		let label = Label()
		return f(label, .nodes(.index(label: label), self))
	}
	func withFreshRightBoundary<T>(_ f: (Label, Rope.Node) throws -> T)
	    rethrows -> T {
		let label = Label()
		return try f(label, .nodes(self, .index(label: label)))
	}
	func withFreshRightBoundary<T>(_ f: (Label, Rope.Node) -> T) -> T {
		let label = Label()
		return f(label, .nodes(self, .index(label: label)))
	}
}

extension Rope.Node {
	public struct Dimensions {
		public let _indices: Int
		public let _boundaries: Int
		public let _units: Int
		public var jots: Int {
			return _indices + _boundaries + _units
		}
		public var steps: Int {
			return _boundaries + _units
		}
		public var units: Int {
			return _units
		}
		public static var zero: Dimensions {
			return Dimensions(indices: 0, boundaries: 0, units: 0)
		}
		public init(indices: Int = 0, boundaries: Int = 0,
		    units: Int = 0) {
			_indices = indices
			_boundaries = boundaries
			_units = units
		}
		public var halfPerimeter: Int {
			return jots
		}
	}
}

extension Rope.Node.Dimensions {
	public static func +(_ l: Self, _ r: Self) -> Self {
		return Self(indices: l._indices + r._indices,
		            boundaries: l._boundaries + r._boundaries,
		            units: l._units + r._units)
	}
}

public extension Rope.Node {
	var leaves: LeafSequence {
		return LeafSequence(of: self)
	}
	var depth: UInt {
		switch self {
		case .leaf(_, _), .empty, .index(_):
			return 0
		case .zone(_, let rope):
			return rope.depth
		case .concat(_, _, let depth, _, _, _):
			return depth
		}
	}
	var content: C {
		switch self {
		case .empty, .index(_):
			return C.empty
		case .zone(_, let rope):
			return rope.content
		case .leaf(_, let s):
			return s
		case .concat(let l, _, _, _, let r, _):
			return l.content + r.content
		}
	}
	var dimensions: Dimensions {
		switch self {
		case Self.concat(_, _, _, _, _, let dims):
			return dims
		case .zone(_, let rope):
			return rope.dimensions + Dimensions(boundaries: 2)
		case .leaf(_, let s):
			// TBD maybe .leaf should cache dimensions?
			let endOffset = s.endIndex.unitOffset(in: s)
			let startOffset = s.startIndex.unitOffset(in: s)
			return Dimensions(units: endOffset - startOffset)
		case .empty:
			return Dimensions.zero
		case .index(_):
			return Dimensions(indices: 1)
		}
	}
	func retrieveNode(at i: Int, on axis: KeyPath<Dimensions, Int>,
	    base: Int = 0) -> (Self, Int, Range<Int>) {
		let boundary = Dimensions(boundaries: 1)
		switch self {
		case .leaf(_, _), .empty, .index(_):
			return (self, i,
			        base..<(base + dimensions[keyPath: axis]))
		case .concat(let ropel, let mid, _, _, let roper, _):
			if i < mid[keyPath: axis] {
				return ropel.retrieveNode(at: i,
				    on: axis, base: base)
			} else {
				return roper.retrieveNode(
				    at: i - mid[keyPath: axis],
				    on: axis,
				    base: base + mid[keyPath: axis])
			}
		/* If `i` is left of the zone's interior... */
		case .zone(_, _) where i < boundary[keyPath: axis]:
			return (self, i,
			        base..<(base + dimensions[keyPath: axis]))
		/* If `i` is in the zone's interior... */
		case .zone(_, let n)
		    where i < (boundary + n.dimensions)[keyPath: axis]:
			return n.retrieveNode(
			    at: i - boundary[keyPath: axis],
			    on: axis,
			    base: base + boundary[keyPath: axis])
		/* If `i` is right of the zone's interior... */
		case .zone(_, _):
			return (self, i,
			        base..<(base + dimensions[keyPath: axis]))
		}
	}
	/* XXX Use zonesEnclosing(_: Rope.Index)? */
	func zonesEnclosing(_ i0: Int) -> [Rope.ZoneController] {
		var path: [Rope.ZoneController] = []
		var i = i0
		var next = self
		while true {
			switch next {
			case .leaf(_, _), .empty, .index(_):
				return path
			case .concat(let l, let mid, _, _, let r, _):
				if i < mid.units {
					next = l
				} else {
					i = i - mid.units
					next = r
				}
			case .zone((let ctlr, _), let content):
				path.append(ctlr)
				next = content
			}
		}
	}
	func zonesEnclosing(_ i: Rope.Index,
	                      under controllers: [Rope.ZoneController] = [])
	    throws -> [Rope.ZoneController] {
		return try zonesEnclosing(i.label, under: controllers)
	}
	func zonesEnclosing(_ label: Label,
	                      under controllers: [Rope.ZoneController] = [])
	    throws -> [Rope.ZoneController] {
		switch self {
		case .index(let w) where w.get() == label:
			return controllers
		case .concat(let l, _, _, _, let r, _)
		    where self.contains(label):
		    	do {
				return try l.zonesEnclosing(label,
				    under: controllers)
			} catch NodeError.indexNotFound {
			       return try r.zonesEnclosing(label,
			           under: controllers)
			}
		case .zone((let ctlr, _), let content):
			return try content.zonesEnclosing(label,
			    under: controllers + [ctlr])
		default:
			throw NodeError.indexNotFound
		}
	}
	func zonesOpening(at i: Rope.Index,
	                    in controllers: [Rope.ZoneController] = [])
	    throws -> [Rope.ZoneController] {
		return try zonesOpening(at: i.label, in: controllers)
	}
	func zonesOpening(at label: Label,
	                    in controllers: [Rope.ZoneController] = [])
	    throws -> [Rope.ZoneController] {
		switch self {
		case .zone((let ctlr, _), let content):
			return try content.zonesOpening(at: label,
			                              in: controllers + [ctlr])
		case .index(let w) where w.get() == label:
			return controllers
		case .concat(let l, let mid, _, let set, let r, _)
		    where set.contains(label.id):
			do {
				return try l.zonesOpening(at: label,
				    in: controllers)
			} catch NodeError.indexNotFound {
				/* If there are characters left of `r`, or any
				 * zones open left of `r`, then the
				 * controllers we have seen on our way down,
				 * `controllers`, do not open at `label`.
				 * Rather, they open at an index on the left.
				 * So leave them out of the list.
				 */
				guard 0 == mid.steps else {
					return try r.zonesOpening(at: label)
				}
				return try r.zonesOpening(at: label,
				    in: controllers)
			}
		default:
			throw NodeError.indexNotFound
		}
	}
	func zonesClosing(at i: Rope.Index,
	                    in controllers: [Rope.ZoneController] = [])
	    throws -> [Rope.ZoneController] {
		return try zonesClosing(at: i.label, in: controllers)
	}
	func offset(of label: Label, on axis: KeyPath<Dimensions, Int>,
	    origin: Int = 0) throws -> Int {
		let boundary = Dimensions(boundaries: 1)
		switch self {
		case .zone(_, let content):
			return try content.offset(of: label, on: axis,
			    origin: origin + boundary[keyPath: axis])
		case .index(let w) where w.get() == label:
			return origin
		case .concat(let l, let mid, _, _, let r, _):
			/* TBD accelerate: check for `label` presence in
			 * `l` and `r`.
			 */
			do {
				return try l.offset(
				    of: label, on: axis, origin: origin)
			} catch NodeError.indexNotFound {
				return try r.offset(
				    of: label, on: axis,
				    origin: origin + mid[keyPath: axis])
			}
		default:
			throw NodeError.indexNotFound
		}
	}
	func zonesClosing(at label: Label,
	                    in controllers: [Rope.ZoneController] = [])
	    throws -> [Rope.ZoneController] {
		switch self {
		case .zone((let ctlr, _), let content):
			return try content.zonesClosing(at: label,
			                              in: controllers + [ctlr])
		case .index(let w) where w.get() == label:
			return controllers
		case .concat(let l, let mid, _, let set, let r, let w)
		    where set.contains(label.id):
			do {
				return try r.zonesClosing(at: label,
				    in: controllers)
			} catch NodeError.indexNotFound {
				/* If there are characters right of `l`, or any
				 * zones open right of `l`, then the
				 * controllers we have seen on our way down,
				 * `controllers`, do not close at `label`.
				 * Rather, they close at an index on the right.
				 * So leave them out of the list.
				 */
				guard mid.steps == w.steps else {
					return try l.zonesClosing(at: label)
				}
				return try l.zonesClosing(at: label,
				    in: controllers)
			}
		default:
			throw NodeError.indexNotFound
		}
	}
	func element(at i: Int) -> Element {
		switch self {
		case .leaf(_, let s):
			let idx = C.Index(unitOffset: i, in: s)
			let c: Element = s[idx]
			return c
		case .concat(let ropel, let mid, _, _, let roper, _):
			if i < mid.units {
				return ropel.element(at: i)
			} else {
				return roper.element(at: i - mid.units)
			}
		case .empty, .index(_):
			fatalError("In \(#function), no element \(i)")
		case .zone(_, let rope):
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
		case (.concat(let l, _, _, _, .leaf(let pat, let p), _),
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
		return dimensions.jots >= fibonacci(index: depth + 2)
	}
	// Return this Node with all of the expired indices removed.
	func cleaned() -> Self? {
		switch self {
		case .empty, .leaf(_, _):
			return self
		case .zone(let z, let n):
			let nn = n.cleaned()
			return .zone(z, nn ?? .empty)
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
		case .empty, .index(_), .leaf(_, _):
			return self
		case .zone(let z, let rope):
			return .zone(z, rope.rebalanced())
		default:
			break
		}
		var slot: [Self?] = []
		let totlen = dimensions.jots
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
				if fip3 >= tree.dimensions.jots {
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
	func subrope(after boundary: Rope.Index, rightSibling: Self = .empty,
	    depth: Int = 0) -> Self? {
		switch self {
		case .zone((let ctlr, let props), let content):
			guard let subzone = ctlr.subrope(of: content,
			    after: boundary, properties: props,
			    depth: depth) else {
				return rightSibling.subrope(after: boundary,
				    depth: depth)
			}
			return subzone.appending(rightSibling)
		case .index(let w) where w.get() == boundary.label:
			return rightSibling
		case .concat(let l, _, _, _, let r, _)
		    where self.contains(boundary.label):
			guard let match = l.subrope(after: boundary,
			    rightSibling: r.appending(rightSibling),
			    depth: depth + 1) else {
				return r.subrope(after: boundary,
				    rightSibling: rightSibling,
				    depth: depth + 1)
			}
			return match
		case .empty where rightSibling == .empty,
		     .index(_) where rightSibling == .empty,
		     .leaf(_, _) where rightSibling == .empty:
			return nil
		default:
			return rightSibling.subrope(after: boundary, depth:
			    depth)
		}
	}
	func splitting(leftSibling: Self = .empty, at boundary: Label,
	    indexOn side: Side, rightSibling: Self = .empty, depth: Int = 0)
	    throws -> (Self, Self) {
		switch self {
		case .index(let w) where w.get() == boundary:
			if side == .right {
				return (leftSibling,
				        self.appending(rightSibling))
			}
			return (leftSibling.appending(self), rightSibling)
		case .concat(let l, _, _, _, let r, _)
		    where self.contains(boundary):
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
		case _ where rightSibling == .empty:
			throw NodeError.indexNotFound
		default:
			return try rightSibling.splitting(leftSibling:
			    leftSibling.appending(self), at: boundary,
			    indexOn: side, depth: depth + 1)
		}
	}
	func splitting(before boundary: Label) throws -> (Self, Self) {
		return try splitting(at: boundary, indexOn: .right)
	}
	func splitting(before boundary: Rope.Index) throws -> (Self, Self) {
		return try splitting(at: boundary.label,
				     indexOn: .right)
	}
	func splitting(after boundary: Label) throws -> (Self, Self) {
		return try splitting(at: boundary, indexOn: .left)
	}
	func splitting(after boundary: Rope.Index) throws -> (Self, Self) {
		return try splitting(at: boundary.label, indexOn: .left)
	}
	func subrope(leftSibling: Self = .empty, upTo boundary: Rope.Index,
	    depth: Int = 0) -> Self? {
		/*
		Swift.print("enter \(" " * depth)\(#function) " +
		            "leftSibling \(leftSibling) self \(self) " +
			    "upTo \(boundary)", terminator: ": ")
		*/
		switch self {
		case .zone((let ctlr, let props), let content):
			guard let subzone = ctlr.subrope(of: content,
			    upTo: boundary, properties: props,
			    depth: depth + 1) else {
				return leftSibling.subrope(upTo: boundary,
				                           depth: depth + 1)
			}
			return leftSibling.appending(subzone)
		case .index(let w) where w.get() == boundary.label:
			return leftSibling
		case .concat(let l, _, _, _, let r, _)
		    where self.contains(boundary.label):
			guard let match = r.subrope(
			    leftSibling: leftSibling.appending(l),
			    upTo: boundary, depth: depth + 1) else {
				return l.subrope(leftSibling: leftSibling,
				    upTo: boundary, depth: depth + 1)
			}
			return match
		case .empty where leftSibling == .empty,
		     .index(_) where leftSibling == .empty,
		     .leaf(_, _) where leftSibling == .empty:
			return nil
		default:
			return leftSibling.subrope(upTo: boundary,
			    depth: depth + 1)
		}
	}
	func compactMap(_ filter: (Self) -> Self?) -> Self? {
		switch self {
		case .zone((let ctlr, let props), let content):
			let filtered = content.compactMap(filter) ?? .empty
			return .zone(under: ctlr, properties: props, filtered)
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
		let suffix = subrope(after: l, depth: depth)
		return suffix?.subrope(upTo: r, depth: depth)
	}
	func replacing(after lowerBound: Rope.Index,
	    upTo upperBound: Rope.Index, with replacement: Content,
	    undoList: ChangeList<Self>?) throws -> Self {
		return try replacing(
		    after: lowerBound.label, upTo: upperBound.label,
		    with: .text(replacement), undoList: undoList)
	}
	/* A naive version of `setController(_:after:upTo:)` tries
	 * to establish a zone that crosses zones.  This
	 * implementation makes sure that the boundaries of the new
	 * zone are both located in the same existing zone, in
	 * which case it forwards to that existing zone, or else that
	 * neither boundary is inside a zone.
	 */
	func setController(_ z: (Rope.ZoneController, Rope.ZoneProperties),
	    after lowerBound: Label, upTo upperBound: Label,
	    undoList: ChangeList<Self>?) throws -> Self {
		switch (try zonesEnclosing(lowerBound).first,
			try zonesEnclosing(upperBound).first) {
		case (nil, nil):
			/* Catch a faulty range where the upperBound
			 * precedes the lowerBound.
			 */
			if try label(upperBound, precedes: lowerBound,
			    by: .jot) {
				throw NodeError.indicesOutOfOrder
			}
			/* Important: don't discard any embedded indices at
			 * `range` boundaries!  Instead, use
			 * splitting(after:) and splitting(before:) to
			 * preserve embedded indices for reuse.
			 */
			let (head, rest) = try splitting(after: lowerBound)
			/* Empty ranges where the same label is used
			 * for the lower and upper bound require special
			 * treatment:
			 */
			if (lowerBound == upperBound) {
				return try rest.withFreshLeftBoundary {
				    (upper, node) in
				        return try Self.nodes(head,
					    node).setController(z,
					        after: lowerBound,
						upTo: upper,
						undoList: undoList)
				}
			}
			let (middle, tail) =
			    try rest.splitting(before: upperBound)
			/* Important: zones in the range do not get an
			 * opportunity to cancel.  They will move to the
			 * interior of the zone, always.
			 */
			undoList?.record { (node, undoList) in
				try node.replacing(
				    after: lowerBound, upTo: upperBound,
				    with: middle,
				    undoList: undoList)
			}
			return .nodes(head, .zone(z, middle), tail)
		case (let loExt?, let hiExt?) where loExt == hiExt:
			let (l, (ctlr, props, m), r) =
			    try segmenting(atZone: loExt)
			let mControlled = try ctlr.setController(z,
			    after: lowerBound, upTo: upperBound, in: m,
			    properties: props, undoList: undoList)
			return l.appending(mControlled).appending(r)
		default:
			throw NodeError.indicesCrossZones
		}
	}
	/* A naive version of `replacing(after:upTo:with:)` splits zones.
	 * This version finds affected zones, splits before and after each
	 * zone, and performs replacement/deletion on each affected zone.
	 */
	func replacing(after lowerBound: Label, upTo upperBound: Label,
	               with replacement: Self, undoList: ChangeList<Self>?)
	    throws -> Self {
		switch (try zonesEnclosing(lowerBound).first,
			try zonesEnclosing(upperBound).first) {
		case (nil, nil):
			/* Catch a faulty range where the upperBound
			 * precedes the lowerBound.
			 */
			if try label(upperBound, precedes: lowerBound,
			    by: .jot) {
				throw NodeError.indicesOutOfOrder
			}
			/* Important: don't discard any embedded indices at
			 * `range` boundaries!  Instead, use
			 * splitting(after:) and splitting(before:) to
			 * preserve embedded indices for reuse.
			 */
			let (head, rest) = try splitting(after: lowerBound)

			/* Empty ranges where the same label is used
			 * for the lower and upper bound require special
			 * treatment:
			 */
			if (lowerBound == upperBound) {
				return try rest.withFreshLeftBoundary {
				    (upper, node) in
				        return try Self.nodes(head,
					    node).replacing(
					        after: lowerBound,
						upTo: upper,
						with: replacement,
						undoList: undoList)
				}
			}
			let (middle, tail) =
			    try rest.splitting(before: upperBound)
			/* Important: perform .replacing() on zones in the
			 * range so that they get an opportunity to cancel if
			 * they are read-only.
			 */
			switch middle.segmentingAtAnyZone() {
			case (_, nil, _):
				break
			/* By our contract with `segmentingAtAnyZone`, the
			 * `middle` subrope left of the .zone is
			 * .zone-free, so we do not need to test for
			 * a read-only .zone's "veto."  We bind `l` only
			 * so that we can record an undo record that
			 * re-inserts it.
			 */
			case (_, (let ctlr, let props, let m)?, let r):
				/* We have to try to replace using the
				 * controller so that a read-only zone can
				 * "veto" the replacement with .empty by
				 * throwing.
				 */
				let _ = try m.withFreshBoundaries {
				    (lower, upper, node) in
					try ctlr.replacing(
					    after: lower, upTo: upper,
					    in: node, properties: props,
					    with: .empty, undoList: nil)
				}
				/* We have to try to replace on `r` just
				 * just in case it contains a read-only zone
				 * that "vetoes" the replacement with .empty by
				 * throwing.
				 */
				let _ = try r.withFreshBoundaries {
					(lower, upper, node) in
					    try node.replacing(
					        after: lower, upTo: upper,
						with: .empty, undoList: nil)
				}
				break
			}
			undoList?.record { (node, undoList) in
				try node.replacing(
				    after: lowerBound, upTo: upperBound,
				    with: middle,
				    undoList: undoList)
			}
			return .nodes(head, replacement, tail)
		case (let loExt?, let hiExt?) where loExt == hiExt:
			let (l, (ctlr, props, m), r) =
			    try segmenting(atZone: loExt)
			let mReplaced = try ctlr.replacing(after: lowerBound,
			    upTo: upperBound, in: m, properties: props,
			    with: replacement, undoList: undoList)
			return l.appending(mReplaced).appending(r)
		case (let loExt?, _):
			let (l, (ctlr, props, m), r) =
			    try segmenting(atZone: loExt)
			let mReplaced: Rope.Node = try m.withFreshRightBoundary{
			    (upper, node) in
			        return try ctlr.replacing(
				    after: lowerBound, upTo: upper, in: node,
				    properties: props, with: replacement,
				    undoList: undoList)
			}
			let rTrimmed: Rope.Node = try r.withFreshLeftBoundary {
			    (lower, node) in
			        return try node.replacing(
				    after: lower, upTo: upperBound,
				    with: .empty, undoList: undoList)
			}
			return l.appending(mReplaced).appending(rTrimmed)
		case (nil, let hiExt?):
			let (l, (ctlr, props, m), r) =
			    try segmenting(atZone: hiExt)
			let lReplaced: Rope.Node = try l.withFreshRightBoundary{
			    (upper, node) in
			        return try node.replacing(
				    after: lowerBound, upTo: upper,
				    with: replacement, undoList: undoList)
			}
			let mTrimmed: Rope.Node = try m.withFreshLeftBoundary {
			    (lower, node) in
			        return try ctlr.replacing(
				    after: lower, upTo: upperBound,
				    in: node, properties: props,
				    with: .empty, undoList: undoList)
			}
			return lReplaced.appending(mTrimmed).appending(r)
		}
	}
	func segmenting(atZone target: Rope.ZoneController,
	    leftSibling: Self = .empty) throws
	    -> (Self, (Rope.ZoneController, Rope.ZoneProperties, Self), Self) {
		switch self {
		case .zone((target, let props), let inner):
			return (leftSibling, (target, props, inner), .empty)
		case .concat(let l, _, _, _, let r, _):
			if case let (head, z, tail)? =
			    try? l.segmenting(atZone: target) {
				return (leftSibling.appending(head),
					z, tail.appending(r))
			} else {
				return try r.segmenting(atZone: target,
				    leftSibling: leftSibling.appending(l))
			}
		case .empty, .zone(_, _), .index(_),
		     .leaf(_, _):
			throw NodeError.zoneNotFound
		}
	}
	func segmentingAtAnyZone(leftSibling: Self = .empty)
	    -> (Self, (Rope.ZoneController, Rope.ZoneProperties, Self)?, Self) {
		switch self {
		case .leaf(_, _), .index(_), .empty:
			return (leftSibling.appending(self), nil, .empty)
		case .zone((let ctlr, let props), let node):
			return (leftSibling, (ctlr, props, node), .empty)
		case .concat(let l, _, _, _, let r, _):
			if case let (head, (ctlr, props, node)?, tail) =
			    l.segmentingAtAnyZone() {
				return (leftSibling.appending(head),
					(ctlr, props, node), tail.appending(r))
			} else {
				return r.segmentingAtAnyZone(
				    leftSibling: leftSibling.appending(l))
			}
		}
	}
	func transformingZone(withLabel target: Label,
	    with f: (_: Rope.ZoneController, _: Rope.ZoneProperties, _: Self)
	                -> Self) -> Self? {
		switch self {
		case .empty, .index(_), .leaf(_, _):
			return nil
		case .zone((let ctlr, let props), let content)
		    where ctlr == target:
			return f(ctlr, props, content)
		case .zone((let ctlr, let props), let n):
			guard let newn = n.transformingZone(withLabel: target,
			    with: f) else {
				return nil
			}
			return .zone(under: ctlr, properties: props, newn)
		case .concat(let l, _, _, let set, let r, _):
			guard set.contains(target.id) else {
				return nil
			}
			if let newl = l.transformingZone(
			    withLabel: target, with: f) {
				return .nodes(newl, r)
			} else if let newr = r.transformingZone(
			    withLabel: target, with: f) {
				return .nodes(l, newr)
			} else {
				return nil
			}
		}
	}
	func insertingFirstIndex(_ label: Label, inZone target: Label) ->Self? {
		return transformingZone(withLabel: target) {
		    (ctlr, props, content) in
		        .zone(under: ctlr, properties: props,
			      .index(label: label), content)
		}
	}
	func insertingLastIndex(_ label: Label, inZone target: Label) -> Self? {
		return transformingZone(withLabel: target) {
		    (ctlr, props, content) in
			.zone(under: ctlr, properties: props,
			    content, .index(label: label))
		}
	}
	func insertingIndex(_ label: Label, afterZone target: Label) -> Self? {
		return transformingZone(withLabel: target) {
		    (ctlr, props, content) in
			.nodes(.zone(under: ctlr, properties: props, content),
			       .index(label: label))
		}
	}
	func insertingIndex(_ label: Label, beforeZone target: Label) -> Self? {
		return transformingZone(withLabel: target) {
		    (ctlr, props, content) in
			.nodes(.index(label: label),
			       .zone(under: ctlr, properties: props, content))
		}
	}
	func label(_ h1: Label, aliases h2: Label) throws -> Bool {
		if h1 == h2 {
			return true
		}
		let precedes = try label(h1, precedes: h2, by: .step)
		let follows = try label(h2, precedes: h1, by: .step)
		return !precedes && !follows
	}
}

public extension Rope.Node {
	func extract(from start: Int, upTo end: Int,
	    on axis: KeyPath<Rope.Node.Dimensions, Int>,
	    filling buffer: inout UnsafeMutablePointer<C.Unit>,
	    defaults units: Rope.BoundaryUnits) {
		let bdry = Dimensions(boundaries: 1)

		if start == end {
			return
		}

		switch self {
		case .concat(let l, let mid, _, _, let r, _):
			let middle = mid[keyPath: axis]
			if start < middle {
				l.extract(from: start,
				    upTo: min(end, middle), on: axis,
				    filling: &buffer, defaults: units)
			}
			if middle < end {
				r.extract(
				    from: max(start, middle) - middle,
				    upTo: end - middle, on: axis,
				    filling: &buffer, defaults: units)
			}
		case .leaf(_, let s):
			s.extract(from: start, upTo: end, filling: &buffer)
		case .empty, .index(_):
			return
		case .zone(_, _) where start < bdry[keyPath: axis]:
			// Insert the open-zone unit.  Adjust `buffer`.
			buffer.pointee = units.open
			buffer += 1
			if end <= bdry[keyPath: axis] {
				return
			}
			return extract(from: bdry[keyPath: axis], upTo: end,
			    on: axis, filling: &buffer, defaults: units)
		case .zone(_, let n)
		    where start < (bdry + n.dimensions)[keyPath: axis]:
			n.extract(
			    from: start - bdry[keyPath: axis],
			    upTo: min(n.dimensions[keyPath: axis],
			              end - bdry[keyPath: axis]),
			    on: axis,
			    filling: &buffer, defaults: units)
			if end <= (bdry + n.dimensions)[keyPath: axis] {
				return
			}
			return extract(
			    from: (bdry + n.dimensions)[keyPath: axis],
			    upTo: end,
			    on: axis, filling: &buffer, defaults: units)
		case .zone(_, _):
			// Insert the close-zone unit.  Adjust `buffer`.
			buffer.pointee = units.close
			buffer += 1
		}
	}
}
