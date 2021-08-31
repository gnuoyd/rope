//
// Copyright (c) 2019, 2020 David Young.  All rights reserved.
//
import Foundation

enum RopeNoSuchElement : Error {
case onInterior
}

extension Range {
	public init<C : Content>(_ r: Range<Rope<C>.Node.Offset>,
	                   in rope: Rope<C>) where Bound == Rope<C>.Index {
		let lower = Rope.Index(abutting: r.lowerBound, on: .right,
		    in: rope)
		let upper = Rope.Index(abutting: r.upperBound, on: .left,
		    in: rope)
		if r.lowerBound != r.upperBound {
			self = lower..<upper
		} else if try! rope.node.label(lower.label,
		                               precedes: upper.label,
					       by: .index) {
			self = lower..<upper
		} else {
			self = upper..<lower
		}
	}
	// TBD add (Bound, Bound) property `orderedAliasedBounds`
}

extension Rope.Node {
	public struct Dimensions {
		public let jots: Int
		public let unitOffset: Offset
		public static var zero: Dimensions {
			return Dimensions(jots: 0, unitOffset: 0)
		}
		public init(jots: Int = 0, unitOffset offset: Offset = 0) {
			self.jots = jots
			self.unitOffset = offset
		}
		public var halfPerimeter: Int {
			guard case .offset(let n) = unitOffset else {
				return jots
			}
			return jots + n
		}
	}
}

extension Rope.Node.Dimensions {
	public static func +(_ l: Self, _ r: Self) -> Self {
		return Self(jots: l.jots + r.jots,
		            unitOffset: l.unitOffset + r.unitOffset)
	}
}

extension Rope.Node.Offset : ExpressibleByIntegerLiteral {
	public typealias IntegerLiteralType = Int
	public init(integerLiteral n: Self.IntegerLiteralType) {
		self = .offset(n)
	}
}

extension Rope.Node {
	public typealias Unit = C.Unit
}

extension Rope.Node.Offset {
	public init(of n: Int) {
		self = .offset(n)
	}
	public var unitOffset: Int {
		switch self {
		case .offset(let n):
			return n
		}
	}
}

/* Use cases:
 *
 * Get/set/add/remove attributes on characters.
 *
 * Get/set/add/remove attributes on a cursor.
 *
 * Enclose a range in an extent.  The range must be
 * well-formed: must begin and end inside the same extent.
 *
 * Insert a cursor between characters or nested between extents.
 *
 * Remove a cursor.
 *
 * "Step" a cursor left or right by a character.
 *
 * "Scoot" a cursor left or right by an extent boundary.
 *
 * "Scoot" a cursor left or right by a cursor?
 *
 * Replace a cursor by an extent; apply the cursor's attributes to
 * the extent's content.
 *
 * Insert some text left of a cursor; apply the cursor's attributes
 * to the text.
 */
public class Rope<C : Content> : Collection {
	public enum Climb {
	case `in`
	case out
	}
	public typealias Content = C
	public typealias Element = Node
	public typealias Offset = Node.Offset
	public enum Index : Comparable {
	case interior(of: Rope, label: Label)
	}
	public class ExtentController : Label {
		public override var id: Id { return .extent(_id) }
		func subrope(of content: Rope.Node, from: Rope.Node.Offset,
		    tightly: Bool, depth: Int = 0) -> Rope.Node {
			let subcontent = content.subrope(from: from,
			    depth: depth)
			return .extent(self, subcontent)
		}
		func subrope(of content: Rope.Node,
		    upTo boundary: Rope.Node.Offset, tightly: Bool,
		    depth: Int = 0) -> Rope.Node {
			let subcontent = content.subrope(upTo: boundary,
			    depth: depth)
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
		func subrope(of content: Rope.Node, upTo boundary: Rope.Index,
		    depth: Int = 0) -> Rope.Node? {
			guard let subcontent = content.subrope(upTo: boundary,
			    depth: depth) else {
				return nil
			}
			return .extent(self, subcontent)
		}
		func node(_ content: Rope.Node, inserting elt: Rope.Node,
		    on side: Rope.Node.Side, of target: Label)
		    throws -> Rope.Node {
			let augmented =
			    try content.inserting(elt, on: side, of: target)
			return .extent(self, augmented)
		}
		func replacing(after lowerBound: Label, upTo upperBound: Label,
		    in content: Rope.Node, with replacement: Rope.Node,
		    recording optChanges: ChangeList<Rope.Node>?)
		    throws -> Rope.Node {
			let replaced = try content.replacing(
			    after: lowerBound, upTo: upperBound,
			    with: replacement, recording: optChanges)
			guard let changes = optChanges else {
				return .extent(self, replaced)
			}
			let newLower = Label(), newUpper = Label()
			changes.record { (node, undoList) in
				return try node.performingReplacement(
				    after: newLower, upTo: newUpper,
				    new: .extent(self, replaced),
				    old: .extent(self, content),
				    undoList: undoList)
			}
			return .nodes(.index(label: newLower),
			              .extent(self, content),
				      .index(label: newUpper))
		}
		func transformingAttributes(after lowerBound: Label,
		    upTo upperBound: Label, in content: Rope.Node,
		    with fn: (Attributes) -> Attributes) throws -> Rope.Node {
			let xformed = try content.transformingAttributes(
			    after: lowerBound, upTo: upperBound, with: fn)
			return .extent(self, xformed)
		}
	}

	/* A Node directly encodes the presence of cursors because it is
	 * possible for a cursor to move up and down the hierarchy of text
	 * extents without changing between-character positions.  A cursor
	 * can appear at the position left of the first character or right of
	 * the last character in a Node.  A cursor can also appear in a
	 * Node that contains no characters.
	 */
	public indirect enum Node {
	public typealias Content = C
	public typealias Element = C.Element
	public enum Offset {
		case offset(Int)
	}
	case cursor(Label, Attributes)
	case index(Weak<Label>)
	case extent(ExtentController, Node)
	case concat(Node, Offset, UInt, LabelSet, Node, Dimensions)
	case leaf(Attributes, C)
	case empty
	}

	private var _delegate: TypeErasedOffsetDelegate? = nil
	public var delegate: TypeErasedOffsetDelegate {
		set {
			_delegate = newValue
		}
		get {
			return _delegate ?? AnyRopeOffsetDelegate(
			    didChange: self.ropeDidChange,
			    attributesDidChange: self.ropeAttributesDidChange)
		}
	}
	private var mutations: UInt = 0
	private var rebalanceInterval: UInt = 32
	internal var _startLabel: Label
	internal var _endLabel: Label
	private var _top: Node
	private var top: Node {
		get {
			return _top
		}
		set {
			var tmp: Node
			mutations += 1
			if mutations.isMultiple(of: rebalanceInterval) {
				tmp = newValue.cleaned() ?? .empty
			} else {
				tmp = newValue
			}
			switch try? tmp.indices(precede: _startLabel) {
			case false?:
				break
			default:
				_startLabel = Label()
				tmp = .nodes(.index(label: _startLabel), tmp)
			}
			switch try? tmp.indices(follow: _endLabel) {
			case false?:
				break
			default:
				_endLabel = Label()
				tmp = .nodes(tmp, .index(label: _endLabel))
			}
			if mutations.isMultiple(of: rebalanceInterval) {
				_top = tmp.rebalanced()
			} else {
				_top = tmp
			}
		}
	}
	public var startIndex: Index {
		return .interior(of: self, label: _startLabel)
	}
	public var endIndex: Index {
		return .interior(of: self, label: _endLabel)
	}

	public init() {
		_startLabel = Label()
		_endLabel = Label()
		_top = .nodes(.index(label: _startLabel),
		              .index(label: _endLabel))
	}
	public init(with nodes: Node...) {
		let prototype = Node.tree(from: nodes)
		var tmp = prototype
		if let leftmost = prototype.leftmostIndexLabel(),
		    case false? = try? prototype.indices(precede: leftmost) {
			_startLabel = leftmost
		} else {
			_startLabel = Label()
			tmp = .nodes(.index(label: _startLabel), tmp)
		}
		if let rightmost = prototype.rightmostIndexLabel(),
		    case false? = try? prototype.indices(follow: rightmost) {
			_endLabel = rightmost
		} else {
			_endLabel = Label()
			tmp = .nodes(tmp, .index(label: _endLabel))
		}
		_top = tmp
	}
	public var node: Node {
		get {
			return top
		}
		set {
			top = newValue
		}
	}
	public func step(_ h1: Label, precedes h2: Label) throws -> Bool {
		return try top.step(h1, precedes: h2)
	}
	public func label(_ h1: Label, aliases h2: Label) throws -> Bool {
		if h1 == h2 {
			return true
		}
		let precedes = try top.step(h1, precedes: h2)
		let follows = try top.step(h2, precedes: h1)
		return !precedes && !follows
	}
	public init<T>(content t: T) where C.SubSequence == T {
		_startLabel = Label()
		_endLabel = Label()
		_top = .nodes(.index(label: _startLabel),
		              Node(content: t),
			      .index(label: _endLabel))
	}
	public func index(after i: Index) -> Index {
		guard i.owner === self else {
			fatalError("Mismatched owner")
		}
		let j = Label()
		switch top.inserting(j, one: .rightStep, after: i.label) {
		case .inchOut:
			fatalError(".interior(_, \(i.label)) already at end?")
		case .absent:
			fatalError(".interior(_, \(i.label)) is absent")
		case .stepOut:
			return endIndex
		case .step(let node):
			top = node
			return .interior(of: self, label: j)
		}
	}
	public func index(before i: Index) -> Index {
		guard i.owner === self else {
			fatalError("Mismatched owner")
		}
		let j = Label()
		switch top.inserting(j, one: .leftStep, after: i.label) {
		case .inchOut:
			fatalError(
			    ".interior(_, \(i.label)) already at start?")
		case .absent:
			fatalError(".interior(_, \(i.label)) is absent")
		case .stepOut:
			return startIndex
		case .step(let node):
			top = node
			return .interior(of: self, label: j)
		}
	}
	public subscript(i: Offset) -> Content.Element {
		return top.element(at: i)
	}
	public subscript(r: Range<Index>) -> Element {
		get {
			if r.isEmpty {
				return .empty
			}
			guard let e = top.subrope(after: r.lowerBound,
			    upTo: r.upperBound) else {
				fatalError("No such range")
			}
			return e
		}
	}
	public subscript(i: Index) -> Element {
		get {
			do {
				return try element(at: i)
			} catch {
				fatalError("No such element")
			}
		}
	}
	public func element(at i: Index) throws -> Element {
		let result = top.element(at: i.label)
		guard case .step(let node) = result else {
			throw RopeNoSuchElement.onInterior
		}
		return node
	}
/*
	public func insert(_ elt: Element, at i: Index) -> Bool {
		guard self === i.owner else {
			return false
		}
		if case .empty = elt {
			return false
		}
		guard let newtop = top.inserting(elt, at: i.label) else {
			return false
		}
		top = newtop
		return true
	}
*/
	/* TBD tests */
	public subscript(_ r: Range<Offset>) -> Content {
		set(replacement) {
			do {
				let undoList = ChangeList<Rope>()
				try replace(r, with: replacement,
				            undoList: undoList)
			} catch {
				fatalError("No such range")
			}
		}
		get {
			let ir = Range(r, in: self)
			return top[ir]
		}
	}
	public func replace(_ r: Range<Offset>, with replacement: Content,
	    undoList: ChangeList<Rope>) throws {
		let ir = Range(r, in: self)
		try replace(ir, with: replacement, undoList: undoList)
	}
	typealias OffsetPair = (lower: Offset, upper: Offset)
	public func replace(_ r: Range<Index>, with replacement: Content,
	    undoList: ChangeList<Rope>) throws {
		/* Create a ChangeList and record .replacing(...) changes on
		 * it. .replacing(...) has labels each change location.
		 *
		 * Apply the ChangeList to `top` to produce `newtop`,
		 * recording the inverse on undoList.
		 *
		 * Finally, record an indication that a text range
		 * was edited with its offsets and any change in length.
		 */
		let changes = ChangeList<Rope.Node>()
		top = try top.replacing(
		    after: r.lowerBound, upTo: r.upperBound,
		    with: replacement, recording: changes)
		let oldOffsets: OffsetPair =
		    try (top.offset(of: r.lowerBound.label),
		         top.offset(of: r.upperBound.label))
		try performReplacement(changes, undoList: undoList)
		let newOffsets: (lower: Offset, upper: Offset) =
		    try (top.offset(of: r.lowerBound.label),
		         top.offset(of: r.upperBound.label))
		delegate.indicateChanges(new: newOffsets, old: oldOffsets,
		    undoList: undoList)
	}
	func performReplacement(_ changes: ChangeList<Rope.Node>,
	     undoList: ChangeList<Rope>) throws {
		let (newtop, reversals) = try changes.play(withTarget: top)
		top = newtop
		undoList.record { (rope, undoList) in
			try self.performReplacement(reversals,
			    undoList: undoList)
			return rope
		}
	}
	public subscript<I>(_ r: Range<Offset>) -> I where C.SubSequence == I {
		set(newValue) {
			self[r] = C(newValue)
		}
		get {
			return self[r][...]
		}
	}
	public func attributes(at i: Offset) -> (Attributes, Range<Offset>) {
		return top.attributes(at: i)
	}
	public func setAttributes(_ attrs: Attributes, range r: Range<Offset>){
		let ir = Range(r, in: self)
		guard let newtop = try? top.settingAttributes(attrs, range: ir)
		    else {
			return
		}
		top = newtop
		delegate.indicateAttributeChanges(on: r)
	}
	public func clearAttributes(on r: Range<Offset>) {
		let ir = Range(r, in: self)
		guard let newtop = try? top.clearingAttributes(on: ir) else {
			return
		}
		top = newtop
	}
	public func offset(of index: Index) throws -> Offset {
		guard case .interior(_, let label) = index else {
			throw RopeNoSuchElement.onInterior
		}
		return try top.offset(of: label)
	}
}

extension Rope : RopeOffsetDelegate {
	public func ropeDidChange(on: Range<Offset>, changeInLength: Int) {
		return
	}
	public func ropeAttributesDidChange(on: Range<Offset>) {
		return
	}
}

extension Rope {
	func steps(follow target: Label) throws -> Bool {
		return try top.steps(follow: target)
	}
	func steps(precede target: Label) throws -> Bool {
		return try top.steps(precede: target)
	}
	var hasSingleIndex: Bool {
		return top.hasSingleIndex
	}
}

extension Rope.Node {
	var hasSingleIndex: Bool {
		return labelSet.extentCount == 0 && length == 0
	}
}

extension Rope {
        public struct UnitView {
		let rope: Rope
		init(rope r: Rope) {
			rope = r
		}
                public subscript(i: Offset) -> Rope.Node.Unit {
                        get {
                                return rope.top.unit(at: i)
                        }
                }
		public var length: Int {
			return rope.top.length
		}
	}
	public var units: UnitView {
                return UnitView(rope: self)
	}
}

extension Rope {
	public func extractContent(_ range: Range<Offset>) -> C.SubSequence {
		return top.extractContent(from: range.lowerBound,
		    upTo: range.upperBound)
	}
	public func extractContent(_ range: Range<Offset>,
	    filling buffer: inout C) {
		top.extractContent(from: range.lowerBound,
		    upTo: range.upperBound, filling: &buffer)
	}
	public func extractUnits(_ range: Range<Offset>,
	    filling buffer: inout UnsafeMutablePointer<C.Unit>){
		return top.extractUnits(from: range.lowerBound,
		    upTo: range.upperBound, filling: &buffer)
	}
}

extension Rope {
	func extentsEnclosing(_ i: Index) throws -> [ExtentController] {
		return try top.extentsEnclosing(i)
	}
	public func extentsClosing(at i: Index) throws -> [ExtentController] {
		return try top.extentsClosing(at: i)
	}
	public func extentsOpening(at i: Index) throws -> [ExtentController] {
		return try top.extentsOpening(at: i)
	}
}

extension Rope {
	public func index(after i: Index, climbing dir: Climb,
	    bottom: inout ExtentController?) -> Index? {
		do {
			if try !top.indices(follow: i.label) {
				return nil
			}
		} catch {
			return nil
		}
		let j = index(after: i)
		switch ((try? extentsEnclosing(i))?.count,
			dir,
		        (try? extentsEnclosing(j))?.count) {
		case (let ni?, .in, let nj?) where ni < nj:
			return j
		case (let ni?, .out, let nj?) where ni > nj:
			return j
		default:
			return nil
		}
	}
	public func index(before i: Index, climbing dir: Climb,
	    bottom: inout ExtentController?) -> Index? {
		do {
			if try !top.indices(precede: i.label) {
				return nil
			}
		} catch {
			return nil
		}
		let j = index(before: i)
		switch (try? extentsEnclosing(i),
			dir,
		        try? extentsEnclosing(j)) {
		case (let ei?, .in, let ej?) where ei.count < ej.count:
			bottom = ej.last
			return j
		case (let ei?, .out, let ej?) where ei.count > ej.count:
			bottom = ej.last
			return j
		default:
			return nil
		}
	}
	public func index(after i: Index, climbing dir: Climb) -> Index? {
		var discard: ExtentController?
		return index(after: i, climbing: dir, bottom: &discard)
	}
	public func index(before i: Index, climbing dir: Climb) -> Index? {
		var discard: ExtentController?
		return index(before: i, climbing: dir, bottom: &discard)
	}
}

public func commonPrefix<S>(_ s1: S, _ s2: S)
    -> [S.Element] where S : Sequence, S.Element : Equatable {
	return zip(s1, s2).prefix { (e1, e2) in e1 == e2 }.map { (e, _) in e }
}

extension Rope {
	public func tightenedSelection(_ selection: Range<Index>)
	    -> (range: Range<Index>,
	        leftControllers: [ExtentController],
	        rightControllers: [ExtentController])? {
		var (l, r) = (selection.lowerBound, selection.upperBound)
		var lo, ro: [ExtentController]
		/* TBD move extentsEnclosing() calls out of loop, use
		 * index(after/before: ..., climbing: .in, bottom: ...) to
		 * get the next deeper extent at each step
		 */
		while true {
			switch (try? extentsEnclosing(l),
                                try? extentsEnclosing(r)) {
			case (let _lo?, let _ro?):
				lo = _lo
				ro = _ro
			default:
				return nil
			}
			if l == r {
				assert(lo == ro)
				return (l..<r, lo, lo)
			}
			if lo.count < ro.count,
			   let next = index(after: l, climbing: .in) {
				l = next
			} else if lo.count > ro.count,
			          let next = index(before: r, climbing: .in) {
				r = next
			} else if lo.count == ro.count,
			          let lnext = index(after: l, climbing: .in),
			    let rnext = index(before: r, climbing: .in) {
				l = lnext
				r = rnext
			} else {
				return (l..<r, lo, ro)
			}
		}
	}
	public func rightLoosenedSelection(_ s: Range<Index>,
	    limit: ExtentController?) -> Range<Index>? {
		let l = s.lowerBound
		var r = s.upperBound
		var bottom: ExtentController?
		guard let extents = try? extentsEnclosing(r) else {
			return nil
		}
		if extents.last == limit {
			return l..<r
		}
		while let next = index(after: r, climbing: .out,
		    bottom: &bottom) {
			r = next
			if bottom == limit {
				break
			}
		}
		return l..<r
	}
	public func leftLoosenedSelection(_ s: Range<Index>,
	    limit: ExtentController?) -> Range<Index>? {
		var l = s.lowerBound
		let r = s.upperBound
		var bottom: ExtentController?
		guard let extents = try? extentsEnclosing(l) else {
			return nil
		}
		if extents.last == limit {
			return l..<r
		}
		while let next = index(before: l, climbing: .out,
		    bottom: &bottom) {
			l = next
			if bottom == limit {
				break
			}
		}
		return l..<r
	}
	public func directedSelection(_ s: Range<Index>)
	    -> (range: Range<Index>,
	        narrow: ExtentController?,
	        wide: ExtentController?)? {
		guard let (tight, lo, ro) = tightenedSelection(s) else {
			return nil
		}
		/* Loosen `tight` inside the innermost controller and
		 * return the loosened range.
		 */
		let common = commonPrefix(lo, ro)
		let looser = leftLoosenedSelection(tight, limit: common.last)!
		let loosest = rightLoosenedSelection(looser,
		    limit: common.last)!
		return (range: loosest, narrow: common.last, wide: common.first)
	}
}

extension Rope {
	public func firstIndex(inExtent target: Label) -> Index? {
		let label = Label()
		guard let newtop = top.insertingFirstIndex(label,
		    inExtent: target) else {
			return nil
		}
		top = newtop
		return .interior(of: self, label: label)
	}
	public func lastIndex(inExtent target: Label) -> Index? {
		let label = Label()
		guard let newtop = top.insertingLastIndex(label,
		    inExtent: target) else {
			return nil
		}
		top = newtop
		return .interior(of: self, label: label)
	}
	public func index(afterExtent target: Label) -> Index? {
		let label = Label()
		guard let newtop = top.insertingIndex(label,
		    afterExtent: target) else {
			return nil
		}
		top = newtop
		return .interior(of: self, label: label)
	}
	public func index(beforeExtent target: Label) -> Index? {
		let label = Label()
		guard let newtop = top.insertingIndex(label,
		    beforeExtent: target) else {
			return nil
		}
		top = newtop
		return .interior(of: self, label: label)
	}
}

extension Rope {
	public func index(_ i: Index, offsetBy _times: Int) -> Index {
		var times = _times
		var result: Index = i
		while times < 0 {
			result = index(before: result)
			times = times + 1
		}
		while times > 0 {
			result = index(after: result)
			times = times - 1
		}
		return result
	}
}

/*
extension Rope : ExpressibleByStringLiteral,
    ExpressibleByExtendedGraphemeClusterLiteral where
    Rope.Content : ExpressibleByStringLiteral {
	public init(stringLiteral s: S) {
		top = Rope.Node(content: s)
	}
}
*/
extension RopeOffsetDelegate {
	func indicateAttributeChanges<T>(
	    on range: Range<Rope<T>.Offset>,
	    undoList: ChangeList<Rope<T>>? = nil) where Rope<T>.Offset == Offset {
		undoList?.record { (rope, undoList) in
			self.indicateAttributeChanges(
			    on: range, undoList: undoList)
			return rope
		}
		ropeAttributesDidChange(on: range)
	}
	func indicateChanges<T>(
	    new: (lower: Rope<T>.Offset, upper: Rope<T>.Offset),
	    old: (lower: Rope<T>.Offset, upper: Rope<T>.Offset),
	    undoList: ChangeList<Rope<T>>) where Rope<T>.Offset == Offset {
		undoList.record { (rope, undoList) in
			self.indicateChanges(
			    new: old,
			    old: new,
			    undoList: undoList)
			return rope
		}
		let length: (new: Int, old: Int) =
		    ((new.upper - new.lower).unitOffset,
		     (old.upper - old.lower).unitOffset)
		let range: Range<Rope<T>.Offset> = old.lower..<old.upper
		ropeDidChange(on: range,
		    changeInLength: length.new - length.old)
	}
}
