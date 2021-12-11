//
// Copyright (c) 2019, 2020, 2021 David Young.  All rights reserved.
//
import Foundation

enum RopeNoSuchElement : Error {
case onInterior
}

extension Range {
	public init<C : Content>(_ r: NSRange,
	                   in rope: Rope<C>) where Bound == Rope<C>.Index {
		self.init(r.range, in: rope)
	}
	public init<C : Content>(_ r: Range<Int>, in rope: Rope<C>)
	    where Bound == Rope<C>.Index {
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
		public init(jots: Int = 0, unitOffset: Offset = 0) {
			self.jots = jots
			self.unitOffset = unitOffset
		}
		public var halfPerimeter: Int {
			return jots + unitOffset
		}
	}
}

extension Rope.Node.Dimensions {
	public static func +(_ l: Self, _ r: Self) -> Self {
		return Self(jots: l.jots + r.jots,
		            unitOffset: l.unitOffset + r.unitOffset)
	}
}

extension Rope.Node {
	public typealias Unit = C.Unit
}

/* Use cases:
 *
 * Get/set/add/remove attributes on characters.
 *
 * Enclose a range in a zone.  The range must be
 * well-formed: must begin and end inside the same zone.
 *
 * "Step" an index left or right by a character.
 *
 * "Scoot" an index left or right by a zone boundary.
 *
 * "Scoot" an index left or right by a label.
 */
public class Rope<C : Content> {
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
	public class ZoneController : Label {
		public override var id: Id { return .zone(_id) }
		func subrope(of content: Rope.Node, from: Rope.Node.Offset,
		    tightly: Bool, depth: Int = 0) -> Rope.Node {
			let subcontent = content.subrope(from: from,
			    depth: depth)
			return .zone(self, subcontent)
		}
		func subrope(of content: Rope.Node,
		    upTo boundary: Rope.Node.Offset, tightly: Bool,
		    depth: Int = 0) -> Rope.Node {
			let subcontent = content.subrope(upTo: boundary,
			    depth: depth)
			return .zone(self, subcontent)
		}
		func subrope(of content: Rope.Node, after boundary: Rope.Index,
		    depth: Int = 0) -> Rope.Node? {
			guard let subcontent = content.subrope(after: boundary,
			    depth: depth) else {
				return nil
			}
			return .zone(self, subcontent)
		}
		func subrope(of content: Rope.Node, upTo boundary: Rope.Index,
		    depth: Int = 0) -> Rope.Node? {
			guard let subcontent = content.subrope(upTo: boundary,
			    depth: depth) else {
				return nil
			}
			return .zone(self, subcontent)
		}
		func setController(_ ctlr: ZoneController,
		    after lowerBound: Label, upTo upperBound: Label,
		    in content: Rope.Node,
		    undoList: ChangeList<Rope.Node>?) throws -> Rope.Node {
			return .zone(self, try content.setController(ctlr,
			    after: lowerBound, upTo: upperBound,
			    undoList: undoList))
		}
		func replacing(after lowerBound: Label, upTo upperBound: Label,
		    in content: Rope.Node, with replacement: Rope.Node,
		    undoList: ChangeList<Rope.Node>?) throws -> Rope.Node {
			return .zone(self, try content.replacing(
			    after: lowerBound, upTo: upperBound,
			    with: replacement, undoList: undoList))
		}
		func transformingAttributes(after lowerBound: Label,
		    upTo upperBound: Label, in content: Rope.Node,
		    with fn: (Attributes) -> Attributes) throws -> Rope.Node {
			let xformed = try content.transformingAttributes(
			    after: lowerBound, upTo: upperBound, with: fn)
			return .zone(self, xformed)
		}
	}

	/* A Node directly encodes the presence of indices because it is
	 * possible for an index to move up and down the hierarchy of text
	 * zones without changing between-character positions.  An index
	 * can appear at the position left of the first character or right of
	 * the last character in a Node.  An index can also appear in a
	 * Node that contains no characters.
	 */
	public indirect enum Node {
	public typealias Content = C
	public typealias Element = C.Element
	public typealias Offset = Int
	case index(Weak<Label>)
	case zone(ZoneController, Node)
	case concat(Node, Offset, UInt, LabelSet, Node, Dimensions)
	case leaf(Attributes, C)
	case empty
	}
	typealias OffsetPair = (lower: Offset, upper: Offset)

	private var _delegate: RopeOffsetDelegate? = nil
	public var delegate: RopeOffsetDelegate {
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
		let oldOffsets: OffsetPair =
		    try (top.offset(of: r.lowerBound.label),
		         top.offset(of: r.upperBound.label))
		top = try top.replacing(
		    after: r.lowerBound, upTo: r.upperBound,
		    with: replacement, undoList: changes)
		let newOffsets: (lower: Offset, upper: Offset) =
		    try (top.offset(of: r.lowerBound.label),
		         top.offset(of: r.upperBound.label))
		undoList.record { (rope, undoList) in
			try rope.applyChanges(changes, undoList: undoList)
			return rope
		}
		delegate.indicateChanges(new: newOffsets, old: oldOffsets,
		    undoList: undoList)
	}
	public func setController(_ ctlr: ZoneController,
	    on r: Range<Index>, undoList: ChangeList<Rope>) throws {
		let changes = ChangeList<Rope.Node>()
		let offsets: OffsetPair =
		    try (top.offset(of: r.lowerBound.label),
		         top.offset(of: r.upperBound.label))
		top = try top.setController(ctlr,
		    after: r.lowerBound.label, upTo: r.upperBound.label,
		    undoList: changes)
		undoList.record { (rope, undoList) in
			try rope.applyChanges(changes, undoList: undoList)
			return rope
		}
		delegate.indicateChanges(new: offsets, old: offsets,
		    undoList: undoList)
	}
	func applyChanges(_ changes: ChangeList<Rope.Node>,
	     undoList: ChangeList<Rope>) throws {
		let (newtop, reversals) = try changes.play(withTarget: top)
		top = newtop
		undoList.record { (rope, undoList) in
			try self.applyChanges(reversals, undoList: undoList)
			return rope
		}
	}
	public func attributes(at i: Offset) -> (Attributes, Range<Offset>) {
		return top.attributes(at: i)
	}
	public func setAttributes(_ attrs: Attributes, range r: Range<Int>) {
		let ir = Range(r, in: self)
		guard let newtop = try? top.settingAttributes(attrs, range: ir)
		    else {
			return
		}
		top = newtop
		delegate.indicateAttributeChanges(on: r,
		    undoList: nil as ChangeList<Rope>?)
	}
	public func clearAttributes(on r: Range<Int>) {
		let ir = Range(r, in: self)
		guard let newtop = try? top.clearingAttributes(on: ir) else {
			return
		}
		top = newtop
		delegate.indicateAttributeChanges(on: r,
		    undoList: nil as ChangeList<Rope>?)
	}
	public func offset(of index: Index) throws -> Offset {
		guard case .interior(_, let label) = index else {
			throw RopeNoSuchElement.onInterior
		}
		return try top.offset(of: label)
	}
}

extension Rope : RopeOffsetDelegate {
	public func ropeDidChange(on: Range<Int>, changeInLength: Int) {
		return
	}
	public func ropeAttributesDidChange(on: Range<Int>) {
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
		return labelSet.zoneCount == 0 && length == 0
	}
}

extension Rope {
        public struct IndexView : BidirectionalCollection {
		let rope: Rope
		public typealias Index = Rope.Index
		public var startIndex: Index {
			return rope.startIndex
		}
		public var endIndex: Index {
			return rope.endIndex
		}
		init(rope r: Rope) {
			rope = r
		}
		public subscript(i: Index) -> Index {
			return i
		}
		public func index(after i: Index) -> Index {
			return rope.index(after: i)
		}
		public func index(before i: Index) -> Index {
			return rope.index(before: i)
		}
	}
	public var indices: IndexView {
                return IndexView(rope: self)
	}
}

extension Rope {
        public struct NestedView : BidirectionalCollection {
		let view: IndexView
		public typealias Index = Rope.Index
		public var startIndex: Index {
			return view.startIndex
		}
		public var endIndex: Index {
			return view.endIndex
		}
		init(rope r: Rope) {
			view = IndexView(rope: r)
		}
		public subscript(i: Index) -> Element {
			get {
				return try! element(at: i)
			}
		}
		func element(at i: Index) throws -> Element {
			let result = view.rope.node.element(at: i.label)
			guard case .step(let node) = result else {
				throw RopeNoSuchElement.onInterior
			}
			return node
		}
		public func index(after i: Index) -> Index {
			return view.index(after: i)
		}
		public func index(before i: Index) -> Index {
			return view.index(before: i)
		}
	}
	public var nests: NestedView {
                return NestedView(rope: self)
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
		public subscript(_ r: Range<Offset>) -> Content {
			get {
				let ir = Range(r, in: rope)
				return rope.top[ir]
			}
		}
		public subscript<I>(_ r: Range<Offset>)
		    -> I where Content.SubSequence == I {
			get {
				return self[r][...]
			}
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
	func zonesEnclosing(_ i: Index) throws -> [ZoneController] {
		return try top.zonesEnclosing(i)
	}
	public func zonesClosing(at i: Index) throws -> [ZoneController] {
		return try top.zonesClosing(at: i)
	}
	public func zonesOpening(at i: Index) throws -> [ZoneController] {
		return try top.zonesOpening(at: i)
	}
}

extension Rope {
	public func index(after i: Index, climbing dir: Climb,
	    bottom: inout ZoneController?) -> Index? {
		do {
			if try !top.indices(follow: i.label) {
				return nil
			}
		} catch {
			return nil
		}
		let j = index(after: i)
		switch ((try? zonesEnclosing(i))?.count,
			dir,
		        (try? zonesEnclosing(j))?.count) {
		case (let ni?, .in, let nj?) where ni < nj:
			return j
		case (let ni?, .out, let nj?) where ni > nj:
			return j
		default:
			return nil
		}
	}
	public func index(before i: Index, climbing dir: Climb,
	    bottom: inout ZoneController?) -> Index? {
		do {
			if try !top.indices(precede: i.label) {
				return nil
			}
		} catch {
			return nil
		}
		let j = index(before: i)
		switch (try? zonesEnclosing(i),
			dir,
		        try? zonesEnclosing(j)) {
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
		var discard: ZoneController?
		return index(after: i, climbing: dir, bottom: &discard)
	}
	public func index(before i: Index, climbing dir: Climb) -> Index? {
		var discard: ZoneController?
		return index(before: i, climbing: dir, bottom: &discard)
	}
}

public func commonPrefix<S>(_ s1: S, _ s2: S)
    -> [S.Element] where S : Sequence, S.Element : Equatable {
	return zip(s1, s2).prefix { (e1, e2) in e1 == e2 }.map { (e, _) in e }
}

extension Rope {
	public func tightenedSelection(_ selection: Range<Index>)
	    throws -> (range: Range<Index>,
	               leftControllers: [ZoneController],
	               rightControllers: [ZoneController]) {
		var (l, r) = (selection.lowerBound, selection.upperBound)
		/* TBD move zonesEnclosing() calls out of loop, use
		 * index(after/before: ..., climbing: .in, bottom: ...) to
		 * get the next deeper zone at each step
		 */
		while true {
			let (lo, ro) = (try zonesEnclosing(l),
			                try zonesEnclosing(r))
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
	    limit: ZoneController?) throws -> Range<Index> {
		let l = s.lowerBound
		var r = s.upperBound
		var bottom: ZoneController?
		let zones = try zonesEnclosing(r)
		if zones.last == limit {
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
	    limit: ZoneController?) throws -> Range<Index> {
		var l = s.lowerBound
		let r = s.upperBound
		var bottom: ZoneController?
		let zones = try zonesEnclosing(l)
		if zones.last == limit {
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
	    throws -> (range: Range<Index>,
	               narrow: ZoneController?,
	               wide: ZoneController?) {
		let (tight, lo, ro) = try tightenedSelection(s)
		/* Loosen `tight` inside the innermost controller and
		 * return the loosened range.
		 */
		let common = commonPrefix(lo, ro)
		let looser =
		    try leftLoosenedSelection(tight, limit: common.last)
		let loosest =
		    try rightLoosenedSelection(looser, limit: common.last)
		return (range: loosest, narrow: common.last, wide: common.first)
	}
}

extension Rope {
	public func firstIndex(inZone target: Label) -> Index? {
		let label = Label()
		guard let newtop = top.insertingFirstIndex(label,
		    inZone: target) else {
			return nil
		}
		top = newtop
		return .interior(of: self, label: label)
	}
	public func lastIndex(inZone target: Label) -> Index? {
		let label = Label()
		guard let newtop = top.insertingLastIndex(label,
		    inZone: target) else {
			return nil
		}
		top = newtop
		return .interior(of: self, label: label)
	}
	public func index(afterZone target: Label) -> Index? {
		let label = Label()
		guard let newtop = top.insertingIndex(label,
		    afterZone: target) else {
			return nil
		}
		top = newtop
		return .interior(of: self, label: label)
	}
	public func index(beforeZone target: Label) -> Index? {
		let label = Label()
		guard let newtop = top.insertingIndex(label,
		    beforeZone: target) else {
			return nil
		}
		top = newtop
		return .interior(of: self, label: label)
	}
}

/*
extension Rope : ExpressibleByStringLiteral where
    Rope.Content : ExpressibleByStringLiteral {
	public convenience init<S>(stringLiteral s: S) where S : ExpressibleByStringLiteral {
		self = Rope(content: Rope.Content(s))
	}
}
*/

extension RopeOffsetDelegate {
	func indicateAttributeChanges<T>(on range: Range<Int>,
	    undoList: ChangeList<Rope<T>>? = nil) {
		undoList?.record { (rope, undoList) in
			self.indicateAttributeChanges(
			    on: range, undoList: undoList)
			return rope
		}
		ropeAttributesDidChange(on: range)
	}
	func indicateChanges<T>(
	    new: (lower: Int, upper: Int),
	    old: (lower: Int, upper: Int),
	    undoList: ChangeList<Rope<T>>) {
		undoList.record { (rope, undoList) in
			self.indicateChanges(
			    new: old,
			    old: new,
			    undoList: undoList)
			return rope
		}
		let length: (new: Int, old: Int) =
		    ((new.upper - new.lower), (old.upper - old.lower))
		ropeDidChange(on: old.lower..<old.upper,
		    changeInLength: length.new - length.old)
	}
}
