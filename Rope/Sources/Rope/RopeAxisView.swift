//
// Copyright (c) 2019, 2020, 2021 David Young.  All rights reserved.
//
extension Rope {
	public struct RopeAxisView {
		public let rope: Rope
		public let axis: KeyPath<Rope.Node.Dimensions, Int>
		public let properties: Rope.BoundaryProperties

		init(of r: Rope, axis a: KeyPath<Rope.Node.Dimensions, Int>,
		     properties p: Rope.BoundaryProperties) {
			rope = r
			axis = a
			properties = p
		}
	}
	public var steps: RopeAxisView {
                return RopeAxisView(of: self, axis: \.steps,
		    properties: boundaryProperties)
	}
	public var units: RopeAxisView {
                return RopeAxisView(of: self, axis: \.units,
		    properties: boundaryProperties)
	}
}

extension Rope.RopeAxisView {
	public subscript(_ i: Int) -> Rope.Content.Unit {
		get {
			var result: Rope.Content.Unit = Rope.Content.Unit.default
			withUnsafeMutablePointer(to: &result) { _buf in
				var buf = _buf
				extract(i..<(i+1), filling: &buf)
			}
			return result
		}
	}
	public subscript(_ r: Range<Int>) -> Rope.Content {
		get {
			let ir = Range(r, within: self)
			return rope.node[ir]
		}
	}
	/* If/when Content boundaries replace Unit boundaries,
	 * the length may be rope.node.dimensions.units +
	 * rope.node.dimensions.boundaries / 2 *
	 * (properties.content.open.count +
	 *  properties.content.close.count). If the boundary
	 * text is not constant, however, then Dimensions will
	 * need to expand to track the cumulative boundary content
	 * length in Units.
	 */
	public var length: Int {
		return rope.node.dimensions[keyPath: axis]
	}
	public func attributes(at i: Int) -> (Attributes, Range<Int>) {
		return rope.node.attributes(at: i, on: axis,
		    defaults: properties.attributes)
	}
	public func extract(_ range: Range<Int>,
	    filling buffer: inout UnsafeMutablePointer<Rope.Content.Unit>) {
		return rope.node.extract(from: range.lowerBound,
		    upTo: range.upperBound, on: axis, filling: &buffer,
		    defaults: properties.units)
	}
}
