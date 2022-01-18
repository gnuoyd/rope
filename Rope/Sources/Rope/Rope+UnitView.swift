//
// Copyright (c) 2020, 2021 David Young.  All rights reserved.
//
extension Rope {
        public struct UnitView : RopeAxisView {
		public typealias Cx = Rope.Content

		public let rope: Rope
		public let axis: KeyPath<Rope.Node.Dimensions, Int>
		public let properties: Rope<Cx>.BoundaryProperties

		init(of r: Rope, axis a: KeyPath<Rope.Node.Dimensions, Int>,
		     properties p: Rope.BoundaryProperties) {
			rope = r
			axis = a
			properties = p
		}
		public func extract(_ range: Range<Int>,
		    filling buffer: inout UnsafeMutablePointer<Unit>) {
			return rope.node.extract(from: range.lowerBound,
			    upTo: range.upperBound, on: axis, filling: &buffer,
			    defaults: properties.units)
		}
	}
	public var units: UnitView {
                return UnitView(of: self, axis: \.units,
		                properties: boundaryProperties)
	}
}
