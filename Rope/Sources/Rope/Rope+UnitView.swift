//
// Copyright (c) 2020, 2021 David Young.  All rights reserved.
//
extension Rope {
        public struct UnitView : RopeAxisView {
		public typealias Cx = Rope.Content

		public let rope: Rope
		public let axis: KeyPath<Rope.Node.Dimensions, Int>
		public let properties: Rope<Cx>.BoundaryProperties

		init(rope r: Rope, properties p: Rope.BoundaryProperties) {
			axis = \.units
			properties = p
			rope = r
		}
		public subscript(_ r: Range<Int>) -> Rope.Content {
			get {
				let ir = Range(r, within: self)
				return rope.node[ir]
			}
		}
		public func extract(_ range: Range<Int>,
		    filling buffer: inout UnsafeMutablePointer<Unit>) {
			return rope.node.extractUnits(from: range.lowerBound,
			    upTo: range.upperBound, filling: &buffer)
		}
	}
	public var units: UnitView {
                return UnitView(rope: self, properties: boundaryProperties)
	}
}
