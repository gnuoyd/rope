//
// Copyright (c) 2020, 2021 David Young.  All rights reserved.
//
extension Rope {
        public struct StepView : RopeAxisView {
		public typealias Cx = Rope.Content
		public let rope: Rope
		public let properties: Rope.BoundaryProperties
		public let axis: KeyPath<Rope.Node.Dimensions, Int>
		init(rope r: Rope, properties p: Rope.BoundaryProperties) {
			axis = \.steps
			rope = r
			properties = p
		}
		public func extract(_ range: Range<Int>,
		    filling buffer: inout UnsafeMutablePointer<Cx.Unit>) {
			return rope.node.extract(from: range.lowerBound,
			    upTo: range.upperBound, on: axis, filling: &buffer,
			    defaults: properties.units)
		}
	}
	public var steps: StepView {
                return StepView(rope: self, properties: boundaryProperties)
	}
}
