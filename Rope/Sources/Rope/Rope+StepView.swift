//
// Copyright (c) 2020, 2021 David Young.  All rights reserved.
//
extension Rope {
        public struct StepView : RopeUnitView {
		public typealias Cx = Rope.Content
		let rope: Rope
		let properties: Rope.BoundaryProperties
		init(rope r: Rope, properties p: Rope.BoundaryProperties) {
			rope = r
			properties = p
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
			return rope.node.dimensions.steps
		}
		public func attributes(at i: Int) -> (Attributes, Range<Int>) {
			return rope.node.attributes(at: i, on: \.steps,
			    defaults: properties.attributes)
		}
		public subscript(_ r: Range<Int>) -> Rope.Content {
			get {
				let ir = Range(r, within: self)
				return rope.node[ir]
			}
		}
		public func extract(_ range: Range<Int>,
		    filling buffer: inout UnsafeMutablePointer<Cx.Unit>) {
			return rope.node.extractSteps(from: range.lowerBound,
			    upTo: range.upperBound, filling: &buffer,
			    units: properties.units)
		}
	}
	public var steps: StepView? {
		guard let p = boundaryProperties else {
			return nil
		}
                return StepView(rope: self, properties: p)
	}
}

