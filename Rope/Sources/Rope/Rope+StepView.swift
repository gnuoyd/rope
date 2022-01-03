extension Rope {
        public struct StepView {
		let rope: Rope
		let properties: Rope.BoundaryProperties
		init(rope r: Rope, properties p: Rope.BoundaryProperties) {
			rope = r
			properties = p
		}
	}
	public var steps: StepView? {
		guard let p = boundaryProperties else {
			return nil
		}
                return StepView(rope: self, properties: p)
	}
}

