extension Rope {
        public struct StepView {
		let rope: Rope
		let properties: Rope.StepProperties
		init(rope r: Rope, properties p: Rope.StepProperties) {
			rope = r
			properties = p
		}
	}
	public var steps: StepView? {
		guard let p = stepProperties else {
			return nil
		}
                return StepView(rope: self, properties: p)
	}
}

