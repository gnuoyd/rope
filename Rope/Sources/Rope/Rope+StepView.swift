extension Rope {
        public struct StepView {
		let rope: Rope
		init(rope r: Rope) {
			rope = r
		}
	}
	public var steps: StepView {
                return StepView(rope: self)
	}
}

