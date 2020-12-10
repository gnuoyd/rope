extension Substring : Content {
	public typealias Element = Character
	public static var empty: Substring { return "" }
	public var length: Int {
		return distance(from: startIndex, to: endIndex)
	}
}
