public struct Weak<O : AnyObject> {
	public typealias Reference = O
	private var _f: () -> O?
	public var get: () -> O? { return _f }
	init(_ o: O) {
		_f = { [weak o] in o }
	}
}

extension Weak : CustomDebugStringConvertible
    where Reference : CustomDebugStringConvertible {
        public var debugDescription: String {
		guard let referent = get() else {
			return "Weak(nil)"
		}
		return "Weak(\(referent.debugDescription))"
	}
}
