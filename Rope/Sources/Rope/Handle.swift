//
// Copyright (c) 2019, 2020 David Young.  All rights reserved.
//

public class Handle : Hashable {
	public enum Id : Hashable {
	typealias Number = UInt64
	case cursor(UInt64)
	case extent(UInt64)
	case index(UInt64)
	}
	static var nextNumber: Id.Number = 0
	var _id: Id.Number
	public var id: Id { return .index(_id) }
	public init() {
		_id = Handle.nextNumber
		Handle.nextNumber = Handle.nextNumber + 1
	}
	public static func ==(_ l: Handle, _ r: Handle) -> Bool {
		return l._id == r._id
	}
	public func hash(into hasher: inout Hasher) {
		hasher.combine(_id)
	}
}

extension Handle.Id : CustomDebugStringConvertible {
	public var debugDescription: String {
		switch self {
		case .cursor(let n):
			return ".cursor(\(n))"
		case .extent(let n):
			return ".extent(\(n))"
		case .index(let n):
			return ".index(\(n))"
		}
	}
}

extension Handle : CustomDebugStringConvertible {
	public var debugDescription: String {
		return "Handle(id: \(id.debugDescription))"
	}
}
