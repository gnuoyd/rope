//
// Copyright (c) 2019, 2020 David Young.  All rights reserved.
//
public class Handle : Hashable {
	public typealias Id = UInt64
	static var nextId: Id = 0
	private var _id: Id
	public var id: Id { return _id }
	public init() {
		_id = Handle.nextId
		Handle.nextId = Handle.nextId + 1
	}
	public static func ==(_ l: Handle, _ r: Handle) -> Bool {
		return l._id == r._id
	}
	public func hash(into hasher: inout Hasher) {
		hasher.combine(_id)
	}
}

extension Handle : CustomDebugStringConvertible {
	public var debugDescription: String {
		return "Handle(id: \(id))"
	}
}
