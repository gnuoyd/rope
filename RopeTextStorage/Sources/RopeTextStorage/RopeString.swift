//
// Copyright (c) 2020, 2021 David Young.  All rights reserved.
//
import AppKit
import Rope

class RopeString : NSString {
	typealias Backing = Rope<ContiguousArray<UTF16.CodeUnit>>
	let backing: Backing
	let units: Backing.UnitView
	init(with backing: Backing) {
		self.backing = backing
		self.units = backing.units
		super.init()
	}
	override init() {
		self.backing = Backing()
		self.units = backing.units
		super.init()
	}
	required init?(coder aDecoder: NSCoder) {
		fatalError()
	}
	required init?(pasteboardPropertyList propertyList: Any,
	    ofType type: NSPasteboard.PasteboardType) {
		fatalError()
	}
	public override var length: Int {
		return units.length
	}
	override func character(at i: Int) -> unichar {
		let c = units[i]
		// Swift.print("character(at: \(i)) -> \(c)")
		return c
	}
	override func getCharacters(_ buffer_in: UnsafeMutablePointer<unichar>,
	    range: NSRange) {
		var buffer = buffer_in
		units.extract(range.range, filling: &buffer)
	}
	override func copy(with zone: NSZone? = nil) -> Any {
		return self
	}
}
