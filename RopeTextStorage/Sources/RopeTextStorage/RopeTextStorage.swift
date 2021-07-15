//
// Copyright (c) 2020 David Young.  All rights reserved.
//
import AppKit
import Rope

public class RopeTextStorage: NSTextStorage {
	public typealias Backing = Rope<ContiguousArray<UTF16.CodeUnit>>
	typealias Offset = Backing.Node.Offset
	let backing: Backing
	let _string: RopeString
	public init(with backing: Backing) {
		self.backing = backing
		_string = RopeString(with: backing)
		super.init()
	}
	public override init() {
		self.backing = Rope()
		_string = RopeString(with: backing)
		super.init()
	}
	required init?(coder: NSCoder) {
		fatalError("init(coder:) has not been implemented")
	}
	required init?(pasteboardPropertyList propertyList: Any,
	   ofType type: NSPasteboard.PasteboardType) {
		fatalError("init(pasteboardPropertyList:ofType:) " +
		    "has not been implemented")
	}
	override public var string: String {
		return _string as String
	}
	override public func attributes(at location: Int,
	    effectiveRange _range: NSRangePointer?)
	    -> [NSAttributedString.Key : Any] {
		let i = Offset(of: location)
		let (attrs, r) = backing.attributes(at: i)
		if let range = _range {
			range.initialize(to: r.nsRange)
		}
/*
		Swift.print("\(attrs.count) attributes on " +
		    "[\(r.lowerBound.unitOffset), " +
		    "\(r.upperBound.unitOffset)], \(attrs)")
*/
		return attrs;
	}
	private func performEditing<T>(_ f: () -> T) -> T {
		beginEditing()
		defer {
			endEditing()
		}
		return f()
	}
	public func withRopeBacking<T>(_ f: (Backing) -> T) -> T {
		performEditing { f(backing) }
	}
	override public func replaceCharacters(in range: NSRange,
	    with str: String) {
		performEditing() {
			let undoList = Backing.Node.UndoList()
			do {
				try backing.replace(Offset.unitRange(range),
				    with: ContiguousArray<UTF16.CodeUnit>(
				              str.utf16[...]),
				    undoList: undoList)
			} catch {
				fatalError("invalid range")
			}
			let actions =
			    NSTextStorageEditActions.editedCharacters.union(
			    .editedAttributes)
			edited(actions, range: range,
			    changeInLength:
			        (str as NSString).length - range.length)
		}
	}
	override public func setAttributes(
	    _ optAttrs: [NSAttributedString.Key : Any]?, range r: NSRange) {
		performEditing() {
			let range = Offset.unitRange(r)
			if let attrs = optAttrs {
				backing.setAttributes(attrs, range: range)
			} else {
				backing.clearAttributes(on: range)
			}
			edited(.editedAttributes, range: r, changeInLength: 0)
		}
	}
}
