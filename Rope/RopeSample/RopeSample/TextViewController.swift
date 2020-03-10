//
//  ViewController.swift
//  RopeSample
//
//  Created by David Young on 24 Feb 20.
//  Copyright © 2020 White Coral Islands. All rights reserved.
//

import Cocoa
import Rope

/*
public class TextViewController : NSViewController {
	public let storage = RopeTextStorage(content: "smack!")
 }

 */
class ViewController: NSViewController {
	@IBOutlet var textView: NSTextView!
	let storage = RopeTextStorage()
	// let storage = RopeTextStorage(content: "smack!")
	override func viewDidLoad() {
		super.viewDidLoad()

		// Do any additional setup after loading the view.
	}
	override public func viewWillAppear() {
		storage.replaceCharacters(in: NSMakeRange(0, 0), with: "smack!")
		textView.layoutManager!.replaceTextStorage(storage)
		Swift.print("storage replaced!")
		if textView.textStorage is RopeTextStorage {
			Swift.print("properly, even!")
		}
		textView.isRulerVisible = true											
		textView.smartInsertDeleteEnabled = false								
		textView.isContinuousSpellCheckingEnabled = false						
		super.viewWillAppear()
	}
	override var representedObject: Any? {
		didSet {
		// Update the view, if already loaded.
		}
	}


}

