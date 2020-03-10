//: A Cocoa based Playground to present user interface

import AppKit
import PlaygroundSupport
import Rope
/*
let nibFile = NSNib.Name("MyView")
var topLevelObjects : NSArray?

Bundle.main.loadNibNamed(nibFile, owner:nil, topLevelObjects: &topLevelObjects)
let views = (topLevelObjects as! Array<Any>).filter { $0 is NSTextView }


// Present the view in Playground
PlaygroundPage.current.liveView = views[0] as! NSView
 */

public class TextViewController : NSViewController {
    public let storage = RopeTextStorage(content: "smack!")
    let textview: NSTextView = NSTextView(frame: NSRect(x: 0, y: 0, width: 100, height: 100))
    override public func loadView() {
        vc.view = textview
    }
    override public func viewWillAppear() {
        // storage.replaceCharacters(in: NSMakeRange(0, 0), with: "smack!")
        textview.layoutManager!.replaceTextStorage(storage)
        Swift.print("storage replaced!")
        if textview.textStorage is RopeTextStorage {
            Swift.print("properly, even!")
        }
        super.viewWillAppear()
    }
}

// let ctlr = TextViewController()
// let view = ctlr.view
// let storage = RopeTextStorage()
// view.text = "Boo!"
// view.backgroundColor = .cyan
// PlaygroundPage.current.liveView = view
// view.needsDisplay = true

let vc = TextViewController()
PlaygroundPage.current.liveView = vc


