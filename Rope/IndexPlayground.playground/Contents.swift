import Cocoa
import Rope

let r: Rope<Substring> = Rope(content: "abcdefghijkl")

r.index(after: r.startIndex)
