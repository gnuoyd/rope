import Cocoa
import Rope

typealias RSS = Rope<Substring>

let r: RSS = Rope(content: "abcdefghijkl")

r.index(after: r.startIndex)

r.indices.count
