//
//  RopeTests.swift
//  RopeTests
//
//  Created by David Young on 19 Aug 19.
//  Copyright © 2019 White Coral Islands. All rights reserved.
//

import XCTest
@testable import Rope

class RopeTests: XCTestCase {

    override func setUp() {
        // Put setup code here. This method is called before the invocation of each test method in the class.
    }

    override func tearDown() {
        // Put teardown code here. This method is called after the invocation of each test method in the class.
    }

    func testFibonacciByIndex() {
        XCTAssert([0, 1, 2, 3, 4, 5].map({ i in fibonacci(index: i)}) == [0, 1, 1, 2, 3, 5])
    }
    
    func testFibonacciGenerator() {
        // Produce the Fibonacci sequence, 0th through 5th element.
        let arr = Fibonacci(through: 5).reduce([], { (arr: [UInt], elt: UInt) -> [UInt] in arr + [elt]})

        XCTAssert(arr == [0, 1, 1, 2, 3, 5])
    }
    func testReleaseHandle() {
        var h = Handle()
        let w = Weak(h)
        h = Handle()
        XCTAssert(w.get() == nil)
    }
    
    func testHoldHandle() {
        let h = Handle()
        let w = Weak(h)
        XCTAssert(w.get() == h)
    }
    
    func testStepAndHoldIndex() {
        let first = Node<Substring>(text: "abc")
        let handle = Handle()
        guard case .step(let second) = first.afterStepInserting(index: handle) else {
            XCTFail("afterStepInserting failed")
            return
        }
        XCTAssert(second.leaves.map({ (x: Node<Substring>) -> Bool in if case .index(let w) = x { return w.get() == handle } else {return false } })[1])
    }
    
    func testStepAndReleaseIndex() {
        let first = Node<Substring>(text: "abc")
        var handle = Handle()
        guard case .step(let second) = first.afterStepInserting(index: handle) else {
            XCTFail("afterStepInserting failed")
            return
        }
        handle = Handle()
        XCTAssert(!second.leaves.map({ (x: Node<Substring>) -> Bool in if case .index(let w) = x { return w.get() == handle } else {return false } })[1])
    }

    static func isIndex(_ n: Node<Substring>) -> Bool {
        if case .index(_) = n {
            return true
        }
        return false
    }

    static func isNilIndex(_ n: Node<Substring>) -> Bool {
        if case .index(let w) = n {
            return w.get() == nil
        }
        return false
    }
    
    func testCleanedHoldingIndices() {
        let emptyRope = Rope<Substring>(text: "abcdefghijkl")
        var indices: [Rope<Substring>.Index]? = []
        
        for i in emptyRope.indices {
            indices?.append(i)
        }
        print(emptyRope.node)
        XCTAssert(emptyRope.node.cleaned()?.leaves.filter(RopeTests.isIndex).count == 11)
    }

    func testCleanedReleasingIndices() {
        let emptyRope = Rope<Substring>(text: "abcdefghijkl")
        var indices: [Rope<Substring>.Index]? = []

        for i in emptyRope.indices {
            indices?.append(i)
        }
        print(emptyRope.node)
        indices = nil
        XCTAssert(emptyRope.node.cleaned()?.leaves.filter(RopeTests.isIndex).count == 0)
    }
    
    func testReleasingIndices() {
        let emptyRope = Rope<Substring>(text: "abcdefghijkl")
        var indices: [Rope<Substring>.Index]? = []
        
        for i in emptyRope.indices {
            indices?.append(i)
        }

        indices = nil
        print(emptyRope.node.leaves)
        print(emptyRope.node.leaves.filter(RopeTests.isNilIndex))

        XCTAssert(emptyRope.node.leaves.filter(RopeTests.isNilIndex).count == 11)
    }

    func testPerformanceExample() {
        // This is an example of a performance test case.
        self.measure {
            // Put the code you want to measure the time of here.
        }
    }

}
