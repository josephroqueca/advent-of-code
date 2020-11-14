import Foundation

protocol StringInitable {
	init?(_ string: String)
}
extension Int: StringInitable {}
extension String: StringInitable {}

func loadInput<T: StringInitable>(as type: T.Type, forTesting: Bool = true) throws -> T? {
	let inputFileName = forTesting ? "test" : "input"
	let fileUrl = URL(
		fileURLWithPath: "../\(inputFileName).txt",
		relativeTo: URL(fileURLWithPath: FileManager.default.currentDirectoryPath)
	)
	let fileContents = try String(contentsOf: fileUrl)
	return T.init(fileContents.trimmingCharacters(in: .whitespacesAndNewlines))
}

func loadInputByLine<T: StringInitable>(as type: T.Type, forTesting: Bool = true) throws -> [T] {
	guard let input = try loadInput(as: String.self, forTesting: forTesting) else { return [] }
	return input.split(separator: "\n").compactMap { T.init(String($0)) }
}

// Solution

var totalFuel = 0
var modules = try! loadInputByLine(as: Int.self)
var i = 0
while i < modules.count {
	let fuelNeeded = modules[i] / 3 - 2
	if fuelNeeded > 0 {
		totalFuel += fuelNeeded
		modules.append(fuelNeeded)
	}
	i += 1
}

print("The total fuel necessary is \(totalFuel)")