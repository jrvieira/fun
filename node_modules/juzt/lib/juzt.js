let pass = true
let testresults = []

module.exports.test = function (description, result) {
	// validate test arguments
	if (typeof description !== 'string') throw new TypeError('test description is not a string')
	if (typeof result !== 'boolean') throw new TypeError('test result is not a boolean expression')

	pass = pass && result
	testresults.push([result ? '\x1b[32m'+'v'+'\x1b[0m' : '\x1b[31m'+'x'+'\x1b[0m' , '\x1b[2m'+description+'\x1b[0m'])

	return result
}

let over = false

module.exports.over = function () {

	if (over) return false
	
	if (testresults.length === 0) {
		
		console.log('\x1b[2m'+'no tests to perform'+'\x1b[0m', '\n')
		
	} else {
		
		for (testresult of testresults) {	
			console.log(...testresult)	
		}
		console.log('\n', pass ? '\x1b[32m'+'all tests passed'+'\x1b[0m' : '\x1b[31m'+'test failed'+'\x1b[0m', '\n')
	
	}

	over = true

	return process.exitCode = pass ? 0 : 1
}
