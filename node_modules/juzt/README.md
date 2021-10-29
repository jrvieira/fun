# juzt
very simple js unit testing


## Installation

```shell
$ npm install juzt
```


## Usage

write descriptive tests that evaluate to true

```javascript
/*
juzt tests take only two arguments:
a description string and a boolean expression
*/

const juzt = require('juzt')

// these tests will pass:

let a = 1
let b = 2
juzt.test('sum of a and b is 3', a + b === 3)
juzt.test('this test passes', true)
juzt.test('', true)
juzt.test('boolean expression', !!a)
juzt.test('boolean expression', !!'test')

let e = null
try { throwanerror() } catch (err) { e = err }
// juzt tests return true on pass and false on fail
if ( juzt.test('e should be an error', e instanceof Error) ) {
	juzt.test('with the correct type and message', e.name === 'ReferenceError' && e.message === 'throwanerror is not defined')
}

// these tests will fail:

b = 1
juzt.test('is the sum of a and b 3?', a + b === 3)
juzt.test('evaluates to false', false)
juzt.test('evaluates to false', !b)
juzt.test('evaluates to false', !'test')

let ee = null
try { a + b } catch (err) { ee = err }
juzt.test('is this an error?', ee instanceof Error)

// these will throw an error:

juzt.test('second argument must be boolean', a)
juzt.test('second argument must be boolean', 1)
juzt.test('second argument must be boolean', 0)
juzt.test(3, true)
juzt.test(true)
juzt.test()

// end testing

juzt.over()
```

run
```shell
node yourtest.js
```


###### Licensed under MIT
