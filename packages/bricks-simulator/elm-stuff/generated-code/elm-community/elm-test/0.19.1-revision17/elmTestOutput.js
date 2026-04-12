// Apply Node polyfills as necessary.
var window = {
  Date: Date,
  addEventListener: function () {},
  removeEventListener: function () {},
};

var location = {
  href: '',
  host: '',
  hostname: '',
  protocol: '',
  origin: '',
  port: '',
  pathname: '',
  search: '',
  hash: '',
  username: '',
  password: '',
};
var document = { body: {}, createTextNode: function () {}, location: location };

if (typeof FileList === 'undefined') {
  FileList = function () {};
}

if (typeof File === 'undefined') {
  File = function () {};
}

if (typeof XMLHttpRequest === 'undefined') {
  XMLHttpRequest = function () {
    return {
      addEventListener: function () {},
      open: function () {},
      send: function () {},
    };
  };

  var oldConsoleWarn = console.warn;
  console.warn = function () {
    if (
      arguments.length === 1 &&
      arguments[0].indexOf('Compiled in DEV mode') === 0
    )
      return;
    return oldConsoleWarn.apply(console, arguments);
  };
}

if (typeof FormData === 'undefined') {
  FormData = function () {
    this._data = [];
  };
  FormData.prototype.append = function () {
    this._data.push(Array.prototype.slice.call(arguments));
  };
}

var Elm = (function(module) {
var __elmTestSymbol = Symbol("elmTestSymbol");
(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.');


var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});



function _Test_runThunk(thunk)
{
  try {
    // Attempt to run the thunk as normal.
    return $elm$core$Result$Ok(thunk(_Utils_Tuple0));
  } catch (err) {
    // If it throws, return an error instead of crashing.
    return $elm$core$Result$Err(err.toString());
  }
}



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}



function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2($elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = $elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = $elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
}


/*
 * Copyright (c) 2010 Mozilla Corporation
 * Copyright (c) 2010 Vladimir Vukicevic
 * Copyright (c) 2013 John Mayer
 * Copyright (c) 2018 Andrey Kuzmin
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 */

// Vector2

var _MJS_v2 = F2(function(x, y) {
    return new Float64Array([x, y]);
});

var _MJS_v2getX = function(a) {
    return a[0];
};

var _MJS_v2getY = function(a) {
    return a[1];
};

var _MJS_v2setX = F2(function(x, a) {
    return new Float64Array([x, a[1]]);
});

var _MJS_v2setY = F2(function(y, a) {
    return new Float64Array([a[0], y]);
});

var _MJS_v2toRecord = function(a) {
    return { x: a[0], y: a[1] };
};

var _MJS_v2fromRecord = function(r) {
    return new Float64Array([r.x, r.y]);
};

var _MJS_v2add = F2(function(a, b) {
    var r = new Float64Array(2);
    r[0] = a[0] + b[0];
    r[1] = a[1] + b[1];
    return r;
});

var _MJS_v2sub = F2(function(a, b) {
    var r = new Float64Array(2);
    r[0] = a[0] - b[0];
    r[1] = a[1] - b[1];
    return r;
});

var _MJS_v2negate = function(a) {
    var r = new Float64Array(2);
    r[0] = -a[0];
    r[1] = -a[1];
    return r;
};

var _MJS_v2direction = F2(function(a, b) {
    var r = new Float64Array(2);
    r[0] = a[0] - b[0];
    r[1] = a[1] - b[1];
    var im = 1.0 / _MJS_v2lengthLocal(r);
    r[0] = r[0] * im;
    r[1] = r[1] * im;
    return r;
});

function _MJS_v2lengthLocal(a) {
    return Math.sqrt(a[0] * a[0] + a[1] * a[1]);
}
var _MJS_v2length = _MJS_v2lengthLocal;

var _MJS_v2lengthSquared = function(a) {
    return a[0] * a[0] + a[1] * a[1];
};

var _MJS_v2distance = F2(function(a, b) {
    var dx = a[0] - b[0];
    var dy = a[1] - b[1];
    return Math.sqrt(dx * dx + dy * dy);
});

var _MJS_v2distanceSquared = F2(function(a, b) {
    var dx = a[0] - b[0];
    var dy = a[1] - b[1];
    return dx * dx + dy * dy;
});

var _MJS_v2normalize = function(a) {
    var r = new Float64Array(2);
    var im = 1.0 / _MJS_v2lengthLocal(a);
    r[0] = a[0] * im;
    r[1] = a[1] * im;
    return r;
};

var _MJS_v2scale = F2(function(k, a) {
    var r = new Float64Array(2);
    r[0] = a[0] * k;
    r[1] = a[1] * k;
    return r;
});

var _MJS_v2dot = F2(function(a, b) {
    return a[0] * b[0] + a[1] * b[1];
});

// Vector3

var _MJS_v3temp1Local = new Float64Array(3);
var _MJS_v3temp2Local = new Float64Array(3);
var _MJS_v3temp3Local = new Float64Array(3);

var _MJS_v3 = F3(function(x, y, z) {
    return new Float64Array([x, y, z]);
});

var _MJS_v3getX = function(a) {
    return a[0];
};

var _MJS_v3getY = function(a) {
    return a[1];
};

var _MJS_v3getZ = function(a) {
    return a[2];
};

var _MJS_v3setX = F2(function(x, a) {
    return new Float64Array([x, a[1], a[2]]);
});

var _MJS_v3setY = F2(function(y, a) {
    return new Float64Array([a[0], y, a[2]]);
});

var _MJS_v3setZ = F2(function(z, a) {
    return new Float64Array([a[0], a[1], z]);
});

var _MJS_v3toRecord = function(a) {
    return { x: a[0], y: a[1], z: a[2] };
};

var _MJS_v3fromRecord = function(r) {
    return new Float64Array([r.x, r.y, r.z]);
};

var _MJS_v3add = F2(function(a, b) {
    var r = new Float64Array(3);
    r[0] = a[0] + b[0];
    r[1] = a[1] + b[1];
    r[2] = a[2] + b[2];
    return r;
});

function _MJS_v3subLocal(a, b, r) {
    if (r === undefined) {
        r = new Float64Array(3);
    }
    r[0] = a[0] - b[0];
    r[1] = a[1] - b[1];
    r[2] = a[2] - b[2];
    return r;
}
var _MJS_v3sub = F2(_MJS_v3subLocal);

var _MJS_v3negate = function(a) {
    var r = new Float64Array(3);
    r[0] = -a[0];
    r[1] = -a[1];
    r[2] = -a[2];
    return r;
};

function _MJS_v3directionLocal(a, b, r) {
    if (r === undefined) {
        r = new Float64Array(3);
    }
    return _MJS_v3normalizeLocal(_MJS_v3subLocal(a, b, r), r);
}
var _MJS_v3direction = F2(_MJS_v3directionLocal);

function _MJS_v3lengthLocal(a) {
    return Math.sqrt(a[0] * a[0] + a[1] * a[1] + a[2] * a[2]);
}
var _MJS_v3length = _MJS_v3lengthLocal;

var _MJS_v3lengthSquared = function(a) {
    return a[0] * a[0] + a[1] * a[1] + a[2] * a[2];
};

var _MJS_v3distance = F2(function(a, b) {
    var dx = a[0] - b[0];
    var dy = a[1] - b[1];
    var dz = a[2] - b[2];
    return Math.sqrt(dx * dx + dy * dy + dz * dz);
});

var _MJS_v3distanceSquared = F2(function(a, b) {
    var dx = a[0] - b[0];
    var dy = a[1] - b[1];
    var dz = a[2] - b[2];
    return dx * dx + dy * dy + dz * dz;
});

function _MJS_v3normalizeLocal(a, r) {
    if (r === undefined) {
        r = new Float64Array(3);
    }
    var im = 1.0 / _MJS_v3lengthLocal(a);
    r[0] = a[0] * im;
    r[1] = a[1] * im;
    r[2] = a[2] * im;
    return r;
}
var _MJS_v3normalize = _MJS_v3normalizeLocal;

var _MJS_v3scale = F2(function(k, a) {
    return new Float64Array([a[0] * k, a[1] * k, a[2] * k]);
});

var _MJS_v3dotLocal = function(a, b) {
    return a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
};
var _MJS_v3dot = F2(_MJS_v3dotLocal);

function _MJS_v3crossLocal(a, b, r) {
    if (r === undefined) {
        r = new Float64Array(3);
    }
    r[0] = a[1] * b[2] - a[2] * b[1];
    r[1] = a[2] * b[0] - a[0] * b[2];
    r[2] = a[0] * b[1] - a[1] * b[0];
    return r;
}
var _MJS_v3cross = F2(_MJS_v3crossLocal);

var _MJS_v3mul4x4 = F2(function(m, v) {
    var w;
    var tmp = _MJS_v3temp1Local;
    var r = new Float64Array(3);

    tmp[0] = m[3];
    tmp[1] = m[7];
    tmp[2] = m[11];
    w = _MJS_v3dotLocal(v, tmp) + m[15];
    tmp[0] = m[0];
    tmp[1] = m[4];
    tmp[2] = m[8];
    r[0] = (_MJS_v3dotLocal(v, tmp) + m[12]) / w;
    tmp[0] = m[1];
    tmp[1] = m[5];
    tmp[2] = m[9];
    r[1] = (_MJS_v3dotLocal(v, tmp) + m[13]) / w;
    tmp[0] = m[2];
    tmp[1] = m[6];
    tmp[2] = m[10];
    r[2] = (_MJS_v3dotLocal(v, tmp) + m[14]) / w;
    return r;
});

// Vector4

var _MJS_v4 = F4(function(x, y, z, w) {
    return new Float64Array([x, y, z, w]);
});

var _MJS_v4getX = function(a) {
    return a[0];
};

var _MJS_v4getY = function(a) {
    return a[1];
};

var _MJS_v4getZ = function(a) {
    return a[2];
};

var _MJS_v4getW = function(a) {
    return a[3];
};

var _MJS_v4setX = F2(function(x, a) {
    return new Float64Array([x, a[1], a[2], a[3]]);
});

var _MJS_v4setY = F2(function(y, a) {
    return new Float64Array([a[0], y, a[2], a[3]]);
});

var _MJS_v4setZ = F2(function(z, a) {
    return new Float64Array([a[0], a[1], z, a[3]]);
});

var _MJS_v4setW = F2(function(w, a) {
    return new Float64Array([a[0], a[1], a[2], w]);
});

var _MJS_v4toRecord = function(a) {
    return { x: a[0], y: a[1], z: a[2], w: a[3] };
};

var _MJS_v4fromRecord = function(r) {
    return new Float64Array([r.x, r.y, r.z, r.w]);
};

var _MJS_v4add = F2(function(a, b) {
    var r = new Float64Array(4);
    r[0] = a[0] + b[0];
    r[1] = a[1] + b[1];
    r[2] = a[2] + b[2];
    r[3] = a[3] + b[3];
    return r;
});

var _MJS_v4sub = F2(function(a, b) {
    var r = new Float64Array(4);
    r[0] = a[0] - b[0];
    r[1] = a[1] - b[1];
    r[2] = a[2] - b[2];
    r[3] = a[3] - b[3];
    return r;
});

var _MJS_v4negate = function(a) {
    var r = new Float64Array(4);
    r[0] = -a[0];
    r[1] = -a[1];
    r[2] = -a[2];
    r[3] = -a[3];
    return r;
};

var _MJS_v4direction = F2(function(a, b) {
    var r = new Float64Array(4);
    r[0] = a[0] - b[0];
    r[1] = a[1] - b[1];
    r[2] = a[2] - b[2];
    r[3] = a[3] - b[3];
    var im = 1.0 / _MJS_v4lengthLocal(r);
    r[0] = r[0] * im;
    r[1] = r[1] * im;
    r[2] = r[2] * im;
    r[3] = r[3] * im;
    return r;
});

function _MJS_v4lengthLocal(a) {
    return Math.sqrt(a[0] * a[0] + a[1] * a[1] + a[2] * a[2] + a[3] * a[3]);
}
var _MJS_v4length = _MJS_v4lengthLocal;

var _MJS_v4lengthSquared = function(a) {
    return a[0] * a[0] + a[1] * a[1] + a[2] * a[2] + a[3] * a[3];
};

var _MJS_v4distance = F2(function(a, b) {
    var dx = a[0] - b[0];
    var dy = a[1] - b[1];
    var dz = a[2] - b[2];
    var dw = a[3] - b[3];
    return Math.sqrt(dx * dx + dy * dy + dz * dz + dw * dw);
});

var _MJS_v4distanceSquared = F2(function(a, b) {
    var dx = a[0] - b[0];
    var dy = a[1] - b[1];
    var dz = a[2] - b[2];
    var dw = a[3] - b[3];
    return dx * dx + dy * dy + dz * dz + dw * dw;
});

var _MJS_v4normalize = function(a) {
    var r = new Float64Array(4);
    var im = 1.0 / _MJS_v4lengthLocal(a);
    r[0] = a[0] * im;
    r[1] = a[1] * im;
    r[2] = a[2] * im;
    r[3] = a[3] * im;
    return r;
};

var _MJS_v4scale = F2(function(k, a) {
    var r = new Float64Array(4);
    r[0] = a[0] * k;
    r[1] = a[1] * k;
    r[2] = a[2] * k;
    r[3] = a[3] * k;
    return r;
});

var _MJS_v4dot = F2(function(a, b) {
    return a[0] * b[0] + a[1] * b[1] + a[2] * b[2] + a[3] * b[3];
});

// Matrix4

var _MJS_m4x4temp1Local = new Float64Array(16);
var _MJS_m4x4temp2Local = new Float64Array(16);

var _MJS_m4x4identity = new Float64Array([
    1.0, 0.0, 0.0, 0.0,
    0.0, 1.0, 0.0, 0.0,
    0.0, 0.0, 1.0, 0.0,
    0.0, 0.0, 0.0, 1.0
]);

var _MJS_m4x4fromRecord = function(r) {
    var m = new Float64Array(16);
    m[0] = r.m11;
    m[1] = r.m21;
    m[2] = r.m31;
    m[3] = r.m41;
    m[4] = r.m12;
    m[5] = r.m22;
    m[6] = r.m32;
    m[7] = r.m42;
    m[8] = r.m13;
    m[9] = r.m23;
    m[10] = r.m33;
    m[11] = r.m43;
    m[12] = r.m14;
    m[13] = r.m24;
    m[14] = r.m34;
    m[15] = r.m44;
    return m;
};

var _MJS_m4x4toRecord = function(m) {
    return {
        m11: m[0], m21: m[1], m31: m[2], m41: m[3],
        m12: m[4], m22: m[5], m32: m[6], m42: m[7],
        m13: m[8], m23: m[9], m33: m[10], m43: m[11],
        m14: m[12], m24: m[13], m34: m[14], m44: m[15]
    };
};

var _MJS_m4x4inverse = function(m) {
    var r = new Float64Array(16);

    r[0] = m[5] * m[10] * m[15] - m[5] * m[11] * m[14] - m[9] * m[6] * m[15] +
        m[9] * m[7] * m[14] + m[13] * m[6] * m[11] - m[13] * m[7] * m[10];
    r[4] = -m[4] * m[10] * m[15] + m[4] * m[11] * m[14] + m[8] * m[6] * m[15] -
        m[8] * m[7] * m[14] - m[12] * m[6] * m[11] + m[12] * m[7] * m[10];
    r[8] = m[4] * m[9] * m[15] - m[4] * m[11] * m[13] - m[8] * m[5] * m[15] +
        m[8] * m[7] * m[13] + m[12] * m[5] * m[11] - m[12] * m[7] * m[9];
    r[12] = -m[4] * m[9] * m[14] + m[4] * m[10] * m[13] + m[8] * m[5] * m[14] -
        m[8] * m[6] * m[13] - m[12] * m[5] * m[10] + m[12] * m[6] * m[9];
    r[1] = -m[1] * m[10] * m[15] + m[1] * m[11] * m[14] + m[9] * m[2] * m[15] -
        m[9] * m[3] * m[14] - m[13] * m[2] * m[11] + m[13] * m[3] * m[10];
    r[5] = m[0] * m[10] * m[15] - m[0] * m[11] * m[14] - m[8] * m[2] * m[15] +
        m[8] * m[3] * m[14] + m[12] * m[2] * m[11] - m[12] * m[3] * m[10];
    r[9] = -m[0] * m[9] * m[15] + m[0] * m[11] * m[13] + m[8] * m[1] * m[15] -
        m[8] * m[3] * m[13] - m[12] * m[1] * m[11] + m[12] * m[3] * m[9];
    r[13] = m[0] * m[9] * m[14] - m[0] * m[10] * m[13] - m[8] * m[1] * m[14] +
        m[8] * m[2] * m[13] + m[12] * m[1] * m[10] - m[12] * m[2] * m[9];
    r[2] = m[1] * m[6] * m[15] - m[1] * m[7] * m[14] - m[5] * m[2] * m[15] +
        m[5] * m[3] * m[14] + m[13] * m[2] * m[7] - m[13] * m[3] * m[6];
    r[6] = -m[0] * m[6] * m[15] + m[0] * m[7] * m[14] + m[4] * m[2] * m[15] -
        m[4] * m[3] * m[14] - m[12] * m[2] * m[7] + m[12] * m[3] * m[6];
    r[10] = m[0] * m[5] * m[15] - m[0] * m[7] * m[13] - m[4] * m[1] * m[15] +
        m[4] * m[3] * m[13] + m[12] * m[1] * m[7] - m[12] * m[3] * m[5];
    r[14] = -m[0] * m[5] * m[14] + m[0] * m[6] * m[13] + m[4] * m[1] * m[14] -
        m[4] * m[2] * m[13] - m[12] * m[1] * m[6] + m[12] * m[2] * m[5];
    r[3] = -m[1] * m[6] * m[11] + m[1] * m[7] * m[10] + m[5] * m[2] * m[11] -
        m[5] * m[3] * m[10] - m[9] * m[2] * m[7] + m[9] * m[3] * m[6];
    r[7] = m[0] * m[6] * m[11] - m[0] * m[7] * m[10] - m[4] * m[2] * m[11] +
        m[4] * m[3] * m[10] + m[8] * m[2] * m[7] - m[8] * m[3] * m[6];
    r[11] = -m[0] * m[5] * m[11] + m[0] * m[7] * m[9] + m[4] * m[1] * m[11] -
        m[4] * m[3] * m[9] - m[8] * m[1] * m[7] + m[8] * m[3] * m[5];
    r[15] = m[0] * m[5] * m[10] - m[0] * m[6] * m[9] - m[4] * m[1] * m[10] +
        m[4] * m[2] * m[9] + m[8] * m[1] * m[6] - m[8] * m[2] * m[5];

    var det = m[0] * r[0] + m[1] * r[4] + m[2] * r[8] + m[3] * r[12];

    if (det === 0) {
        return $elm$core$Maybe$Nothing;
    }

    det = 1.0 / det;

    for (var i = 0; i < 16; i = i + 1) {
        r[i] = r[i] * det;
    }

    return $elm$core$Maybe$Just(r);
};

var _MJS_m4x4inverseOrthonormal = function(m) {
    var r = _MJS_m4x4transposeLocal(m);
    var t = [m[12], m[13], m[14]];
    r[3] = r[7] = r[11] = 0;
    r[12] = -_MJS_v3dotLocal([r[0], r[4], r[8]], t);
    r[13] = -_MJS_v3dotLocal([r[1], r[5], r[9]], t);
    r[14] = -_MJS_v3dotLocal([r[2], r[6], r[10]], t);
    return r;
};

function _MJS_m4x4makeFrustumLocal(left, right, bottom, top, znear, zfar) {
    var r = new Float64Array(16);

    r[0] = 2 * znear / (right - left);
    r[1] = 0;
    r[2] = 0;
    r[3] = 0;
    r[4] = 0;
    r[5] = 2 * znear / (top - bottom);
    r[6] = 0;
    r[7] = 0;
    r[8] = (right + left) / (right - left);
    r[9] = (top + bottom) / (top - bottom);
    r[10] = -(zfar + znear) / (zfar - znear);
    r[11] = -1;
    r[12] = 0;
    r[13] = 0;
    r[14] = -2 * zfar * znear / (zfar - znear);
    r[15] = 0;

    return r;
}
var _MJS_m4x4makeFrustum = F6(_MJS_m4x4makeFrustumLocal);

var _MJS_m4x4makePerspective = F4(function(fovy, aspect, znear, zfar) {
    var ymax = znear * Math.tan(fovy * Math.PI / 360.0);
    var ymin = -ymax;
    var xmin = ymin * aspect;
    var xmax = ymax * aspect;

    return _MJS_m4x4makeFrustumLocal(xmin, xmax, ymin, ymax, znear, zfar);
});

function _MJS_m4x4makeOrthoLocal(left, right, bottom, top, znear, zfar) {
    var r = new Float64Array(16);

    r[0] = 2 / (right - left);
    r[1] = 0;
    r[2] = 0;
    r[3] = 0;
    r[4] = 0;
    r[5] = 2 / (top - bottom);
    r[6] = 0;
    r[7] = 0;
    r[8] = 0;
    r[9] = 0;
    r[10] = -2 / (zfar - znear);
    r[11] = 0;
    r[12] = -(right + left) / (right - left);
    r[13] = -(top + bottom) / (top - bottom);
    r[14] = -(zfar + znear) / (zfar - znear);
    r[15] = 1;

    return r;
}
var _MJS_m4x4makeOrtho = F6(_MJS_m4x4makeOrthoLocal);

var _MJS_m4x4makeOrtho2D = F4(function(left, right, bottom, top) {
    return _MJS_m4x4makeOrthoLocal(left, right, bottom, top, -1, 1);
});

function _MJS_m4x4mulLocal(a, b) {
    var r = new Float64Array(16);
    var a11 = a[0];
    var a21 = a[1];
    var a31 = a[2];
    var a41 = a[3];
    var a12 = a[4];
    var a22 = a[5];
    var a32 = a[6];
    var a42 = a[7];
    var a13 = a[8];
    var a23 = a[9];
    var a33 = a[10];
    var a43 = a[11];
    var a14 = a[12];
    var a24 = a[13];
    var a34 = a[14];
    var a44 = a[15];
    var b11 = b[0];
    var b21 = b[1];
    var b31 = b[2];
    var b41 = b[3];
    var b12 = b[4];
    var b22 = b[5];
    var b32 = b[6];
    var b42 = b[7];
    var b13 = b[8];
    var b23 = b[9];
    var b33 = b[10];
    var b43 = b[11];
    var b14 = b[12];
    var b24 = b[13];
    var b34 = b[14];
    var b44 = b[15];

    r[0] = a11 * b11 + a12 * b21 + a13 * b31 + a14 * b41;
    r[1] = a21 * b11 + a22 * b21 + a23 * b31 + a24 * b41;
    r[2] = a31 * b11 + a32 * b21 + a33 * b31 + a34 * b41;
    r[3] = a41 * b11 + a42 * b21 + a43 * b31 + a44 * b41;
    r[4] = a11 * b12 + a12 * b22 + a13 * b32 + a14 * b42;
    r[5] = a21 * b12 + a22 * b22 + a23 * b32 + a24 * b42;
    r[6] = a31 * b12 + a32 * b22 + a33 * b32 + a34 * b42;
    r[7] = a41 * b12 + a42 * b22 + a43 * b32 + a44 * b42;
    r[8] = a11 * b13 + a12 * b23 + a13 * b33 + a14 * b43;
    r[9] = a21 * b13 + a22 * b23 + a23 * b33 + a24 * b43;
    r[10] = a31 * b13 + a32 * b23 + a33 * b33 + a34 * b43;
    r[11] = a41 * b13 + a42 * b23 + a43 * b33 + a44 * b43;
    r[12] = a11 * b14 + a12 * b24 + a13 * b34 + a14 * b44;
    r[13] = a21 * b14 + a22 * b24 + a23 * b34 + a24 * b44;
    r[14] = a31 * b14 + a32 * b24 + a33 * b34 + a34 * b44;
    r[15] = a41 * b14 + a42 * b24 + a43 * b34 + a44 * b44;

    return r;
}
var _MJS_m4x4mul = F2(_MJS_m4x4mulLocal);

var _MJS_m4x4mulAffine = F2(function(a, b) {
    var r = new Float64Array(16);
    var a11 = a[0];
    var a21 = a[1];
    var a31 = a[2];
    var a12 = a[4];
    var a22 = a[5];
    var a32 = a[6];
    var a13 = a[8];
    var a23 = a[9];
    var a33 = a[10];
    var a14 = a[12];
    var a24 = a[13];
    var a34 = a[14];

    var b11 = b[0];
    var b21 = b[1];
    var b31 = b[2];
    var b12 = b[4];
    var b22 = b[5];
    var b32 = b[6];
    var b13 = b[8];
    var b23 = b[9];
    var b33 = b[10];
    var b14 = b[12];
    var b24 = b[13];
    var b34 = b[14];

    r[0] = a11 * b11 + a12 * b21 + a13 * b31;
    r[1] = a21 * b11 + a22 * b21 + a23 * b31;
    r[2] = a31 * b11 + a32 * b21 + a33 * b31;
    r[3] = 0;
    r[4] = a11 * b12 + a12 * b22 + a13 * b32;
    r[5] = a21 * b12 + a22 * b22 + a23 * b32;
    r[6] = a31 * b12 + a32 * b22 + a33 * b32;
    r[7] = 0;
    r[8] = a11 * b13 + a12 * b23 + a13 * b33;
    r[9] = a21 * b13 + a22 * b23 + a23 * b33;
    r[10] = a31 * b13 + a32 * b23 + a33 * b33;
    r[11] = 0;
    r[12] = a11 * b14 + a12 * b24 + a13 * b34 + a14;
    r[13] = a21 * b14 + a22 * b24 + a23 * b34 + a24;
    r[14] = a31 * b14 + a32 * b24 + a33 * b34 + a34;
    r[15] = 1;

    return r;
});

var _MJS_m4x4makeRotate = F2(function(angle, axis) {
    var r = new Float64Array(16);
    axis = _MJS_v3normalizeLocal(axis, _MJS_v3temp1Local);
    var x = axis[0];
    var y = axis[1];
    var z = axis[2];
    var c = Math.cos(angle);
    var c1 = 1 - c;
    var s = Math.sin(angle);

    r[0] = x * x * c1 + c;
    r[1] = y * x * c1 + z * s;
    r[2] = z * x * c1 - y * s;
    r[3] = 0;
    r[4] = x * y * c1 - z * s;
    r[5] = y * y * c1 + c;
    r[6] = y * z * c1 + x * s;
    r[7] = 0;
    r[8] = x * z * c1 + y * s;
    r[9] = y * z * c1 - x * s;
    r[10] = z * z * c1 + c;
    r[11] = 0;
    r[12] = 0;
    r[13] = 0;
    r[14] = 0;
    r[15] = 1;

    return r;
});

var _MJS_m4x4rotate = F3(function(angle, axis, m) {
    var r = new Float64Array(16);
    var im = 1.0 / _MJS_v3lengthLocal(axis);
    var x = axis[0] * im;
    var y = axis[1] * im;
    var z = axis[2] * im;
    var c = Math.cos(angle);
    var c1 = 1 - c;
    var s = Math.sin(angle);
    var xs = x * s;
    var ys = y * s;
    var zs = z * s;
    var xyc1 = x * y * c1;
    var xzc1 = x * z * c1;
    var yzc1 = y * z * c1;
    var t11 = x * x * c1 + c;
    var t21 = xyc1 + zs;
    var t31 = xzc1 - ys;
    var t12 = xyc1 - zs;
    var t22 = y * y * c1 + c;
    var t32 = yzc1 + xs;
    var t13 = xzc1 + ys;
    var t23 = yzc1 - xs;
    var t33 = z * z * c1 + c;
    var m11 = m[0], m21 = m[1], m31 = m[2], m41 = m[3];
    var m12 = m[4], m22 = m[5], m32 = m[6], m42 = m[7];
    var m13 = m[8], m23 = m[9], m33 = m[10], m43 = m[11];
    var m14 = m[12], m24 = m[13], m34 = m[14], m44 = m[15];

    r[0] = m11 * t11 + m12 * t21 + m13 * t31;
    r[1] = m21 * t11 + m22 * t21 + m23 * t31;
    r[2] = m31 * t11 + m32 * t21 + m33 * t31;
    r[3] = m41 * t11 + m42 * t21 + m43 * t31;
    r[4] = m11 * t12 + m12 * t22 + m13 * t32;
    r[5] = m21 * t12 + m22 * t22 + m23 * t32;
    r[6] = m31 * t12 + m32 * t22 + m33 * t32;
    r[7] = m41 * t12 + m42 * t22 + m43 * t32;
    r[8] = m11 * t13 + m12 * t23 + m13 * t33;
    r[9] = m21 * t13 + m22 * t23 + m23 * t33;
    r[10] = m31 * t13 + m32 * t23 + m33 * t33;
    r[11] = m41 * t13 + m42 * t23 + m43 * t33;
    r[12] = m14,
    r[13] = m24;
    r[14] = m34;
    r[15] = m44;

    return r;
});

function _MJS_m4x4makeScale3Local(x, y, z) {
    var r = new Float64Array(16);

    r[0] = x;
    r[1] = 0;
    r[2] = 0;
    r[3] = 0;
    r[4] = 0;
    r[5] = y;
    r[6] = 0;
    r[7] = 0;
    r[8] = 0;
    r[9] = 0;
    r[10] = z;
    r[11] = 0;
    r[12] = 0;
    r[13] = 0;
    r[14] = 0;
    r[15] = 1;

    return r;
}
var _MJS_m4x4makeScale3 = F3(_MJS_m4x4makeScale3Local);

var _MJS_m4x4makeScale = function(v) {
    return _MJS_m4x4makeScale3Local(v[0], v[1], v[2]);
};

var _MJS_m4x4scale3 = F4(function(x, y, z, m) {
    var r = new Float64Array(16);

    r[0] = m[0] * x;
    r[1] = m[1] * x;
    r[2] = m[2] * x;
    r[3] = m[3] * x;
    r[4] = m[4] * y;
    r[5] = m[5] * y;
    r[6] = m[6] * y;
    r[7] = m[7] * y;
    r[8] = m[8] * z;
    r[9] = m[9] * z;
    r[10] = m[10] * z;
    r[11] = m[11] * z;
    r[12] = m[12];
    r[13] = m[13];
    r[14] = m[14];
    r[15] = m[15];

    return r;
});

var _MJS_m4x4scale = F2(function(v, m) {
    var r = new Float64Array(16);
    var x = v[0];
    var y = v[1];
    var z = v[2];

    r[0] = m[0] * x;
    r[1] = m[1] * x;
    r[2] = m[2] * x;
    r[3] = m[3] * x;
    r[4] = m[4] * y;
    r[5] = m[5] * y;
    r[6] = m[6] * y;
    r[7] = m[7] * y;
    r[8] = m[8] * z;
    r[9] = m[9] * z;
    r[10] = m[10] * z;
    r[11] = m[11] * z;
    r[12] = m[12];
    r[13] = m[13];
    r[14] = m[14];
    r[15] = m[15];

    return r;
});

function _MJS_m4x4makeTranslate3Local(x, y, z) {
    var r = new Float64Array(16);

    r[0] = 1;
    r[1] = 0;
    r[2] = 0;
    r[3] = 0;
    r[4] = 0;
    r[5] = 1;
    r[6] = 0;
    r[7] = 0;
    r[8] = 0;
    r[9] = 0;
    r[10] = 1;
    r[11] = 0;
    r[12] = x;
    r[13] = y;
    r[14] = z;
    r[15] = 1;

    return r;
}
var _MJS_m4x4makeTranslate3 = F3(_MJS_m4x4makeTranslate3Local);

var _MJS_m4x4makeTranslate = function(v) {
    return _MJS_m4x4makeTranslate3Local(v[0], v[1], v[2]);
};

var _MJS_m4x4translate3 = F4(function(x, y, z, m) {
    var r = new Float64Array(16);
    var m11 = m[0];
    var m21 = m[1];
    var m31 = m[2];
    var m41 = m[3];
    var m12 = m[4];
    var m22 = m[5];
    var m32 = m[6];
    var m42 = m[7];
    var m13 = m[8];
    var m23 = m[9];
    var m33 = m[10];
    var m43 = m[11];

    r[0] = m11;
    r[1] = m21;
    r[2] = m31;
    r[3] = m41;
    r[4] = m12;
    r[5] = m22;
    r[6] = m32;
    r[7] = m42;
    r[8] = m13;
    r[9] = m23;
    r[10] = m33;
    r[11] = m43;
    r[12] = m11 * x + m12 * y + m13 * z + m[12];
    r[13] = m21 * x + m22 * y + m23 * z + m[13];
    r[14] = m31 * x + m32 * y + m33 * z + m[14];
    r[15] = m41 * x + m42 * y + m43 * z + m[15];

    return r;
});

var _MJS_m4x4translate = F2(function(v, m) {
    var r = new Float64Array(16);
    var x = v[0];
    var y = v[1];
    var z = v[2];
    var m11 = m[0];
    var m21 = m[1];
    var m31 = m[2];
    var m41 = m[3];
    var m12 = m[4];
    var m22 = m[5];
    var m32 = m[6];
    var m42 = m[7];
    var m13 = m[8];
    var m23 = m[9];
    var m33 = m[10];
    var m43 = m[11];

    r[0] = m11;
    r[1] = m21;
    r[2] = m31;
    r[3] = m41;
    r[4] = m12;
    r[5] = m22;
    r[6] = m32;
    r[7] = m42;
    r[8] = m13;
    r[9] = m23;
    r[10] = m33;
    r[11] = m43;
    r[12] = m11 * x + m12 * y + m13 * z + m[12];
    r[13] = m21 * x + m22 * y + m23 * z + m[13];
    r[14] = m31 * x + m32 * y + m33 * z + m[14];
    r[15] = m41 * x + m42 * y + m43 * z + m[15];

    return r;
});

var _MJS_m4x4makeLookAt = F3(function(eye, center, up) {
    var z = _MJS_v3directionLocal(eye, center, _MJS_v3temp1Local);
    var x = _MJS_v3normalizeLocal(_MJS_v3crossLocal(up, z, _MJS_v3temp2Local), _MJS_v3temp2Local);
    var y = _MJS_v3normalizeLocal(_MJS_v3crossLocal(z, x, _MJS_v3temp3Local), _MJS_v3temp3Local);
    var tm1 = _MJS_m4x4temp1Local;
    var tm2 = _MJS_m4x4temp2Local;

    tm1[0] = x[0];
    tm1[1] = y[0];
    tm1[2] = z[0];
    tm1[3] = 0;
    tm1[4] = x[1];
    tm1[5] = y[1];
    tm1[6] = z[1];
    tm1[7] = 0;
    tm1[8] = x[2];
    tm1[9] = y[2];
    tm1[10] = z[2];
    tm1[11] = 0;
    tm1[12] = 0;
    tm1[13] = 0;
    tm1[14] = 0;
    tm1[15] = 1;

    tm2[0] = 1; tm2[1] = 0; tm2[2] = 0; tm2[3] = 0;
    tm2[4] = 0; tm2[5] = 1; tm2[6] = 0; tm2[7] = 0;
    tm2[8] = 0; tm2[9] = 0; tm2[10] = 1; tm2[11] = 0;
    tm2[12] = -eye[0]; tm2[13] = -eye[1]; tm2[14] = -eye[2]; tm2[15] = 1;

    return _MJS_m4x4mulLocal(tm1, tm2);
});


function _MJS_m4x4transposeLocal(m) {
    var r = new Float64Array(16);

    r[0] = m[0]; r[1] = m[4]; r[2] = m[8]; r[3] = m[12];
    r[4] = m[1]; r[5] = m[5]; r[6] = m[9]; r[7] = m[13];
    r[8] = m[2]; r[9] = m[6]; r[10] = m[10]; r[11] = m[14];
    r[12] = m[3]; r[13] = m[7]; r[14] = m[11]; r[15] = m[15];

    return r;
}
var _MJS_m4x4transpose = _MJS_m4x4transposeLocal;

var _MJS_m4x4makeBasis = F3(function(vx, vy, vz) {
    var r = new Float64Array(16);

    r[0] = vx[0];
    r[1] = vx[1];
    r[2] = vx[2];
    r[3] = 0;
    r[4] = vy[0];
    r[5] = vy[1];
    r[6] = vy[2];
    r[7] = 0;
    r[8] = vz[0];
    r[9] = vz[1];
    r[10] = vz[2];
    r[11] = 0;
    r[12] = 0;
    r[13] = 0;
    r[14] = 0;
    r[15] = 1;

    return r;
});
var $elm$core$List$cons = _List_cons;
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$GT = {$: 'GT'};
var $elm$core$Basics$LT = {$: 'LT'};
var $author$project$Test$Reporter$Reporter$ConsoleReport = function (a) {
	return {$: 'ConsoleReport', a: a};
};
var $author$project$Console$Text$Monochrome = {$: 'Monochrome'};
var $elm$core$Debug$todo = _Debug_todo;
var $author$project$Test$Runner$Node$checkHelperReplaceMe___ = function (_v0) {
	return _Debug_todo(
		'Test.Runner.Node',
		{
			start: {line: 362, column: 5},
			end: {line: 362, column: 15}
		})('The regex for replacing this Debug.todo with some real code must have failed since you see this message!\n\nPlease report this bug: https://github.com/rtfeldman/node-test-runner/issues/new\n');
};
var $author$project$Test$Runner$Node$check = value => value && value.__elmTestSymbol === __elmTestSymbol ? $elm$core$Maybe$Just(value) : $elm$core$Maybe$Nothing;
var $elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var $elm$core$Basics$False = {$: 'False'};
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var $elm$core$Maybe$Nothing = {$: 'Nothing'};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 'Nothing') {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / $elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = {$: 'True'};
var $elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $author$project$Test$Runner$Node$Receive = function (a) {
	return {$: 'Receive', a: a};
};
var $elm_explorations$test$Test$Runner$Failure$DuplicatedName = {$: 'DuplicatedName'};
var $elm_explorations$test$Test$Internal$ElmTestVariant__Batch = function (a) {
	return {__elmTestSymbol: __elmTestSymbol, $: 'ElmTestVariant__Batch', a: a};
};
var $elm_explorations$test$Test$Runner$Failure$EmptyList = {$: 'EmptyList'};
var $elm_explorations$test$Test$Runner$Failure$Invalid = function (a) {
	return {$: 'Invalid', a: a};
};
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
	});
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$core$Set$Set_elm_builtin = function (a) {
	return {$: 'Set_elm_builtin', a: a};
};
var $elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$core$Set$empty = $elm$core$Set$Set_elm_builtin($elm$core$Dict$empty);
var $elm$core$Dict$Black = {$: 'Black'};
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = {$: 'Red'};
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1.$) {
				case 'LT':
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Set$insert = F2(
	function (key, _v0) {
		var dict = _v0.a;
		return $elm$core$Set$Set_elm_builtin(
			A3($elm$core$Dict$insert, key, _Utils_Tuple0, dict));
	});
var $elm$core$Dict$isEmpty = function (dict) {
	if (dict.$ === 'RBEmpty_elm_builtin') {
		return true;
	} else {
		return false;
	}
};
var $elm$core$Set$isEmpty = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$isEmpty(dict);
};
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Dict$member = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$get, key, dict);
		if (_v0.$ === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var $elm$core$Set$member = F2(
	function (key, _v0) {
		var dict = _v0.a;
		return A2($elm$core$Dict$member, key, dict);
	});
var $elm_explorations$test$Test$Internal$duplicatedName = function (tests) {
	var names = function (test) {
		names:
		while (true) {
			switch (test.$) {
				case 'ElmTestVariant__Labeled':
					var str = test.a;
					return _List_fromArray(
						[str]);
				case 'ElmTestVariant__Batch':
					var subtests = test.a;
					return A2($elm$core$List$concatMap, names, subtests);
				case 'ElmTestVariant__UnitTest':
					return _List_Nil;
				case 'ElmTestVariant__FuzzTest':
					return _List_Nil;
				case 'ElmTestVariant__Skipped':
					var subTest = test.a;
					var $temp$test = subTest;
					test = $temp$test;
					continue names;
				default:
					var subTest = test.a;
					var $temp$test = subTest;
					test = $temp$test;
					continue names;
			}
		}
	};
	var accumDuplicates = F2(
		function (newName, _v2) {
			var dups = _v2.a;
			var uniques = _v2.b;
			return A2($elm$core$Set$member, newName, uniques) ? _Utils_Tuple2(
				A2($elm$core$Set$insert, newName, dups),
				uniques) : _Utils_Tuple2(
				dups,
				A2($elm$core$Set$insert, newName, uniques));
		});
	var _v1 = A3(
		$elm$core$List$foldl,
		accumDuplicates,
		_Utils_Tuple2($elm$core$Set$empty, $elm$core$Set$empty),
		A2($elm$core$List$concatMap, names, tests));
	var dupsAccum = _v1.a;
	var uniquesAccum = _v1.b;
	return $elm$core$Set$isEmpty(dupsAccum) ? $elm$core$Result$Ok(uniquesAccum) : $elm$core$Result$Err(dupsAccum);
};
var $elm_explorations$test$Test$Internal$ElmTestVariant__UnitTest = function (a) {
	return {__elmTestSymbol: __elmTestSymbol, $: 'ElmTestVariant__UnitTest', a: a};
};
var $elm_explorations$test$Test$Expectation$Fail = function (a) {
	return {$: 'Fail', a: a};
};
var $elm_explorations$test$Test$Distribution$NoDistribution = {$: 'NoDistribution'};
var $elm_explorations$test$Test$Expectation$fail = function (_v0) {
	var reason = _v0.reason;
	var description = _v0.description;
	return $elm_explorations$test$Test$Expectation$Fail(
		{description: description, distributionReport: $elm_explorations$test$Test$Distribution$NoDistribution, given: $elm$core$Maybe$Nothing, reason: reason});
};
var $elm_explorations$test$Test$Internal$failNow = function (record) {
	return $elm_explorations$test$Test$Internal$ElmTestVariant__UnitTest(
		function (_v0) {
			return _List_fromArray(
				[
					$elm_explorations$test$Test$Expectation$fail(record)
				]);
		});
};
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $elm_explorations$test$Test$concat = function (tests) {
	if ($elm$core$List$isEmpty(tests)) {
		return $elm_explorations$test$Test$Internal$failNow(
			{
				description: 'This `concat` has no tests in it. Let\'s give it some!',
				reason: $elm_explorations$test$Test$Runner$Failure$Invalid($elm_explorations$test$Test$Runner$Failure$EmptyList)
			});
	} else {
		var _v0 = $elm_explorations$test$Test$Internal$duplicatedName(tests);
		if (_v0.$ === 'Err') {
			var dups = _v0.a;
			var dupDescription = function (duped) {
				return 'A test group contains multiple tests named \'' + (duped + '\'. Do some renaming so that tests have unique names.');
			};
			return $elm_explorations$test$Test$Internal$failNow(
				{
					description: A2(
						$elm$core$String$join,
						'\n',
						A2(
							$elm$core$List$map,
							dupDescription,
							$elm$core$Set$toList(dups))),
					reason: $elm_explorations$test$Test$Runner$Failure$Invalid($elm_explorations$test$Test$Runner$Failure$DuplicatedName)
				});
		} else {
			return $elm_explorations$test$Test$Internal$ElmTestVariant__Batch(tests);
		}
	}
};
var $elm_explorations$test$Test$Runner$Failure$BadDescription = {$: 'BadDescription'};
var $elm_explorations$test$Test$Internal$ElmTestVariant__Labeled = F2(
	function (a, b) {
		return {__elmTestSymbol: __elmTestSymbol, $: 'ElmTestVariant__Labeled', a: a, b: b};
	});
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$trim = _String_trim;
var $elm_explorations$test$Test$describe = F2(
	function (untrimmedDesc, tests) {
		var desc = $elm$core$String$trim(untrimmedDesc);
		if ($elm$core$String$isEmpty(desc)) {
			return $elm_explorations$test$Test$Internal$failNow(
				{
					description: 'This `describe` has a blank description. Let\'s give it a useful one!',
					reason: $elm_explorations$test$Test$Runner$Failure$Invalid($elm_explorations$test$Test$Runner$Failure$BadDescription)
				});
		} else {
			if ($elm$core$List$isEmpty(tests)) {
				return $elm_explorations$test$Test$Internal$failNow(
					{
						description: 'This `describe ' + (desc + '` has no tests in it. Let\'s give it some!'),
						reason: $elm_explorations$test$Test$Runner$Failure$Invalid($elm_explorations$test$Test$Runner$Failure$EmptyList)
					});
			} else {
				var _v0 = $elm_explorations$test$Test$Internal$duplicatedName(tests);
				if (_v0.$ === 'Err') {
					var dups = _v0.a;
					var dupDescription = function (duped) {
						return 'Contains multiple tests named \'' + (duped + '\'. Let\'s rename them so we know which is which.');
					};
					return A2(
						$elm_explorations$test$Test$Internal$ElmTestVariant__Labeled,
						desc,
						$elm_explorations$test$Test$Internal$failNow(
							{
								description: A2(
									$elm$core$String$join,
									'\n',
									A2(
										$elm$core$List$map,
										dupDescription,
										$elm$core$Set$toList(dups))),
								reason: $elm_explorations$test$Test$Runner$Failure$Invalid($elm_explorations$test$Test$Runner$Failure$DuplicatedName)
							}));
				} else {
					var childrenNames = _v0.a;
					return A2($elm$core$Set$member, desc, childrenNames) ? A2(
						$elm_explorations$test$Test$Internal$ElmTestVariant__Labeled,
						desc,
						$elm_explorations$test$Test$Internal$failNow(
							{
								description: 'The test \'' + (desc + '\' contains a child test of the same name. Let\'s rename them so we know which is which.'),
								reason: $elm_explorations$test$Test$Runner$Failure$Invalid($elm_explorations$test$Test$Runner$Failure$DuplicatedName)
							})) : A2(
						$elm_explorations$test$Test$Internal$ElmTestVariant__Labeled,
						desc,
						$elm_explorations$test$Test$Internal$ElmTestVariant__Batch(tests));
				}
			}
		}
	});
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $author$project$Test$Runner$Node$elmTestPort__receive = _Platform_incomingPort('elmTestPort__receive', $elm$json$Json$Decode$value);
var $author$project$Test$Reporter$Reporter$TestReporter = F4(
	function (format, reportBegin, reportComplete, reportSummary) {
		return {format: format, reportBegin: reportBegin, reportComplete: reportComplete, reportSummary: reportSummary};
	});
var $elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, obj) {
					var k = _v0.a;
					var v = _v0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(_Utils_Tuple0),
			pairs));
};
var $author$project$Console$Text$Default = {$: 'Default'};
var $author$project$Console$Text$Normal = {$: 'Normal'};
var $author$project$Console$Text$Text = F2(
	function (a, b) {
		return {$: 'Text', a: a, b: b};
	});
var $author$project$Console$Text$plain = $author$project$Console$Text$Text(
	{background: $author$project$Console$Text$Default, foreground: $author$project$Console$Text$Default, modifiers: _List_Nil, style: $author$project$Console$Text$Normal});
var $author$project$Test$Reporter$Console$pluralize = F3(
	function (singular, plural, count) {
		var suffix = (count === 1) ? singular : plural;
		return A2(
			$elm$core$String$join,
			' ',
			_List_fromArray(
				[
					$elm$core$String$fromInt(count),
					suffix
				]));
	});
var $elm$json$Json$Encode$string = _Json_wrap;
var $author$project$Test$Runner$Node$Vendor$Console$colorsInverted = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[7m', str, '\u001B[27m']));
};
var $author$project$Test$Runner$Node$Vendor$Console$dark = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[2m', str, '\u001B[22m']));
};
var $author$project$Console$Text$applyModifiersHelp = F2(
	function (modifier, str) {
		if (modifier.$ === 'Inverted') {
			return $author$project$Test$Runner$Node$Vendor$Console$colorsInverted(str);
		} else {
			return $author$project$Test$Runner$Node$Vendor$Console$dark(str);
		}
	});
var $author$project$Console$Text$applyModifiers = F2(
	function (modifiers, str) {
		return A3($elm$core$List$foldl, $author$project$Console$Text$applyModifiersHelp, str, modifiers);
	});
var $author$project$Test$Runner$Node$Vendor$Console$bold = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[1m', str, '\u001B[22m']));
};
var $author$project$Test$Runner$Node$Vendor$Console$underline = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[4m', str, '\u001B[24m']));
};
var $author$project$Console$Text$applyStyle = F2(
	function (style, str) {
		switch (style.$) {
			case 'Normal':
				return str;
			case 'Bold':
				return $author$project$Test$Runner$Node$Vendor$Console$bold(str);
			default:
				return $author$project$Test$Runner$Node$Vendor$Console$underline(str);
		}
	});
var $author$project$Test$Runner$Node$Vendor$Console$bgBlack = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[40m', str, '\u001B[49m']));
};
var $author$project$Test$Runner$Node$Vendor$Console$bgBlue = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[44m', str, '\u001B[49m']));
};
var $author$project$Test$Runner$Node$Vendor$Console$bgCyan = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[46m', str, '\u001B[49m']));
};
var $author$project$Test$Runner$Node$Vendor$Console$bgGreen = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[42m', str, '\u001B[49m']));
};
var $author$project$Test$Runner$Node$Vendor$Console$bgMagenta = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[45m', str, '\u001B[49m']));
};
var $author$project$Test$Runner$Node$Vendor$Console$bgRed = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[41m', str, '\u001B[49m']));
};
var $author$project$Test$Runner$Node$Vendor$Console$bgWhite = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[47m', str, '\u001B[49m']));
};
var $author$project$Test$Runner$Node$Vendor$Console$bgYellow = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[43m', str, '\u001B[49m']));
};
var $author$project$Console$Text$colorizeBackground = F2(
	function (color, str) {
		switch (color.$) {
			case 'Default':
				return str;
			case 'Red':
				return $author$project$Test$Runner$Node$Vendor$Console$bgRed(str);
			case 'Green':
				return $author$project$Test$Runner$Node$Vendor$Console$bgGreen(str);
			case 'Yellow':
				return $author$project$Test$Runner$Node$Vendor$Console$bgYellow(str);
			case 'Black':
				return $author$project$Test$Runner$Node$Vendor$Console$bgBlack(str);
			case 'Blue':
				return $author$project$Test$Runner$Node$Vendor$Console$bgBlue(str);
			case 'Magenta':
				return $author$project$Test$Runner$Node$Vendor$Console$bgMagenta(str);
			case 'Cyan':
				return $author$project$Test$Runner$Node$Vendor$Console$bgCyan(str);
			default:
				return $author$project$Test$Runner$Node$Vendor$Console$bgWhite(str);
		}
	});
var $author$project$Test$Runner$Node$Vendor$Console$black = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[30m', str, '\u001B[39m']));
};
var $author$project$Test$Runner$Node$Vendor$Console$blue = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[34m', str, '\u001B[39m']));
};
var $author$project$Test$Runner$Node$Vendor$Console$cyan = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[36m', str, '\u001B[39m']));
};
var $author$project$Test$Runner$Node$Vendor$Console$green = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[32m', str, '\u001B[39m']));
};
var $author$project$Test$Runner$Node$Vendor$Console$magenta = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[35m', str, '\u001B[39m']));
};
var $author$project$Test$Runner$Node$Vendor$Console$red = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[31m', str, '\u001B[39m']));
};
var $author$project$Test$Runner$Node$Vendor$Console$white = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[37m', str, '\u001B[39m']));
};
var $author$project$Test$Runner$Node$Vendor$Console$yellow = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[33m', str, '\u001B[39m']));
};
var $author$project$Console$Text$colorizeForeground = F2(
	function (color, str) {
		switch (color.$) {
			case 'Default':
				return str;
			case 'Red':
				return $author$project$Test$Runner$Node$Vendor$Console$red(str);
			case 'Green':
				return $author$project$Test$Runner$Node$Vendor$Console$green(str);
			case 'Yellow':
				return $author$project$Test$Runner$Node$Vendor$Console$yellow(str);
			case 'Black':
				return $author$project$Test$Runner$Node$Vendor$Console$black(str);
			case 'Blue':
				return $author$project$Test$Runner$Node$Vendor$Console$blue(str);
			case 'Magenta':
				return $author$project$Test$Runner$Node$Vendor$Console$magenta(str);
			case 'Cyan':
				return $author$project$Test$Runner$Node$Vendor$Console$cyan(str);
			default:
				return $author$project$Test$Runner$Node$Vendor$Console$white(str);
		}
	});
var $author$project$Console$Text$render = F2(
	function (useColor, txt) {
		if (txt.$ === 'Text') {
			var attrs = txt.a;
			var str = txt.b;
			if (useColor.$ === 'UseColor') {
				return A2(
					$author$project$Console$Text$applyStyle,
					attrs.style,
					A2(
						$author$project$Console$Text$applyModifiers,
						attrs.modifiers,
						A2(
							$author$project$Console$Text$colorizeForeground,
							attrs.foreground,
							A2($author$project$Console$Text$colorizeBackground, attrs.background, str))));
			} else {
				return str;
			}
		} else {
			var texts = txt.a;
			return A2(
				$elm$core$String$join,
				'',
				A2(
					$elm$core$List$map,
					$author$project$Console$Text$render(useColor),
					texts));
		}
	});
var $author$project$Test$Reporter$Console$textToValue = F2(
	function (useColor, txt) {
		return $elm$json$Json$Encode$string(
			A2($author$project$Console$Text$render, useColor, txt));
	});
var $author$project$Test$Reporter$Console$reportBegin = F2(
	function (useColor, _v0) {
		var initialSeed = _v0.initialSeed;
		var testCount = _v0.testCount;
		var fuzzRuns = _v0.fuzzRuns;
		var globs = _v0.globs;
		var prefix = 'Running ' + (A3($author$project$Test$Reporter$Console$pluralize, 'test', 'tests', testCount) + ('. To reproduce these results, run: elm-test --fuzz ' + ($elm$core$String$fromInt(fuzzRuns) + (' --seed ' + $elm$core$String$fromInt(initialSeed)))));
		return $elm$core$Maybe$Just(
			$elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'type',
						$elm$json$Json$Encode$string('begin')),
						_Utils_Tuple2(
						'output',
						A2(
							$author$project$Test$Reporter$Console$textToValue,
							useColor,
							$author$project$Console$Text$plain(
								A2(
									$elm$core$String$join,
									' ',
									A2($elm$core$List$cons, prefix, globs)) + '\n')))
					])));
	});
var $author$project$Test$Reporter$JUnit$reportBegin = function (_v0) {
	return $elm$core$Maybe$Nothing;
};
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(_Utils_Tuple0),
				entries));
	});
var $author$project$Test$Reporter$Json$reportBegin = function (_v0) {
	var initialSeed = _v0.initialSeed;
	var testCount = _v0.testCount;
	var fuzzRuns = _v0.fuzzRuns;
	var paths = _v0.paths;
	var globs = _v0.globs;
	return $elm$core$Maybe$Just(
		$elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'event',
					$elm$json$Json$Encode$string('runStart')),
					_Utils_Tuple2(
					'testCount',
					$elm$json$Json$Encode$string(
						$elm$core$String$fromInt(testCount))),
					_Utils_Tuple2(
					'fuzzRuns',
					$elm$json$Json$Encode$string(
						$elm$core$String$fromInt(fuzzRuns))),
					_Utils_Tuple2(
					'globs',
					A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, globs)),
					_Utils_Tuple2(
					'paths',
					A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, paths)),
					_Utils_Tuple2(
					'initialSeed',
					$elm$json$Json$Encode$string(
						$elm$core$String$fromInt(initialSeed)))
				])));
};
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm_explorations$test$AsciiTable$AlignLeft = {$: 'AlignLeft'};
var $elm_explorations$test$AsciiTable$AlignRight = {$: 'AlignRight'};
var $elm_explorations$test$Test$Runner$Distribution$bars = 30;
var $elm$core$String$cons = _String_cons;
var $elm$core$String$fromChar = function (_char) {
	return A2($elm$core$String$cons, _char, '');
};
var $elm$core$String$length = _String_length;
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var $elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			$elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var $elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3($elm$core$String$repeatHelp, n, chunk, '');
	});
var $elm$core$String$padRight = F3(
	function (n, _char, string) {
		return _Utils_ap(
			string,
			A2(
				$elm$core$String$repeat,
				n - $elm$core$String$length(string),
				$elm$core$String$fromChar(_char)));
	});
var $elm$core$Basics$round = _Basics_round;
var $elm_explorations$test$Test$Runner$Distribution$barView = function (_v0) {
	var runsElapsed = _v0.runsElapsed;
	var count = _v0.count;
	var percentage = count / runsElapsed;
	var barsForPercentage = percentage * $elm_explorations$test$Test$Runner$Distribution$bars;
	var fullBars = $elm$core$Basics$round(barsForPercentage);
	return A3(
		$elm$core$String$padRight,
		$elm_explorations$test$Test$Runner$Distribution$bars,
		_Utils_chr('░'),
		A2($elm$core$String$repeat, fullBars, '█'));
};
var $elm$core$String$fromFloat = _String_fromNumber;
var $elm$core$List$map3 = _List_map3;
var $elm$core$List$maximum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$max, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$String$padLeft = F3(
	function (n, _char, string) {
		return _Utils_ap(
			A2(
				$elm$core$String$repeat,
				n - $elm$core$String$length(string),
				$elm$core$String$fromChar(_char)),
			string);
	});
var $elm$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (n <= 0) {
				return result;
			} else {
				var $temp$result = A2($elm$core$List$cons, value, result),
					$temp$n = n - 1,
					$temp$value = value;
				result = $temp$result;
				n = $temp$n;
				value = $temp$value;
				continue repeatHelp;
			}
		}
	});
var $elm$core$List$repeat = F2(
	function (n, value) {
		return A3($elm$core$List$repeatHelp, _List_Nil, n, value);
	});
var $elm_explorations$test$MicroListExtra$rowsLength = function (listOfLists) {
	if (!listOfLists.b) {
		return 0;
	} else {
		var x = listOfLists.a;
		return $elm$core$List$length(x);
	}
};
var $elm_explorations$test$MicroListExtra$transpose = function (listOfLists) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$List$map2($elm$core$List$cons),
		A2(
			$elm$core$List$repeat,
			$elm_explorations$test$MicroListExtra$rowsLength(listOfLists),
			_List_Nil),
		listOfLists);
};
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $elm_explorations$test$AsciiTable$view = F2(
	function (columns, items) {
		var padFn = F3(
			function (length, align, string) {
				if (align.$ === 'AlignLeft') {
					return A3(
						$elm$core$String$padRight,
						length,
						_Utils_chr(' '),
						string);
				} else {
					return A3(
						$elm$core$String$padLeft,
						length,
						_Utils_chr(' '),
						string);
				}
			});
		var columnData = A2(
			$elm$core$List$map,
			function (col) {
				return A2($elm$core$List$map, col.toString, items);
			},
			columns);
		var columnLengths = A2(
			$elm$core$List$map,
			function (colRows) {
				return A2(
					$elm$core$Maybe$withDefault,
					0,
					$elm$core$List$maximum(
						A2($elm$core$List$map, $elm$core$String$length, colRows)));
			},
			columnData);
		var paddedColumnData = A4(
			$elm$core$List$map3,
			F3(
				function (col, colLength, colStrings) {
					return A2(
						$elm$core$List$map,
						A2(padFn, colLength, col.align),
						colStrings);
				}),
			columns,
			columnLengths,
			columnData);
		return A3(
			$elm$core$List$map2,
			F2(
				function (item, rowCells) {
					return {
						item: item,
						renderedRow: A2($elm$core$String$join, '  ', rowCells)
					};
				}),
			items,
			$elm_explorations$test$MicroListExtra$transpose(paddedColumnData));
	});
var $elm_explorations$test$Test$Runner$Distribution$viewLabels = function (labels) {
	return $elm$core$List$isEmpty(labels) ? '<uncategorized>' : A2($elm$core$String$join, ', ', labels);
};
var $elm_explorations$test$Test$Runner$Distribution$formatAsciiTable = F2(
	function (runsElapsed, items) {
		return A2(
			$elm_explorations$test$AsciiTable$view,
			_List_fromArray(
				[
					{
					align: $elm_explorations$test$AsciiTable$AlignLeft,
					toString: function (_v0) {
						var labels = _v0.a;
						return '  ' + ($elm_explorations$test$Test$Runner$Distribution$viewLabels(labels) + ':');
					}
				},
					{
					align: $elm_explorations$test$AsciiTable$AlignRight,
					toString: function (_v1) {
						var percentage = _v1.c;
						return $elm$core$String$fromFloat(percentage) + '%';
					}
				},
					{
					align: $elm_explorations$test$AsciiTable$AlignRight,
					toString: function (_v2) {
						var count = _v2.b;
						return '(' + ($elm$core$String$fromInt(count) + 'x)');
					}
				},
					{
					align: $elm_explorations$test$AsciiTable$AlignLeft,
					toString: function (_v3) {
						var count = _v3.b;
						return $elm_explorations$test$Test$Runner$Distribution$barView(
							{count: count, runsElapsed: runsElapsed});
					}
				}
				]),
			items);
	});
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.e.d.$ === 'RBNode_elm_builtin') && (dict.e.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.d.d.$ === 'RBNode_elm_builtin') && (dict.d.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Black')) {
					if (right.d.$ === 'RBNode_elm_builtin') {
						if (right.d.a.$ === 'Black') {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor.$ === 'Black') {
			if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === 'RBNode_elm_builtin') {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Black')) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === 'RBNode_elm_builtin') {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBNode_elm_builtin') {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === 'RBNode_elm_builtin') {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$diff = F2(
	function (t1, t2) {
		return A3(
			$elm$core$Dict$foldl,
			F3(
				function (k, v, t) {
					return A2($elm$core$Dict$remove, k, t);
				}),
			t1,
			t2);
	});
var $elm$core$Set$diff = F2(
	function (_v0, _v1) {
		var dict1 = _v0.a;
		var dict2 = _v1.a;
		return $elm$core$Set$Set_elm_builtin(
			A2($elm$core$Dict$diff, dict1, dict2));
	});
var $elm$core$Set$fromList = function (list) {
	return A3($elm$core$List$foldl, $elm$core$Set$insert, $elm$core$Set$empty, list);
};
var $elm$core$Basics$not = _Basics_not;
var $elm_explorations$test$Test$Runner$Distribution$isStrictSubset = F2(
	function (all, combination) {
		var combinationSet = $elm$core$Set$fromList(combination);
		var containsCombinationFully = function (set) {
			return (!$elm$core$Set$isEmpty(
				A2($elm$core$Set$diff, set, combinationSet))) && $elm$core$Set$isEmpty(
				A2($elm$core$Set$diff, combinationSet, set));
		};
		var allSets = A2(
			$elm$core$List$map,
			A2($elm$core$Basics$composeR, $elm$core$Tuple$first, $elm$core$Set$fromList),
			all);
		return A2($elm$core$List$any, containsCombinationFully, allSets);
	});
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$core$List$partition = F2(
	function (pred, list) {
		var step = F2(
			function (x, _v0) {
				var trues = _v0.a;
				var falses = _v0.b;
				return pred(x) ? _Utils_Tuple2(
					A2($elm$core$List$cons, x, trues),
					falses) : _Utils_Tuple2(
					trues,
					A2($elm$core$List$cons, x, falses));
			});
		return A3(
			$elm$core$List$foldr,
			step,
			_Utils_Tuple2(_List_Nil, _List_Nil),
			list);
	});
var $elm$core$String$replace = F3(
	function (before, after, string) {
		return A2(
			$elm$core$String$join,
			after,
			A2($elm$core$String$split, before, string));
	});
var $elm$core$List$sortBy = _List_sortBy;
var $elm_explorations$test$MicroListExtra$findIndexHelp = F3(
	function (index, predicate, list) {
		findIndexHelp:
		while (true) {
			if (!list.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var x = list.a;
				var xs = list.b;
				if (predicate(x)) {
					return $elm$core$Maybe$Just(index);
				} else {
					var $temp$index = index + 1,
						$temp$predicate = predicate,
						$temp$list = xs;
					index = $temp$index;
					predicate = $temp$predicate;
					list = $temp$list;
					continue findIndexHelp;
				}
			}
		}
	});
var $elm_explorations$test$MicroListExtra$findIndex = $elm_explorations$test$MicroListExtra$findIndexHelp(0);
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var $elm$core$List$takeReverse = F3(
	function (n, list, kept) {
		takeReverse:
		while (true) {
			if (n <= 0) {
				return kept;
			} else {
				if (!list.b) {
					return kept;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs,
						$temp$kept = A2($elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var $elm$core$List$takeTailRec = F2(
	function (n, list) {
		return $elm$core$List$reverse(
			A3($elm$core$List$takeReverse, n, list, _List_Nil));
	});
var $elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _v0 = _Utils_Tuple2(n, list);
			_v0$1:
			while (true) {
				_v0$5:
				while (true) {
					if (!_v0.b.b) {
						return list;
					} else {
						if (_v0.b.b.b) {
							switch (_v0.a) {
								case 1:
									break _v0$1;
								case 2:
									var _v2 = _v0.b;
									var x = _v2.a;
									var _v3 = _v2.b;
									var y = _v3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_v0.b.b.b.b) {
										var _v4 = _v0.b;
										var x = _v4.a;
										var _v5 = _v4.b;
										var y = _v5.a;
										var _v6 = _v5.b;
										var z = _v6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _v0$5;
									}
								default:
									if (_v0.b.b.b.b && _v0.b.b.b.b.b) {
										var _v7 = _v0.b;
										var x = _v7.a;
										var _v8 = _v7.b;
										var y = _v8.a;
										var _v9 = _v8.b;
										var z = _v9.a;
										var _v10 = _v9.b;
										var w = _v10.a;
										var tl = _v10.b;
										return (ctr > 1000) ? A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A2($elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A3($elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _v0$5;
									}
							}
						} else {
							if (_v0.a === 1) {
								break _v0$1;
							} else {
								break _v0$5;
							}
						}
					}
				}
				return list;
			}
			var _v1 = _v0.b;
			var x = _v1.a;
			return _List_fromArray(
				[x]);
		}
	});
var $elm$core$List$take = F2(
	function (n, list) {
		return A3($elm$core$List$takeFast, 0, n, list);
	});
var $elm_explorations$test$MicroListExtra$splitAt = F2(
	function (n, xs) {
		return _Utils_Tuple2(
			A2($elm$core$List$take, n, xs),
			A2($elm$core$List$drop, n, xs));
	});
var $elm_explorations$test$MicroListExtra$splitWhen = F2(
	function (predicate, list) {
		return A2(
			$elm$core$Maybe$map,
			function (i) {
				return A2($elm_explorations$test$MicroListExtra$splitAt, i, list);
			},
			A2($elm_explorations$test$MicroListExtra$findIndex, predicate, list));
	});
var $elm_explorations$test$Test$Runner$Distribution$formatTable = function (_v0) {
	var distributionCount = _v0.distributionCount;
	var runsElapsed = _v0.runsElapsed;
	var runsElapsed_ = runsElapsed;
	var distributionList = $elm$core$Dict$toList(distributionCount);
	var distribution = A2(
		$elm$core$List$map,
		function (_v8) {
			var labels = _v8.a;
			var count = _v8.b;
			var percentage = $elm$core$Basics$round((count / runsElapsed_) * 1000) / 10;
			return _Utils_Tuple3(labels, count, percentage);
		},
		A2(
			$elm$core$List$filter,
			function (_v7) {
				var labels = _v7.a;
				var count = _v7.b;
				return !(($elm$core$List$length(labels) === 1) && ((!count) && A2($elm_explorations$test$Test$Runner$Distribution$isStrictSubset, distributionList, labels)));
			},
			distributionList));
	var _v1 = A2(
		$elm$core$List$partition,
		function (_v3) {
			var labels = _v3.a;
			return $elm$core$List$length(labels) <= 1;
		},
		A2(
			$elm$core$List$sortBy,
			function (_v2) {
				var count = _v2.b;
				return -count;
			},
			distribution));
	var baseRows = _v1.a;
	var combinationsRows = _v1.b;
	var reorderedTable = _Utils_ap(baseRows, combinationsRows);
	var rawTable = A2($elm_explorations$test$Test$Runner$Distribution$formatAsciiTable, runsElapsed_, reorderedTable);
	var _v4 = A2(
		$elm$core$Maybe$withDefault,
		_Utils_Tuple2(rawTable, _List_Nil),
		A2(
			$elm_explorations$test$MicroListExtra$splitWhen,
			function (_v5) {
				var item = _v5.item;
				var _v6 = item;
				var labels = _v6.a;
				return $elm$core$List$length(labels) > 1;
			},
			rawTable));
	var base = _v4.a;
	var combinations = _v4.b;
	var baseString = A2(
		$elm$core$String$join,
		'\n',
		A2(
			$elm$core$List$map,
			function ($) {
				return $.renderedRow;
			},
			base));
	var combinationsString_ = $elm$core$List$isEmpty(combinations) ? '' : A3(
		$elm$core$String$replace,
		'{COMBINATIONS}',
		A2(
			$elm$core$String$join,
			'\n',
			A2(
				$elm$core$List$map,
				function ($) {
					return $.renderedRow;
				},
				combinations)),
		'\n\nCombinations (included in the above base counts):\n{COMBINATIONS}');
	var table = _Utils_ap(baseString, combinationsString_);
	return A3($elm$core$String$replace, '{CATEGORIES}', table, 'Distribution report:\n====================\n{CATEGORIES}');
};
var $elm_explorations$test$Test$Distribution$distributionReportTable = function (r) {
	return $elm_explorations$test$Test$Runner$Distribution$formatTable(r);
};
var $author$project$Test$Reporter$Console$distributionReportToString = function (distributionReport) {
	switch (distributionReport.$) {
		case 'NoDistribution':
			return $elm$core$Maybe$Nothing;
		case 'DistributionToReport':
			var r = distributionReport.a;
			return $elm$core$Maybe$Just(
				$elm_explorations$test$Test$Distribution$distributionReportTable(r));
		case 'DistributionCheckSucceeded':
			return $elm$core$Maybe$Nothing;
		default:
			var r = distributionReport.a;
			return $elm$core$Maybe$Just(
				$elm_explorations$test$Test$Distribution$distributionReportTable(r));
	}
};
var $author$project$Console$Text$Texts = function (a) {
	return {$: 'Texts', a: a};
};
var $author$project$Console$Text$concat = $author$project$Console$Text$Texts;
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $author$project$Console$Text$Dark = {$: 'Dark'};
var $author$project$Console$Text$dark = function (txt) {
	if (txt.$ === 'Text') {
		var styles = txt.a;
		var str = txt.b;
		return A2(
			$author$project$Console$Text$Text,
			_Utils_update(
				styles,
				{
					modifiers: A2($elm$core$List$cons, $author$project$Console$Text$Dark, styles.modifiers)
				}),
			str);
	} else {
		var texts = txt.a;
		return $author$project$Console$Text$Texts(
			A2($elm$core$List$map, $author$project$Console$Text$dark, texts));
	}
};
var $elm_explorations$test$Test$Runner$formatLabels = F3(
	function (formatDescription, formatTest, labels) {
		var _v0 = A2(
			$elm$core$List$filter,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, $elm$core$String$isEmpty),
			labels);
		if (!_v0.b) {
			return _List_Nil;
		} else {
			var test = _v0.a;
			var descriptions = _v0.b;
			return $elm$core$List$reverse(
				A2(
					$elm$core$List$cons,
					formatTest(test),
					A2($elm$core$List$map, formatDescription, descriptions)));
		}
	});
var $author$project$Console$Text$Red = {$: 'Red'};
var $author$project$Console$Text$red = $author$project$Console$Text$Text(
	{background: $author$project$Console$Text$Default, foreground: $author$project$Console$Text$Red, modifiers: _List_Nil, style: $author$project$Console$Text$Normal});
var $author$project$Test$Reporter$Console$withChar = F2(
	function (icon, str) {
		return $elm$core$String$fromChar(icon) + (' ' + (str + '\n'));
	});
var $author$project$Test$Reporter$Console$failureLabelsToText = A2(
	$elm$core$Basics$composeR,
	A2(
		$elm_explorations$test$Test$Runner$formatLabels,
		A2(
			$elm$core$Basics$composeL,
			A2($elm$core$Basics$composeL, $author$project$Console$Text$dark, $author$project$Console$Text$plain),
			$author$project$Test$Reporter$Console$withChar(
				_Utils_chr('↓'))),
		A2(
			$elm$core$Basics$composeL,
			$author$project$Console$Text$red,
			$author$project$Test$Reporter$Console$withChar(
				_Utils_chr('✗')))),
	$author$project$Console$Text$concat);
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (_v0.$ === 'Just') {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$core$Basics$always = F2(
	function (a, _v0) {
		return a;
	});
var $elm$core$Array$fromListHelp = F3(
	function (list, nodeList, nodeListSize) {
		fromListHelp:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, list);
			var jsArray = _v0.a;
			var remainingItems = _v0.b;
			if (_Utils_cmp(
				$elm$core$Elm$JsArray$length(jsArray),
				$elm$core$Array$branchFactor) < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					true,
					{nodeList: nodeList, nodeListSize: nodeListSize, tail: jsArray});
			} else {
				var $temp$list = remainingItems,
					$temp$nodeList = A2(
					$elm$core$List$cons,
					$elm$core$Array$Leaf(jsArray),
					nodeList),
					$temp$nodeListSize = nodeListSize + 1;
				list = $temp$list;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue fromListHelp;
			}
		}
	});
var $elm$core$Array$fromList = function (list) {
	if (!list.b) {
		return $elm$core$Array$empty;
	} else {
		return A3($elm$core$Array$fromListHelp, list, _List_Nil, 0);
	}
};
var $elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var $elm$core$Array$bitMask = 4294967295 >>> (32 - $elm$core$Array$shiftStep);
var $elm$core$Basics$ge = _Utils_ge;
var $elm$core$Elm$JsArray$unsafeGet = _JsArray_unsafeGet;
var $elm$core$Array$getHelp = F3(
	function (shift, index, tree) {
		getHelp:
		while (true) {
			var pos = $elm$core$Array$bitMask & (index >>> shift);
			var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (_v0.$ === 'SubTree') {
				var subTree = _v0.a;
				var $temp$shift = shift - $elm$core$Array$shiftStep,
					$temp$index = index,
					$temp$tree = subTree;
				shift = $temp$shift;
				index = $temp$index;
				tree = $temp$tree;
				continue getHelp;
			} else {
				var values = _v0.a;
				return A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, values);
			}
		}
	});
var $elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var $elm$core$Array$tailIndex = function (len) {
	return (len >>> 5) << 5;
};
var $elm$core$Array$get = F2(
	function (index, _v0) {
		var len = _v0.a;
		var startShift = _v0.b;
		var tree = _v0.c;
		var tail = _v0.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? $elm$core$Maybe$Nothing : ((_Utils_cmp(
			index,
			$elm$core$Array$tailIndex(len)) > -1) ? $elm$core$Maybe$Just(
			A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, tail)) : $elm$core$Maybe$Just(
			A3($elm$core$Array$getHelp, startShift, index, tree)));
	});
var $elm$core$Array$length = function (_v0) {
	var len = _v0.a;
	return len;
};
var $author$project$Test$Runner$Node$Vendor$Diff$Added = function (a) {
	return {$: 'Added', a: a};
};
var $author$project$Test$Runner$Node$Vendor$Diff$CannotGetA = function (a) {
	return {$: 'CannotGetA', a: a};
};
var $author$project$Test$Runner$Node$Vendor$Diff$CannotGetB = function (a) {
	return {$: 'CannotGetB', a: a};
};
var $author$project$Test$Runner$Node$Vendor$Diff$NoChange = function (a) {
	return {$: 'NoChange', a: a};
};
var $author$project$Test$Runner$Node$Vendor$Diff$Removed = function (a) {
	return {$: 'Removed', a: a};
};
var $author$project$Test$Runner$Node$Vendor$Diff$UnexpectedPath = F2(
	function (a, b) {
		return {$: 'UnexpectedPath', a: a, b: b};
	});
var $author$project$Test$Runner$Node$Vendor$Diff$makeChangesHelp = F5(
	function (changes, getA, getB, _v0, path) {
		makeChangesHelp:
		while (true) {
			var x = _v0.a;
			var y = _v0.b;
			if (!path.b) {
				return $elm$core$Result$Ok(changes);
			} else {
				var _v2 = path.a;
				var prevX = _v2.a;
				var prevY = _v2.b;
				var tail = path.b;
				var change = function () {
					if (_Utils_eq(x - 1, prevX) && _Utils_eq(y - 1, prevY)) {
						var _v4 = getA(x);
						if (_v4.$ === 'Just') {
							var a = _v4.a;
							return $elm$core$Result$Ok(
								$author$project$Test$Runner$Node$Vendor$Diff$NoChange(a));
						} else {
							return $elm$core$Result$Err(
								$author$project$Test$Runner$Node$Vendor$Diff$CannotGetA(x));
						}
					} else {
						if (_Utils_eq(x, prevX)) {
							var _v5 = getB(y);
							if (_v5.$ === 'Just') {
								var b = _v5.a;
								return $elm$core$Result$Ok(
									$author$project$Test$Runner$Node$Vendor$Diff$Added(b));
							} else {
								return $elm$core$Result$Err(
									$author$project$Test$Runner$Node$Vendor$Diff$CannotGetB(y));
							}
						} else {
							if (_Utils_eq(y, prevY)) {
								var _v6 = getA(x);
								if (_v6.$ === 'Just') {
									var a = _v6.a;
									return $elm$core$Result$Ok(
										$author$project$Test$Runner$Node$Vendor$Diff$Removed(a));
								} else {
									return $elm$core$Result$Err(
										$author$project$Test$Runner$Node$Vendor$Diff$CannotGetA(x));
								}
							} else {
								return $elm$core$Result$Err(
									A2(
										$author$project$Test$Runner$Node$Vendor$Diff$UnexpectedPath,
										_Utils_Tuple2(x, y),
										path));
							}
						}
					}
				}();
				if (change.$ === 'Err') {
					var err = change.a;
					return $elm$core$Result$Err(err);
				} else {
					var c = change.a;
					var $temp$changes = A2($elm$core$List$cons, c, changes),
						$temp$getA = getA,
						$temp$getB = getB,
						$temp$_v0 = _Utils_Tuple2(prevX, prevY),
						$temp$path = tail;
					changes = $temp$changes;
					getA = $temp$getA;
					getB = $temp$getB;
					_v0 = $temp$_v0;
					path = $temp$path;
					continue makeChangesHelp;
				}
			}
		}
	});
var $author$project$Test$Runner$Node$Vendor$Diff$makeChanges = F3(
	function (getA, getB, path) {
		if (!path.b) {
			return $elm$core$Result$Ok(_List_Nil);
		} else {
			var latest = path.a;
			var tail = path.b;
			return A5($author$project$Test$Runner$Node$Vendor$Diff$makeChangesHelp, _List_Nil, getA, getB, latest, tail);
		}
	});
var $author$project$Test$Runner$Node$Vendor$Diff$Continue = function (a) {
	return {$: 'Continue', a: a};
};
var $author$project$Test$Runner$Node$Vendor$Diff$Found = function (a) {
	return {$: 'Found', a: a};
};
var $elm$core$Elm$JsArray$unsafeSet = _JsArray_unsafeSet;
var $elm$core$Array$setHelp = F4(
	function (shift, index, value, tree) {
		var pos = $elm$core$Array$bitMask & (index >>> shift);
		var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
		if (_v0.$ === 'SubTree') {
			var subTree = _v0.a;
			var newSub = A4($elm$core$Array$setHelp, shift - $elm$core$Array$shiftStep, index, value, subTree);
			return A3(
				$elm$core$Elm$JsArray$unsafeSet,
				pos,
				$elm$core$Array$SubTree(newSub),
				tree);
		} else {
			var values = _v0.a;
			var newLeaf = A3($elm$core$Elm$JsArray$unsafeSet, $elm$core$Array$bitMask & index, value, values);
			return A3(
				$elm$core$Elm$JsArray$unsafeSet,
				pos,
				$elm$core$Array$Leaf(newLeaf),
				tree);
		}
	});
var $elm$core$Array$set = F3(
	function (index, value, array) {
		var len = array.a;
		var startShift = array.b;
		var tree = array.c;
		var tail = array.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? array : ((_Utils_cmp(
			index,
			$elm$core$Array$tailIndex(len)) > -1) ? A4(
			$elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			tree,
			A3($elm$core$Elm$JsArray$unsafeSet, $elm$core$Array$bitMask & index, value, tail)) : A4(
			$elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			A4($elm$core$Array$setHelp, startShift, index, value, tree),
			tail));
	});
var $author$project$Test$Runner$Node$Vendor$Diff$step = F4(
	function (snake_, offset, k, v) {
		var fromTop = A2(
			$elm$core$Maybe$withDefault,
			_List_Nil,
			A2($elm$core$Array$get, (k + 1) + offset, v));
		var fromLeft = A2(
			$elm$core$Maybe$withDefault,
			_List_Nil,
			A2($elm$core$Array$get, (k - 1) + offset, v));
		var _v0 = function () {
			var _v2 = _Utils_Tuple2(fromLeft, fromTop);
			if (!_v2.a.b) {
				if (!_v2.b.b) {
					return _Utils_Tuple2(
						_List_Nil,
						_Utils_Tuple2(0, 0));
				} else {
					var _v3 = _v2.b;
					var _v4 = _v3.a;
					var topX = _v4.a;
					var topY = _v4.b;
					return _Utils_Tuple2(
						fromTop,
						_Utils_Tuple2(topX + 1, topY));
				}
			} else {
				if (!_v2.b.b) {
					var _v5 = _v2.a;
					var _v6 = _v5.a;
					var leftX = _v6.a;
					var leftY = _v6.b;
					return _Utils_Tuple2(
						fromLeft,
						_Utils_Tuple2(leftX, leftY + 1));
				} else {
					var _v7 = _v2.a;
					var _v8 = _v7.a;
					var leftX = _v8.a;
					var leftY = _v8.b;
					var _v9 = _v2.b;
					var _v10 = _v9.a;
					var topX = _v10.a;
					var topY = _v10.b;
					return (_Utils_cmp(leftY + 1, topY) > -1) ? _Utils_Tuple2(
						fromLeft,
						_Utils_Tuple2(leftX, leftY + 1)) : _Utils_Tuple2(
						fromTop,
						_Utils_Tuple2(topX + 1, topY));
				}
			}
		}();
		var path = _v0.a;
		var _v1 = _v0.b;
		var x = _v1.a;
		var y = _v1.b;
		var _v11 = A3(
			snake_,
			x + 1,
			y + 1,
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(x, y),
				path));
		var newPath = _v11.a;
		var goal = _v11.b;
		return goal ? $author$project$Test$Runner$Node$Vendor$Diff$Found(newPath) : $author$project$Test$Runner$Node$Vendor$Diff$Continue(
			A3($elm$core$Array$set, k + offset, newPath, v));
	});
var $author$project$Test$Runner$Node$Vendor$Diff$onpLoopK = F4(
	function (snake_, offset, ks, v) {
		onpLoopK:
		while (true) {
			if (!ks.b) {
				return $author$project$Test$Runner$Node$Vendor$Diff$Continue(v);
			} else {
				var k = ks.a;
				var ks_ = ks.b;
				var _v1 = A4($author$project$Test$Runner$Node$Vendor$Diff$step, snake_, offset, k, v);
				if (_v1.$ === 'Found') {
					var path = _v1.a;
					return $author$project$Test$Runner$Node$Vendor$Diff$Found(path);
				} else {
					var v_ = _v1.a;
					var $temp$snake_ = snake_,
						$temp$offset = offset,
						$temp$ks = ks_,
						$temp$v = v_;
					snake_ = $temp$snake_;
					offset = $temp$offset;
					ks = $temp$ks;
					v = $temp$v;
					continue onpLoopK;
				}
			}
		}
	});
var $author$project$Test$Runner$Node$Vendor$Diff$onpLoopP = F5(
	function (snake_, delta, offset, p, v) {
		onpLoopP:
		while (true) {
			var ks = (delta > 0) ? _Utils_ap(
				$elm$core$List$reverse(
					A2($elm$core$List$range, delta + 1, delta + p)),
				A2($elm$core$List$range, -p, delta)) : _Utils_ap(
				$elm$core$List$reverse(
					A2($elm$core$List$range, delta + 1, p)),
				A2($elm$core$List$range, (-p) + delta, delta));
			var _v0 = A4($author$project$Test$Runner$Node$Vendor$Diff$onpLoopK, snake_, offset, ks, v);
			if (_v0.$ === 'Found') {
				var path = _v0.a;
				return path;
			} else {
				var v_ = _v0.a;
				var $temp$snake_ = snake_,
					$temp$delta = delta,
					$temp$offset = offset,
					$temp$p = p + 1,
					$temp$v = v_;
				snake_ = $temp$snake_;
				delta = $temp$delta;
				offset = $temp$offset;
				p = $temp$p;
				v = $temp$v;
				continue onpLoopP;
			}
		}
	});
var $author$project$Test$Runner$Node$Vendor$Diff$snake = F5(
	function (getA, getB, nextX, nextY, path) {
		snake:
		while (true) {
			var _v0 = _Utils_Tuple2(
				getA(nextX),
				getB(nextY));
			_v0$2:
			while (true) {
				if (_v0.a.$ === 'Just') {
					if (_v0.b.$ === 'Just') {
						var a = _v0.a.a;
						var b = _v0.b.a;
						if (_Utils_eq(a, b)) {
							var $temp$getA = getA,
								$temp$getB = getB,
								$temp$nextX = nextX + 1,
								$temp$nextY = nextY + 1,
								$temp$path = A2(
								$elm$core$List$cons,
								_Utils_Tuple2(nextX, nextY),
								path);
							getA = $temp$getA;
							getB = $temp$getB;
							nextX = $temp$nextX;
							nextY = $temp$nextY;
							path = $temp$path;
							continue snake;
						} else {
							return _Utils_Tuple2(path, false);
						}
					} else {
						break _v0$2;
					}
				} else {
					if (_v0.b.$ === 'Nothing') {
						var _v1 = _v0.a;
						var _v2 = _v0.b;
						return _Utils_Tuple2(path, true);
					} else {
						break _v0$2;
					}
				}
			}
			return _Utils_Tuple2(path, false);
		}
	});
var $author$project$Test$Runner$Node$Vendor$Diff$onp = F4(
	function (getA, getB, m, n) {
		var v = A2(
			$elm$core$Array$initialize,
			(m + n) + 1,
			$elm$core$Basics$always(_List_Nil));
		var delta = n - m;
		return A5(
			$author$project$Test$Runner$Node$Vendor$Diff$onpLoopP,
			A2($author$project$Test$Runner$Node$Vendor$Diff$snake, getA, getB),
			delta,
			m,
			0,
			v);
	});
var $author$project$Test$Runner$Node$Vendor$Diff$testDiff = F2(
	function (a, b) {
		var arrB = $elm$core$Array$fromList(b);
		var getB = function (y) {
			return A2($elm$core$Array$get, y - 1, arrB);
		};
		var n = $elm$core$Array$length(arrB);
		var arrA = $elm$core$Array$fromList(a);
		var getA = function (x) {
			return A2($elm$core$Array$get, x - 1, arrA);
		};
		var m = $elm$core$Array$length(arrA);
		var path = A4($author$project$Test$Runner$Node$Vendor$Diff$onp, getA, getB, m, n);
		return A3($author$project$Test$Runner$Node$Vendor$Diff$makeChanges, getA, getB, path);
	});
var $author$project$Test$Runner$Node$Vendor$Diff$diff = F2(
	function (a, b) {
		var _v0 = A2($author$project$Test$Runner$Node$Vendor$Diff$testDiff, a, b);
		if (_v0.$ === 'Ok') {
			var changes = _v0.a;
			return changes;
		} else {
			return _List_Nil;
		}
	});
var $author$project$Test$Reporter$Highlightable$Highlighted = function (a) {
	return {$: 'Highlighted', a: a};
};
var $author$project$Test$Reporter$Highlightable$Plain = function (a) {
	return {$: 'Plain', a: a};
};
var $author$project$Test$Reporter$Highlightable$fromDiff = function (diff) {
	switch (diff.$) {
		case 'Added':
			return _List_Nil;
		case 'Removed':
			var _char = diff.a;
			return _List_fromArray(
				[
					$author$project$Test$Reporter$Highlightable$Highlighted(_char)
				]);
		default:
			var _char = diff.a;
			return _List_fromArray(
				[
					$author$project$Test$Reporter$Highlightable$Plain(_char)
				]);
	}
};
var $author$project$Test$Reporter$Highlightable$diffLists = F2(
	function (expected, actual) {
		return A2(
			$elm$core$List$concatMap,
			$author$project$Test$Reporter$Highlightable$fromDiff,
			A2($author$project$Test$Runner$Node$Vendor$Diff$diff, expected, actual));
	});
var $elm$core$String$toFloat = _String_toFloat;
var $author$project$Test$Reporter$Console$Format$isFloat = function (str) {
	var _v0 = $elm$core$String$toFloat(str);
	if (_v0.$ === 'Just') {
		return true;
	} else {
		return false;
	}
};
var $author$project$Test$Reporter$Highlightable$map = F2(
	function (transform, highlightable) {
		if (highlightable.$ === 'Highlighted') {
			var val = highlightable.a;
			return $author$project$Test$Reporter$Highlightable$Highlighted(
				transform(val));
		} else {
			var val = highlightable.a;
			return $author$project$Test$Reporter$Highlightable$Plain(
				transform(val));
		}
	});
var $elm$core$Basics$neq = _Utils_notEqual;
var $elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var $author$project$Test$Reporter$Highlightable$resolve = F2(
	function (_v0, highlightable) {
		var fromPlain = _v0.fromPlain;
		var fromHighlighted = _v0.fromHighlighted;
		if (highlightable.$ === 'Highlighted') {
			var val = highlightable.a;
			return fromHighlighted(val);
		} else {
			var val = highlightable.a;
			return fromPlain(val);
		}
	});
var $elm$core$String$foldr = _String_foldr;
var $elm$core$String$toList = function (string) {
	return A3($elm$core$String$foldr, $elm$core$List$cons, _List_Nil, string);
};
var $author$project$Test$Reporter$Console$Format$highlightEqual = F2(
	function (expected, actual) {
		if ((expected === '\"\"') || (actual === '\"\"')) {
			return $elm$core$Maybe$Nothing;
		} else {
			if ($author$project$Test$Reporter$Console$Format$isFloat(expected) && $author$project$Test$Reporter$Console$Format$isFloat(actual)) {
				return $elm$core$Maybe$Nothing;
			} else {
				var isHighlighted = $author$project$Test$Reporter$Highlightable$resolve(
					{
						fromHighlighted: $elm$core$Basics$always(true),
						fromPlain: $elm$core$Basics$always(false)
					});
				var expectedChars = $elm$core$String$toList(expected);
				var edgeCount = function (highlightedString) {
					var highlights = A2($elm$core$List$map, isHighlighted, highlightedString);
					return $elm$core$List$length(
						A2(
							$elm$core$List$filter,
							function (_v0) {
								var lhs = _v0.a;
								var rhs = _v0.b;
								return !_Utils_eq(lhs, rhs);
							},
							A3(
								$elm$core$List$map2,
								$elm$core$Tuple$pair,
								A2($elm$core$List$drop, 1, highlights),
								highlights)));
				};
				var actualChars = $elm$core$String$toList(actual);
				var highlightedActual = A2(
					$elm$core$List$map,
					$author$project$Test$Reporter$Highlightable$map($elm$core$String$fromChar),
					A2($author$project$Test$Reporter$Highlightable$diffLists, actualChars, expectedChars));
				var highlightedExpected = A2(
					$elm$core$List$map,
					$author$project$Test$Reporter$Highlightable$map($elm$core$String$fromChar),
					A2($author$project$Test$Reporter$Highlightable$diffLists, expectedChars, actualChars));
				var plainCharCount = $elm$core$List$length(
					A2(
						$elm$core$List$filter,
						A2($elm$core$Basics$composeL, $elm$core$Basics$not, isHighlighted),
						highlightedExpected));
				return ((_Utils_cmp(
					edgeCount(highlightedActual),
					plainCharCount) > 0) || (_Utils_cmp(
					edgeCount(highlightedExpected),
					plainCharCount) > 0)) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
					_Utils_Tuple2(highlightedExpected, highlightedActual));
			}
		}
	});
var $author$project$Test$Reporter$Console$Format$verticalBar = F3(
	function (comparison, expected, actual) {
		return A2(
			$elm$core$String$join,
			'\n',
			_List_fromArray(
				[actual, '╷', '│ ' + comparison, '╵', expected]));
	});
var $author$project$Test$Reporter$Console$Format$listDiffToString = F4(
	function (index, description, _v0, originals) {
		listDiffToString:
		while (true) {
			var actual = _v0.actual;
			var expected = _v0.expected;
			var _v1 = _Utils_Tuple2(expected, actual);
			if (!_v1.a.b) {
				if (!_v1.b.b) {
					return A2(
						$elm$core$String$join,
						'',
						_List_fromArray(
							[
								'Two lists were unequal previously, yet ended up equal later.',
								'This should never happen!',
								'Please report this bug to https://github.com/elm-community/elm-test/issues - and include these lists: ',
								'\n',
								A2($elm$core$String$join, ', ', originals.originalExpected),
								'\n',
								A2($elm$core$String$join, ', ', originals.originalActual)
							]));
				} else {
					var _v3 = _v1.b;
					return A3(
						$author$project$Test$Reporter$Console$Format$verticalBar,
						description + ' was longer than',
						A2($elm$core$String$join, ', ', originals.originalExpected),
						A2($elm$core$String$join, ', ', originals.originalActual));
				}
			} else {
				if (!_v1.b.b) {
					var _v2 = _v1.a;
					return A3(
						$author$project$Test$Reporter$Console$Format$verticalBar,
						description + ' was shorter than',
						A2($elm$core$String$join, ', ', originals.originalExpected),
						A2($elm$core$String$join, ', ', originals.originalActual));
				} else {
					var _v4 = _v1.a;
					var firstExpected = _v4.a;
					var restExpected = _v4.b;
					var _v5 = _v1.b;
					var firstActual = _v5.a;
					var restActual = _v5.b;
					if (_Utils_eq(firstExpected, firstActual)) {
						var $temp$index = index + 1,
							$temp$description = description,
							$temp$_v0 = {actual: restActual, expected: restExpected},
							$temp$originals = originals;
						index = $temp$index;
						description = $temp$description;
						_v0 = $temp$_v0;
						originals = $temp$originals;
						continue listDiffToString;
					} else {
						return A2(
							$elm$core$String$join,
							'',
							_List_fromArray(
								[
									A3(
									$author$project$Test$Reporter$Console$Format$verticalBar,
									description,
									A2($elm$core$String$join, ', ', originals.originalExpected),
									A2($elm$core$String$join, ', ', originals.originalActual)),
									'\n\nThe first diff is at index ',
									$elm$core$String$fromInt(index),
									': it was `',
									firstActual,
									'`, but `',
									firstExpected,
									'` was expected.'
								]));
					}
				}
			}
		}
	});
var $author$project$Test$Reporter$Console$Format$format = F3(
	function (formatEquality, description, reason) {
		switch (reason.$) {
			case 'Custom':
				return description;
			case 'Equality':
				var expected = reason.a;
				var actual = reason.b;
				var _v1 = A2($author$project$Test$Reporter$Console$Format$highlightEqual, expected, actual);
				if (_v1.$ === 'Nothing') {
					return A3($author$project$Test$Reporter$Console$Format$verticalBar, description, expected, actual);
				} else {
					var _v2 = _v1.a;
					var highlightedExpected = _v2.a;
					var highlightedActual = _v2.b;
					var _v3 = A2(formatEquality, highlightedExpected, highlightedActual);
					var formattedExpected = _v3.a;
					var formattedActual = _v3.b;
					return A3($author$project$Test$Reporter$Console$Format$verticalBar, description, formattedExpected, formattedActual);
				}
			case 'Comparison':
				var first = reason.a;
				var second = reason.b;
				return A3($author$project$Test$Reporter$Console$Format$verticalBar, description, first, second);
			case 'TODO':
				return description;
			case 'Invalid':
				if (reason.a.$ === 'BadDescription') {
					var _v4 = reason.a;
					return (description === '') ? 'The empty string is not a valid test description.' : ('This is an invalid test description: ' + description);
				} else {
					return description;
				}
			case 'ListDiff':
				var expected = reason.a;
				var actual = reason.b;
				return A4(
					$author$project$Test$Reporter$Console$Format$listDiffToString,
					0,
					description,
					{actual: actual, expected: expected},
					{originalActual: actual, originalExpected: expected});
			default:
				var missing = reason.a.missing;
				var extra = reason.a.extra;
				var actual = reason.a.actual;
				var expected = reason.a.expected;
				var missingStr = $elm$core$List$isEmpty(missing) ? '' : ('\nThese keys are missing: ' + function (d) {
					return '[ ' + (d + ' ]');
				}(
					A2($elm$core$String$join, ', ', missing)));
				var extraStr = $elm$core$List$isEmpty(extra) ? '' : ('\nThese keys are extra: ' + function (d) {
					return '[ ' + (d + ' ]');
				}(
					A2($elm$core$String$join, ', ', extra)));
				return A2(
					$elm$core$String$join,
					'',
					_List_fromArray(
						[
							A3($author$project$Test$Reporter$Console$Format$verticalBar, description, expected, actual),
							'\n',
							extraStr,
							missingStr
						]));
		}
	});
var $author$project$Test$Reporter$Console$Format$Color$fromHighlightable = $author$project$Test$Reporter$Highlightable$resolve(
	{fromHighlighted: $author$project$Test$Runner$Node$Vendor$Console$colorsInverted, fromPlain: $elm$core$Basics$identity});
var $author$project$Test$Reporter$Console$Format$Color$formatEquality = F2(
	function (highlightedExpected, highlightedActual) {
		var formattedExpected = A2(
			$elm$core$String$join,
			'',
			A2($elm$core$List$map, $author$project$Test$Reporter$Console$Format$Color$fromHighlightable, highlightedExpected));
		var formattedActual = A2(
			$elm$core$String$join,
			'',
			A2($elm$core$List$map, $author$project$Test$Reporter$Console$Format$Color$fromHighlightable, highlightedActual));
		return _Utils_Tuple2(formattedExpected, formattedActual);
	});
var $author$project$Test$Reporter$Console$Format$Monochrome$fromHighlightable = function (indicator) {
	return $author$project$Test$Reporter$Highlightable$resolve(
		{
			fromHighlighted: function (_char) {
				return _Utils_Tuple2(_char, indicator);
			},
			fromPlain: function (_char) {
				return _Utils_Tuple2(_char, ' ');
			}
		});
};
var $elm$core$List$unzip = function (pairs) {
	var step = F2(
		function (_v0, _v1) {
			var x = _v0.a;
			var y = _v0.b;
			var xs = _v1.a;
			var ys = _v1.b;
			return _Utils_Tuple2(
				A2($elm$core$List$cons, x, xs),
				A2($elm$core$List$cons, y, ys));
		});
	return A3(
		$elm$core$List$foldr,
		step,
		_Utils_Tuple2(_List_Nil, _List_Nil),
		pairs);
};
var $author$project$Test$Reporter$Console$Format$Monochrome$formatEquality = F2(
	function (highlightedExpected, highlightedActual) {
		var _v0 = $elm$core$List$unzip(
			A2(
				$elm$core$List$map,
				$author$project$Test$Reporter$Console$Format$Monochrome$fromHighlightable('▲'),
				highlightedExpected));
		var formattedExpected = _v0.a;
		var expectedIndicators = _v0.b;
		var combinedExpected = A2(
			$elm$core$String$join,
			'\n',
			_List_fromArray(
				[
					A2($elm$core$String$join, '', formattedExpected),
					A2($elm$core$String$join, '', expectedIndicators)
				]));
		var _v1 = $elm$core$List$unzip(
			A2(
				$elm$core$List$map,
				$author$project$Test$Reporter$Console$Format$Monochrome$fromHighlightable('▼'),
				highlightedActual));
		var formattedActual = _v1.a;
		var actualIndicators = _v1.b;
		var combinedActual = A2(
			$elm$core$String$join,
			'\n',
			_List_fromArray(
				[
					A2($elm$core$String$join, '', actualIndicators),
					A2($elm$core$String$join, '', formattedActual)
				]));
		return _Utils_Tuple2(combinedExpected, combinedActual);
	});
var $author$project$Test$Reporter$Console$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n',
		A2(
			$elm$core$List$map,
			$elm$core$Basics$append('    '),
			A2($elm$core$String$split, '\n', str)));
};
var $author$project$Test$Reporter$Console$failureToText = F2(
	function (useColor, _v0) {
		var reason = _v0.a.reason;
		var description = _v0.a.description;
		var given = _v0.a.given;
		var distributionReport = _v0.b;
		var givenText = A2(
			$elm$core$Maybe$map,
			function (str) {
				return $author$project$Console$Text$dark(
					$author$project$Console$Text$plain('\nGiven ' + (str + '\n')));
			},
			given);
		var formatEquality = function () {
			if (useColor.$ === 'Monochrome') {
				return $author$project$Test$Reporter$Console$Format$Monochrome$formatEquality;
			} else {
				return $author$project$Test$Reporter$Console$Format$Color$formatEquality;
			}
		}();
		var messageText = $author$project$Console$Text$plain(
			'\n' + ($author$project$Test$Reporter$Console$indent(
				A3($author$project$Test$Reporter$Console$Format$format, formatEquality, description, reason)) + '\n\n'));
		var distributionText = A2(
			$elm$core$Maybe$map,
			function (str) {
				return $author$project$Console$Text$dark(
					$author$project$Console$Text$plain(
						'\n' + ($author$project$Test$Reporter$Console$indent(str) + '\n')));
			},
			$author$project$Test$Reporter$Console$distributionReportToString(distributionReport));
		return $author$project$Console$Text$concat(
			A2(
				$elm$core$List$filterMap,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						distributionText,
						givenText,
						$elm$core$Maybe$Just(messageText)
					])));
	});
var $author$project$Test$Reporter$Console$failuresToText = F3(
	function (useColor, labels, failures) {
		return $author$project$Console$Text$concat(
			A2(
				$elm$core$List$cons,
				$author$project$Test$Reporter$Console$failureLabelsToText(labels),
				A2(
					$elm$core$List$map,
					$author$project$Test$Reporter$Console$failureToText(useColor),
					failures)));
	});
var $author$project$Test$Reporter$Console$getStatus = function (outcome) {
	switch (outcome.$) {
		case 'Failed':
			return 'fail';
		case 'Todo':
			return 'todo';
		default:
			return 'pass';
	}
};
var $author$project$Console$Text$Green = {$: 'Green'};
var $author$project$Console$Text$green = $author$project$Console$Text$Text(
	{background: $author$project$Console$Text$Default, foreground: $author$project$Console$Text$Green, modifiers: _List_Nil, style: $author$project$Console$Text$Normal});
var $author$project$Test$Reporter$Console$passedLabelsToText = A2(
	$elm$core$Basics$composeR,
	A2(
		$elm_explorations$test$Test$Runner$formatLabels,
		A2(
			$elm$core$Basics$composeL,
			A2($elm$core$Basics$composeL, $author$project$Console$Text$dark, $author$project$Console$Text$plain),
			$author$project$Test$Reporter$Console$withChar(
				_Utils_chr('↓'))),
		A2(
			$elm$core$Basics$composeL,
			$author$project$Console$Text$green,
			$author$project$Test$Reporter$Console$withChar(
				_Utils_chr('✓')))),
	$author$project$Console$Text$concat);
var $author$project$Test$Reporter$Console$passedToText = F2(
	function (labels, distributionReport) {
		return $author$project$Console$Text$concat(
			_List_fromArray(
				[
					$author$project$Test$Reporter$Console$passedLabelsToText(labels),
					$author$project$Console$Text$dark(
					$author$project$Console$Text$plain(
						'\n' + ($author$project$Test$Reporter$Console$indent(distributionReport) + '\n\n')))
				]));
	});
var $author$project$Test$Reporter$Console$reportComplete = F2(
	function (useColor, _v0) {
		var outcome = _v0.outcome;
		var labels = _v0.labels;
		return $elm$json$Json$Encode$object(
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					'type',
					$elm$json$Json$Encode$string('complete')),
				A2(
					$elm$core$List$cons,
					_Utils_Tuple2(
						'status',
						$elm$json$Json$Encode$string(
							$author$project$Test$Reporter$Console$getStatus(outcome))),
					function () {
						switch (outcome.$) {
							case 'Passed':
								var distributionReport = outcome.a;
								var _v2 = $author$project$Test$Reporter$Console$distributionReportToString(distributionReport);
								if (_v2.$ === 'Nothing') {
									return _List_Nil;
								} else {
									var report = _v2.a;
									return _List_fromArray(
										[
											_Utils_Tuple2(
											'distributionReport',
											A2(
												$author$project$Test$Reporter$Console$textToValue,
												useColor,
												A2($author$project$Test$Reporter$Console$passedToText, labels, report)))
										]);
								}
							case 'Failed':
								var failures = outcome.a;
								return _List_fromArray(
									[
										_Utils_Tuple2(
										'failure',
										A2(
											$author$project$Test$Reporter$Console$textToValue,
											useColor,
											A3($author$project$Test$Reporter$Console$failuresToText, useColor, labels, failures)))
									]);
							default:
								var str = outcome.a;
								return _List_fromArray(
									[
										_Utils_Tuple2(
										'todo',
										$elm$json$Json$Encode$string(str)),
										_Utils_Tuple2(
										'labels',
										A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, labels))
									]);
						}
					}())));
	});
var $author$project$Test$Reporter$JUnit$encodeDuration = function (time) {
	return $elm$json$Json$Encode$string(
		$elm$core$String$fromFloat(time / 1000));
};
var $author$project$Test$Reporter$JUnit$distributionReportToString = function (distributionReport) {
	switch (distributionReport.$) {
		case 'NoDistribution':
			return $elm$core$Maybe$Nothing;
		case 'DistributionToReport':
			var r = distributionReport.a;
			return $elm$core$Maybe$Just(
				$elm_explorations$test$Test$Distribution$distributionReportTable(r));
		case 'DistributionCheckSucceeded':
			return $elm$core$Maybe$Nothing;
		default:
			var r = distributionReport.a;
			return $elm$core$Maybe$Just(
				$elm_explorations$test$Test$Distribution$distributionReportTable(r));
	}
};
var $author$project$Test$Reporter$JUnit$encodeDistributionReport = function (reportText) {
	return _Utils_Tuple2(
		'system-out',
		$elm$json$Json$Encode$string(reportText));
};
var $author$project$Test$Reporter$JUnit$encodeFailureTuple = function (message) {
	return _Utils_Tuple2(
		'failure',
		$elm$json$Json$Encode$string(message));
};
var $author$project$Test$Reporter$JUnit$reasonToString = F2(
	function (description, reason) {
		switch (reason.$) {
			case 'Custom':
				return description;
			case 'Equality':
				var expected = reason.a;
				var actual = reason.b;
				return expected + ('\n\nwas not equal to\n\n' + actual);
			case 'Comparison':
				var first = reason.a;
				var second = reason.b;
				return first + ('\n\nfailed when compared with ' + (description + (' on\n\n' + second)));
			case 'TODO':
				return 'TODO: ' + description;
			case 'Invalid':
				if (reason.a.$ === 'BadDescription') {
					var _v1 = reason.a;
					var explanation = (description === '') ? 'The empty string is not a valid test description.' : ('This is an invalid test description: ' + description);
					return 'Invalid test: ' + explanation;
				} else {
					return 'Invalid test: ' + description;
				}
			case 'ListDiff':
				var expected = reason.a;
				var actual = reason.b;
				return A2($elm$core$String$join, ', ', expected) + ('\n\nhad different elements than\n\n' + A2($elm$core$String$join, ', ', actual));
			default:
				var missing = reason.a.missing;
				var extra = reason.a.extra;
				var actual = reason.a.actual;
				var expected = reason.a.expected;
				return expected + ('\n\nhad different contents than\n\n' + (actual + ('\n\nthese were extra:\n\n' + (A2($elm$core$String$join, '\n', extra) + ('\n\nthese were missing:\n\n' + A2($elm$core$String$join, '\n', missing))))));
		}
	});
var $author$project$Test$Reporter$JUnit$formatFailure = function (_v0) {
	var reason = _v0.reason;
	var description = _v0.description;
	var given = _v0.given;
	var message = A2($author$project$Test$Reporter$JUnit$reasonToString, description, reason);
	if (given.$ === 'Just') {
		var str = given.a;
		return 'Given ' + (str + ('\n\n' + message));
	} else {
		return message;
	}
};
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $elm$core$List$singleton = function (value) {
	return _List_fromArray(
		[value]);
};
var $author$project$Test$Reporter$JUnit$encodeOutcome = function (outcome) {
	switch (outcome.$) {
		case 'Passed':
			var distributionReport = outcome.a;
			return A2(
				$elm$core$Maybe$withDefault,
				_List_Nil,
				A2(
					$elm$core$Maybe$map,
					A2($elm$core$Basics$composeR, $author$project$Test$Reporter$JUnit$encodeDistributionReport, $elm$core$List$singleton),
					$author$project$Test$Reporter$JUnit$distributionReportToString(distributionReport)));
		case 'Failed':
			var failures = outcome.a;
			var message = A2(
				$elm$core$String$join,
				'\n\n\n',
				A2(
					$elm$core$List$map,
					A2($elm$core$Basics$composeR, $elm$core$Tuple$first, $author$project$Test$Reporter$JUnit$formatFailure),
					failures));
			var distributionReports = A2(
				$elm$core$String$join,
				'\n\n\n',
				A2(
					$elm$core$List$filterMap,
					A2($elm$core$Basics$composeR, $elm$core$Tuple$second, $author$project$Test$Reporter$JUnit$distributionReportToString),
					failures));
			var nonemptyDistributionReports = $elm$core$String$isEmpty(distributionReports) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(distributionReports);
			return A2(
				$elm$core$List$filterMap,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						$elm$core$Maybe$Just(
						$author$project$Test$Reporter$JUnit$encodeFailureTuple(message)),
						A2($elm$core$Maybe$map, $author$project$Test$Reporter$JUnit$encodeDistributionReport, nonemptyDistributionReports)
					]));
		default:
			var message = outcome.a;
			return _List_fromArray(
				[
					$author$project$Test$Reporter$JUnit$encodeFailureTuple('TODO: ' + message)
				]);
	}
};
var $author$project$Test$Reporter$JUnit$formatClassAndName = function (labels) {
	if (labels.b) {
		var head = labels.a;
		var rest = labels.b;
		return _Utils_Tuple2(
			A2(
				$elm$core$String$join,
				' ',
				$elm$core$List$reverse(rest)),
			head);
	} else {
		return _Utils_Tuple2('', '');
	}
};
var $author$project$Test$Reporter$JUnit$reportComplete = function (_v0) {
	var outcome = _v0.outcome;
	var duration = _v0.duration;
	var labels = _v0.labels;
	var _v1 = $author$project$Test$Reporter$JUnit$formatClassAndName(labels);
	var classname = _v1.a;
	var name = _v1.b;
	return $elm$json$Json$Encode$object(
		_Utils_ap(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'@classname',
					$elm$json$Json$Encode$string(classname)),
					_Utils_Tuple2(
					'@name',
					$elm$json$Json$Encode$string(name)),
					_Utils_Tuple2(
					'@time',
					$author$project$Test$Reporter$JUnit$encodeDuration(duration))
				]),
			$author$project$Test$Reporter$JUnit$encodeOutcome(outcome)));
};
var $elm$json$Json$Encode$int = _Json_wrap;
var $author$project$Test$Reporter$Json$encodeDistributionCount = function (dict) {
	return A2(
		$elm$json$Json$Encode$list,
		function (_v0) {
			var labels = _v0.a;
			var count = _v0.b;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'labels',
						A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, labels)),
						_Utils_Tuple2(
						'count',
						$elm$json$Json$Encode$int(count))
					]));
		},
		$elm$core$Dict$toList(dict));
};
var $author$project$Test$Reporter$Json$encodeSumType = F2(
	function (sumType, data) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'type',
					$elm$json$Json$Encode$string(sumType)),
					_Utils_Tuple2('data', data)
				]));
	});
var $elm$json$Json$Encode$float = _Json_wrap;
var $elm$json$Json$Encode$null = _Json_encodeNull;
var $author$project$Test$Reporter$Json$encodeDistributionReport = function (distributionReport) {
	switch (distributionReport.$) {
		case 'NoDistribution':
			return A2($author$project$Test$Reporter$Json$encodeSumType, 'NoDistribution', $elm$json$Json$Encode$null);
		case 'DistributionToReport':
			var r = distributionReport.a;
			return A2(
				$author$project$Test$Reporter$Json$encodeSumType,
				'DistributionToReport',
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'distributionCount',
							$author$project$Test$Reporter$Json$encodeDistributionCount(r.distributionCount)),
							_Utils_Tuple2(
							'runsElapsed',
							$elm$json$Json$Encode$int(r.runsElapsed))
						])));
		case 'DistributionCheckSucceeded':
			var r = distributionReport.a;
			return A2(
				$author$project$Test$Reporter$Json$encodeSumType,
				'DistributionCheckSucceeded',
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'distributionCount',
							$author$project$Test$Reporter$Json$encodeDistributionCount(r.distributionCount)),
							_Utils_Tuple2(
							'runsElapsed',
							$elm$json$Json$Encode$int(r.runsElapsed))
						])));
		default:
			var r = distributionReport.a;
			return A2(
				$author$project$Test$Reporter$Json$encodeSumType,
				'DistributionCheckFailed',
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'distributionCount',
							$author$project$Test$Reporter$Json$encodeDistributionCount(r.distributionCount)),
							_Utils_Tuple2(
							'runsElapsed',
							$elm$json$Json$Encode$int(r.runsElapsed)),
							_Utils_Tuple2(
							'badLabel',
							$elm$json$Json$Encode$string(r.badLabel)),
							_Utils_Tuple2(
							'badLabelPercentage',
							$elm$json$Json$Encode$float(r.badLabelPercentage)),
							_Utils_Tuple2(
							'expectedDistribution',
							$elm$json$Json$Encode$string(r.expectedDistribution))
						])));
	}
};
var $author$project$Test$Reporter$Json$encodeDistributionReports = function (outcome) {
	switch (outcome.$) {
		case 'Failed':
			var failures = outcome.a;
			return A2(
				$elm$core$List$map,
				A2($elm$core$Basics$composeR, $elm$core$Tuple$second, $author$project$Test$Reporter$Json$encodeDistributionReport),
				failures);
		case 'Todo':
			return _List_Nil;
		default:
			var distributionReport = outcome.a;
			return _List_fromArray(
				[
					$author$project$Test$Reporter$Json$encodeDistributionReport(distributionReport)
				]);
	}
};
var $author$project$Test$Reporter$Json$encodeReason = F2(
	function (description, reason) {
		switch (reason.$) {
			case 'Custom':
				return A2(
					$author$project$Test$Reporter$Json$encodeSumType,
					'Custom',
					$elm$json$Json$Encode$string(description));
			case 'Equality':
				var expected = reason.a;
				var actual = reason.b;
				return A2(
					$author$project$Test$Reporter$Json$encodeSumType,
					'Equality',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'expected',
								$elm$json$Json$Encode$string(expected)),
								_Utils_Tuple2(
								'actual',
								$elm$json$Json$Encode$string(actual)),
								_Utils_Tuple2(
								'comparison',
								$elm$json$Json$Encode$string(description))
							])));
			case 'Comparison':
				var first = reason.a;
				var second = reason.b;
				return A2(
					$author$project$Test$Reporter$Json$encodeSumType,
					'Comparison',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'first',
								$elm$json$Json$Encode$string(first)),
								_Utils_Tuple2(
								'second',
								$elm$json$Json$Encode$string(second)),
								_Utils_Tuple2(
								'comparison',
								$elm$json$Json$Encode$string(description))
							])));
			case 'TODO':
				return A2(
					$author$project$Test$Reporter$Json$encodeSumType,
					'TODO',
					$elm$json$Json$Encode$string(description));
			case 'Invalid':
				if (reason.a.$ === 'BadDescription') {
					var _v1 = reason.a;
					var explanation = (description === '') ? 'The empty string is not a valid test description.' : ('This is an invalid test description: ' + description);
					return A2(
						$author$project$Test$Reporter$Json$encodeSumType,
						'Invalid',
						$elm$json$Json$Encode$string(explanation));
				} else {
					return A2(
						$author$project$Test$Reporter$Json$encodeSumType,
						'Invalid',
						$elm$json$Json$Encode$string(description));
				}
			case 'ListDiff':
				var expected = reason.a;
				var actual = reason.b;
				return A2(
					$author$project$Test$Reporter$Json$encodeSumType,
					'ListDiff',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'expected',
								A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, expected)),
								_Utils_Tuple2(
								'actual',
								A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, actual))
							])));
			default:
				var missing = reason.a.missing;
				var extra = reason.a.extra;
				var actual = reason.a.actual;
				var expected = reason.a.expected;
				return A2(
					$author$project$Test$Reporter$Json$encodeSumType,
					'CollectionDiff',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'expected',
								$elm$json$Json$Encode$string(expected)),
								_Utils_Tuple2(
								'actual',
								$elm$json$Json$Encode$string(actual)),
								_Utils_Tuple2(
								'extra',
								A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, extra)),
								_Utils_Tuple2(
								'missing',
								A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, missing))
							])));
		}
	});
var $author$project$Test$Reporter$Json$encodeFailure = function (_v0) {
	var reason = _v0.reason;
	var description = _v0.description;
	var given = _v0.given;
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'given',
				A2(
					$elm$core$Maybe$withDefault,
					$elm$json$Json$Encode$null,
					A2($elm$core$Maybe$map, $elm$json$Json$Encode$string, given))),
				_Utils_Tuple2(
				'message',
				$elm$json$Json$Encode$string(description)),
				_Utils_Tuple2(
				'reason',
				A2($author$project$Test$Reporter$Json$encodeReason, description, reason))
			]));
};
var $author$project$Test$Reporter$Json$encodeFailures = function (outcome) {
	switch (outcome.$) {
		case 'Failed':
			var failures = outcome.a;
			return A2(
				$elm$core$List$map,
				A2($elm$core$Basics$composeR, $elm$core$Tuple$first, $author$project$Test$Reporter$Json$encodeFailure),
				failures);
		case 'Todo':
			var str = outcome.a;
			return _List_fromArray(
				[
					$elm$json$Json$Encode$string(str)
				]);
		default:
			return _List_Nil;
	}
};
var $author$project$Test$Reporter$Json$encodeLabels = function (labels) {
	return A2(
		$elm$json$Json$Encode$list,
		$elm$json$Json$Encode$string,
		$elm$core$List$reverse(labels));
};
var $author$project$Test$Reporter$Json$getStatus = function (outcome) {
	switch (outcome.$) {
		case 'Failed':
			return 'fail';
		case 'Todo':
			return 'todo';
		default:
			return 'pass';
	}
};
var $author$project$Test$Reporter$Json$reportComplete = function (_v0) {
	var outcome = _v0.outcome;
	var labels = _v0.labels;
	var duration = _v0.duration;
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'event',
				$elm$json$Json$Encode$string('testCompleted')),
				_Utils_Tuple2(
				'status',
				$elm$json$Json$Encode$string(
					$author$project$Test$Reporter$Json$getStatus(outcome))),
				_Utils_Tuple2(
				'labels',
				$author$project$Test$Reporter$Json$encodeLabels(labels)),
				_Utils_Tuple2(
				'failures',
				A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					$author$project$Test$Reporter$Json$encodeFailures(outcome))),
				_Utils_Tuple2(
				'distributionReports',
				A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					$author$project$Test$Reporter$Json$encodeDistributionReports(outcome))),
				_Utils_Tuple2(
				'duration',
				$elm$json$Json$Encode$string(
					$elm$core$String$fromInt(duration)))
			]));
};
var $author$project$Test$Reporter$Console$formatDuration = function (time) {
	return $elm$core$String$fromFloat(time) + ' ms';
};
var $author$project$Test$Reporter$Console$stat = F2(
	function (label, value) {
		return $author$project$Console$Text$concat(
			_List_fromArray(
				[
					$author$project$Console$Text$dark(
					$author$project$Console$Text$plain(label)),
					$author$project$Console$Text$plain(value + '\n')
				]));
	});
var $author$project$Test$Reporter$Console$todoLabelsToText = A2(
	$elm$core$Basics$composeR,
	A2(
		$elm_explorations$test$Test$Runner$formatLabels,
		A2(
			$elm$core$Basics$composeL,
			A2($elm$core$Basics$composeL, $author$project$Console$Text$dark, $author$project$Console$Text$plain),
			$author$project$Test$Reporter$Console$withChar(
				_Utils_chr('↓'))),
		A2(
			$elm$core$Basics$composeL,
			A2($elm$core$Basics$composeL, $author$project$Console$Text$dark, $author$project$Console$Text$plain),
			$author$project$Test$Reporter$Console$withChar(
				_Utils_chr('↓')))),
	$author$project$Console$Text$concat);
var $author$project$Test$Reporter$Console$todoToChalk = function (message) {
	return $author$project$Console$Text$plain('◦ TODO: ' + (message + '\n\n'));
};
var $author$project$Test$Reporter$Console$todosToText = function (_v0) {
	var labels = _v0.a;
	var failure = _v0.b;
	return $author$project$Console$Text$concat(
		_List_fromArray(
			[
				$author$project$Test$Reporter$Console$todoLabelsToText(labels),
				$author$project$Test$Reporter$Console$todoToChalk(failure)
			]));
};
var $author$project$Test$Reporter$Console$summarizeTodos = A2(
	$elm$core$Basics$composeR,
	$elm$core$List$map($author$project$Test$Reporter$Console$todosToText),
	$author$project$Console$Text$concat);
var $author$project$Console$Text$Underline = {$: 'Underline'};
var $author$project$Console$Text$underline = function (txt) {
	if (txt.$ === 'Text') {
		var styles = txt.a;
		var str = txt.b;
		return A2(
			$author$project$Console$Text$Text,
			_Utils_update(
				styles,
				{style: $author$project$Console$Text$Underline}),
			str);
	} else {
		var texts = txt.a;
		return $author$project$Console$Text$Texts(
			A2($elm$core$List$map, $author$project$Console$Text$dark, texts));
	}
};
var $author$project$Console$Text$Yellow = {$: 'Yellow'};
var $author$project$Console$Text$yellow = $author$project$Console$Text$Text(
	{background: $author$project$Console$Text$Default, foreground: $author$project$Console$Text$Yellow, modifiers: _List_Nil, style: $author$project$Console$Text$Normal});
var $author$project$Test$Reporter$Console$reportSummary = F3(
	function (useColor, _v0, autoFail) {
		var duration = _v0.duration;
		var failed = _v0.failed;
		var passed = _v0.passed;
		var todos = _v0.todos;
		var todoStats = function () {
			var _v7 = $elm$core$List$length(todos);
			if (!_v7) {
				return $author$project$Console$Text$plain('');
			} else {
				var numTodos = _v7;
				return A2(
					$author$project$Test$Reporter$Console$stat,
					'Todo:     ',
					$elm$core$String$fromInt(numTodos));
			}
		}();
		var individualTodos = (failed > 0) ? $author$project$Console$Text$plain('') : $author$project$Test$Reporter$Console$summarizeTodos(
			$elm$core$List$reverse(todos));
		var headlineResult = function () {
			var _v3 = _Utils_Tuple3(
				autoFail,
				failed,
				$elm$core$List$length(todos));
			_v3$4:
			while (true) {
				if (_v3.a.$ === 'Nothing') {
					if (!_v3.b) {
						switch (_v3.c) {
							case 0:
								var _v4 = _v3.a;
								return $elm$core$Result$Ok('TEST RUN PASSED');
							case 1:
								var _v5 = _v3.a;
								return $elm$core$Result$Err(
									_Utils_Tuple3($author$project$Console$Text$yellow, 'TEST RUN INCOMPLETE', ' because there is 1 TODO remaining'));
							default:
								var _v6 = _v3.a;
								var numTodos = _v3.c;
								return $elm$core$Result$Err(
									_Utils_Tuple3(
										$author$project$Console$Text$yellow,
										'TEST RUN INCOMPLETE',
										' because there are ' + ($elm$core$String$fromInt(numTodos) + ' TODOs remaining')));
						}
					} else {
						break _v3$4;
					}
				} else {
					if (!_v3.b) {
						var failure = _v3.a.a;
						return $elm$core$Result$Err(
							_Utils_Tuple3($author$project$Console$Text$yellow, 'TEST RUN INCOMPLETE', ' because ' + failure));
					} else {
						break _v3$4;
					}
				}
			}
			return $elm$core$Result$Err(
				_Utils_Tuple3($author$project$Console$Text$red, 'TEST RUN FAILED', ''));
		}();
		var headline = function () {
			if (headlineResult.$ === 'Ok') {
				var str = headlineResult.a;
				return $author$project$Console$Text$underline(
					$author$project$Console$Text$green('\n' + (str + '\n\n')));
			} else {
				var _v2 = headlineResult.a;
				var colorize = _v2.a;
				var str = _v2.b;
				var suffix = _v2.c;
				return $author$project$Console$Text$concat(
					_List_fromArray(
						[
							$author$project$Console$Text$underline(
							colorize('\n' + str)),
							colorize(suffix + '\n\n')
						]));
			}
		}();
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'type',
					$elm$json$Json$Encode$string('summary')),
					_Utils_Tuple2(
					'summary',
					$elm$json$Json$Encode$string(
						A2(
							$author$project$Console$Text$render,
							useColor,
							$author$project$Console$Text$concat(
								_List_fromArray(
									[
										headline,
										A2(
										$author$project$Test$Reporter$Console$stat,
										'Duration: ',
										$author$project$Test$Reporter$Console$formatDuration(duration)),
										A2(
										$author$project$Test$Reporter$Console$stat,
										'Passed:   ',
										$elm$core$String$fromInt(passed)),
										A2(
										$author$project$Test$Reporter$Console$stat,
										'Failed:   ',
										$elm$core$String$fromInt(failed)),
										todoStats,
										individualTodos
									])))))
				]));
	});
var $author$project$Test$Reporter$TestResults$Failed = function (a) {
	return {$: 'Failed', a: a};
};
var $author$project$Test$Reporter$JUnit$encodeExtraFailure = function (_v0) {
	return $author$project$Test$Reporter$JUnit$reportComplete(
		{
			duration: 0,
			labels: _List_Nil,
			outcome: $author$project$Test$Reporter$TestResults$Failed(_List_Nil)
		});
};
var $author$project$Test$Reporter$JUnit$reportSummary = F2(
	function (_v0, autoFail) {
		var failed = _v0.failed;
		var duration = _v0.duration;
		var testCount = _v0.testCount;
		var extraFailures = function () {
			var _v1 = _Utils_Tuple2(failed, autoFail);
			if ((!_v1.a) && (_v1.b.$ === 'Just')) {
				var failure = _v1.b.a;
				return _List_fromArray(
					[
						$author$project$Test$Reporter$JUnit$encodeExtraFailure(failure)
					]);
			} else {
				return _List_Nil;
			}
		}();
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'testsuite',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'@name',
								$elm$json$Json$Encode$string('elm-test')),
								_Utils_Tuple2(
								'@package',
								$elm$json$Json$Encode$string('elm-test')),
								_Utils_Tuple2(
								'@tests',
								$elm$json$Json$Encode$int(testCount)),
								_Utils_Tuple2(
								'@failures',
								$elm$json$Json$Encode$int(failed)),
								_Utils_Tuple2(
								'@errors',
								$elm$json$Json$Encode$int(0)),
								_Utils_Tuple2(
								'@time',
								$elm$json$Json$Encode$float(duration)),
								_Utils_Tuple2(
								'testcase',
								A2($elm$json$Json$Encode$list, $elm$core$Basics$identity, extraFailures))
							])))
				]));
	});
var $author$project$Test$Reporter$Json$reportSummary = F2(
	function (_v0, autoFail) {
		var failed = _v0.failed;
		var passed = _v0.passed;
		var duration = _v0.duration;
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'event',
					$elm$json$Json$Encode$string('runComplete')),
					_Utils_Tuple2(
					'passed',
					$elm$json$Json$Encode$string(
						$elm$core$String$fromInt(passed))),
					_Utils_Tuple2(
					'failed',
					$elm$json$Json$Encode$string(
						$elm$core$String$fromInt(failed))),
					_Utils_Tuple2(
					'duration',
					$elm$json$Json$Encode$string(
						$elm$core$String$fromFloat(duration))),
					_Utils_Tuple2(
					'autoFail',
					A2(
						$elm$core$Maybe$withDefault,
						$elm$json$Json$Encode$null,
						A2($elm$core$Maybe$map, $elm$json$Json$Encode$string, autoFail)))
				]));
	});
var $author$project$Test$Reporter$Reporter$createReporter = function (report) {
	switch (report.$) {
		case 'JsonReport':
			return A4($author$project$Test$Reporter$Reporter$TestReporter, 'JSON', $author$project$Test$Reporter$Json$reportBegin, $author$project$Test$Reporter$Json$reportComplete, $author$project$Test$Reporter$Json$reportSummary);
		case 'ConsoleReport':
			var useColor = report.a;
			return A4(
				$author$project$Test$Reporter$Reporter$TestReporter,
				'CHALK',
				$author$project$Test$Reporter$Console$reportBegin(useColor),
				$author$project$Test$Reporter$Console$reportComplete(useColor),
				$author$project$Test$Reporter$Console$reportSummary(useColor));
		default:
			return A4($author$project$Test$Reporter$Reporter$TestReporter, 'JUNIT', $author$project$Test$Reporter$JUnit$reportBegin, $author$project$Test$Reporter$JUnit$reportComplete, $author$project$Test$Reporter$JUnit$reportSummary);
	}
};
var $author$project$Test$Runner$Node$elmTestPort__send = _Platform_outgoingPort('elmTestPort__send', $elm$json$Json$Encode$string);
var $author$project$Test$Runner$Node$failInit = F3(
	function (message, report, _v0) {
		var model = {
			autoFail: $elm$core$Maybe$Nothing,
			available: $elm$core$Dict$empty,
			nextTestToRun: 0,
			processes: 0,
			results: _List_Nil,
			runInfo: {fuzzRuns: 0, globs: _List_Nil, initialSeed: 0, paths: _List_Nil, testCount: 0},
			testReporter: $author$project$Test$Reporter$Reporter$createReporter(report)
		};
		var cmd = $author$project$Test$Runner$Node$elmTestPort__send(
			A2(
				$elm$json$Json$Encode$encode,
				0,
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'type',
							$elm$json$Json$Encode$string('SUMMARY')),
							_Utils_Tuple2(
							'exitCode',
							$elm$json$Json$Encode$int(1)),
							_Utils_Tuple2(
							'message',
							$elm$json$Json$Encode$string(message))
						]))));
		return _Utils_Tuple2(model, cmd);
	});
var $elm_explorations$test$Test$Runner$Invalid = function (a) {
	return {$: 'Invalid', a: a};
};
var $elm_explorations$test$Test$Runner$Only = function (a) {
	return {$: 'Only', a: a};
};
var $elm_explorations$test$Test$Runner$Plain = function (a) {
	return {$: 'Plain', a: a};
};
var $elm_explorations$test$Test$Runner$Skipping = function (a) {
	return {$: 'Skipping', a: a};
};
var $elm_explorations$test$Test$Runner$countRunnables = function (runnable) {
	countRunnables:
	while (true) {
		if (runnable.$ === 'Runnable') {
			return 1;
		} else {
			var runner = runnable.b;
			var $temp$runnable = runner;
			runnable = $temp$runnable;
			continue countRunnables;
		}
	}
};
var $elm_explorations$test$Test$Runner$countAllRunnables = A2(
	$elm$core$List$foldl,
	A2($elm$core$Basics$composeR, $elm_explorations$test$Test$Runner$countRunnables, $elm$core$Basics$add),
	0);
var $elm_explorations$test$Test$Runner$Labeled = F2(
	function (a, b) {
		return {$: 'Labeled', a: a, b: b};
	});
var $elm_explorations$test$Test$Runner$Runnable = function (a) {
	return {$: 'Runnable', a: a};
};
var $elm_explorations$test$Test$Runner$Thunk = function (a) {
	return {$: 'Thunk', a: a};
};
var $elm_explorations$test$Test$Runner$emptyDistribution = function (seed) {
	return {all: _List_Nil, only: _List_Nil, seed: seed, skipped: _List_Nil};
};
var $elm$core$Bitwise$xor = _Bitwise_xor;
var $elm_explorations$test$Test$Runner$fnvHash = F2(
	function (a, b) {
		return ((a ^ b) * 16777619) >>> 0;
	});
var $elm_explorations$test$Test$Runner$fnvHashString = F2(
	function (hash, str) {
		return A3(
			$elm$core$List$foldl,
			$elm_explorations$test$Test$Runner$fnvHash,
			hash,
			A2(
				$elm$core$List$map,
				$elm$core$Char$toCode,
				$elm$core$String$toList(str)));
	});
var $elm_explorations$test$Test$Runner$fnvInit = 2166136261;
var $elm$random$Random$Generator = function (a) {
	return {$: 'Generator', a: a};
};
var $elm$random$Random$Seed = F2(
	function (a, b) {
		return {$: 'Seed', a: a, b: b};
	});
var $elm$random$Random$next = function (_v0) {
	var state0 = _v0.a;
	var incr = _v0.b;
	return A2($elm$random$Random$Seed, ((state0 * 1664525) + incr) >>> 0, incr);
};
var $elm$random$Random$peel = function (_v0) {
	var state = _v0.a;
	var word = (state ^ (state >>> ((state >>> 28) + 4))) * 277803737;
	return ((word >>> 22) ^ word) >>> 0;
};
var $elm$random$Random$int = F2(
	function (a, b) {
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v0 = (_Utils_cmp(a, b) < 0) ? _Utils_Tuple2(a, b) : _Utils_Tuple2(b, a);
				var lo = _v0.a;
				var hi = _v0.b;
				var range = (hi - lo) + 1;
				if (!((range - 1) & range)) {
					return _Utils_Tuple2(
						(((range - 1) & $elm$random$Random$peel(seed0)) >>> 0) + lo,
						$elm$random$Random$next(seed0));
				} else {
					var threshhold = (((-range) >>> 0) % range) >>> 0;
					var accountForBias = function (seed) {
						accountForBias:
						while (true) {
							var x = $elm$random$Random$peel(seed);
							var seedN = $elm$random$Random$next(seed);
							if (_Utils_cmp(x, threshhold) < 0) {
								var $temp$seed = seedN;
								seed = $temp$seed;
								continue accountForBias;
							} else {
								return _Utils_Tuple2((x % range) + lo, seedN);
							}
						}
					};
					return accountForBias(seed0);
				}
			});
	});
var $elm$random$Random$map3 = F4(
	function (func, _v0, _v1, _v2) {
		var genA = _v0.a;
		var genB = _v1.a;
		var genC = _v2.a;
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v3 = genA(seed0);
				var a = _v3.a;
				var seed1 = _v3.b;
				var _v4 = genB(seed1);
				var b = _v4.a;
				var seed2 = _v4.b;
				var _v5 = genC(seed2);
				var c = _v5.a;
				var seed3 = _v5.b;
				return _Utils_Tuple2(
					A3(func, a, b, c),
					seed3);
			});
	});
var $elm$core$Bitwise$or = _Bitwise_or;
var $elm$random$Random$step = F2(
	function (_v0, seed) {
		var generator = _v0.a;
		return generator(seed);
	});
var $elm$random$Random$independentSeed = $elm$random$Random$Generator(
	function (seed0) {
		var makeIndependentSeed = F3(
			function (state, b, c) {
				return $elm$random$Random$next(
					A2($elm$random$Random$Seed, state, (1 | (b ^ c)) >>> 0));
			});
		var gen = A2($elm$random$Random$int, 0, 4294967295);
		return A2(
			$elm$random$Random$step,
			A4($elm$random$Random$map3, makeIndependentSeed, gen, gen, gen),
			seed0);
	});
var $elm$random$Random$initialSeed = function (x) {
	var _v0 = $elm$random$Random$next(
		A2($elm$random$Random$Seed, 0, 1013904223));
	var state1 = _v0.a;
	var incr = _v0.b;
	var state2 = (state1 + x) >>> 0;
	return $elm$random$Random$next(
		A2($elm$random$Random$Seed, state2, incr));
};
var $elm$random$Random$maxInt = 2147483647;
var $elm_explorations$test$Test$Runner$batchDistribute = F4(
	function (hashed, runs, test, prev) {
		var next = A4($elm_explorations$test$Test$Runner$distributeSeedsHelp, hashed, runs, prev.seed, test);
		return {
			all: _Utils_ap(prev.all, next.all),
			only: _Utils_ap(prev.only, next.only),
			seed: next.seed,
			skipped: _Utils_ap(prev.skipped, next.skipped)
		};
	});
var $elm_explorations$test$Test$Runner$distributeSeedsHelp = F4(
	function (hashed, runs, seed, test) {
		switch (test.$) {
			case 'ElmTestVariant__UnitTest':
				var aRun = test.a;
				return {
					all: _List_fromArray(
						[
							$elm_explorations$test$Test$Runner$Runnable(
							$elm_explorations$test$Test$Runner$Thunk(
								function (_v1) {
									return aRun(_Utils_Tuple0);
								}))
						]),
					only: _List_Nil,
					seed: seed,
					skipped: _List_Nil
				};
			case 'ElmTestVariant__FuzzTest':
				var aRun = test.a;
				var _v2 = A2($elm$random$Random$step, $elm$random$Random$independentSeed, seed);
				var firstSeed = _v2.a;
				var nextSeed = _v2.b;
				return {
					all: _List_fromArray(
						[
							$elm_explorations$test$Test$Runner$Runnable(
							$elm_explorations$test$Test$Runner$Thunk(
								function (_v3) {
									return A2(aRun, firstSeed, runs);
								}))
						]),
					only: _List_Nil,
					seed: nextSeed,
					skipped: _List_Nil
				};
			case 'ElmTestVariant__Labeled':
				var description = test.a;
				var subTest = test.b;
				if (hashed) {
					var next = A4($elm_explorations$test$Test$Runner$distributeSeedsHelp, true, runs, seed, subTest);
					return {
						all: A2(
							$elm$core$List$map,
							$elm_explorations$test$Test$Runner$Labeled(description),
							next.all),
						only: A2(
							$elm$core$List$map,
							$elm_explorations$test$Test$Runner$Labeled(description),
							next.only),
						seed: next.seed,
						skipped: A2(
							$elm$core$List$map,
							$elm_explorations$test$Test$Runner$Labeled(description),
							next.skipped)
					};
				} else {
					var intFromSeed = A2(
						$elm$random$Random$step,
						A2($elm$random$Random$int, 0, $elm$random$Random$maxInt),
						seed).a;
					var hashedSeed = $elm$random$Random$initialSeed(
						A2(
							$elm_explorations$test$Test$Runner$fnvHash,
							intFromSeed,
							A2($elm_explorations$test$Test$Runner$fnvHashString, $elm_explorations$test$Test$Runner$fnvInit, description)));
					var next = A4($elm_explorations$test$Test$Runner$distributeSeedsHelp, true, runs, hashedSeed, subTest);
					return {
						all: A2(
							$elm$core$List$map,
							$elm_explorations$test$Test$Runner$Labeled(description),
							next.all),
						only: A2(
							$elm$core$List$map,
							$elm_explorations$test$Test$Runner$Labeled(description),
							next.only),
						seed: seed,
						skipped: A2(
							$elm$core$List$map,
							$elm_explorations$test$Test$Runner$Labeled(description),
							next.skipped)
					};
				}
			case 'ElmTestVariant__Skipped':
				var subTest = test.a;
				var next = A4($elm_explorations$test$Test$Runner$distributeSeedsHelp, hashed, runs, seed, subTest);
				return {all: _List_Nil, only: _List_Nil, seed: next.seed, skipped: next.all};
			case 'ElmTestVariant__Only':
				var subTest = test.a;
				var next = A4($elm_explorations$test$Test$Runner$distributeSeedsHelp, hashed, runs, seed, subTest);
				return _Utils_update(
					next,
					{only: next.all});
			default:
				var tests = test.a;
				return A3(
					$elm$core$List$foldl,
					A2($elm_explorations$test$Test$Runner$batchDistribute, hashed, runs),
					$elm_explorations$test$Test$Runner$emptyDistribution(seed),
					tests);
		}
	});
var $elm_explorations$test$Test$Runner$distributeSeeds = $elm_explorations$test$Test$Runner$distributeSeedsHelp(false);
var $elm_explorations$test$Test$Runner$Failure$Custom = {$: 'Custom'};
var $elm_explorations$test$Expect$fail = function (str) {
	return $elm_explorations$test$Test$Expectation$fail(
		{description: str, reason: $elm_explorations$test$Test$Runner$Failure$Custom});
};
var $elm_explorations$test$Test$Runner$runThunk = _Test_runThunk;
var $elm_explorations$test$Test$Runner$run = function (_v0) {
	var fn = _v0.a;
	var _v1 = $elm_explorations$test$Test$Runner$runThunk(fn);
	if (_v1.$ === 'Ok') {
		var test = _v1.a;
		return test;
	} else {
		var message = _v1.a;
		return _List_fromArray(
			[
				$elm_explorations$test$Expect$fail('This test failed because it threw an exception: \"' + (message + '\"'))
			]);
	}
};
var $elm_explorations$test$Test$Runner$fromRunnableTreeHelp = F2(
	function (labels, runner) {
		fromRunnableTreeHelp:
		while (true) {
			if (runner.$ === 'Runnable') {
				var runnable = runner.a;
				return _List_fromArray(
					[
						{
						labels: labels,
						run: function (_v1) {
							return $elm_explorations$test$Test$Runner$run(runnable);
						}
					}
					]);
			} else {
				var label = runner.a;
				var subRunner = runner.b;
				var $temp$labels = A2($elm$core$List$cons, label, labels),
					$temp$runner = subRunner;
				labels = $temp$labels;
				runner = $temp$runner;
				continue fromRunnableTreeHelp;
			}
		}
	});
var $elm_explorations$test$Test$Runner$fromRunnableTree = $elm_explorations$test$Test$Runner$fromRunnableTreeHelp(_List_Nil);
var $elm_explorations$test$Test$Runner$fromTest = F3(
	function (runs, seed, test) {
		if (runs < 1) {
			return $elm_explorations$test$Test$Runner$Invalid(
				'Test runner run count must be at least 1, not ' + $elm$core$String$fromInt(runs));
		} else {
			var distribution = A3($elm_explorations$test$Test$Runner$distributeSeeds, runs, seed, test);
			return $elm$core$List$isEmpty(distribution.only) ? ((!$elm_explorations$test$Test$Runner$countAllRunnables(distribution.skipped)) ? $elm_explorations$test$Test$Runner$Plain(
				A2($elm$core$List$concatMap, $elm_explorations$test$Test$Runner$fromRunnableTree, distribution.all)) : $elm_explorations$test$Test$Runner$Skipping(
				A2($elm$core$List$concatMap, $elm_explorations$test$Test$Runner$fromRunnableTree, distribution.all))) : $elm_explorations$test$Test$Runner$Only(
				A2($elm$core$List$concatMap, $elm_explorations$test$Test$Runner$fromRunnableTree, distribution.only));
		}
	});
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $author$project$Test$Runner$Node$init = F2(
	function (_v0, _v1) {
		var runners = _v0.runners;
		var report = _v0.report;
		var initialSeed = _v0.initialSeed;
		var fuzzRuns = _v0.fuzzRuns;
		var paths = _v0.paths;
		var globs = _v0.globs;
		var processes = _v0.processes;
		var testReporter = $author$project$Test$Reporter$Reporter$createReporter(report);
		var _v2 = function () {
			switch (runners.$) {
				case 'Plain':
					var runnerList = runners.a;
					return {
						autoFail: $elm$core$Maybe$Nothing,
						indexedRunners: A2(
							$elm$core$List$indexedMap,
							F2(
								function (a, b) {
									return _Utils_Tuple2(a, b);
								}),
							runnerList)
					};
				case 'Only':
					var runnerList = runners.a;
					return {
						autoFail: $elm$core$Maybe$Just('Test.only was used'),
						indexedRunners: A2(
							$elm$core$List$indexedMap,
							F2(
								function (a, b) {
									return _Utils_Tuple2(a, b);
								}),
							runnerList)
					};
				case 'Skipping':
					var runnerList = runners.a;
					return {
						autoFail: $elm$core$Maybe$Just('Test.skip was used'),
						indexedRunners: A2(
							$elm$core$List$indexedMap,
							F2(
								function (a, b) {
									return _Utils_Tuple2(a, b);
								}),
							runnerList)
					};
				default:
					var str = runners.a;
					return {
						autoFail: $elm$core$Maybe$Just(str),
						indexedRunners: _List_Nil
					};
			}
		}();
		var autoFail = _v2.autoFail;
		var indexedRunners = _v2.indexedRunners;
		var testCount = $elm$core$List$length(indexedRunners);
		var model = {
			autoFail: autoFail,
			available: $elm$core$Dict$fromList(indexedRunners),
			nextTestToRun: 0,
			processes: processes,
			results: _List_Nil,
			runInfo: {fuzzRuns: fuzzRuns, globs: globs, initialSeed: initialSeed, paths: paths, testCount: testCount},
			testReporter: testReporter
		};
		return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
	});
var $author$project$Test$Runner$Node$noTestsFoundError = function (globs) {
	return $elm$core$List$isEmpty(globs) ? $elm$core$String$trim('\nNo exposed values of type Test found in the tests/ directory.\n\nAre there tests in any .elm file in the tests/ directory?\nIf not – add some!\nIf there are – are they exposed?\n        ') : A3(
		$elm$core$String$replace,
		'%globs',
		A2($elm$core$String$join, '\n', globs),
		$elm$core$String$trim('\nNo exposed values of type Test found in files matching:\n\n%globs\n\nAre the above patterns correct? Maybe try running elm-test with no arguments?\n\nAre there tests in any of the matched files?\nIf not – add some!\nIf there are – are they exposed?\n        '));
};
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $author$project$Test$Runner$Node$Dispatch = function (a) {
	return {$: 'Dispatch', a: a};
};
var $elm$json$Json$Decode$decodeValue = _Json_run;
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $author$project$Test$Runner$JsMessage$Summary = F3(
	function (a, b, c) {
		return {$: 'Summary', a: a, b: b, c: c};
	});
var $author$project$Test$Runner$JsMessage$Test = function (a) {
	return {$: 'Test', a: a};
};
var $elm$json$Json$Decode$fail = _Json_fail;
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$float = _Json_decodeFloat;
var $elm$json$Json$Decode$list = _Json_decodeList;
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map3 = _Json_map3;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$Test$Runner$JsMessage$todoDecoder = A3(
	$elm$json$Json$Decode$map2,
	F2(
		function (a, b) {
			return _Utils_Tuple2(a, b);
		}),
	A2(
		$elm$json$Json$Decode$field,
		'labels',
		$elm$json$Json$Decode$list($elm$json$Json$Decode$string)),
	A2($elm$json$Json$Decode$field, 'todo', $elm$json$Json$Decode$string));
var $author$project$Test$Runner$JsMessage$decodeMessageFromType = function (messageType) {
	switch (messageType) {
		case 'TEST':
			return A2(
				$elm$json$Json$Decode$map,
				$author$project$Test$Runner$JsMessage$Test,
				A2($elm$json$Json$Decode$field, 'index', $elm$json$Json$Decode$int));
		case 'SUMMARY':
			return A4(
				$elm$json$Json$Decode$map3,
				$author$project$Test$Runner$JsMessage$Summary,
				A2($elm$json$Json$Decode$field, 'duration', $elm$json$Json$Decode$float),
				A2($elm$json$Json$Decode$field, 'failures', $elm$json$Json$Decode$int),
				A2(
					$elm$json$Json$Decode$field,
					'todos',
					$elm$json$Json$Decode$list($author$project$Test$Runner$JsMessage$todoDecoder)));
		default:
			return $elm$json$Json$Decode$fail('Unrecognized message type: ' + messageType);
	}
};
var $author$project$Test$Runner$JsMessage$decoder = A2(
	$elm$json$Json$Decode$andThen,
	$author$project$Test$Runner$JsMessage$decodeMessageFromType,
	A2($elm$json$Json$Decode$field, 'type', $elm$json$Json$Decode$string));
var $author$project$Test$Runner$Node$Complete = F4(
	function (a, b, c, d) {
		return {$: 'Complete', a: a, b: b, c: c, d: d};
	});
var $elm$time$Time$Name = function (a) {
	return {$: 'Name', a: a};
};
var $elm$time$Time$Offset = function (a) {
	return {$: 'Offset', a: a};
};
var $elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 'Zone', a: a, b: b};
	});
var $elm$time$Time$customZone = $elm$time$Time$Zone;
var $elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var $elm$time$Time$millisToPosix = $elm$time$Time$Posix;
var $elm$time$Time$now = _Time_now($elm$time$Time$millisToPosix);
var $author$project$Test$Reporter$TestResults$Passed = function (a) {
	return {$: 'Passed', a: a};
};
var $author$project$Test$Reporter$TestResults$Todo = function (a) {
	return {$: 'Todo', a: a};
};
var $elm_explorations$test$Test$Runner$getDistributionReport = function (expectation) {
	if (expectation.$ === 'Pass') {
		var distributionReport = expectation.a.distributionReport;
		return distributionReport;
	} else {
		var distributionReport = expectation.a.distributionReport;
		return distributionReport;
	}
};
var $elm_explorations$test$Test$Runner$getFailureReason = function (expectation) {
	if (expectation.$ === 'Pass') {
		return $elm$core$Maybe$Nothing;
	} else {
		var record = expectation.a;
		return $elm$core$Maybe$Just(
			{description: record.description, given: record.given, reason: record.reason});
	}
};
var $elm_explorations$test$Test$Runner$Failure$TODO = {$: 'TODO'};
var $elm_explorations$test$Test$Runner$isTodo = function (expectation) {
	if (expectation.$ === 'Pass') {
		return false;
	} else {
		var reason = expectation.a.reason;
		return _Utils_eq(reason, $elm_explorations$test$Test$Runner$Failure$TODO);
	}
};
var $author$project$Test$Reporter$TestResults$outcomesFromExpectationsHelp = F2(
	function (expectation, builder) {
		var _v0 = $elm_explorations$test$Test$Runner$getFailureReason(expectation);
		if (_v0.$ === 'Just') {
			var failure = _v0.a;
			return $elm_explorations$test$Test$Runner$isTodo(expectation) ? _Utils_update(
				builder,
				{
					todos: A2($elm$core$List$cons, failure.description, builder.todos)
				}) : _Utils_update(
				builder,
				{
					failures: A2(
						$elm$core$List$cons,
						_Utils_Tuple2(
							failure,
							$elm_explorations$test$Test$Runner$getDistributionReport(expectation)),
						builder.failures)
				});
		} else {
			return _Utils_update(
				builder,
				{
					passes: A2(
						$elm$core$List$cons,
						$elm_explorations$test$Test$Runner$getDistributionReport(expectation),
						builder.passes)
				});
		}
	});
var $author$project$Test$Reporter$TestResults$outcomesFromExpectations = function (expectations) {
	if (expectations.b) {
		if (!expectations.b.b) {
			var expectation = expectations.a;
			var _v1 = $elm_explorations$test$Test$Runner$getFailureReason(expectation);
			if (_v1.$ === 'Nothing') {
				return _List_fromArray(
					[
						$author$project$Test$Reporter$TestResults$Passed(
						$elm_explorations$test$Test$Runner$getDistributionReport(expectation))
					]);
			} else {
				var failure = _v1.a;
				return $elm_explorations$test$Test$Runner$isTodo(expectation) ? _List_fromArray(
					[
						$author$project$Test$Reporter$TestResults$Todo(failure.description)
					]) : _List_fromArray(
					[
						$author$project$Test$Reporter$TestResults$Failed(
						_List_fromArray(
							[
								_Utils_Tuple2(
								failure,
								$elm_explorations$test$Test$Runner$getDistributionReport(expectation))
							]))
					]);
			}
		} else {
			var builder = A3(
				$elm$core$List$foldl,
				$author$project$Test$Reporter$TestResults$outcomesFromExpectationsHelp,
				{failures: _List_Nil, passes: _List_Nil, todos: _List_Nil},
				expectations);
			var failuresList = function () {
				var _v2 = builder.failures;
				if (!_v2.b) {
					return _List_Nil;
				} else {
					var failures = _v2;
					return _List_fromArray(
						[
							$author$project$Test$Reporter$TestResults$Failed(failures)
						]);
				}
			}();
			return $elm$core$List$concat(
				_List_fromArray(
					[
						A2($elm$core$List$map, $author$project$Test$Reporter$TestResults$Passed, builder.passes),
						A2($elm$core$List$map, $author$project$Test$Reporter$TestResults$Todo, builder.todos),
						failuresList
					]));
		}
	} else {
		return _List_Nil;
	}
};
var $elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(_Utils_Tuple0);
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0.a;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return _Utils_Tuple0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(_Utils_Tuple0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0.a;
		return $elm$core$Task$Perform(
			A2($elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2($elm$core$Task$map, toMessage, task)));
	});
var $author$project$Test$Runner$Node$sendResults = F3(
	function (isFinished, testReporter, results) {
		var typeStr = isFinished ? 'FINISHED' : 'RESULTS';
		var addToKeyValues = F2(
			function (_v0, list) {
				var testId = _v0.a;
				var result = _v0.b;
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(
						$elm$core$String$fromInt(testId),
						testReporter.reportComplete(result)),
					list);
			});
		return $author$project$Test$Runner$Node$elmTestPort__send(
			A2(
				$elm$json$Json$Encode$encode,
				0,
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'type',
							$elm$json$Json$Encode$string(typeStr)),
							_Utils_Tuple2(
							'results',
							$elm$json$Json$Encode$object(
								A3($elm$core$List$foldl, addToKeyValues, _List_Nil, results)))
						]))));
	});
var $author$project$Test$Runner$Node$dispatch = F2(
	function (model, startTime) {
		var _v0 = A2($elm$core$Dict$get, model.nextTestToRun, model.available);
		if (_v0.$ === 'Nothing') {
			return A3($author$project$Test$Runner$Node$sendResults, true, model.testReporter, model.results);
		} else {
			var config = _v0.a;
			var outcomes = $author$project$Test$Reporter$TestResults$outcomesFromExpectations(
				config.run(_Utils_Tuple0));
			return A2(
				$elm$core$Task$perform,
				A3($author$project$Test$Runner$Node$Complete, config.labels, outcomes, startTime),
				$elm$time$Time$now);
		}
	});
var $author$project$Test$Reporter$TestResults$isFailure = function (outcome) {
	if (outcome.$ === 'Failed') {
		return true;
	} else {
		return false;
	}
};
var $elm$time$Time$posixToMillis = function (_v0) {
	var millis = _v0.a;
	return millis;
};
var $author$project$Test$Runner$Node$sendBegin = function (model) {
	var extraFields = function () {
		var _v0 = model.testReporter.reportBegin(model.runInfo);
		if (_v0.$ === 'Just') {
			var report = _v0.a;
			return _List_fromArray(
				[
					_Utils_Tuple2('message', report)
				]);
		} else {
			return _List_Nil;
		}
	}();
	var baseFields = _List_fromArray(
		[
			_Utils_Tuple2(
			'type',
			$elm$json$Json$Encode$string('BEGIN')),
			_Utils_Tuple2(
			'testCount',
			$elm$json$Json$Encode$int(model.runInfo.testCount))
		]);
	return $author$project$Test$Runner$Node$elmTestPort__send(
		A2(
			$elm$json$Json$Encode$encode,
			0,
			$elm$json$Json$Encode$object(
				_Utils_ap(baseFields, extraFields))));
};
var $author$project$Test$Runner$Node$update = F2(
	function (msg, model) {
		var testReporter = model.testReporter;
		switch (msg.$) {
			case 'Receive':
				var val = msg.a;
				var _v1 = A2($elm$json$Json$Decode$decodeValue, $author$project$Test$Runner$JsMessage$decoder, val);
				if (_v1.$ === 'Ok') {
					if (_v1.a.$ === 'Summary') {
						var _v2 = _v1.a;
						var duration = _v2.a;
						var failed = _v2.b;
						var todos = _v2.c;
						var testCount = model.runInfo.testCount;
						var summaryInfo = {
							duration: duration,
							failed: failed,
							passed: (testCount - failed) - $elm$core$List$length(todos),
							testCount: testCount,
							todos: todos
						};
						var summary = A2(testReporter.reportSummary, summaryInfo, model.autoFail);
						var exitCode = (failed > 0) ? 2 : ((_Utils_eq(model.autoFail, $elm$core$Maybe$Nothing) && $elm$core$List$isEmpty(todos)) ? 0 : 3);
						var cmd = $author$project$Test$Runner$Node$elmTestPort__send(
							A2(
								$elm$json$Json$Encode$encode,
								0,
								$elm$json$Json$Encode$object(
									_List_fromArray(
										[
											_Utils_Tuple2(
											'type',
											$elm$json$Json$Encode$string('SUMMARY')),
											_Utils_Tuple2(
											'exitCode',
											$elm$json$Json$Encode$int(exitCode)),
											_Utils_Tuple2('message', summary)
										]))));
						return _Utils_Tuple2(model, cmd);
					} else {
						var index = _v1.a.a;
						var cmd = A2($elm$core$Task$perform, $author$project$Test$Runner$Node$Dispatch, $elm$time$Time$now);
						return _Utils_eq(index, -1) ? _Utils_Tuple2(
							_Utils_update(
								model,
								{nextTestToRun: index + model.processes}),
							$elm$core$Platform$Cmd$batch(
								_List_fromArray(
									[
										cmd,
										$author$project$Test$Runner$Node$sendBegin(model)
									]))) : _Utils_Tuple2(
							_Utils_update(
								model,
								{nextTestToRun: index}),
							cmd);
					}
				} else {
					var err = _v1.a;
					var cmd = $author$project$Test$Runner$Node$elmTestPort__send(
						A2(
							$elm$json$Json$Encode$encode,
							0,
							$elm$json$Json$Encode$object(
								_List_fromArray(
									[
										_Utils_Tuple2(
										'type',
										$elm$json$Json$Encode$string('ERROR')),
										_Utils_Tuple2(
										'message',
										$elm$json$Json$Encode$string(
											$elm$json$Json$Decode$errorToString(err)))
									]))));
					return _Utils_Tuple2(model, cmd);
				}
			case 'Dispatch':
				var startTime = msg.a;
				return _Utils_Tuple2(
					model,
					A2($author$project$Test$Runner$Node$dispatch, model, startTime));
			default:
				var labels = msg.a;
				var outcomes = msg.b;
				var startTime = msg.c;
				var endTime = msg.d;
				var nextTestToRun = model.nextTestToRun + model.processes;
				var isFinished = _Utils_cmp(nextTestToRun, model.runInfo.testCount) > -1;
				var duration = $elm$time$Time$posixToMillis(endTime) - $elm$time$Time$posixToMillis(startTime);
				var prependOutcome = F2(
					function (outcome, rest) {
						return A2(
							$elm$core$List$cons,
							_Utils_Tuple2(
								model.nextTestToRun,
								{duration: duration, labels: labels, outcome: outcome}),
							rest);
					});
				var results = A3($elm$core$List$foldl, prependOutcome, model.results, outcomes);
				if (isFinished || A2($elm$core$List$any, $author$project$Test$Reporter$TestResults$isFailure, outcomes)) {
					var cmd = A3($author$project$Test$Runner$Node$sendResults, isFinished, testReporter, results);
					return isFinished ? _Utils_Tuple2(model, cmd) : _Utils_Tuple2(
						_Utils_update(
							model,
							{nextTestToRun: nextTestToRun, results: _List_Nil}),
						$elm$core$Platform$Cmd$batch(
							_List_fromArray(
								[
									cmd,
									A2($elm$core$Task$perform, $author$project$Test$Runner$Node$Dispatch, $elm$time$Time$now)
								])));
				} else {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{nextTestToRun: nextTestToRun, results: results}),
						A2($elm$core$Task$perform, $author$project$Test$Runner$Node$Dispatch, $elm$time$Time$now));
				}
		}
	});
var $elm$core$Platform$worker = _Platform_worker;
var $author$project$Test$Runner$Node$run = F2(
	function (_v0, possiblyTests) {
		var processes = _v0.processes;
		var paths = _v0.paths;
		var globs = _v0.globs;
		var report = _v0.report;
		var seed = _v0.seed;
		var runs = _v0.runs;
		var tests = A2(
			$elm$core$List$filterMap,
			function (_v4) {
				var moduleName = _v4.a;
				var maybeModuleTests = _v4.b;
				var moduleTests = A2($elm$core$List$filterMap, $elm$core$Basics$identity, maybeModuleTests);
				return $elm$core$List$isEmpty(moduleTests) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
					A2($elm_explorations$test$Test$describe, moduleName, moduleTests));
			},
			possiblyTests);
		if ($elm$core$List$isEmpty(tests)) {
			return $elm$core$Platform$worker(
				{
					init: A2(
						$author$project$Test$Runner$Node$failInit,
						$author$project$Test$Runner$Node$noTestsFoundError(globs),
						report),
					subscriptions: function (_v1) {
						return $elm$core$Platform$Sub$none;
					},
					update: F2(
						function (_v2, model) {
							return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
						})
				});
		} else {
			var runners = A3(
				$elm_explorations$test$Test$Runner$fromTest,
				runs,
				$elm$random$Random$initialSeed(seed),
				$elm_explorations$test$Test$concat(tests));
			var wrappedInit = $author$project$Test$Runner$Node$init(
				{fuzzRuns: runs, globs: globs, initialSeed: seed, paths: paths, processes: processes, report: report, runners: runners});
			return $elm$core$Platform$worker(
				{
					init: wrappedInit,
					subscriptions: function (_v3) {
						return $author$project$Test$Runner$Node$elmTestPort__receive($author$project$Test$Runner$Node$Receive);
					},
					update: $author$project$Test$Runner$Node$update
				});
		}
	});
var $elm_explorations$test$Expect$Absolute = function (a) {
	return {$: 'Absolute', a: a};
};
var $author$project$LDraw$Resolve$Loaded = function (a) {
	return {$: 'Loaded', a: a};
};
var $author$project$LDraw$Types$SubFileRef = function (a) {
	return {$: 'SubFileRef', a: a};
};
var $author$project$Gear$Components$AxleLike = {$: 'AxleLike'};
var $author$project$Gear$Components$Beam = {$: 'Beam'};
var $author$project$Gear$ComponentsTest$componentSpecs = _List_fromArray(
	[
		{kind: $author$project$Gear$Components$AxleLike, partFile: '3706.dat'},
		{kind: $author$project$Gear$Components$Beam, partFile: '32316.dat'}
	]);
var $elm_explorations$test$Test$Runner$Failure$Equality = F2(
	function (a, b) {
		return {$: 'Equality', a: a, b: b};
	});
var $elm$core$String$contains = _String_contains;
var $elm_explorations$test$Test$Expectation$Pass = function (a) {
	return {$: 'Pass', a: a};
};
var $elm_explorations$test$Expect$pass = $elm_explorations$test$Test$Expectation$Pass(
	{distributionReport: $elm_explorations$test$Test$Distribution$NoDistribution});
var $elm_explorations$test$Test$Internal$toString = _Debug_toString;
var $elm_explorations$test$Expect$testWith = F5(
	function (makeReason, label, runTest, expected, actual) {
		return A2(runTest, actual, expected) ? $elm_explorations$test$Expect$pass : $elm_explorations$test$Test$Expectation$fail(
			{
				description: label,
				reason: A2(
					makeReason,
					$elm_explorations$test$Test$Internal$toString(expected),
					$elm_explorations$test$Test$Internal$toString(actual))
			});
	});
var $elm$core$String$toInt = _String_toInt;
var $elm_explorations$test$Expect$equateWith = F4(
	function (reason, comparison, b, a) {
		var isJust = function (x) {
			if (x.$ === 'Just') {
				return true;
			} else {
				return false;
			}
		};
		var isFloat = function (x) {
			return isJust(
				$elm$core$String$toFloat(x)) && (!isJust(
				$elm$core$String$toInt(x)));
		};
		var usesFloats = isFloat(
			$elm_explorations$test$Test$Internal$toString(a)) || isFloat(
			$elm_explorations$test$Test$Internal$toString(b));
		var floatError = A2($elm$core$String$contains, reason, 'not') ? 'Do not use Expect.notEqual with floats. Use Expect.notWithin instead.' : 'Do not use Expect.equal with floats. Use Expect.within instead.';
		return usesFloats ? $elm_explorations$test$Expect$fail(floatError) : A5($elm_explorations$test$Expect$testWith, $elm_explorations$test$Test$Runner$Failure$Equality, reason, comparison, b, a);
	});
var $elm_explorations$test$Expect$equal = A2($elm_explorations$test$Expect$equateWith, 'Expect.equal', $elm$core$Basics$eq);
var $elm_explorations$linear_algebra$Math$Matrix4$identity = _MJS_m4x4identity;
var $elm_explorations$linear_algebra$Math$Vector3$length = _MJS_v3length;
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Gear$Components$matchSpec = F2(
	function (specs, file) {
		return $elm$core$List$head(
			A2(
				$elm$core$List$filter,
				function (spec) {
					return _Utils_eq(spec.partFile, file);
				},
				specs));
	});
var $elm_explorations$linear_algebra$Math$Matrix4$mul = _MJS_m4x4mul;
var $elm_explorations$linear_algebra$Math$Vector3$scale = _MJS_v3scale;
var $elm_explorations$linear_algebra$Math$Vector3$sub = _MJS_v3sub;
var $elm_explorations$linear_algebra$Math$Matrix4$transform = _MJS_v3mul4x4;
var $elm_explorations$linear_algebra$Math$Vector3$vec3 = _MJS_v3;
var $author$project$Gear$Components$walkLine = F6(
	function (specs, cache, parentColor, worldMat, line, acc) {
		if (line.$ === 'SubFileRef') {
			var color = line.a.color;
			var transform = line.a.transform;
			var file = line.a.file;
			var combinedMat = A2($elm_explorations$linear_algebra$Math$Matrix4$mul, worldMat, transform);
			var childColor = ((color === 16) || _Utils_eq(color, -1)) ? parentColor : color;
			var _v1 = A2($author$project$Gear$Components$matchSpec, specs, file);
			if (_v1.$ === 'Just') {
				var spec = _v1.a;
				var origin = A2(
					$elm_explorations$linear_algebra$Math$Matrix4$transform,
					combinedMat,
					A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 0));
				var axisEnd = A2(
					$elm_explorations$linear_algebra$Math$Matrix4$transform,
					combinedMat,
					A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 1));
				var rawAxis = A2($elm_explorations$linear_algebra$Math$Vector3$sub, axisEnd, origin);
				var axisLen = $elm_explorations$linear_algebra$Math$Vector3$length(rawAxis);
				var axis = (axisLen < 1.0e-6) ? A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 1) : A2($elm_explorations$linear_algebra$Math$Vector3$scale, 1 / axisLen, rawAxis);
				return A2(
					$elm$core$List$cons,
					{color: childColor, kind: spec.kind, partFile: spec.partFile, worldAxis: axis, worldMatrix: combinedMat, worldPosition: origin},
					acc);
			} else {
				var _v2 = A2($elm$core$Dict$get, file, cache);
				if ((_v2.$ === 'Just') && (_v2.a.$ === 'Loaded')) {
					var subLines = _v2.a.a;
					return A6($author$project$Gear$Components$walkLines, specs, subLines, cache, childColor, combinedMat, acc);
				} else {
					return acc;
				}
			}
		} else {
			return acc;
		}
	});
var $author$project$Gear$Components$walkLines = F6(
	function (specs, lines, cache, parentColor, worldMat, acc) {
		return A3(
			$elm$core$List$foldl,
			A4($author$project$Gear$Components$walkLine, specs, cache, parentColor, worldMat),
			acc,
			lines);
	});
var $author$project$Gear$Components$extractComponents = F3(
	function (specs, lines, cache) {
		return $elm$core$List$reverse(
			A6($author$project$Gear$Components$walkLines, specs, lines, cache, 15, $elm_explorations$linear_algebra$Math$Matrix4$identity, _List_Nil));
	});
var $elm_explorations$linear_algebra$Math$Vector3$getX = _MJS_v3getX;
var $elm_explorations$linear_algebra$Math$Matrix4$makeTranslate3 = _MJS_m4x4makeTranslate3;
var $elm_explorations$test$Test$Internal$blankDescriptionFailure = $elm_explorations$test$Test$Internal$failNow(
	{
		description: 'This test has a blank description. Let\'s give it a useful one!',
		reason: $elm_explorations$test$Test$Runner$Failure$Invalid($elm_explorations$test$Test$Runner$Failure$BadDescription)
	});
var $elm_explorations$test$Test$test = F2(
	function (untrimmedDesc, thunk) {
		var desc = $elm$core$String$trim(untrimmedDesc);
		return $elm$core$String$isEmpty(desc) ? $elm_explorations$test$Test$Internal$blankDescriptionFailure : A2(
			$elm_explorations$test$Test$Internal$ElmTestVariant__Labeled,
			desc,
			$elm_explorations$test$Test$Internal$ElmTestVariant__UnitTest(
				function (_v0) {
					return _List_fromArray(
						[
							thunk(_Utils_Tuple0)
						]);
				}));
	});
var $elm_explorations$test$Test$Runner$Failure$Comparison = F2(
	function (a, b) {
		return {$: 'Comparison', a: a, b: b};
	});
var $elm_explorations$test$Expect$compareWith = $elm_explorations$test$Expect$testWith($elm_explorations$test$Test$Runner$Failure$Comparison);
var $elm_explorations$test$Expect$absolute = function (tolerance) {
	switch (tolerance.$) {
		case 'Absolute':
			var val = tolerance.a;
			return val;
		case 'AbsoluteOrRelative':
			var val = tolerance.a;
			return val;
		default:
			return 0;
	}
};
var $elm_explorations$test$Expect$relative = function (tolerance) {
	switch (tolerance.$) {
		case 'Relative':
			var val = tolerance.a;
			return val;
		case 'AbsoluteOrRelative':
			var val = tolerance.b;
			return val;
		default:
			return 0;
	}
};
var $elm_explorations$test$Expect$nonNegativeToleranceError = F3(
	function (tolerance, name, result) {
		return (($elm_explorations$test$Expect$absolute(tolerance) < 0) && ($elm_explorations$test$Expect$relative(tolerance) < 0)) ? $elm_explorations$test$Test$Expectation$fail(
			{description: 'Expect.' + (name + ' was given negative absolute and relative tolerances'), reason: $elm_explorations$test$Test$Runner$Failure$Custom}) : (($elm_explorations$test$Expect$absolute(tolerance) < 0) ? $elm_explorations$test$Test$Expectation$fail(
			{description: 'Expect.' + (name + ' was given a negative absolute tolerance'), reason: $elm_explorations$test$Test$Runner$Failure$Custom}) : (($elm_explorations$test$Expect$relative(tolerance) < 0) ? $elm_explorations$test$Test$Expectation$fail(
			{description: 'Expect.' + (name + ' was given a negative relative tolerance'), reason: $elm_explorations$test$Test$Runner$Failure$Custom}) : result));
	});
var $elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var $elm_explorations$test$Expect$withinCompare = F3(
	function (tolerance, a, b) {
		var withinRelativeTolerance = ((_Utils_cmp(
			a - $elm$core$Basics$abs(
				a * $elm_explorations$test$Expect$relative(tolerance)),
			b) < 1) && (_Utils_cmp(
			b,
			a + $elm$core$Basics$abs(
				a * $elm_explorations$test$Expect$relative(tolerance))) < 1)) || ((_Utils_cmp(
			b - $elm$core$Basics$abs(
				b * $elm_explorations$test$Expect$relative(tolerance)),
			a) < 1) && (_Utils_cmp(
			a,
			b + $elm$core$Basics$abs(
				b * $elm_explorations$test$Expect$relative(tolerance))) < 1));
		var withinAbsoluteTolerance = (_Utils_cmp(
			a - $elm_explorations$test$Expect$absolute(tolerance),
			b) < 1) && (_Utils_cmp(
			b,
			a + $elm_explorations$test$Expect$absolute(tolerance)) < 1);
		return _Utils_eq(a, b) || (withinAbsoluteTolerance || withinRelativeTolerance);
	});
var $elm_explorations$test$Expect$within = F3(
	function (tolerance, lower, upper) {
		return A3(
			$elm_explorations$test$Expect$nonNegativeToleranceError,
			tolerance,
			'within',
			A4(
				$elm_explorations$test$Expect$compareWith,
				'Expect.within ' + $elm_explorations$test$Test$Internal$toString(tolerance),
				$elm_explorations$test$Expect$withinCompare(tolerance),
				lower,
				upper));
	});
var $author$project$Gear$ComponentsTest$suite = A2(
	$elm_explorations$test$Test$describe,
	'Gear.Components.extractComponents',
	_List_fromArray(
		[
			A2(
			$elm_explorations$test$Test$test,
			'detects direct top-level axle and beam refs',
			function (_v0) {
				var lines = _List_fromArray(
					[
						$author$project$LDraw$Types$SubFileRef(
						{color: 16, file: '3706.dat', transform: $elm_explorations$linear_algebra$Math$Matrix4$identity}),
						$author$project$LDraw$Types$SubFileRef(
						{color: 16, file: '32316.dat', transform: $elm_explorations$linear_algebra$Math$Matrix4$identity})
					]);
				var instances = A3($author$project$Gear$Components$extractComponents, $author$project$Gear$ComponentsTest$componentSpecs, lines, $elm$core$Dict$empty);
				return A2(
					$elm_explorations$test$Expect$equal,
					2,
					$elm$core$List$length(instances));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'recurses into cached subfiles',
			function (_v1) {
				var topLevel = _List_fromArray(
					[
						$author$project$LDraw$Types$SubFileRef(
						{color: 16, file: 'assembly.dat', transform: $elm_explorations$linear_algebra$Math$Matrix4$identity})
					]);
				var cache = $elm$core$Dict$fromList(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'assembly.dat',
							$author$project$LDraw$Resolve$Loaded(
								_List_fromArray(
									[
										$author$project$LDraw$Types$SubFileRef(
										{
											color: 16,
											file: '3706.dat',
											transform: A3($elm_explorations$linear_algebra$Math$Matrix4$makeTranslate3, 10, 0, 0)
										})
									])))
						]));
				var instances = A3($author$project$Gear$Components$extractComponents, $author$project$Gear$ComponentsTest$componentSpecs, topLevel, cache);
				return A2(
					$elm_explorations$test$Expect$equal,
					1,
					$elm$core$List$length(instances));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'world position includes transform',
			function (_v2) {
				var lines = _List_fromArray(
					[
						$author$project$LDraw$Types$SubFileRef(
						{
							color: 16,
							file: '3706.dat',
							transform: A3($elm_explorations$linear_algebra$Math$Matrix4$makeTranslate3, 12, 0, 0)
						})
					]);
				var _v3 = A3($author$project$Gear$Components$extractComponents, $author$project$Gear$ComponentsTest$componentSpecs, lines, $elm$core$Dict$empty);
				if (_v3.b && (!_v3.b.b)) {
					var inst = _v3.a;
					return A3(
						$elm_explorations$test$Expect$within,
						$elm_explorations$test$Expect$Absolute(1.0e-4),
						12.0,
						$elm_explorations$linear_algebra$Math$Vector3$getX(inst.worldPosition));
				} else {
					return $elm_explorations$test$Expect$fail('Expected exactly one component');
				}
			})
		]));
var $elm_explorations$test$Expect$allHelp = F2(
	function (list, query) {
		allHelp:
		while (true) {
			if (!list.b) {
				return $elm_explorations$test$Expect$pass;
			} else {
				var check = list.a;
				var rest = list.b;
				var _v1 = check(query);
				if (_v1.$ === 'Pass') {
					var $temp$list = rest,
						$temp$query = query;
					list = $temp$list;
					query = $temp$query;
					continue allHelp;
				} else {
					var outcome = _v1;
					return outcome;
				}
			}
		}
	});
var $elm_explorations$test$Expect$all = F2(
	function (list, query) {
		return $elm$core$List$isEmpty(list) ? $elm_explorations$test$Test$Expectation$fail(
			{
				description: 'Expect.all was given an empty list. You must make at least one expectation to have a valid test!',
				reason: $elm_explorations$test$Test$Runner$Failure$Invalid($elm_explorations$test$Test$Runner$Failure$EmptyList)
			}) : A2($elm_explorations$test$Expect$allHelp, list, query);
	});
var $elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _v0 = alter(
			A2($elm$core$Dict$get, targetKey, dictionary));
		if (_v0.$ === 'Just') {
			var value = _v0.a;
			return A3($elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2($elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var $author$project$Gear$Detect$addConnection = F3(
	function (from, to, dict) {
		return A3(
			$elm$core$Dict$update,
			from,
			function (existing) {
				if (existing.$ === 'Nothing') {
					return $elm$core$Maybe$Just(
						_List_fromArray(
							[to]));
				} else {
					var xs = existing.a;
					return $elm$core$Maybe$Just(
						A2($elm$core$List$cons, to, xs));
				}
			},
			dict);
	});
var $author$project$Gear$Detect$isWorm = function (spec) {
	return spec.teeth === 1;
};
var $elm_explorations$linear_algebra$Math$Vector3$distance = _MJS_v3distance;
var $elm_explorations$linear_algebra$Math$Vector3$dot = _MJS_v3dot;
var $author$project$Gear$Detect$gearAxis = function (inst) {
	var origin = A2(
		$elm_explorations$linear_algebra$Math$Matrix4$transform,
		inst.worldMatrix,
		A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 0));
	var alongLocalZ = A2(
		$elm_explorations$linear_algebra$Math$Matrix4$transform,
		inst.worldMatrix,
		A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 1));
	var raw = A2($elm_explorations$linear_algebra$Math$Vector3$sub, alongLocalZ, origin);
	var len = $elm_explorations$linear_algebra$Math$Vector3$length(raw);
	return (len < 1.0e-6) ? A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 1) : A2($elm_explorations$linear_algebra$Math$Vector3$scale, 1 / len, raw);
};
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var $author$project$Gear$Detect$isBevelLike = function (spec) {
	return A2(
		$elm$core$List$member,
		spec.partFile,
		_List_fromArray(
			['32198.dat', '32269.dat']));
};
var $author$project$Gear$Detect$isCrownLike = function (spec) {
	return spec.partFile === '3650b.dat';
};
var $elm$core$Basics$clamp = F3(
	function (low, high, number) {
		return (_Utils_cmp(number, low) < 0) ? low : ((_Utils_cmp(number, high) > 0) ? high : number);
	});
var $author$project$Gear$Detect$meshToleranceFor = function (expectedDistance) {
	return A3($elm$core$Basics$clamp, 0.8, 2.0, expectedDistance * 0.035);
};
var $author$project$Gear$Detect$meshing = F2(
	function (g1, g2) {
		var expected = g1.spec.pitchRadius + g2.spec.pitchRadius;
		var radialTolerance = $author$project$Gear$Detect$meshToleranceFor(expected);
		var dist = A2($elm_explorations$linear_algebra$Math$Vector3$distance, g1.worldPosition, g2.worldPosition);
		var centerDelta = A2($elm_explorations$linear_algebra$Math$Vector3$sub, g2.worldPosition, g1.worldPosition);
		var axis2 = $author$project$Gear$Detect$gearAxis(g2);
		var axis1 = $author$project$Gear$Detect$gearAxis(g1);
		var axialOffset = $elm$core$Basics$abs(
			A2($elm_explorations$linear_algebra$Math$Vector3$dot, centerDelta, axis1));
		var absDot = $elm$core$Basics$abs(
			A2($elm_explorations$linear_algebra$Math$Vector3$dot, axis1, axis2));
		var axisOk = ($author$project$Gear$Detect$isCrownLike(g1.spec) || $author$project$Gear$Detect$isCrownLike(g2.spec)) ? (absDot <= 0.35) : (($author$project$Gear$Detect$isBevelLike(g1.spec) || $author$project$Gear$Detect$isBevelLike(g2.spec)) ? (absDot <= 0.35) : ((absDot >= 0.9) && (axialOffset <= 2.0)));
		return axisOk && (_Utils_cmp(
			$elm$core$Basics$abs(dist - expected),
			radialTolerance) < 1);
	});
var $author$project$Gear$Detect$buildGearGraph = function (instances) {
	var arr = $elm$core$Array$fromList(instances);
	var n = $elm$core$Array$length(arr);
	var connections = A3(
		$elm$core$List$foldl,
		F2(
			function (i, acc) {
				return A3(
					$elm$core$List$foldl,
					F2(
						function (j, acc2) {
							var _v0 = _Utils_Tuple2(
								A2($elm$core$Array$get, i, arr),
								A2($elm$core$Array$get, j, arr));
							if ((_v0.a.$ === 'Just') && (_v0.b.$ === 'Just')) {
								var g1 = _v0.a.a;
								var g2 = _v0.b.a;
								return A2($author$project$Gear$Detect$meshing, g1, g2) ? ($author$project$Gear$Detect$isWorm(g1.spec) ? A3($author$project$Gear$Detect$addConnection, i, j, acc2) : ($author$project$Gear$Detect$isWorm(g2.spec) ? A3($author$project$Gear$Detect$addConnection, j, i, acc2) : A3(
									$author$project$Gear$Detect$addConnection,
									j,
									i,
									A3($author$project$Gear$Detect$addConnection, i, j, acc2)))) : acc2;
							} else {
								return acc2;
							}
						}),
					acc,
					A2($elm$core$List$range, i + 1, n - 1));
			}),
		$elm$core$Dict$empty,
		A2($elm$core$List$range, 0, n - 1));
	return {connections: connections, instances: arr};
};
var $author$project$Gear$DetectTest$emptyCache = $elm$core$Dict$empty;
var $author$project$Gear$Detect$matchGear = F2(
	function (gearSpecs, file) {
		return $elm$core$List$head(
			A2(
				$elm$core$List$filter,
				function (spec) {
					return _Utils_eq(spec.partFile, file);
				},
				gearSpecs));
	});
var $author$project$Gear$Detect$walkLine = F6(
	function (gearSpecs, cache, parentColor, worldMat, line, acc) {
		if (line.$ === 'SubFileRef') {
			var color = line.a.color;
			var transform = line.a.transform;
			var file = line.a.file;
			var combinedMat = A2($elm_explorations$linear_algebra$Math$Matrix4$mul, worldMat, transform);
			var childColor = ((color === 16) || _Utils_eq(color, -1)) ? parentColor : color;
			var _v1 = A2($author$project$Gear$Detect$matchGear, gearSpecs, file);
			if (_v1.$ === 'Just') {
				var spec = _v1.a;
				var worldPos = A2(
					$elm_explorations$linear_algebra$Math$Matrix4$transform,
					combinedMat,
					A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 0));
				var inst = {color: childColor, id: 0, spec: spec, worldMatrix: combinedMat, worldPosition: worldPos};
				return A2($elm$core$List$cons, inst, acc);
			} else {
				var _v2 = A2($elm$core$Dict$get, file, cache);
				if ((_v2.$ === 'Just') && (_v2.a.$ === 'Loaded')) {
					var subLines = _v2.a.a;
					return A6($author$project$Gear$Detect$walkLines, gearSpecs, subLines, cache, childColor, combinedMat, acc);
				} else {
					return acc;
				}
			}
		} else {
			return acc;
		}
	});
var $author$project$Gear$Detect$walkLines = F6(
	function (gearSpecs, lines, cache, parentColor, worldMat, acc) {
		return A3(
			$elm$core$List$foldl,
			A4($author$project$Gear$Detect$walkLine, gearSpecs, cache, parentColor, worldMat),
			acc,
			lines);
	});
var $author$project$Gear$Detect$extractGears = F3(
	function (gearSpecs, lines, cache) {
		return A2(
			$elm$core$List$indexedMap,
			F2(
				function (i, inst) {
					return _Utils_update(
						inst,
						{id: i});
				}),
			$elm$core$List$reverse(
				A6($author$project$Gear$Detect$walkLines, gearSpecs, lines, cache, 15, $elm_explorations$linear_algebra$Math$Matrix4$identity, _List_Nil)));
	});
var $author$project$Gear$DetectTest$gearRef = F4(
	function (file, x, y, z) {
		return $author$project$LDraw$Types$SubFileRef(
			{
				color: 16,
				file: file,
				transform: A3($elm_explorations$linear_algebra$Math$Matrix4$makeTranslate3, x, y, z)
			});
	});
var $elm_explorations$linear_algebra$Math$Matrix4$makeRotate = _MJS_m4x4makeRotate;
var $elm$core$Basics$pi = _Basics_pi;
var $author$project$Gear$DetectTest$spec16T = {partFile: '4019.dat', pitchRadius: 20.0, teeth: 16};
var $author$project$Gear$DetectTest$spec8T = {partFile: '3647.dat', pitchRadius: 10.0, teeth: 8};
var $author$project$Gear$DetectTest$specBevel20T = {partFile: '32198.dat', pitchRadius: 25.0, teeth: 20};
var $author$project$Gear$DetectTest$specCrown24T = {partFile: '3650b.dat', pitchRadius: 30.0, teeth: 24};
var $author$project$Gear$DetectTest$testGearSpecs = _List_fromArray(
	[$author$project$Gear$DetectTest$spec8T, $author$project$Gear$DetectTest$spec16T, $author$project$Gear$DetectTest$specBevel20T, $author$project$Gear$DetectTest$specCrown24T]);
var $author$project$Gear$DetectTest$suite = A2(
	$elm_explorations$test$Test$describe,
	'Gear.Detect',
	_List_fromArray(
		[
			A2(
			$elm_explorations$test$Test$describe,
			'extractGears',
			_List_fromArray(
				[
					A2(
					$elm_explorations$test$Test$test,
					'no SubFileRefs → empty list',
					function (_v0) {
						return A2(
							$elm_explorations$test$Expect$equal,
							0,
							$elm$core$List$length(
								A3($author$project$Gear$Detect$extractGears, $author$project$Gear$DetectTest$testGearSpecs, _List_Nil, $author$project$Gear$DetectTest$emptyCache)));
					}),
					A2(
					$elm_explorations$test$Test$test,
					'non-gear SubFileRef is ignored',
					function (_v1) {
						var lines = _List_fromArray(
							[
								$author$project$LDraw$Types$SubFileRef(
								{color: 16, file: '3001.dat', transform: $elm_explorations$linear_algebra$Math$Matrix4$identity})
							]);
						return A2(
							$elm_explorations$test$Expect$equal,
							0,
							$elm$core$List$length(
								A3($author$project$Gear$Detect$extractGears, $author$project$Gear$DetectTest$testGearSpecs, lines, $author$project$Gear$DetectTest$emptyCache)));
					}),
					A2(
					$elm_explorations$test$Test$test,
					'single 8T gear reference is detected',
					function (_v2) {
						return A2(
							$elm_explorations$test$Expect$equal,
							1,
							$elm$core$List$length(
								A3(
									$author$project$Gear$Detect$extractGears,
									$author$project$Gear$DetectTest$testGearSpecs,
									_List_fromArray(
										[
											A4($author$project$Gear$DetectTest$gearRef, '3647.dat', 0, 0, 0)
										]),
									$author$project$Gear$DetectTest$emptyCache)));
					}),
					A2(
					$elm_explorations$test$Test$test,
					'detected gear has correct spec',
					function (_v3) {
						var _v4 = A3(
							$author$project$Gear$Detect$extractGears,
							$author$project$Gear$DetectTest$testGearSpecs,
							_List_fromArray(
								[
									A4($author$project$Gear$DetectTest$gearRef, '3647.dat', 0, 0, 0)
								]),
							$author$project$Gear$DetectTest$emptyCache);
						if (_v4.b && (!_v4.b.b)) {
							var inst = _v4.a;
							return A2($elm_explorations$test$Expect$equal, '3647.dat', inst.spec.partFile);
						} else {
							return $elm_explorations$test$Expect$fail('Expected exactly one gear');
						}
					}),
					A2(
					$elm_explorations$test$Test$test,
					'world position reflects translation',
					function (_v5) {
						var _v6 = A3(
							$author$project$Gear$Detect$extractGears,
							$author$project$Gear$DetectTest$testGearSpecs,
							_List_fromArray(
								[
									A4($author$project$Gear$DetectTest$gearRef, '3647.dat', 10, 0, 0)
								]),
							$author$project$Gear$DetectTest$emptyCache);
						if (_v6.b && (!_v6.b.b)) {
							var inst = _v6.a;
							return A3(
								$elm_explorations$test$Expect$within,
								$elm_explorations$test$Expect$Absolute(1.0e-4),
								10.0,
								$elm_explorations$linear_algebra$Math$Vector3$getX(inst.worldPosition));
						} else {
							return $elm_explorations$test$Expect$fail('Expected exactly one gear');
						}
					}),
					A2(
					$elm_explorations$test$Test$test,
					'two gear refs produce two instances',
					function (_v7) {
						var lines = _List_fromArray(
							[
								A4($author$project$Gear$DetectTest$gearRef, '3647.dat', 0, 0, 0),
								A4($author$project$Gear$DetectTest$gearRef, '4019.dat', 40, 0, 0)
							]);
						return A2(
							$elm_explorations$test$Expect$equal,
							2,
							$elm$core$List$length(
								A3($author$project$Gear$Detect$extractGears, $author$project$Gear$DetectTest$testGearSpecs, lines, $author$project$Gear$DetectTest$emptyCache)));
					}),
					A2(
					$elm_explorations$test$Test$test,
					'IDs are assigned 0, 1, 2, ...',
					function (_v8) {
						var lines = _List_fromArray(
							[
								A4($author$project$Gear$DetectTest$gearRef, '3647.dat', 0, 0, 0),
								A4($author$project$Gear$DetectTest$gearRef, '4019.dat', 40, 0, 0)
							]);
						var ids = A2(
							$elm$core$List$map,
							function ($) {
								return $.id;
							},
							A3($author$project$Gear$Detect$extractGears, $author$project$Gear$DetectTest$testGearSpecs, lines, $author$project$Gear$DetectTest$emptyCache));
						return A2(
							$elm_explorations$test$Expect$equal,
							_List_fromArray(
								[0, 1]),
							ids);
					})
				])),
			A2(
			$elm_explorations$test$Test$describe,
			'buildGearGraph — adjacency',
			_List_fromArray(
				[
					A2(
					$elm_explorations$test$Test$test,
					'no instances → empty connections',
					function (_v9) {
						return A2(
							$elm_explorations$test$Expect$equal,
							true,
							$elm$core$Dict$isEmpty(
								$author$project$Gear$Detect$buildGearGraph(_List_Nil).connections));
					}),
					A2(
					$elm_explorations$test$Test$test,
					'single gear → no connections',
					function (_v10) {
						var gear = {
							color: 16,
							id: 0,
							spec: $author$project$Gear$DetectTest$spec8T,
							worldMatrix: $elm_explorations$linear_algebra$Math$Matrix4$identity,
							worldPosition: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 0)
						};
						return A2(
							$elm_explorations$test$Expect$equal,
							true,
							$elm$core$Dict$isEmpty(
								$author$project$Gear$Detect$buildGearGraph(
									_List_fromArray(
										[gear])).connections));
					}),
					A2(
					$elm_explorations$test$Test$test,
					'8T + 16T at exact mesh distance → connected',
					function (_v11) {
						var g2 = {
							color: 16,
							id: 1,
							spec: $author$project$Gear$DetectTest$spec16T,
							worldMatrix: $elm_explorations$linear_algebra$Math$Matrix4$identity,
							worldPosition: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 30, 0, 0)
						};
						var g1 = {
							color: 16,
							id: 0,
							spec: $author$project$Gear$DetectTest$spec8T,
							worldMatrix: $elm_explorations$linear_algebra$Math$Matrix4$identity,
							worldPosition: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 0)
						};
						var graph = $author$project$Gear$Detect$buildGearGraph(
							_List_fromArray(
								[g1, g2]));
						return A2(
							$elm_explorations$test$Expect$equal,
							true,
							A2(
								$elm$core$List$member,
								1,
								A2(
									$elm$core$Maybe$withDefault,
									_List_Nil,
									A2($elm$core$Dict$get, 0, graph.connections))));
					}),
					A2(
					$elm_explorations$test$Test$test,
					'connection is symmetric (both directions)',
					function (_v12) {
						var g2 = {
							color: 16,
							id: 1,
							spec: $author$project$Gear$DetectTest$spec16T,
							worldMatrix: $elm_explorations$linear_algebra$Math$Matrix4$identity,
							worldPosition: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 30, 0, 0)
						};
						var g1 = {
							color: 16,
							id: 0,
							spec: $author$project$Gear$DetectTest$spec8T,
							worldMatrix: $elm_explorations$linear_algebra$Math$Matrix4$identity,
							worldPosition: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 0)
						};
						var graph = $author$project$Gear$Detect$buildGearGraph(
							_List_fromArray(
								[g1, g2]));
						return A2(
							$elm_explorations$test$Expect$all,
							_List_fromArray(
								[
									function (g) {
									return A2(
										$elm_explorations$test$Expect$equal,
										true,
										A2(
											$elm$core$List$member,
											1,
											A2(
												$elm$core$Maybe$withDefault,
												_List_Nil,
												A2($elm$core$Dict$get, 0, g.connections))));
								},
									function (g) {
									return A2(
										$elm_explorations$test$Expect$equal,
										true,
										A2(
											$elm$core$List$member,
											0,
											A2(
												$elm$core$Maybe$withDefault,
												_List_Nil,
												A2($elm$core$Dict$get, 1, g.connections))));
								}
								]),
							graph);
					}),
					A2(
					$elm_explorations$test$Test$test,
					'two gears too far apart → not connected',
					function (_v13) {
						var g2 = {
							color: 16,
							id: 1,
							spec: $author$project$Gear$DetectTest$spec16T,
							worldMatrix: $elm_explorations$linear_algebra$Math$Matrix4$identity,
							worldPosition: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 200, 0, 0)
						};
						var g1 = {
							color: 16,
							id: 0,
							spec: $author$project$Gear$DetectTest$spec8T,
							worldMatrix: $elm_explorations$linear_algebra$Math$Matrix4$identity,
							worldPosition: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 0)
						};
						var graph = $author$project$Gear$Detect$buildGearGraph(
							_List_fromArray(
								[g1, g2]));
						return A2(
							$elm_explorations$test$Expect$equal,
							_List_Nil,
							A2(
								$elm$core$Maybe$withDefault,
								_List_Nil,
								A2($elm$core$Dict$get, 0, graph.connections)));
					}),
					A2(
					$elm_explorations$test$Test$test,
					'parallel gears with axial offset do not mesh even at same distance',
					function (_v14) {
						var g2 = {
							color: 16,
							id: 1,
							spec: $author$project$Gear$DetectTest$spec16T,
							worldMatrix: $elm_explorations$linear_algebra$Math$Matrix4$identity,
							worldPosition: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 29.5804, 0, 5)
						};
						var g1 = {
							color: 16,
							id: 0,
							spec: $author$project$Gear$DetectTest$spec8T,
							worldMatrix: $elm_explorations$linear_algebra$Math$Matrix4$identity,
							worldPosition: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 0)
						};
						var graph = $author$project$Gear$Detect$buildGearGraph(
							_List_fromArray(
								[g1, g2]));
						return A2(
							$elm_explorations$test$Expect$equal,
							_List_Nil,
							A2(
								$elm$core$Maybe$withDefault,
								_List_Nil,
								A2($elm$core$Dict$get, 0, graph.connections)));
					}),
					A2(
					$elm_explorations$test$Test$test,
					'instances array length matches input',
					function (_v15) {
						var g2 = {
							color: 16,
							id: 1,
							spec: $author$project$Gear$DetectTest$spec16T,
							worldMatrix: $elm_explorations$linear_algebra$Math$Matrix4$identity,
							worldPosition: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 40, 0, 0)
						};
						var g1 = {
							color: 16,
							id: 0,
							spec: $author$project$Gear$DetectTest$spec8T,
							worldMatrix: $elm_explorations$linear_algebra$Math$Matrix4$identity,
							worldPosition: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 0)
						};
						return A2(
							$elm_explorations$test$Expect$equal,
							2,
							$elm$core$Array$length(
								$author$project$Gear$Detect$buildGearGraph(
									_List_fromArray(
										[g1, g2])).instances));
					}),
					A2(
					$elm_explorations$test$Test$test,
					'bevel gears connect when axes are perpendicular',
					function (_v16) {
						var g2 = {
							color: 16,
							id: 1,
							spec: $author$project$Gear$DetectTest$specBevel20T,
							worldMatrix: A2(
								$elm_explorations$linear_algebra$Math$Matrix4$makeRotate,
								$elm$core$Basics$pi / 2,
								A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 1, 0, 0)),
							worldPosition: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 50, 0, 0)
						};
						var g1 = {
							color: 16,
							id: 0,
							spec: $author$project$Gear$DetectTest$specBevel20T,
							worldMatrix: $elm_explorations$linear_algebra$Math$Matrix4$identity,
							worldPosition: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 0)
						};
						var graph = $author$project$Gear$Detect$buildGearGraph(
							_List_fromArray(
								[g1, g2]));
						return A2(
							$elm_explorations$test$Expect$equal,
							true,
							A2(
								$elm$core$List$member,
								1,
								A2(
									$elm$core$Maybe$withDefault,
									_List_Nil,
									A2($elm$core$Dict$get, 0, graph.connections))));
					}),
					A2(
					$elm_explorations$test$Test$test,
					'bevel gears do not connect when axes are parallel',
					function (_v17) {
						var g2 = {
							color: 16,
							id: 1,
							spec: $author$project$Gear$DetectTest$specBevel20T,
							worldMatrix: $elm_explorations$linear_algebra$Math$Matrix4$identity,
							worldPosition: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 50, 0, 0)
						};
						var g1 = {
							color: 16,
							id: 0,
							spec: $author$project$Gear$DetectTest$specBevel20T,
							worldMatrix: $elm_explorations$linear_algebra$Math$Matrix4$identity,
							worldPosition: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 0)
						};
						var graph = $author$project$Gear$Detect$buildGearGraph(
							_List_fromArray(
								[g1, g2]));
						return A2(
							$elm_explorations$test$Expect$equal,
							_List_Nil,
							A2(
								$elm$core$Maybe$withDefault,
								_List_Nil,
								A2($elm$core$Dict$get, 0, graph.connections)));
					}),
					A2(
					$elm_explorations$test$Test$test,
					'crown meshes with spur when axes are perpendicular',
					function (_v18) {
						var spur = {
							color: 16,
							id: 1,
							spec: $author$project$Gear$DetectTest$spec8T,
							worldMatrix: A2(
								$elm_explorations$linear_algebra$Math$Matrix4$makeRotate,
								$elm$core$Basics$pi / 2,
								A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 1, 0, 0)),
							worldPosition: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 40, 0, 0)
						};
						var crown = {
							color: 16,
							id: 0,
							spec: $author$project$Gear$DetectTest$specCrown24T,
							worldMatrix: $elm_explorations$linear_algebra$Math$Matrix4$identity,
							worldPosition: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 0)
						};
						var graph = $author$project$Gear$Detect$buildGearGraph(
							_List_fromArray(
								[crown, spur]));
						return A2(
							$elm_explorations$test$Expect$equal,
							true,
							A2(
								$elm$core$List$member,
								1,
								A2(
									$elm$core$Maybe$withDefault,
									_List_Nil,
									A2($elm$core$Dict$get, 0, graph.connections))));
					}),
					A2(
					$elm_explorations$test$Test$test,
					'crown does not mesh with spur when axes are parallel',
					function (_v19) {
						var spur = {
							color: 16,
							id: 1,
							spec: $author$project$Gear$DetectTest$spec8T,
							worldMatrix: $elm_explorations$linear_algebra$Math$Matrix4$identity,
							worldPosition: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 40, 0, 0)
						};
						var crown = {
							color: 16,
							id: 0,
							spec: $author$project$Gear$DetectTest$specCrown24T,
							worldMatrix: $elm_explorations$linear_algebra$Math$Matrix4$identity,
							worldPosition: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 0)
						};
						var graph = $author$project$Gear$Detect$buildGearGraph(
							_List_fromArray(
								[crown, spur]));
						return A2(
							$elm_explorations$test$Expect$equal,
							_List_Nil,
							A2(
								$elm$core$Maybe$withDefault,
								_List_Nil,
								A2($elm$core$Dict$get, 0, graph.connections)));
					})
				]))
		]));
var $author$project$Gear$Physics$angleAt = F2(
	function (angles, gearId) {
		return A2(
			$elm$core$Maybe$withDefault,
			0.0,
			A2($elm$core$Dict$get, gearId, angles));
	});
var $author$project$Gear$PhysicsTest$spec = F2(
	function (teeth, pitchRadius) {
		return {partFile: 'test.dat', pitchRadius: pitchRadius, teeth: teeth};
	});
var $author$project$Gear$PhysicsTest$makeInstance = F3(
	function (id, teeth, pr) {
		return {
			color: 16,
			id: id,
			spec: A2($author$project$Gear$PhysicsTest$spec, teeth, pr),
			worldMatrix: $elm_explorations$linear_algebra$Math$Matrix4$identity,
			worldPosition: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 0)
		};
	});
var $author$project$Gear$PhysicsTest$chainGraph = F3(
	function (t0, t1, t2) {
		return {
			connections: $elm$core$Dict$fromList(
				_List_fromArray(
					[
						_Utils_Tuple2(
						0,
						_List_fromArray(
							[1])),
						_Utils_Tuple2(
						1,
						_List_fromArray(
							[0, 2])),
						_Utils_Tuple2(
						2,
						_List_fromArray(
							[1]))
					])),
			instances: $elm$core$Array$fromList(
				_List_fromArray(
					[
						A3($author$project$Gear$PhysicsTest$makeInstance, 0, t0, 16),
						A3($author$project$Gear$PhysicsTest$makeInstance, 1, t1, 24),
						A3($author$project$Gear$PhysicsTest$makeInstance, 2, t2, 16)
					]))
		};
	});
var $author$project$Gear$PhysicsTest$isolatedGraph = {
	connections: $elm$core$Dict$empty,
	instances: $elm$core$Array$fromList(
		_List_fromArray(
			[
				A3($author$project$Gear$PhysicsTest$makeInstance, 0, 8, 16),
				A3($author$project$Gear$PhysicsTest$makeInstance, 1, 16, 24)
			]))
};
var $author$project$Gear$Physics$gearTeeth = F2(
	function (graph, gearId) {
		return A2(
			$elm$core$Maybe$map,
			A2(
				$elm$core$Basics$composeR,
				function ($) {
					return $.spec;
				},
				function ($) {
					return $.teeth;
				}),
			A2($elm$core$Array$get, gearId, graph.instances));
	});
var $author$project$Gear$Physics$meshRatio = F2(
	function (t1, t2) {
		var baseRatio = t1 / t2;
		return ((t1 === 1) || (t2 === 1)) ? baseRatio : (-baseRatio);
	});
var $author$project$Gear$Physics$bfsStep = F4(
	function (graph, queue, visited, angles) {
		bfsStep:
		while (true) {
			if (!queue.b) {
				return angles;
			} else {
				var _v1 = queue.a;
				var currentId = _v1.a;
				var currentAngle = _v1.b;
				var rest = queue.b;
				var neighbours = A2(
					$elm$core$Maybe$withDefault,
					_List_Nil,
					A2($elm$core$Dict$get, currentId, graph.connections));
				var unvisited = A2(
					$elm$core$List$filter,
					function (n) {
						return !A2($elm$core$Set$member, n, visited);
					},
					neighbours);
				var _v2 = A3(
					$elm$core$List$foldl,
					F2(
						function (neighbourId, _v3) {
							var vis = _v3.a;
							var ang = _v3.b;
							var entries = _v3.c;
							var _v4 = _Utils_Tuple2(
								A2($author$project$Gear$Physics$gearTeeth, graph, currentId),
								A2($author$project$Gear$Physics$gearTeeth, graph, neighbourId));
							if ((_v4.a.$ === 'Just') && (_v4.b.$ === 'Just')) {
								var t1 = _v4.a.a;
								var t2 = _v4.b.a;
								var neighbourAngle = currentAngle * A2($author$project$Gear$Physics$meshRatio, t1, t2);
								return _Utils_Tuple3(
									A2($elm$core$Set$insert, neighbourId, vis),
									A3($elm$core$Dict$insert, neighbourId, neighbourAngle, ang),
									A2(
										$elm$core$List$cons,
										_Utils_Tuple2(neighbourId, neighbourAngle),
										entries));
							} else {
								return _Utils_Tuple3(vis, ang, entries);
							}
						}),
					_Utils_Tuple3(visited, angles, _List_Nil),
					unvisited);
				var newVisited = _v2.a;
				var newAngles = _v2.b;
				var newEntries = _v2.c;
				var $temp$graph = graph,
					$temp$queue = _Utils_ap(
					rest,
					$elm$core$List$reverse(newEntries)),
					$temp$visited = newVisited,
					$temp$angles = newAngles;
				graph = $temp$graph;
				queue = $temp$queue;
				visited = $temp$visited;
				angles = $temp$angles;
				continue bfsStep;
			}
		}
	});
var $elm$core$Dict$singleton = F2(
	function (key, value) {
		return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
	});
var $elm$core$Set$singleton = function (key) {
	return $elm$core$Set$Set_elm_builtin(
		A2($elm$core$Dict$singleton, key, _Utils_Tuple0));
};
var $author$project$Gear$Physics$propagate = F3(
	function (graph, motorId, motorAngle) {
		return A4(
			$author$project$Gear$Physics$bfsStep,
			graph,
			_List_fromArray(
				[
					_Utils_Tuple2(motorId, motorAngle)
				]),
			$elm$core$Set$singleton(motorId),
			A2($elm$core$Dict$singleton, motorId, motorAngle));
	});
var $elm$core$Dict$sizeHelp = F2(
	function (n, dict) {
		sizeHelp:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return n;
			} else {
				var left = dict.d;
				var right = dict.e;
				var $temp$n = A2($elm$core$Dict$sizeHelp, n + 1, right),
					$temp$dict = left;
				n = $temp$n;
				dict = $temp$dict;
				continue sizeHelp;
			}
		}
	});
var $elm$core$Dict$size = function (dict) {
	return A2($elm$core$Dict$sizeHelp, 0, dict);
};
var $author$project$Gear$PhysicsTest$twoGearGraph = F2(
	function (t0, t1) {
		return {
			connections: $elm$core$Dict$fromList(
				_List_fromArray(
					[
						_Utils_Tuple2(
						0,
						_List_fromArray(
							[1])),
						_Utils_Tuple2(
						1,
						_List_fromArray(
							[0]))
					])),
			instances: $elm$core$Array$fromList(
				_List_fromArray(
					[
						A3($author$project$Gear$PhysicsTest$makeInstance, 0, t0, 16),
						A3($author$project$Gear$PhysicsTest$makeInstance, 1, t1, 24)
					]))
		};
	});
var $author$project$Gear$PhysicsTest$suite = A2(
	$elm_explorations$test$Test$describe,
	'Gear.Physics',
	_List_fromArray(
		[
			A2(
			$elm_explorations$test$Test$describe,
			'propagate — basic',
			_List_fromArray(
				[
					A2(
					$elm_explorations$test$Test$test,
					'motor gear maps to motorAngle',
					function (_v0) {
						return A3(
							$elm_explorations$test$Expect$within,
							$elm_explorations$test$Expect$Absolute(1.0e-6),
							1.0,
							function (f) {
								return f(0);
							}(
								$author$project$Gear$Physics$angleAt(
									A3(
										$author$project$Gear$Physics$propagate,
										A2($author$project$Gear$PhysicsTest$twoGearGraph, 8, 16),
										0,
										1.0))));
					}),
					A2(
					$elm_explorations$test$Test$test,
					'disconnected gear is absent from result',
					function (_v1) {
						return A2(
							$elm_explorations$test$Expect$equal,
							false,
							A2(
								$elm$core$Dict$member,
								1,
								A3($author$project$Gear$Physics$propagate, $author$project$Gear$PhysicsTest$isolatedGraph, 0, 1.0)));
					}),
					A2(
					$elm_explorations$test$Test$test,
					'empty graph returns only motor gear',
					function (_v2) {
						var emptyGraph = {
							connections: $elm$core$Dict$empty,
							instances: $elm$core$Array$fromList(
								_List_fromArray(
									[
										A3($author$project$Gear$PhysicsTest$makeInstance, 0, 8, 16)
									]))
						};
						var result = A3($author$project$Gear$Physics$propagate, emptyGraph, 0, 2.0);
						return A2(
							$elm_explorations$test$Expect$equal,
							$elm$core$Dict$fromList(
								_List_fromArray(
									[
										_Utils_Tuple2(0, 2.0)
									])),
							result);
					})
				])),
			A2(
			$elm_explorations$test$Test$describe,
			'propagate — gear ratios',
			_List_fromArray(
				[
					A2(
					$elm_explorations$test$Test$test,
					'8T driving 16T: neighbour angle is -0.5 × motor',
					function (_v3) {
						return A3(
							$elm_explorations$test$Expect$within,
							$elm_explorations$test$Expect$Absolute(1.0e-6),
							-0.5,
							function (f) {
								return f(1);
							}(
								$author$project$Gear$Physics$angleAt(
									A3(
										$author$project$Gear$Physics$propagate,
										A2($author$project$Gear$PhysicsTest$twoGearGraph, 8, 16),
										0,
										1.0))));
					}),
					A2(
					$elm_explorations$test$Test$test,
					'16T driving 8T: neighbour angle is -2 × motor',
					function (_v4) {
						return A3(
							$elm_explorations$test$Expect$within,
							$elm_explorations$test$Expect$Absolute(1.0e-6),
							-2.0,
							function (f) {
								return f(1);
							}(
								$author$project$Gear$Physics$angleAt(
									A3(
										$author$project$Gear$Physics$propagate,
										A2($author$project$Gear$PhysicsTest$twoGearGraph, 16, 8),
										0,
										1.0))));
					}),
					A2(
					$elm_explorations$test$Test$test,
					'equal gears (8T → 8T): neighbour angle is -1 × motor',
					function (_v5) {
						return A3(
							$elm_explorations$test$Expect$within,
							$elm_explorations$test$Expect$Absolute(1.0e-6),
							-1.0,
							function (f) {
								return f(1);
							}(
								$author$project$Gear$Physics$angleAt(
									A3(
										$author$project$Gear$Physics$propagate,
										A2($author$project$Gear$PhysicsTest$twoGearGraph, 8, 8),
										0,
										1.0))));
					}),
					A2(
					$elm_explorations$test$Test$test,
					'worm (1T) driving 24T: neighbour angle is +1/24 × motor (no sign inversion)',
					function (_v6) {
						return A3(
							$elm_explorations$test$Expect$within,
							$elm_explorations$test$Expect$Absolute(1.0e-6),
							1 / 24,
							function (f) {
								return f(1);
							}(
								$author$project$Gear$Physics$angleAt(
									A3(
										$author$project$Gear$Physics$propagate,
										A2($author$project$Gear$PhysicsTest$twoGearGraph, 1, 24),
										0,
										1.0))));
					})
				])),
			A2(
			$elm_explorations$test$Test$describe,
			'propagate — 3-gear chain (8T → 16T → 8T)',
			_List_fromArray(
				[
					A2(
					$elm_explorations$test$Test$test,
					'gear 0 ratio = 1.0 (motor)',
					function (_v7) {
						return A3(
							$elm_explorations$test$Expect$within,
							$elm_explorations$test$Expect$Absolute(1.0e-6),
							1.0,
							function (f) {
								return f(0);
							}(
								$author$project$Gear$Physics$angleAt(
									A3(
										$author$project$Gear$Physics$propagate,
										A3($author$project$Gear$PhysicsTest$chainGraph, 8, 16, 8),
										0,
										1.0))));
					}),
					A2(
					$elm_explorations$test$Test$test,
					'gear 1 ratio = -0.5',
					function (_v8) {
						return A3(
							$elm_explorations$test$Expect$within,
							$elm_explorations$test$Expect$Absolute(1.0e-6),
							-0.5,
							function (f) {
								return f(1);
							}(
								$author$project$Gear$Physics$angleAt(
									A3(
										$author$project$Gear$Physics$propagate,
										A3($author$project$Gear$PhysicsTest$chainGraph, 8, 16, 8),
										0,
										1.0))));
					}),
					A2(
					$elm_explorations$test$Test$test,
					'gear 2 ratio = +1.0 (two sign inversions, same tooth count as gear 0)',
					function (_v9) {
						return A3(
							$elm_explorations$test$Expect$within,
							$elm_explorations$test$Expect$Absolute(1.0e-6),
							1.0,
							function (f) {
								return f(2);
							}(
								$author$project$Gear$Physics$angleAt(
									A3(
										$author$project$Gear$Physics$propagate,
										A3($author$project$Gear$PhysicsTest$chainGraph, 8, 16, 8),
										0,
										1.0))));
					}),
					A2(
					$elm_explorations$test$Test$test,
					'all three gears are in the result',
					function (_v10) {
						var result = A3(
							$author$project$Gear$Physics$propagate,
							A3($author$project$Gear$PhysicsTest$chainGraph, 8, 16, 8),
							0,
							1.0);
						return A2(
							$elm_explorations$test$Expect$equal,
							3,
							$elm$core$Dict$size(result));
					})
				])),
			A2(
			$elm_explorations$test$Test$describe,
			'angleAt',
			_List_fromArray(
				[
					A2(
					$elm_explorations$test$Test$test,
					'returns correct angle for present key',
					function (_v11) {
						return A3(
							$elm_explorations$test$Expect$within,
							$elm_explorations$test$Expect$Absolute(1.0e-6),
							1.5,
							A2(
								$author$project$Gear$Physics$angleAt,
								$elm$core$Dict$fromList(
									_List_fromArray(
										[
											_Utils_Tuple2(0, 1.5)
										])),
								0));
					}),
					A2(
					$elm_explorations$test$Test$test,
					'returns 0.0 for absent key',
					function (_v12) {
						return A3(
							$elm_explorations$test$Expect$within,
							$elm_explorations$test$Expect$Absolute(1.0e-6),
							0.0,
							A2($author$project$Gear$Physics$angleAt, $elm$core$Dict$empty, 99));
					})
				]))
		]));
var $author$project$LDraw$Colors$colorTable = $elm$core$Dict$fromList(
	_List_fromArray(
		[
			_Utils_Tuple2(
			0,
			{alpha: 1.0, b: 0.067, g: 0.067, r: 0.067}),
			_Utils_Tuple2(
			1,
			{alpha: 1.0, b: 0.749, g: 0.333, r: 0.0}),
			_Utils_Tuple2(
			2,
			{alpha: 1.0, b: 0.243, g: 0.478, r: 0.145}),
			_Utils_Tuple2(
			3,
			{alpha: 1.0, b: 0.561, g: 0.514, r: 0.0}),
			_Utils_Tuple2(
			4,
			{alpha: 1.0, b: 0.035, g: 0.102, r: 0.788}),
			_Utils_Tuple2(
			5,
			{alpha: 1.0, b: 0.439, g: 0.184, r: 0.784}),
			_Utils_Tuple2(
			6,
			{alpha: 1.0, b: 0.067, g: 0.212, r: 0.357}),
			_Utils_Tuple2(
			7,
			{alpha: 1.0, b: 0.608, g: 0.631, r: 0.608}),
			_Utils_Tuple2(
			8,
			{alpha: 1.0, b: 0.353, g: 0.373, r: 0.392}),
			_Utils_Tuple2(
			9,
			{alpha: 1.0, b: 0.933, g: 0.831, r: 0.682}),
			_Utils_Tuple2(
			10,
			{alpha: 1.0, b: 0.231, g: 0.78, r: 0.294}),
			_Utils_Tuple2(
			11,
			{alpha: 1.0, b: 0.682, g: 0.659, r: 0.0}),
			_Utils_Tuple2(
			12,
			{alpha: 1.0, b: 0.478, g: 0.565, r: 0.988}),
			_Utils_Tuple2(
			13,
			{alpha: 1.0, b: 0.749, g: 0.671, r: 0.988}),
			_Utils_Tuple2(
			14,
			{alpha: 1.0, b: 0.027, g: 0.812, r: 0.988}),
			_Utils_Tuple2(
			15,
			{alpha: 1.0, b: 1.0, g: 1.0, r: 1.0}),
			_Utils_Tuple2(
			17,
			{alpha: 1.0, b: 0.71, g: 0.902, r: 0.71}),
			_Utils_Tuple2(
			18,
			{alpha: 1.0, b: 0.624, g: 0.929, r: 0.988}),
			_Utils_Tuple2(
			19,
			{alpha: 1.0, b: 0.612, g: 0.835, r: 0.902}),
			_Utils_Tuple2(
			20,
			{alpha: 1.0, b: 0.878, g: 0.729, r: 0.812}),
			_Utils_Tuple2(
			22,
			{alpha: 1.0, b: 0.49, g: 0.082, r: 0.373}),
			_Utils_Tuple2(
			23,
			{alpha: 1.0, b: 0.62, g: 0.141, r: 0.122}),
			_Utils_Tuple2(
			25,
			{alpha: 1.0, b: 0.122, g: 0.502, r: 0.988}),
			_Utils_Tuple2(
			26,
			{alpha: 1.0, b: 0.502, g: 0.0, r: 0.627}),
			_Utils_Tuple2(
			27,
			{alpha: 1.0, b: 0.118, g: 0.878, r: 0.749}),
			_Utils_Tuple2(
			28,
			{alpha: 1.0, b: 0.337, g: 0.537, r: 0.639}),
			_Utils_Tuple2(
			29,
			{alpha: 1.0, b: 0.78, g: 0.671, r: 0.988}),
			_Utils_Tuple2(
			30,
			{alpha: 1.0, b: 0.718, g: 0.525, r: 0.667}),
			_Utils_Tuple2(
			31,
			{alpha: 1.0, b: 0.847, g: 0.714, r: 0.792}),
			_Utils_Tuple2(
			36,
			{alpha: 1.0, b: 0.518, g: 0.769, r: 0.988}),
			_Utils_Tuple2(
			38,
			{alpha: 1.0, b: 0.047, g: 0.251, r: 0.651}),
			_Utils_Tuple2(
			40,
			{alpha: 0.5, b: 0.243, g: 0.243, r: 0.243}),
			_Utils_Tuple2(
			41,
			{alpha: 0.5, b: 0.008, g: 0.071, r: 0.902}),
			_Utils_Tuple2(
			42,
			{alpha: 0.5, b: 0.0, g: 0.988, r: 0.773}),
			_Utils_Tuple2(
			43,
			{alpha: 0.5, b: 0.988, g: 0.851, r: 0.537}),
			_Utils_Tuple2(
			44,
			{alpha: 0.5, b: 0.741, g: 0.525, r: 0.682}),
			_Utils_Tuple2(
			45,
			{alpha: 0.5, b: 0.573, g: 0.4, r: 0.878}),
			_Utils_Tuple2(
			46,
			{alpha: 0.5, b: 0.165, g: 0.855, r: 0.988}),
			_Utils_Tuple2(
			47,
			{alpha: 0.5, b: 0.878, g: 0.878, r: 0.878}),
			_Utils_Tuple2(
			57,
			{alpha: 0.5, b: 0.122, g: 0.502, r: 0.988}),
			_Utils_Tuple2(
			68,
			{alpha: 1.0, b: 0.518, g: 0.769, r: 0.988}),
			_Utils_Tuple2(
			69,
			{alpha: 1.0, b: 0.624, g: 0.106, r: 0.584}),
			_Utils_Tuple2(
			70,
			{alpha: 1.0, b: 0.067, g: 0.188, r: 0.408}),
			_Utils_Tuple2(
			71,
			{alpha: 1.0, b: 0.749, g: 0.722, r: 0.694}),
			_Utils_Tuple2(
			72,
			{alpha: 1.0, b: 0.451, g: 0.427, r: 0.4}),
			_Utils_Tuple2(
			73,
			{alpha: 1.0, b: 0.808, g: 0.631, r: 0.486}),
			_Utils_Tuple2(
			74,
			{alpha: 1.0, b: 0.396, g: 0.733, r: 0.467}),
			_Utils_Tuple2(
			77,
			{alpha: 1.0, b: 0.749, g: 0.671, r: 0.988}),
			_Utils_Tuple2(
			78,
			{alpha: 1.0, b: 0.663, g: 0.847, r: 0.988}),
			_Utils_Tuple2(
			84,
			{alpha: 1.0, b: 0.184, g: 0.396, r: 0.71}),
			_Utils_Tuple2(
			85,
			{alpha: 1.0, b: 0.294, g: 0.09, r: 0.243}),
			_Utils_Tuple2(
			86,
			{alpha: 1.0, b: 0.114, g: 0.286, r: 0.58}),
			_Utils_Tuple2(
			89,
			{alpha: 1.0, b: 0.659, g: 0.341, r: 0.251}),
			_Utils_Tuple2(
			92,
			{alpha: 1.0, b: 0.302, g: 0.549, r: 0.851}),
			_Utils_Tuple2(
			100,
			{alpha: 1.0, b: 0.639, g: 0.729, r: 0.988}),
			_Utils_Tuple2(
			110,
			{alpha: 1.0, b: 0.6, g: 0.247, r: 0.247}),
			_Utils_Tuple2(
			112,
			{alpha: 1.0, b: 0.682, g: 0.412, r: 0.412}),
			_Utils_Tuple2(
			115,
			{alpha: 1.0, b: 0.176, g: 0.765, r: 0.624}),
			_Utils_Tuple2(
			118,
			{alpha: 1.0, b: 0.843, g: 0.902, r: 0.667}),
			_Utils_Tuple2(
			120,
			{alpha: 1.0, b: 0.494, g: 0.902, r: 0.812}),
			_Utils_Tuple2(
			125,
			{alpha: 1.0, b: 0.369, g: 0.671, r: 0.988}),
			_Utils_Tuple2(
			128,
			{alpha: 1.0, b: 0.047, g: 0.251, r: 0.651}),
			_Utils_Tuple2(
			151,
			{alpha: 1.0, b: 0.878, g: 0.875, r: 0.859}),
			_Utils_Tuple2(
			191,
			{alpha: 1.0, b: 0.047, g: 0.671, r: 0.988}),
			_Utils_Tuple2(
			212,
			{alpha: 1.0, b: 0.988, g: 0.812, r: 0.624}),
			_Utils_Tuple2(
			216,
			{alpha: 1.0, b: 0.082, g: 0.165, r: 0.671}),
			_Utils_Tuple2(
			226,
			{alpha: 1.0, b: 0.447, g: 0.929, r: 0.988}),
			_Utils_Tuple2(
			232,
			{alpha: 1.0, b: 0.878, g: 0.753, r: 0.533}),
			_Utils_Tuple2(
			256,
			{alpha: 1.0, b: 0.067, g: 0.067, r: 0.067}),
			_Utils_Tuple2(
			272,
			{alpha: 1.0, b: 0.475, g: 0.173, r: 0.0}),
			_Utils_Tuple2(
			288,
			{alpha: 1.0, b: 0.09, g: 0.267, r: 0.055}),
			_Utils_Tuple2(
			308,
			{alpha: 1.0, b: 0.055, g: 0.114, r: 0.188}),
			_Utils_Tuple2(
			320,
			{alpha: 1.0, b: 0.027, g: 0.0, r: 0.486}),
			_Utils_Tuple2(
			321,
			{alpha: 1.0, b: 0.773, g: 0.549, r: 0.0}),
			_Utils_Tuple2(
			322,
			{alpha: 1.0, b: 0.867, g: 0.71, r: 0.341}),
			_Utils_Tuple2(
			323,
			{alpha: 1.0, b: 0.886, g: 0.945, r: 0.788}),
			_Utils_Tuple2(
			326,
			{alpha: 1.0, b: 0.576, g: 0.929, r: 0.835}),
			_Utils_Tuple2(
			329,
			{alpha: 1.0, b: 0.902, g: 1.0, r: 1.0}),
			_Utils_Tuple2(
			334,
			{alpha: 1.0, b: 0.224, g: 0.608, r: 0.769}),
			_Utils_Tuple2(
			335,
			{alpha: 1.0, b: 0.459, g: 0.49, r: 0.694}),
			_Utils_Tuple2(
			366,
			{alpha: 1.0, b: 0.071, g: 0.376, r: 0.671}),
			_Utils_Tuple2(
			373,
			{alpha: 1.0, b: 0.592, g: 0.486, r: 0.58}),
			_Utils_Tuple2(
			378,
			{alpha: 1.0, b: 0.518, g: 0.608, r: 0.482}),
			_Utils_Tuple2(
			379,
			{alpha: 1.0, b: 0.616, g: 0.518, r: 0.427}),
			_Utils_Tuple2(
			383,
			{alpha: 1.0, b: 0.878, g: 0.878, r: 0.878}),
			_Utils_Tuple2(
			462,
			{alpha: 1.0, b: 0.369, g: 0.671, r: 0.988}),
			_Utils_Tuple2(
			484,
			{alpha: 1.0, b: 0.047, g: 0.251, r: 0.651}),
			_Utils_Tuple2(
			503,
			{alpha: 1.0, b: 0.867, g: 0.878, r: 0.878})
		]));
var $author$project$LDraw$Colors$lookupColor = function (code) {
	return A2(
		$elm$core$Maybe$withDefault,
		{alpha: 1.0, b: 1.0, g: 0.0, r: 1.0},
		A2($elm$core$Dict$get, code, $author$project$LDraw$Colors$colorTable));
};
var $author$project$LDraw$Colors$resolveColor = F2(
	function (parentColor, thisColor) {
		return ((thisColor === 16) || _Utils_eq(thisColor, -1)) ? $author$project$LDraw$Colors$lookupColor(parentColor) : ((thisColor === 24) ? {alpha: 1.0, b: 0.0, g: 0.0, r: 0.0} : $author$project$LDraw$Colors$lookupColor(thisColor));
	});
var $author$project$LDraw$ColorsTest$suite = A2(
	$elm_explorations$test$Test$describe,
	'LDraw.Colors.resolveColor',
	_List_fromArray(
		[
			A2(
			$elm_explorations$test$Test$test,
			'Studio color -1 inherits parent color like 16',
			function (_v0) {
				return A2(
					$elm_explorations$test$Expect$equal,
					A2($author$project$LDraw$Colors$resolveColor, 4, 16),
					A2($author$project$LDraw$Colors$resolveColor, 4, -1));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'supports additional known LEGO colors from shared catalog',
			function (_v1) {
				return A2(
					$elm_explorations$test$Expect$all,
					_List_fromArray(
						[
							function (_v2) {
							return A2(
								$elm_explorations$test$Expect$equal,
								{alpha: 0.5, b: 0.573, g: 0.4, r: 0.878},
								A2($author$project$LDraw$Colors$resolveColor, 15, 45));
						},
							function (_v3) {
							return A2(
								$elm_explorations$test$Expect$equal,
								{alpha: 1.0, b: 0.624, g: 0.106, r: 0.584},
								A2($author$project$LDraw$Colors$resolveColor, 15, 69));
						},
							function (_v4) {
							return A2(
								$elm_explorations$test$Expect$equal,
								{alpha: 1.0, b: 0.067, g: 0.067, r: 0.067},
								A2($author$project$LDraw$Colors$resolveColor, 15, 256));
						},
							function (_v5) {
							return A2(
								$elm_explorations$test$Expect$equal,
								{alpha: 1.0, b: 0.902, g: 1.0, r: 1.0},
								A2($author$project$LDraw$Colors$resolveColor, 15, 329));
						},
							function (_v6) {
							return A2(
								$elm_explorations$test$Expect$equal,
								{alpha: 1.0, b: 0.071, g: 0.376, r: 0.671},
								A2($author$project$LDraw$Colors$resolveColor, 15, 366));
						}
						]),
					_Utils_Tuple0);
			}),
			A2(
			$elm_explorations$test$Test$test,
			'unknown colors still fallback to magenta for visibility',
			function (_v7) {
				return A2(
					$elm_explorations$test$Expect$equal,
					{alpha: 1.0, b: 1.0, g: 0.0, r: 1.0},
					A2($author$project$LDraw$Colors$resolveColor, 15, 9999));
			})
		]));
var $author$project$LDraw$Types$Comment = function (a) {
	return {$: 'Comment', a: a};
};
var $author$project$LDraw$Types$ConditionalLine = function (a) {
	return {$: 'ConditionalLine', a: a};
};
var $author$project$LDraw$Types$LineSegment = function (a) {
	return {$: 'LineSegment', a: a};
};
var $author$project$LDraw$Types$Triangle = function (a) {
	return {$: 'Triangle', a: a};
};
var $author$project$LDraw$GeometryTest$allVertices = function (triangles) {
	return A2(
		$elm$core$List$concatMap,
		function (_v0) {
			var a = _v0.a;
			var b = _v0.b;
			var c = _v0.c;
			return _List_fromArray(
				[a, b, c]);
		},
		triangles);
};
var $author$project$LDraw$GeometryTest$emptyCache = $elm$core$Dict$empty;
var $elm_explorations$linear_algebra$Math$Vector3$cross = _MJS_v3cross;
var $author$project$LDraw$Geometry$faceNormal = F3(
	function (p1, p2, p3) {
		var edge2 = A2($elm_explorations$linear_algebra$Math$Vector3$sub, p3, p1);
		var edge1 = A2($elm_explorations$linear_algebra$Math$Vector3$sub, p2, p1);
		var cross = A2($elm_explorations$linear_algebra$Math$Vector3$cross, edge1, edge2);
		var len = $elm_explorations$linear_algebra$Math$Vector3$length(cross);
		return (len < 1.0e-8) ? A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 1, 0) : A2($elm_explorations$linear_algebra$Math$Vector3$scale, 1.0 / len, cross);
	});
var $author$project$LDraw$Geometry$mkVertex = F3(
	function (position, normal, color) {
		return {color: color, normal: normal, position: position};
	});
var $elm_explorations$linear_algebra$Math$Vector4$vec4 = _MJS_v4;
var $author$project$LDraw$Colors$toVec4 = function (c) {
	return A4($elm_explorations$linear_algebra$Math$Vector4$vec4, c.r, c.g, c.b, c.alpha);
};
var $elm_explorations$linear_algebra$Math$Vector3$getY = _MJS_v3getY;
var $elm_explorations$linear_algebra$Math$Vector3$getZ = _MJS_v3getZ;
var $author$project$LDraw$Geometry$transformPoint = F2(
	function (mat, p) {
		var tp = A2($elm_explorations$linear_algebra$Math$Matrix4$transform, mat, p);
		return A3(
			$elm_explorations$linear_algebra$Math$Vector3$vec3,
			$elm_explorations$linear_algebra$Math$Vector3$getX(tp),
			-$elm_explorations$linear_algebra$Math$Vector3$getY(tp),
			$elm_explorations$linear_algebra$Math$Vector3$getZ(tp));
	});
var $author$project$LDraw$Geometry$flattenLine = F5(
	function (cache, parentColor, transform, line, acc) {
		switch (line.$) {
			case 'SubFileRef':
				var ref = line.a;
				var _v1 = A2($elm$core$Dict$get, ref.file, cache);
				if ((_v1.$ === 'Just') && (_v1.a.$ === 'Loaded')) {
					var subLines = _v1.a.a;
					var childTransform = A2($elm_explorations$linear_algebra$Math$Matrix4$mul, transform, ref.transform);
					var childColor = (ref.color === 16) ? parentColor : ref.color;
					return A5($author$project$LDraw$Geometry$flattenLines, subLines, cache, childColor, childTransform, acc);
				} else {
					return acc;
				}
			case 'Triangle':
				var tri = line.a;
				var p3 = A2($author$project$LDraw$Geometry$transformPoint, transform, tri.p3);
				var p2 = A2($author$project$LDraw$Geometry$transformPoint, transform, tri.p2);
				var p1 = A2($author$project$LDraw$Geometry$transformPoint, transform, tri.p1);
				var normal = A3($author$project$LDraw$Geometry$faceNormal, p1, p2, p3);
				var color = $author$project$LDraw$Colors$toVec4(
					A2($author$project$LDraw$Colors$resolveColor, parentColor, tri.color));
				return _Utils_update(
					acc,
					{
						triangles: A2(
							$elm$core$List$cons,
							_Utils_Tuple3(
								A3($author$project$LDraw$Geometry$mkVertex, p1, normal, color),
								A3($author$project$LDraw$Geometry$mkVertex, p2, normal, color),
								A3($author$project$LDraw$Geometry$mkVertex, p3, normal, color)),
							acc.triangles)
					});
			case 'Quad':
				var quad = line.a;
				var p4 = A2($author$project$LDraw$Geometry$transformPoint, transform, quad.p4);
				var p3 = A2($author$project$LDraw$Geometry$transformPoint, transform, quad.p3);
				var p2 = A2($author$project$LDraw$Geometry$transformPoint, transform, quad.p2);
				var p1 = A2($author$project$LDraw$Geometry$transformPoint, transform, quad.p1);
				var n2 = A3($author$project$LDraw$Geometry$faceNormal, p1, p3, p4);
				var n1 = A3($author$project$LDraw$Geometry$faceNormal, p1, p2, p3);
				var color = $author$project$LDraw$Colors$toVec4(
					A2($author$project$LDraw$Colors$resolveColor, parentColor, quad.color));
				return _Utils_update(
					acc,
					{
						triangles: A2(
							$elm$core$List$cons,
							_Utils_Tuple3(
								A3($author$project$LDraw$Geometry$mkVertex, p1, n1, color),
								A3($author$project$LDraw$Geometry$mkVertex, p2, n1, color),
								A3($author$project$LDraw$Geometry$mkVertex, p3, n1, color)),
							A2(
								$elm$core$List$cons,
								_Utils_Tuple3(
									A3($author$project$LDraw$Geometry$mkVertex, p1, n2, color),
									A3($author$project$LDraw$Geometry$mkVertex, p3, n2, color),
									A3($author$project$LDraw$Geometry$mkVertex, p4, n2, color)),
								acc.triangles))
					});
			case 'LineSegment':
				var seg = line.a;
				return _Utils_update(
					acc,
					{
						lines: A2(
							$elm$core$List$cons,
							_Utils_Tuple2(
								A2($author$project$LDraw$Geometry$transformPoint, transform, seg.p1),
								A2($author$project$LDraw$Geometry$transformPoint, transform, seg.p2)),
							acc.lines)
					});
			case 'Comment':
				return acc;
			default:
				var cond = line.a;
				return _Utils_update(
					acc,
					{
						conditionalLines: A2(
							$elm$core$List$cons,
							{
								c1: A2($author$project$LDraw$Geometry$transformPoint, transform, cond.c1),
								c2: A2($author$project$LDraw$Geometry$transformPoint, transform, cond.c2),
								p1: A2($author$project$LDraw$Geometry$transformPoint, transform, cond.p1),
								p2: A2($author$project$LDraw$Geometry$transformPoint, transform, cond.p2)
							},
							acc.conditionalLines)
					});
		}
	});
var $author$project$LDraw$Geometry$flattenLines = F5(
	function (lines, cache, parentColor, transform, acc) {
		return A3(
			$elm$core$List$foldl,
			A3($author$project$LDraw$Geometry$flattenLine, cache, parentColor, transform),
			acc,
			lines);
	});
var $author$project$LDraw$Geometry$isBfcCertify = function (line) {
	if (line.$ === 'Comment') {
		var text = line.a;
		return A2($elm$core$String$contains, 'BFC CERTIFY CCW', text);
	} else {
		return false;
	}
};
var $author$project$LDraw$Geometry$hasBfcCertify = function (lines) {
	return A2($elm$core$List$any, $author$project$LDraw$Geometry$isBfcCertify, lines);
};
var $author$project$LDraw$Geometry$flatten = F4(
	function (lines, cache, parentColor, worldTransform) {
		var result = A5(
			$author$project$LDraw$Geometry$flattenLines,
			lines,
			cache,
			parentColor,
			worldTransform,
			{bfcCertified: false, conditionalLines: _List_Nil, lines: _List_Nil, triangles: _List_Nil});
		var bfc = $author$project$LDraw$Geometry$hasBfcCertify(lines);
		return _Utils_update(
			result,
			{bfcCertified: bfc, triangles: result.triangles});
	});
var $elm_explorations$linear_algebra$Math$Vector4$getX = _MJS_v4getX;
var $elm_explorations$linear_algebra$Math$Vector4$getY = _MJS_v4getY;
var $elm_explorations$linear_algebra$Math$Vector4$getZ = _MJS_v4getZ;
var $elm_explorations$test$Expect$greaterThan = A2($elm_explorations$test$Expect$compareWith, 'Expect.greaterThan', $elm$core$Basics$gt);
var $author$project$LDraw$GeometryTest$isAtOrigin = function (p) {
	return ($elm$core$Basics$abs(
		$elm_explorations$linear_algebra$Math$Vector3$getX(p)) < 1.0e-6) && (($elm$core$Basics$abs(
		$elm_explorations$linear_algebra$Math$Vector3$getY(p)) < 1.0e-6) && ($elm$core$Basics$abs(
		$elm_explorations$linear_algebra$Math$Vector3$getZ(p)) < 1.0e-6));
};
var $elm_explorations$linear_algebra$Math$Matrix4$makeTranslate = _MJS_m4x4makeTranslate;
var $author$project$LDraw$Types$Quad = function (a) {
	return {$: 'Quad', a: a};
};
var $author$project$LDraw$GeometryTest$xzQuad = function (color) {
	return $author$project$LDraw$Types$Quad(
		{
			color: color,
			p1: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, -1, 0, -1),
			p2: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 1, 0, -1),
			p3: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 1, 0, 1),
			p4: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, -1, 0, 1)
		});
};
var $author$project$LDraw$GeometryTest$xzTriangle = function (color) {
	return $author$project$LDraw$Types$Triangle(
		{
			color: color,
			p1: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 0),
			p2: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 1, 0, 0),
			p3: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 1)
		});
};
var $author$project$LDraw$GeometryTest$suite = A2(
	$elm_explorations$test$Test$describe,
	'LDraw.Geometry.flatten',
	_List_fromArray(
		[
			A2(
			$elm_explorations$test$Test$describe,
			'triangle count',
			_List_fromArray(
				[
					A2(
					$elm_explorations$test$Test$test,
					'one Triangle line produces 1 triangle',
					function (_v0) {
						var result = A4(
							$author$project$LDraw$Geometry$flatten,
							_List_fromArray(
								[
									$author$project$LDraw$GeometryTest$xzTriangle(4)
								]),
							$author$project$LDraw$GeometryTest$emptyCache,
							15,
							$elm_explorations$linear_algebra$Math$Matrix4$identity);
						return A2(
							$elm_explorations$test$Expect$equal,
							1,
							$elm$core$List$length(result.triangles));
					}),
					A2(
					$elm_explorations$test$Test$test,
					'one Quad line produces 2 triangles',
					function (_v1) {
						var result = A4(
							$author$project$LDraw$Geometry$flatten,
							_List_fromArray(
								[
									$author$project$LDraw$GeometryTest$xzQuad(4)
								]),
							$author$project$LDraw$GeometryTest$emptyCache,
							15,
							$elm_explorations$linear_algebra$Math$Matrix4$identity);
						return A2(
							$elm_explorations$test$Expect$equal,
							2,
							$elm$core$List$length(result.triangles));
					}),
					A2(
					$elm_explorations$test$Test$test,
					'three Triangles produce 3 triangles',
					function (_v2) {
						var lines = _List_fromArray(
							[
								$author$project$LDraw$GeometryTest$xzTriangle(4),
								$author$project$LDraw$GeometryTest$xzTriangle(2),
								$author$project$LDraw$GeometryTest$xzTriangle(1)
							]);
						var result = A4($author$project$LDraw$Geometry$flatten, lines, $author$project$LDraw$GeometryTest$emptyCache, 15, $elm_explorations$linear_algebra$Math$Matrix4$identity);
						return A2(
							$elm_explorations$test$Expect$equal,
							3,
							$elm$core$List$length(result.triangles));
					}),
					A2(
					$elm_explorations$test$Test$test,
					'Comment lines produce no geometry',
					function (_v3) {
						var result = A4(
							$author$project$LDraw$Geometry$flatten,
							_List_fromArray(
								[
									$author$project$LDraw$Types$Comment('0 BFC CERTIFY CCW'),
									$author$project$LDraw$Types$Comment('just a comment')
								]),
							$author$project$LDraw$GeometryTest$emptyCache,
							15,
							$elm_explorations$linear_algebra$Math$Matrix4$identity);
						return A2(
							$elm_explorations$test$Expect$equal,
							0,
							$elm$core$List$length(result.triangles));
					})
				])),
			A2(
			$elm_explorations$test$Test$describe,
			'Y-axis negation (LDraw Y-down → Y-up)',
			_List_fromArray(
				[
					A2(
					$elm_explorations$test$Test$test,
					'a point at LDraw Y=8 comes out at output Y=-8',
					function (_v4) {
						var lines = _List_fromArray(
							[
								$author$project$LDraw$Types$Triangle(
								{
									color: 4,
									p1: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 8, 0),
									p2: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 1, 8, 0),
									p3: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 8, 1)
								})
							]);
						var result = A4($author$project$LDraw$Geometry$flatten, lines, $author$project$LDraw$GeometryTest$emptyCache, 15, $elm_explorations$linear_algebra$Math$Matrix4$identity);
						var _v5 = result.triangles;
						if (_v5.b) {
							var _v6 = _v5.a;
							var v = _v6.a;
							return A3(
								$elm_explorations$test$Expect$within,
								$elm_explorations$test$Expect$Absolute(1.0e-6),
								-8.0,
								$elm_explorations$linear_algebra$Math$Vector3$getY(v.position));
						} else {
							return $elm_explorations$test$Expect$fail('Expected a triangle');
						}
					}),
					A2(
					$elm_explorations$test$Test$test,
					'a point at LDraw Y=0 stays at output Y=0',
					function (_v7) {
						var lines = _List_fromArray(
							[
								$author$project$LDraw$GeometryTest$xzTriangle(4)
							]);
						var result = A4($author$project$LDraw$Geometry$flatten, lines, $author$project$LDraw$GeometryTest$emptyCache, 15, $elm_explorations$linear_algebra$Math$Matrix4$identity);
						var _v8 = result.triangles;
						if (_v8.b) {
							var _v9 = _v8.a;
							var v = _v9.a;
							return A3(
								$elm_explorations$test$Expect$within,
								$elm_explorations$test$Expect$Absolute(1.0e-6),
								0.0,
								$elm_explorations$linear_algebra$Math$Vector3$getY(v.position));
						} else {
							return $elm_explorations$test$Expect$fail('Expected a triangle');
						}
					})
				])),
			A2(
			$elm_explorations$test$Test$describe,
			'normal direction',
			_List_fromArray(
				[
					A2(
					$elm_explorations$test$Test$test,
					'XZ-plane triangle (Y=0) has normal pointing up (+Y)',
					function (_v10) {
						var result = A4(
							$author$project$LDraw$Geometry$flatten,
							_List_fromArray(
								[
									$author$project$LDraw$GeometryTest$xzTriangle(4)
								]),
							$author$project$LDraw$GeometryTest$emptyCache,
							15,
							$elm_explorations$linear_algebra$Math$Matrix4$identity);
						var _v11 = result.triangles;
						if (_v11.b) {
							var _v12 = _v11.a;
							var v = _v12.a;
							return A2(
								$elm_explorations$test$Expect$greaterThan,
								0.0,
								$elm$core$Basics$abs(
									$elm_explorations$linear_algebra$Math$Vector3$getY(v.normal)));
						} else {
							return $elm_explorations$test$Expect$fail('Expected a triangle');
						}
					}),
					A2(
					$elm_explorations$test$Test$test,
					'normal is a unit vector',
					function (_v13) {
						var result = A4(
							$author$project$LDraw$Geometry$flatten,
							_List_fromArray(
								[
									$author$project$LDraw$GeometryTest$xzTriangle(4)
								]),
							$author$project$LDraw$GeometryTest$emptyCache,
							15,
							$elm_explorations$linear_algebra$Math$Matrix4$identity);
						var _v14 = result.triangles;
						if (_v14.b) {
							var _v15 = _v14.a;
							var v = _v15.a;
							return A3(
								$elm_explorations$test$Expect$within,
								$elm_explorations$test$Expect$Absolute(1.0e-5),
								1.0,
								$elm_explorations$linear_algebra$Math$Vector3$length(v.normal));
						} else {
							return $elm_explorations$test$Expect$fail('Expected a triangle');
						}
					}),
					A2(
					$elm_explorations$test$Test$test,
					'shared vertices across adjacent faces keep per-face normals',
					function (_v16) {
						var lines = _List_fromArray(
							[
								$author$project$LDraw$Types$Triangle(
								{
									color: 4,
									p1: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 0),
									p2: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 1, 0, 0),
									p3: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 1)
								}),
								$author$project$LDraw$Types$Triangle(
								{
									color: 4,
									p1: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 0),
									p2: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 1, 0, 0),
									p3: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 1, 0)
								})
							]);
						var result = A4($author$project$LDraw$Geometry$flatten, lines, $author$project$LDraw$GeometryTest$emptyCache, 15, $elm_explorations$linear_algebra$Math$Matrix4$identity);
						var originNormals = A2(
							$elm$core$List$map,
							function ($) {
								return $.normal;
							},
							A2(
								$elm$core$List$filter,
								function (v) {
									return $author$project$LDraw$GeometryTest$isAtOrigin(v.position);
								},
								$author$project$LDraw$GeometryTest$allVertices(result.triangles)));
						if (originNormals.b) {
							var n = originNormals.a;
							return A2(
								$elm_explorations$test$Expect$all,
								_List_fromArray(
									[
										function (normal) {
										return A2(
											$elm_explorations$test$Expect$equal,
											true,
											($elm$core$Basics$abs(
												$elm_explorations$linear_algebra$Math$Vector3$getY(normal)) < 1.0e-6) || ($elm$core$Basics$abs(
												$elm_explorations$linear_algebra$Math$Vector3$getZ(normal)) < 1.0e-6));
									},
										function (normal) {
										return A3(
											$elm_explorations$test$Expect$within,
											$elm_explorations$test$Expect$Absolute(1.0e-5),
											1.0,
											$elm_explorations$linear_algebra$Math$Vector3$length(normal));
									}
									]),
								n);
						} else {
							return $elm_explorations$test$Expect$fail('Expected at least one origin vertex');
						}
					})
				])),
			A2(
			$elm_explorations$test$Test$describe,
			'transform accumulation',
			_List_fromArray(
				[
					A2(
					$elm_explorations$test$Test$test,
					'translation shifts vertex positions',
					function (_v18) {
						var translationMat = $elm_explorations$linear_algebra$Math$Matrix4$makeTranslate(
							A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 10, 0, 0));
						var result = A4(
							$author$project$LDraw$Geometry$flatten,
							_List_fromArray(
								[
									$author$project$LDraw$GeometryTest$xzTriangle(4)
								]),
							$author$project$LDraw$GeometryTest$emptyCache,
							15,
							translationMat);
						var _v19 = result.triangles;
						if (_v19.b) {
							var _v20 = _v19.a;
							var v1 = _v20.a;
							return A3(
								$elm_explorations$test$Expect$within,
								$elm_explorations$test$Expect$Absolute(1.0e-5),
								10.0,
								$elm_explorations$linear_algebra$Math$Vector3$getX(v1.position));
						} else {
							return $elm_explorations$test$Expect$fail('Expected a triangle');
						}
					}),
					A2(
					$elm_explorations$test$Test$test,
					'sub-file transform accumulates with world transform',
					function (_v21) {
						var subTranslation = A3($elm_explorations$linear_algebra$Math$Matrix4$makeTranslate3, 20, 0, 0);
						var subFileRef = $author$project$LDraw$Types$SubFileRef(
							{color: 16, file: 'sub.dat', transform: subTranslation});
						var subContent = _List_fromArray(
							[
								$author$project$LDraw$GeometryTest$xzTriangle(4)
							]);
						var cache = $elm$core$Dict$fromList(
							_List_fromArray(
								[
									_Utils_Tuple2(
									'sub.dat',
									$author$project$LDraw$Resolve$Loaded(subContent))
								]));
						var result = A4(
							$author$project$LDraw$Geometry$flatten,
							_List_fromArray(
								[subFileRef]),
							cache,
							15,
							$elm_explorations$linear_algebra$Math$Matrix4$identity);
						var _v22 = result.triangles;
						if (_v22.b) {
							var _v23 = _v22.a;
							var v = _v23.a;
							return A3(
								$elm_explorations$test$Expect$within,
								$elm_explorations$test$Expect$Absolute(1.0e-5),
								20.0,
								$elm_explorations$linear_algebra$Math$Vector3$getX(v.position));
						} else {
							return $elm_explorations$test$Expect$fail('Expected a triangle from sub-file');
						}
					})
				])),
			A2(
			$elm_explorations$test$Test$describe,
			'color inheritance',
			_List_fromArray(
				[
					A2(
					$elm_explorations$test$Test$test,
					'color 16 in triangle inherits parent color (blue → code 1)',
					function (_v24) {
						var lines = _List_fromArray(
							[
								$author$project$LDraw$Types$Triangle(
								{
									color: 16,
									p1: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 0),
									p2: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 1, 0, 0),
									p3: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 1)
								})
							]);
						var result = A4($author$project$LDraw$Geometry$flatten, lines, $author$project$LDraw$GeometryTest$emptyCache, 1, $elm_explorations$linear_algebra$Math$Matrix4$identity);
						var _v25 = result.triangles;
						if (_v25.b) {
							var _v26 = _v25.a;
							var v = _v26.a;
							return A3(
								$elm_explorations$test$Expect$within,
								$elm_explorations$test$Expect$Absolute(0.05),
								0.0,
								$elm_explorations$linear_algebra$Math$Vector4$getX(v.color));
						} else {
							return $elm_explorations$test$Expect$fail('Expected a triangle');
						}
					}),
					A2(
					$elm_explorations$test$Test$test,
					'unknown color falls back to magenta (r=1, g=0, b=1)',
					function (_v27) {
						var lines = _List_fromArray(
							[
								$author$project$LDraw$Types$Triangle(
								{
									color: 9999,
									p1: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 0),
									p2: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 1, 0, 0),
									p3: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 1)
								})
							]);
						var result = A4($author$project$LDraw$Geometry$flatten, lines, $author$project$LDraw$GeometryTest$emptyCache, 15, $elm_explorations$linear_algebra$Math$Matrix4$identity);
						var _v28 = result.triangles;
						if (_v28.b) {
							var _v29 = _v28.a;
							var v = _v29.a;
							return A2(
								$elm_explorations$test$Expect$all,
								_List_fromArray(
									[
										function (col) {
										return A3(
											$elm_explorations$test$Expect$within,
											$elm_explorations$test$Expect$Absolute(1.0e-5),
											1.0,
											$elm_explorations$linear_algebra$Math$Vector4$getX(col));
									},
										function (col) {
										return A3(
											$elm_explorations$test$Expect$within,
											$elm_explorations$test$Expect$Absolute(1.0e-5),
											0.0,
											$elm_explorations$linear_algebra$Math$Vector4$getY(col));
									},
										function (col) {
										return A3(
											$elm_explorations$test$Expect$within,
											$elm_explorations$test$Expect$Absolute(1.0e-5),
											1.0,
											$elm_explorations$linear_algebra$Math$Vector4$getZ(col));
									}
									]),
								v.color);
						} else {
							return $elm_explorations$test$Expect$fail('Expected a triangle');
						}
					})
				])),
			A2(
			$elm_explorations$test$Test$describe,
			'missing sub-file',
			_List_fromArray(
				[
					A2(
					$elm_explorations$test$Test$test,
					'sub-file absent from cache is silently skipped',
					function (_v30) {
						var subFileRef = $author$project$LDraw$Types$SubFileRef(
							{color: 16, file: 'missing.dat', transform: $elm_explorations$linear_algebra$Math$Matrix4$identity});
						var lines = _List_fromArray(
							[
								subFileRef,
								$author$project$LDraw$GeometryTest$xzTriangle(4)
							]);
						var result = A4($author$project$LDraw$Geometry$flatten, lines, $author$project$LDraw$GeometryTest$emptyCache, 15, $elm_explorations$linear_algebra$Math$Matrix4$identity);
						return A2(
							$elm_explorations$test$Expect$equal,
							1,
							$elm$core$List$length(result.triangles));
					})
				])),
			A2(
			$elm_explorations$test$Test$describe,
			'edge lines',
			_List_fromArray(
				[
					A2(
					$elm_explorations$test$Test$test,
					'LineSegment produces one line pair',
					function (_v31) {
						var lines = _List_fromArray(
							[
								$author$project$LDraw$Types$LineSegment(
								{
									color: 24,
									p1: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 0),
									p2: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 10, 0, 0)
								})
							]);
						var result = A4($author$project$LDraw$Geometry$flatten, lines, $author$project$LDraw$GeometryTest$emptyCache, 15, $elm_explorations$linear_algebra$Math$Matrix4$identity);
						return A2(
							$elm_explorations$test$Expect$equal,
							1,
							$elm$core$List$length(result.lines));
					})
				])),
			A2(
			$elm_explorations$test$Test$describe,
			'conditional lines',
			_List_fromArray(
				[
					A2(
					$elm_explorations$test$Test$test,
					'ConditionalLine is retained for runtime visibility checks',
					function (_v32) {
						var lines = _List_fromArray(
							[
								$author$project$LDraw$Types$ConditionalLine(
								{
									c1: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 1),
									c2: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 1, 1),
									color: 24,
									p1: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 0),
									p2: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 10, 0, 0)
								})
							]);
						var result = A4($author$project$LDraw$Geometry$flatten, lines, $author$project$LDraw$GeometryTest$emptyCache, 15, $elm_explorations$linear_algebra$Math$Matrix4$identity);
						return A2(
							$elm_explorations$test$Expect$equal,
							1,
							$elm$core$List$length(result.conditionalLines));
					})
				])),
			A2(
			$elm_explorations$test$Test$describe,
			'BFC certification',
			_List_fromArray(
				[
					A2(
					$elm_explorations$test$Test$test,
					'file with BFC CERTIFY CCW sets bfcCertified = True',
					function (_v33) {
						var lines = _List_fromArray(
							[
								$author$project$LDraw$Types$Comment('BFC CERTIFY CCW'),
								$author$project$LDraw$GeometryTest$xzTriangle(4)
							]);
						var result = A4($author$project$LDraw$Geometry$flatten, lines, $author$project$LDraw$GeometryTest$emptyCache, 15, $elm_explorations$linear_algebra$Math$Matrix4$identity);
						return A2($elm_explorations$test$Expect$equal, true, result.bfcCertified);
					}),
					A2(
					$elm_explorations$test$Test$test,
					'file without BFC declaration sets bfcCertified = False',
					function (_v34) {
						var result = A4(
							$author$project$LDraw$Geometry$flatten,
							_List_fromArray(
								[
									$author$project$LDraw$GeometryTest$xzTriangle(4)
								]),
							$author$project$LDraw$GeometryTest$emptyCache,
							15,
							$elm_explorations$linear_algebra$Math$Matrix4$identity);
						return A2($elm_explorations$test$Expect$equal, false, result.bfcCertified);
					})
				]))
		]));
var $elm$core$String$lines = _String_lines;
var $author$project$LDraw$Parser$floatList = function (strs) {
	var parsed = A2($elm$core$List$filterMap, $elm$core$String$toFloat, strs);
	return _Utils_eq(
		$elm$core$List$length(parsed),
		$elm$core$List$length(strs)) ? $elm$core$Maybe$Just(parsed) : $elm$core$Maybe$Nothing;
};
var $author$project$LDraw$Parser$getF = F2(
	function (idx, lst) {
		return A2(
			$elm$core$Maybe$withDefault,
			0.0,
			$elm$core$List$head(
				A2($elm$core$List$drop, idx, lst)));
	});
var $elm$core$Maybe$map2 = F3(
	function (func, ma, mb) {
		if (ma.$ === 'Nothing') {
			return $elm$core$Maybe$Nothing;
		} else {
			var a = ma.a;
			if (mb.$ === 'Nothing') {
				return $elm$core$Maybe$Nothing;
			} else {
				var b = mb.a;
				return $elm$core$Maybe$Just(
					A2(func, a, b));
			}
		}
	});
var $author$project$LDraw$Parser$parseConditional = function (tokens) {
	if (((((((((((((tokens.b && tokens.b.b) && tokens.b.b.b) && tokens.b.b.b.b) && tokens.b.b.b.b.b) && tokens.b.b.b.b.b.b) && tokens.b.b.b.b.b.b.b) && tokens.b.b.b.b.b.b.b.b) && tokens.b.b.b.b.b.b.b.b.b) && tokens.b.b.b.b.b.b.b.b.b.b) && tokens.b.b.b.b.b.b.b.b.b.b.b) && tokens.b.b.b.b.b.b.b.b.b.b.b.b) && tokens.b.b.b.b.b.b.b.b.b.b.b.b.b) && (!tokens.b.b.b.b.b.b.b.b.b.b.b.b.b.b)) {
		var c = tokens.a;
		var _v1 = tokens.b;
		var x1 = _v1.a;
		var _v2 = _v1.b;
		var y1 = _v2.a;
		var _v3 = _v2.b;
		var z1 = _v3.a;
		var _v4 = _v3.b;
		var x2 = _v4.a;
		var _v5 = _v4.b;
		var y2 = _v5.a;
		var _v6 = _v5.b;
		var z2 = _v6.a;
		var _v7 = _v6.b;
		var x3 = _v7.a;
		var _v8 = _v7.b;
		var y3 = _v8.a;
		var _v9 = _v8.b;
		var z3 = _v9.a;
		var _v10 = _v9.b;
		var x4 = _v10.a;
		var _v11 = _v10.b;
		var y4 = _v11.a;
		var _v12 = _v11.b;
		var z4 = _v12.a;
		return A3(
			$elm$core$Maybe$map2,
			F2(
				function (color, pts) {
					return $author$project$LDraw$Types$ConditionalLine(
						{
							c1: A3(
								$elm_explorations$linear_algebra$Math$Vector3$vec3,
								A2($author$project$LDraw$Parser$getF, 6, pts),
								A2($author$project$LDraw$Parser$getF, 7, pts),
								A2($author$project$LDraw$Parser$getF, 8, pts)),
							c2: A3(
								$elm_explorations$linear_algebra$Math$Vector3$vec3,
								A2($author$project$LDraw$Parser$getF, 9, pts),
								A2($author$project$LDraw$Parser$getF, 10, pts),
								A2($author$project$LDraw$Parser$getF, 11, pts)),
							color: color,
							p1: A3(
								$elm_explorations$linear_algebra$Math$Vector3$vec3,
								A2($author$project$LDraw$Parser$getF, 0, pts),
								A2($author$project$LDraw$Parser$getF, 1, pts),
								A2($author$project$LDraw$Parser$getF, 2, pts)),
							p2: A3(
								$elm_explorations$linear_algebra$Math$Vector3$vec3,
								A2($author$project$LDraw$Parser$getF, 3, pts),
								A2($author$project$LDraw$Parser$getF, 4, pts),
								A2($author$project$LDraw$Parser$getF, 5, pts))
						});
				}),
			$elm$core$String$toInt(c),
			$author$project$LDraw$Parser$floatList(
				_List_fromArray(
					[x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, y4, z4])));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$LDraw$Parser$parseLineSegment = function (tokens) {
	if (((((((tokens.b && tokens.b.b) && tokens.b.b.b) && tokens.b.b.b.b) && tokens.b.b.b.b.b) && tokens.b.b.b.b.b.b) && tokens.b.b.b.b.b.b.b) && (!tokens.b.b.b.b.b.b.b.b)) {
		var c = tokens.a;
		var _v1 = tokens.b;
		var x1 = _v1.a;
		var _v2 = _v1.b;
		var y1 = _v2.a;
		var _v3 = _v2.b;
		var z1 = _v3.a;
		var _v4 = _v3.b;
		var x2 = _v4.a;
		var _v5 = _v4.b;
		var y2 = _v5.a;
		var _v6 = _v5.b;
		var z2 = _v6.a;
		return A3(
			$elm$core$Maybe$map2,
			F2(
				function (color, pts) {
					return $author$project$LDraw$Types$LineSegment(
						{
							color: color,
							p1: A3(
								$elm_explorations$linear_algebra$Math$Vector3$vec3,
								A2($author$project$LDraw$Parser$getF, 0, pts),
								A2($author$project$LDraw$Parser$getF, 1, pts),
								A2($author$project$LDraw$Parser$getF, 2, pts)),
							p2: A3(
								$elm_explorations$linear_algebra$Math$Vector3$vec3,
								A2($author$project$LDraw$Parser$getF, 3, pts),
								A2($author$project$LDraw$Parser$getF, 4, pts),
								A2($author$project$LDraw$Parser$getF, 5, pts))
						});
				}),
			$elm$core$String$toInt(c),
			$author$project$LDraw$Parser$floatList(
				_List_fromArray(
					[x1, y1, z1, x2, y2, z2])));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$LDraw$Parser$parseQuad = function (tokens) {
	if (((((((((((((tokens.b && tokens.b.b) && tokens.b.b.b) && tokens.b.b.b.b) && tokens.b.b.b.b.b) && tokens.b.b.b.b.b.b) && tokens.b.b.b.b.b.b.b) && tokens.b.b.b.b.b.b.b.b) && tokens.b.b.b.b.b.b.b.b.b) && tokens.b.b.b.b.b.b.b.b.b.b) && tokens.b.b.b.b.b.b.b.b.b.b.b) && tokens.b.b.b.b.b.b.b.b.b.b.b.b) && tokens.b.b.b.b.b.b.b.b.b.b.b.b.b) && (!tokens.b.b.b.b.b.b.b.b.b.b.b.b.b.b)) {
		var c = tokens.a;
		var _v1 = tokens.b;
		var x1 = _v1.a;
		var _v2 = _v1.b;
		var y1 = _v2.a;
		var _v3 = _v2.b;
		var z1 = _v3.a;
		var _v4 = _v3.b;
		var x2 = _v4.a;
		var _v5 = _v4.b;
		var y2 = _v5.a;
		var _v6 = _v5.b;
		var z2 = _v6.a;
		var _v7 = _v6.b;
		var x3 = _v7.a;
		var _v8 = _v7.b;
		var y3 = _v8.a;
		var _v9 = _v8.b;
		var z3 = _v9.a;
		var _v10 = _v9.b;
		var x4 = _v10.a;
		var _v11 = _v10.b;
		var y4 = _v11.a;
		var _v12 = _v11.b;
		var z4 = _v12.a;
		return A3(
			$elm$core$Maybe$map2,
			F2(
				function (color, pts) {
					return $author$project$LDraw$Types$Quad(
						{
							color: color,
							p1: A3(
								$elm_explorations$linear_algebra$Math$Vector3$vec3,
								A2($author$project$LDraw$Parser$getF, 0, pts),
								A2($author$project$LDraw$Parser$getF, 1, pts),
								A2($author$project$LDraw$Parser$getF, 2, pts)),
							p2: A3(
								$elm_explorations$linear_algebra$Math$Vector3$vec3,
								A2($author$project$LDraw$Parser$getF, 3, pts),
								A2($author$project$LDraw$Parser$getF, 4, pts),
								A2($author$project$LDraw$Parser$getF, 5, pts)),
							p3: A3(
								$elm_explorations$linear_algebra$Math$Vector3$vec3,
								A2($author$project$LDraw$Parser$getF, 6, pts),
								A2($author$project$LDraw$Parser$getF, 7, pts),
								A2($author$project$LDraw$Parser$getF, 8, pts)),
							p4: A3(
								$elm_explorations$linear_algebra$Math$Vector3$vec3,
								A2($author$project$LDraw$Parser$getF, 9, pts),
								A2($author$project$LDraw$Parser$getF, 10, pts),
								A2($author$project$LDraw$Parser$getF, 11, pts))
						});
				}),
			$elm$core$String$toInt(c),
			$author$project$LDraw$Parser$floatList(
				_List_fromArray(
					[x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, y4, z4])));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm_explorations$linear_algebra$Math$Matrix4$fromRecord = _MJS_m4x4fromRecord;
var $elm$core$Maybe$map5 = F6(
	function (func, ma, mb, mc, md, me) {
		if (ma.$ === 'Nothing') {
			return $elm$core$Maybe$Nothing;
		} else {
			var a = ma.a;
			if (mb.$ === 'Nothing') {
				return $elm$core$Maybe$Nothing;
			} else {
				var b = mb.a;
				if (mc.$ === 'Nothing') {
					return $elm$core$Maybe$Nothing;
				} else {
					var c = mc.a;
					if (md.$ === 'Nothing') {
						return $elm$core$Maybe$Nothing;
					} else {
						var d = md.a;
						if (me.$ === 'Nothing') {
							return $elm$core$Maybe$Nothing;
						} else {
							var e = me.a;
							return $elm$core$Maybe$Just(
								A5(func, a, b, c, d, e));
						}
					}
				}
			}
		}
	});
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$startsWith = _String_startsWith;
var $author$project$LDraw$Parser$stripLeadingSegments = function (value) {
	stripLeadingSegments:
	while (true) {
		if (A2($elm$core$String$startsWith, './', value)) {
			var $temp$value = A2($elm$core$String$dropLeft, 2, value);
			value = $temp$value;
			continue stripLeadingSegments;
		} else {
			if (A2($elm$core$String$startsWith, '/', value)) {
				var $temp$value = A2($elm$core$String$dropLeft, 1, value);
				value = $temp$value;
				continue stripLeadingSegments;
			} else {
				if (A2($elm$core$String$startsWith, 'ldraw/', value)) {
					var $temp$value = A2($elm$core$String$dropLeft, 6, value);
					value = $temp$value;
					continue stripLeadingSegments;
				} else {
					if (A2($elm$core$String$startsWith, 'parts/s/', value)) {
						return 's/' + A2($elm$core$String$dropLeft, 8, value);
					} else {
						if (A2($elm$core$String$startsWith, 'parts/', value)) {
							return A2($elm$core$String$dropLeft, 6, value);
						} else {
							if (A2($elm$core$String$startsWith, 'p/48/', value)) {
								return '48/' + A2($elm$core$String$dropLeft, 5, value);
							} else {
								if (A2($elm$core$String$startsWith, 'p/', value)) {
									return A2($elm$core$String$dropLeft, 2, value);
								} else {
									if (A2($elm$core$String$startsWith, 'models/', value)) {
										return A2($elm$core$String$dropLeft, 7, value);
									} else {
										return value;
									}
								}
							}
						}
					}
				}
			}
		}
	}
};
var $elm$core$String$toLower = _String_toLower;
var $author$project$LDraw$Parser$normaliseName = function (raw) {
	return $author$project$LDraw$Parser$stripLeadingSegments(
		$elm$core$String$trim(
			A3(
				$elm$core$String$replace,
				'\\',
				'/',
				$elm$core$String$toLower(raw))));
};
var $author$project$LDraw$Parser$parseSubFileRef = function (tokens) {
	if ((((((((((((((tokens.b && tokens.b.b) && tokens.b.b.b) && tokens.b.b.b.b) && tokens.b.b.b.b.b) && tokens.b.b.b.b.b.b) && tokens.b.b.b.b.b.b.b) && tokens.b.b.b.b.b.b.b.b) && tokens.b.b.b.b.b.b.b.b.b) && tokens.b.b.b.b.b.b.b.b.b.b) && tokens.b.b.b.b.b.b.b.b.b.b.b) && tokens.b.b.b.b.b.b.b.b.b.b.b.b) && tokens.b.b.b.b.b.b.b.b.b.b.b.b.b) && tokens.b.b.b.b.b.b.b.b.b.b.b.b.b.b) && (!tokens.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b)) {
		var c = tokens.a;
		var _v1 = tokens.b;
		var x = _v1.a;
		var _v2 = _v1.b;
		var y = _v2.a;
		var _v3 = _v2.b;
		var z = _v3.a;
		var _v4 = _v3.b;
		var a = _v4.a;
		var _v5 = _v4.b;
		var b = _v5.a;
		var _v6 = _v5.b;
		var d = _v6.a;
		var _v7 = _v6.b;
		var e = _v7.a;
		var _v8 = _v7.b;
		var f = _v8.a;
		var _v9 = _v8.b;
		var g = _v9.a;
		var _v10 = _v9.b;
		var h = _v10.a;
		var _v11 = _v10.b;
		var i = _v11.a;
		var _v12 = _v11.b;
		var j = _v12.a;
		var _v13 = _v12.b;
		var file = _v13.a;
		return A6(
			$elm$core$Maybe$map5,
			F5(
				function (color, tx, ty, tz, rot) {
					return $author$project$LDraw$Types$SubFileRef(
						{
							color: color,
							file: $author$project$LDraw$Parser$normaliseName(file),
							transform: $elm_explorations$linear_algebra$Math$Matrix4$fromRecord(
								{
									m11: A2($author$project$LDraw$Parser$getF, 0, rot),
									m12: A2($author$project$LDraw$Parser$getF, 1, rot),
									m13: A2($author$project$LDraw$Parser$getF, 2, rot),
									m14: tx,
									m21: A2($author$project$LDraw$Parser$getF, 3, rot),
									m22: A2($author$project$LDraw$Parser$getF, 4, rot),
									m23: A2($author$project$LDraw$Parser$getF, 5, rot),
									m24: ty,
									m31: A2($author$project$LDraw$Parser$getF, 6, rot),
									m32: A2($author$project$LDraw$Parser$getF, 7, rot),
									m33: A2($author$project$LDraw$Parser$getF, 8, rot),
									m34: tz,
									m41: 0,
									m42: 0,
									m43: 0,
									m44: 1
								})
						});
				}),
			$elm$core$String$toInt(c),
			$elm$core$String$toFloat(x),
			$elm$core$String$toFloat(y),
			$elm$core$String$toFloat(z),
			$author$project$LDraw$Parser$floatList(
				_List_fromArray(
					[a, b, d, e, f, g, h, i, j])));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$LDraw$Parser$parseSubFileRefV2 = function (tokens) {
	if (((tokens.b && tokens.b.b) && tokens.b.b.b) && tokens.b.b.b.b) {
		var color = tokens.a;
		var _v1 = tokens.b;
		var _v2 = _v1.b;
		var _v3 = _v2.b;
		var rest = _v3.b;
		return $author$project$LDraw$Parser$parseSubFileRef(
			A2($elm$core$List$cons, color, rest));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$LDraw$Parser$parseTriangle = function (tokens) {
	if ((((((((((tokens.b && tokens.b.b) && tokens.b.b.b) && tokens.b.b.b.b) && tokens.b.b.b.b.b) && tokens.b.b.b.b.b.b) && tokens.b.b.b.b.b.b.b) && tokens.b.b.b.b.b.b.b.b) && tokens.b.b.b.b.b.b.b.b.b) && tokens.b.b.b.b.b.b.b.b.b.b) && (!tokens.b.b.b.b.b.b.b.b.b.b.b)) {
		var c = tokens.a;
		var _v1 = tokens.b;
		var x1 = _v1.a;
		var _v2 = _v1.b;
		var y1 = _v2.a;
		var _v3 = _v2.b;
		var z1 = _v3.a;
		var _v4 = _v3.b;
		var x2 = _v4.a;
		var _v5 = _v4.b;
		var y2 = _v5.a;
		var _v6 = _v5.b;
		var z2 = _v6.a;
		var _v7 = _v6.b;
		var x3 = _v7.a;
		var _v8 = _v7.b;
		var y3 = _v8.a;
		var _v9 = _v8.b;
		var z3 = _v9.a;
		return A3(
			$elm$core$Maybe$map2,
			F2(
				function (color, pts) {
					return $author$project$LDraw$Types$Triangle(
						{
							color: color,
							p1: A3(
								$elm_explorations$linear_algebra$Math$Vector3$vec3,
								A2($author$project$LDraw$Parser$getF, 0, pts),
								A2($author$project$LDraw$Parser$getF, 1, pts),
								A2($author$project$LDraw$Parser$getF, 2, pts)),
							p2: A3(
								$elm_explorations$linear_algebra$Math$Vector3$vec3,
								A2($author$project$LDraw$Parser$getF, 3, pts),
								A2($author$project$LDraw$Parser$getF, 4, pts),
								A2($author$project$LDraw$Parser$getF, 5, pts)),
							p3: A3(
								$elm_explorations$linear_algebra$Math$Vector3$vec3,
								A2($author$project$LDraw$Parser$getF, 6, pts),
								A2($author$project$LDraw$Parser$getF, 7, pts),
								A2($author$project$LDraw$Parser$getF, 8, pts))
						});
				}),
			$elm$core$String$toInt(c),
			$author$project$LDraw$Parser$floatList(
				_List_fromArray(
					[x1, y1, z1, x2, y2, z2, x3, y3, z3])));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$String$words = _String_words;
var $author$project$LDraw$Parser$parseLine = function (raw) {
	var _v0 = $elm$core$String$words(raw);
	if (!_v0.b) {
		return $elm$core$Maybe$Nothing;
	} else {
		var typeStr = _v0.a;
		var rest = _v0.b;
		switch (typeStr) {
			case '0':
				return $elm$core$Maybe$Just(
					$author$project$LDraw$Types$Comment(
						A2($elm$core$String$join, ' ', rest)));
			case '1':
				return $author$project$LDraw$Parser$parseSubFileRef(rest);
			case '11':
				return $author$project$LDraw$Parser$parseSubFileRefV2(rest);
			case '2':
				return $author$project$LDraw$Parser$parseLineSegment(rest);
			case '3':
				return $author$project$LDraw$Parser$parseTriangle(rest);
			case '4':
				return $author$project$LDraw$Parser$parseQuad(rest);
			case '5':
				return $author$project$LDraw$Parser$parseConditional(rest);
			default:
				return $elm$core$Maybe$Nothing;
		}
	}
};
var $author$project$LDraw$Parser$parseFile = function (text) {
	return A2(
		$elm$core$List$filterMap,
		$author$project$LDraw$Parser$parseLine,
		$elm$core$String$lines(text));
};
var $author$project$LDraw$Parser$parseFileName = function (line) {
	var ws = $elm$core$String$words(line);
	if (((ws.b && (ws.a === '0')) && ws.b.b) && (ws.b.a === 'FILE')) {
		var _v1 = ws.b;
		var rest = _v1.b;
		return $elm$core$Maybe$Just(
			$author$project$LDraw$Parser$normaliseName(
				A2($elm$core$String$join, ' ', rest)));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$LDraw$Parser$splitMpd = function (text) {
	var step = F2(
		function (line, _v4) {
			var currentName = _v4.a;
			var currentLines = _v4.b;
			var acc = _v4.c;
			var _v2 = $author$project$LDraw$Parser$parseFileName(line);
			if (_v2.$ === 'Just') {
				var name = _v2.a;
				var flushed = function () {
					if (currentName.$ === 'Just') {
						var n = currentName.a;
						return A3(
							$elm$core$Dict$insert,
							n,
							A2(
								$elm$core$String$join,
								'\n',
								$elm$core$List$reverse(currentLines)),
							acc);
					} else {
						return acc;
					}
				}();
				return _Utils_Tuple3(
					$elm$core$Maybe$Just(name),
					_List_Nil,
					flushed);
			} else {
				return _Utils_Tuple3(
					currentName,
					A2($elm$core$List$cons, line, currentLines),
					acc);
			}
		});
	var lines = $elm$core$String$lines(text);
	var _v0 = A3(
		$elm$core$List$foldl,
		step,
		_Utils_Tuple3($elm$core$Maybe$Nothing, _List_Nil, $elm$core$Dict$empty),
		lines);
	var lastName = _v0.a;
	var lastLines = _v0.b;
	var result = _v0.c;
	if (lastName.$ === 'Just') {
		var n = lastName.a;
		return A3(
			$elm$core$Dict$insert,
			n,
			A2(
				$elm$core$String$join,
				'\n',
				$elm$core$List$reverse(lastLines)),
			result);
	} else {
		return result;
	}
};
var $elm$core$Debug$toString = _Debug_toString;
var $author$project$LDraw$ParserTest$suite = A2(
	$elm_explorations$test$Test$describe,
	'LDraw.Parser',
	_List_fromArray(
		[
			A2(
			$elm_explorations$test$Test$describe,
			'parseLine',
			_List_fromArray(
				[
					A2(
					$elm_explorations$test$Test$test,
					'type 0 comment',
					function (_v0) {
						return A2(
							$elm_explorations$test$Expect$equal,
							$elm$core$Maybe$Just(
								$author$project$LDraw$Types$Comment('This is a comment')),
							$author$project$LDraw$Parser$parseLine('0 This is a comment'));
					}),
					A2(
					$elm_explorations$test$Test$test,
					'type 0 empty comment',
					function (_v1) {
						return A2(
							$elm_explorations$test$Expect$equal,
							$elm$core$Maybe$Just(
								$author$project$LDraw$Types$Comment('')),
							$author$project$LDraw$Parser$parseLine('0'));
					}),
					A2(
					$elm_explorations$test$Test$test,
					'empty line returns Nothing',
					function (_v2) {
						return A2(
							$elm_explorations$test$Expect$equal,
							$elm$core$Maybe$Nothing,
							$author$project$LDraw$Parser$parseLine(''));
					}),
					A2(
					$elm_explorations$test$Test$test,
					'whitespace-only returns Nothing',
					function (_v3) {
						return A2(
							$elm_explorations$test$Expect$equal,
							$elm$core$Maybe$Nothing,
							$author$project$LDraw$Parser$parseLine('   '));
					}),
					A2(
					$elm_explorations$test$Test$test,
					'unknown type digit returns Nothing',
					function (_v4) {
						return A2(
							$elm_explorations$test$Expect$equal,
							$elm$core$Maybe$Nothing,
							$author$project$LDraw$Parser$parseLine('9 some garbage'));
					}),
					A2(
					$elm_explorations$test$Test$test,
					'type 1 identity transform',
					function (_v5) {
						var result = $author$project$LDraw$Parser$parseLine('1 16 0 0 0 1 0 0 0 1 0 0 0 1 3647.dat');
						if ((result.$ === 'Just') && (result.a.$ === 'SubFileRef')) {
							var ref = result.a.a;
							return A2(
								$elm_explorations$test$Expect$all,
								_List_fromArray(
									[
										function (r) {
										return A2($elm_explorations$test$Expect$equal, 16, r.color);
									},
										function (r) {
										return A2($elm_explorations$test$Expect$equal, '3647.dat', r.file);
									},
										function (r) {
										var p = A2(
											$elm_explorations$linear_algebra$Math$Matrix4$transform,
											r.transform,
											A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 0));
										return A2(
											$elm_explorations$test$Expect$equal,
											A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 0),
											p);
									}
									]),
								ref);
						} else {
							return $elm_explorations$test$Expect$fail(
								'Expected SubFileRef, got: ' + $elm$core$Debug$toString(result));
						}
					}),
					A2(
					$elm_explorations$test$Test$test,
					'type 1 translation',
					function (_v7) {
						var result = $author$project$LDraw$Parser$parseLine('1 4 20 -8 0 1 0 0 0 1 0 0 0 1 stud.dat');
						if ((result.$ === 'Just') && (result.a.$ === 'SubFileRef')) {
							var ref = result.a.a;
							return A2(
								$elm_explorations$test$Expect$all,
								_List_fromArray(
									[
										function (r) {
										return A2($elm_explorations$test$Expect$equal, 4, r.color);
									},
										function (r) {
										return A2($elm_explorations$test$Expect$equal, 'stud.dat', r.file);
									},
										function (r) {
										var p = A2(
											$elm_explorations$linear_algebra$Math$Matrix4$transform,
											r.transform,
											A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 0));
										return A2(
											$elm_explorations$test$Expect$equal,
											A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 20, -8, 0),
											p);
									}
									]),
								ref);
						} else {
							return $elm_explorations$test$Expect$fail(
								'Expected SubFileRef, got: ' + $elm$core$Debug$toString(result));
						}
					}),
					A2(
					$elm_explorations$test$Test$test,
					'type 1 filename normalised to lowercase',
					function (_v9) {
						var result = $author$project$LDraw$Parser$parseLine('1 16 0 0 0 1 0 0 0 1 0 0 0 1 STUD.DAT');
						if ((result.$ === 'Just') && (result.a.$ === 'SubFileRef')) {
							var ref = result.a.a;
							return A2($elm_explorations$test$Expect$equal, 'stud.dat', ref.file);
						} else {
							return $elm_explorations$test$Expect$fail('Expected SubFileRef');
						}
					}),
					A2(
					$elm_explorations$test$Test$test,
					'type 1 backslash in filename normalised to forward slash',
					function (_v11) {
						var result = $author$project$LDraw$Parser$parseLine('1 16 0 0 0 1 0 0 0 1 0 0 0 1 s/stud4.dat');
						if ((result.$ === 'Just') && (result.a.$ === 'SubFileRef')) {
							var ref = result.a.a;
							return A2($elm_explorations$test$Expect$equal, 's/stud4.dat', ref.file);
						} else {
							return $elm_explorations$test$Expect$fail('Expected SubFileRef');
						}
					}),
					A2(
					$elm_explorations$test$Test$test,
					'type 11 (Studio v2) sub-file ref parses like type 1',
					function (_v13) {
						var result = $author$project$LDraw$Parser$parseLine('11 226 1 False 0 0 -72 -10 1 0 0 0 1 0 0 0 1 3702.dat');
						if ((result.$ === 'Just') && (result.a.$ === 'SubFileRef')) {
							var ref = result.a.a;
							return A2(
								$elm_explorations$test$Expect$all,
								_List_fromArray(
									[
										function (r) {
										return A2($elm_explorations$test$Expect$equal, 226, r.color);
									},
										function (r) {
										return A2($elm_explorations$test$Expect$equal, '3702.dat', r.file);
									},
										function (r) {
										var p = A2(
											$elm_explorations$linear_algebra$Math$Matrix4$transform,
											r.transform,
											A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 0));
										return A2(
											$elm_explorations$test$Expect$equal,
											A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, -72, -10),
											p);
									}
									]),
								ref);
						} else {
							return $elm_explorations$test$Expect$fail('Expected SubFileRef');
						}
					}),
					A2(
					$elm_explorations$test$Test$test,
					'type 1 parts prefix is stripped',
					function (_v15) {
						var result = $author$project$LDraw$Parser$parseLine('1 16 0 0 0 1 0 0 0 1 0 0 0 1 parts/3001.dat');
						if ((result.$ === 'Just') && (result.a.$ === 'SubFileRef')) {
							var ref = result.a.a;
							return A2($elm_explorations$test$Expect$equal, '3001.dat', ref.file);
						} else {
							return $elm_explorations$test$Expect$fail('Expected SubFileRef');
						}
					}),
					A2(
					$elm_explorations$test$Test$test,
					'type 1 primitive prefixes keep resolver hints',
					function (_v17) {
						var lowRes = $author$project$LDraw$Parser$parseLine('1 16 0 0 0 1 0 0 0 1 0 0 0 1 p/1-4ring3.dat');
						var hiRes = $author$project$LDraw$Parser$parseLine('1 16 0 0 0 1 0 0 0 1 0 0 0 1 p/48/stud.dat');
						var _v18 = _Utils_Tuple2(lowRes, hiRes);
						if ((((_v18.a.$ === 'Just') && (_v18.a.a.$ === 'SubFileRef')) && (_v18.b.$ === 'Just')) && (_v18.b.a.$ === 'SubFileRef')) {
							var lowRef = _v18.a.a.a;
							var hiRef = _v18.b.a.a;
							return A2(
								$elm_explorations$test$Expect$all,
								_List_fromArray(
									[
										function (_v19) {
										return A2($elm_explorations$test$Expect$equal, '1-4ring3.dat', lowRef.file);
									},
										function (_v20) {
										return A2($elm_explorations$test$Expect$equal, '48/stud.dat', hiRef.file);
									}
									]),
								_Utils_Tuple0);
						} else {
							return $elm_explorations$test$Expect$fail('Expected both SubFileRefs');
						}
					}),
					A2(
					$elm_explorations$test$Test$test,
					'type 2 line segment',
					function (_v21) {
						var result = $author$project$LDraw$Parser$parseLine('2 24 0 0 0 10 0 0');
						if ((result.$ === 'Just') && (result.a.$ === 'LineSegment')) {
							var seg = result.a.a;
							return A2(
								$elm_explorations$test$Expect$all,
								_List_fromArray(
									[
										function (s) {
										return A2($elm_explorations$test$Expect$equal, 24, s.color);
									},
										function (s) {
										return A2(
											$elm_explorations$test$Expect$equal,
											A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 0),
											s.p1);
									},
										function (s) {
										return A2(
											$elm_explorations$test$Expect$equal,
											A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 10, 0, 0),
											s.p2);
									}
									]),
								seg);
						} else {
							return $elm_explorations$test$Expect$fail(
								'Expected LineSegment, got: ' + $elm$core$Debug$toString(result));
						}
					}),
					A2(
					$elm_explorations$test$Test$test,
					'type 3 triangle',
					function (_v23) {
						var result = $author$project$LDraw$Parser$parseLine('3 4 1 0 0 0 1 0 0 0 1');
						if ((result.$ === 'Just') && (result.a.$ === 'Triangle')) {
							var tri = result.a.a;
							return A2(
								$elm_explorations$test$Expect$all,
								_List_fromArray(
									[
										function (t) {
										return A2($elm_explorations$test$Expect$equal, 4, t.color);
									},
										function (t) {
										return A2(
											$elm_explorations$test$Expect$equal,
											A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 1, 0, 0),
											t.p1);
									},
										function (t) {
										return A2(
											$elm_explorations$test$Expect$equal,
											A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 1, 0),
											t.p2);
									},
										function (t) {
										return A2(
											$elm_explorations$test$Expect$equal,
											A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 1),
											t.p3);
									}
									]),
								tri);
						} else {
							return $elm_explorations$test$Expect$fail(
								'Expected Triangle, got: ' + $elm$core$Debug$toString(result));
						}
					}),
					A2(
					$elm_explorations$test$Test$test,
					'type 4 quad',
					function (_v25) {
						var result = $author$project$LDraw$Parser$parseLine('4 16 -1 0 -1 1 0 -1 1 0 1 -1 0 1');
						if ((result.$ === 'Just') && (result.a.$ === 'Quad')) {
							var quad = result.a.a;
							return A2(
								$elm_explorations$test$Expect$all,
								_List_fromArray(
									[
										function (q) {
										return A2($elm_explorations$test$Expect$equal, 16, q.color);
									},
										function (q) {
										return A2(
											$elm_explorations$test$Expect$equal,
											A3($elm_explorations$linear_algebra$Math$Vector3$vec3, -1, 0, -1),
											q.p1);
									},
										function (q) {
										return A2(
											$elm_explorations$test$Expect$equal,
											A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 1, 0, -1),
											q.p2);
									},
										function (q) {
										return A2(
											$elm_explorations$test$Expect$equal,
											A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 1, 0, 1),
											q.p3);
									},
										function (q) {
										return A2(
											$elm_explorations$test$Expect$equal,
											A3($elm_explorations$linear_algebra$Math$Vector3$vec3, -1, 0, 1),
											q.p4);
									}
									]),
								quad);
						} else {
							return $elm_explorations$test$Expect$fail(
								'Expected Quad, got: ' + $elm$core$Debug$toString(result));
						}
					}),
					A2(
					$elm_explorations$test$Test$test,
					'type 5 conditional line',
					function (_v27) {
						var result = $author$project$LDraw$Parser$parseLine('5 24 0 0 0 1 0 0 0 0 1 1 0 1');
						if ((result.$ === 'Just') && (result.a.$ === 'ConditionalLine')) {
							var cond = result.a.a;
							return A2(
								$elm_explorations$test$Expect$all,
								_List_fromArray(
									[
										function (c) {
										return A2($elm_explorations$test$Expect$equal, 24, c.color);
									},
										function (c) {
										return A2(
											$elm_explorations$test$Expect$equal,
											A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 0),
											c.p1);
									},
										function (c) {
										return A2(
											$elm_explorations$test$Expect$equal,
											A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 1, 0, 0),
											c.p2);
									},
										function (c) {
										return A2(
											$elm_explorations$test$Expect$equal,
											A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 1),
											c.c1);
									},
										function (c) {
										return A2(
											$elm_explorations$test$Expect$equal,
											A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 1, 0, 1),
											c.c2);
									}
									]),
								cond);
						} else {
							return $elm_explorations$test$Expect$fail(
								'Expected ConditionalLine, got: ' + $elm$core$Debug$toString(result));
						}
					}),
					A2(
					$elm_explorations$test$Test$test,
					'malformed float returns Nothing',
					function (_v29) {
						return A2(
							$elm_explorations$test$Expect$equal,
							$elm$core$Maybe$Nothing,
							$author$project$LDraw$Parser$parseLine('3 4 a 0 0 0 1 0 0 0 1'));
					}),
					A2(
					$elm_explorations$test$Test$test,
					'too few tokens for type 3 returns Nothing',
					function (_v30) {
						return A2(
							$elm_explorations$test$Expect$equal,
							$elm$core$Maybe$Nothing,
							$author$project$LDraw$Parser$parseLine('3 4 1 0 0'));
					})
				])),
			A2(
			$elm_explorations$test$Test$describe,
			'parseFile',
			_List_fromArray(
				[
					A2(
					$elm_explorations$test$Test$test,
					'parses multiple lines, skips empties',
					function (_v31) {
						var input = '0 First comment\n\n3 4 1 0 0 0 1 0 0 0 1\n   ';
						var result = $author$project$LDraw$Parser$parseFile(input);
						return A2(
							$elm_explorations$test$Expect$equal,
							2,
							$elm$core$List$length(result));
					}),
					A2(
					$elm_explorations$test$Test$test,
					'returns lines in order',
					function (_v32) {
						var result = $author$project$LDraw$Parser$parseFile('0 A\n0 B\n0 C');
						return A2(
							$elm_explorations$test$Expect$equal,
							_List_fromArray(
								[
									$author$project$LDraw$Types$Comment('A'),
									$author$project$LDraw$Types$Comment('B'),
									$author$project$LDraw$Types$Comment('C')
								]),
							result);
					})
				])),
			A2(
			$elm_explorations$test$Test$describe,
			'splitMpd',
			_List_fromArray(
				[
					A2(
					$elm_explorations$test$Test$test,
					'splits on FILE markers',
					function (_v33) {
						var input = '0 FILE main.ldr\n0 A comment\n0 FILE sub.dat\n3 4 0 0 0 1 0 0 0 0 1';
						var result = $author$project$LDraw$Parser$splitMpd(input);
						return A2(
							$elm_explorations$test$Expect$all,
							_List_fromArray(
								[
									function (d) {
									return A2(
										$elm_explorations$test$Expect$equal,
										true,
										A2($elm$core$Dict$member, 'main.ldr', d));
								},
									function (d) {
									return A2(
										$elm_explorations$test$Expect$equal,
										true,
										A2($elm$core$Dict$member, 'sub.dat', d));
								},
									function (d) {
									return A2(
										$elm_explorations$test$Expect$equal,
										2,
										$elm$core$Dict$size(d));
								}
								]),
							result);
					}),
					A2(
					$elm_explorations$test$Test$test,
					'FILE names are normalised to lowercase',
					function (_v34) {
						var input = '0 FILE MAIN.LDR\n0 comment';
						var result = $author$project$LDraw$Parser$splitMpd(input);
						return A2(
							$elm_explorations$test$Expect$equal,
							true,
							A2($elm$core$Dict$member, 'main.ldr', result));
					}),
					A2(
					$elm_explorations$test$Test$test,
					'non-MPD file returns empty dict',
					function (_v35) {
						var input = '0 This is just a plain file\n3 4 0 0 0 1 0 0 0 0 1';
						return A2(
							$elm_explorations$test$Expect$equal,
							$elm$core$Dict$empty,
							$author$project$LDraw$Parser$splitMpd(input));
					}),
					A2(
					$elm_explorations$test$Test$test,
					'content between FILE markers is correct',
					function (_v36) {
						var input = '0 FILE a.dat\nline1\nline2\n0 FILE b.dat\nline3';
						var result = $author$project$LDraw$Parser$splitMpd(input);
						return A2(
							$elm_explorations$test$Expect$equal,
							$elm$core$Maybe$Just('line1\nline2'),
							A2($elm$core$Dict$get, 'a.dat', result));
					})
				]))
		]));
var $author$project$PackageSmokeTest$suite = A2(
	$elm_explorations$test$Test$describe,
	'bricks-simulator package',
	_List_fromArray(
		[
			A2(
			$elm_explorations$test$Test$test,
			'test harness is wired',
			function (_v0) {
				return A2($elm_explorations$test$Expect$equal, true, true);
			})
		]));
var $elm_explorations$test$Expect$atLeast = A2($elm_explorations$test$Expect$compareWith, 'Expect.atLeast', $elm$core$Basics$ge);
var $elm_explorations$test$Expect$atMost = A2($elm_explorations$test$Expect$compareWith, 'Expect.atMost', $elm$core$Basics$le);
var $author$project$Render$Style$clampVec3 = function (color) {
	return A3(
		$elm_explorations$linear_algebra$Math$Vector3$vec3,
		A3(
			$elm$core$Basics$clamp,
			0,
			1,
			$elm_explorations$linear_algebra$Math$Vector3$getX(color)),
		A3(
			$elm$core$Basics$clamp,
			0,
			1,
			$elm_explorations$linear_algebra$Math$Vector3$getY(color)),
		A3(
			$elm$core$Basics$clamp,
			0,
			1,
			$elm_explorations$linear_algebra$Math$Vector3$getZ(color)));
};
var $author$project$Render$Style$clampVibrance = function (amount) {
	return A3($elm$core$Basics$clamp, -0.5, 0.5, amount);
};
var $elm_explorations$linear_algebra$Math$Vector3$normalize = _MJS_v3normalize;
var $author$project$Render$Style$defaultStyle = {
	ambientStrength: 0.55,
	edgeColor: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0.18, 0.19, 0.2),
	lightDirection: $elm_explorations$linear_algebra$Math$Vector3$normalize(
		A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0.25, 1.0, 0.35)),
	rimPower: 2.2,
	rimStrength: 0.08,
	specularPower: 18.0,
	specularStrength: 0.14,
	vibrance: 0.12
};
var $author$project$Render$Style$normalizedDirection = function (raw) {
	return ($elm_explorations$linear_algebra$Math$Vector3$length(raw) < 1.0e-6) ? $author$project$Render$Style$defaultStyle.lightDirection : $elm_explorations$linear_algebra$Math$Vector3$normalize(raw);
};
var $author$project$Render$Style$clampStyle = function (style) {
	return _Utils_update(
		style,
		{
			ambientStrength: A3($elm$core$Basics$clamp, 0, 1, style.ambientStrength),
			edgeColor: $author$project$Render$Style$clampVec3(style.edgeColor),
			lightDirection: $author$project$Render$Style$normalizedDirection(style.lightDirection),
			rimPower: A3($elm$core$Basics$clamp, 0.1, 8, style.rimPower),
			rimStrength: A3($elm$core$Basics$clamp, 0, 1, style.rimStrength),
			specularPower: A3($elm$core$Basics$clamp, 1, 64, style.specularPower),
			specularStrength: A3($elm$core$Basics$clamp, 0, 1, style.specularStrength),
			vibrance: $author$project$Render$Style$clampVibrance(style.vibrance)
		});
};
var $elm_explorations$test$Expect$notEqual = A2($elm_explorations$test$Expect$equateWith, 'Expect.notEqual', $elm$core$Basics$neq);
var $author$project$Render$StyleTest$suite = A2(
	$elm_explorations$test$Test$describe,
	'Render.Style',
	_List_fromArray(
		[
			A2(
			$elm_explorations$test$Test$test,
			'clampVibrance bounds to conservative range',
			function (_v0) {
				return A2(
					$elm_explorations$test$Expect$all,
					_List_fromArray(
						[
							function (_v1) {
							return A3(
								$elm_explorations$test$Expect$within,
								$elm_explorations$test$Expect$Absolute(0.0001),
								-0.5,
								$author$project$Render$Style$clampVibrance(-2));
						},
							function (_v2) {
							return A3(
								$elm_explorations$test$Expect$within,
								$elm_explorations$test$Expect$Absolute(0.0001),
								0.5,
								$author$project$Render$Style$clampVibrance(2));
						},
							function (_v3) {
							return A3(
								$elm_explorations$test$Expect$within,
								$elm_explorations$test$Expect$Absolute(0.0001),
								0.2,
								$author$project$Render$Style$clampVibrance(0.2));
						}
						]),
					_Utils_Tuple0);
			}),
			A2(
			$elm_explorations$test$Test$test,
			'clampStyle normalizes and bounds values',
			function (_v4) {
				var clamped = $author$project$Render$Style$clampStyle(
					{
						ambientStrength: 3,
						edgeColor: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, -1, 0.4, 2),
						lightDirection: A3($elm_explorations$linear_algebra$Math$Vector3$vec3, 0, 0, 0),
						rimPower: 0.01,
						rimStrength: -2,
						specularPower: 100,
						specularStrength: -1,
						vibrance: 99
					});
				return A2(
					$elm_explorations$test$Expect$all,
					_List_fromArray(
						[
							function (_v5) {
							return A3(
								$elm_explorations$test$Expect$within,
								$elm_explorations$test$Expect$Absolute(0.0001),
								1,
								clamped.ambientStrength);
						},
							function (_v6) {
							return A3(
								$elm_explorations$test$Expect$within,
								$elm_explorations$test$Expect$Absolute(0.0001),
								0,
								clamped.specularStrength);
						},
							function (_v7) {
							return A3(
								$elm_explorations$test$Expect$within,
								$elm_explorations$test$Expect$Absolute(0.0001),
								64,
								clamped.specularPower);
						},
							function (_v8) {
							return A3(
								$elm_explorations$test$Expect$within,
								$elm_explorations$test$Expect$Absolute(0.0001),
								0,
								clamped.rimStrength);
						},
							function (_v9) {
							return A3(
								$elm_explorations$test$Expect$within,
								$elm_explorations$test$Expect$Absolute(0.0001),
								0.1,
								clamped.rimPower);
						},
							function (_v10) {
							return A3(
								$elm_explorations$test$Expect$within,
								$elm_explorations$test$Expect$Absolute(0.0001),
								0.5,
								clamped.vibrance);
						},
							function (_v11) {
							return A3(
								$elm_explorations$test$Expect$within,
								$elm_explorations$test$Expect$Absolute(0.0001),
								0,
								$elm_explorations$linear_algebra$Math$Vector3$getX(clamped.edgeColor));
						},
							function (_v12) {
							return A3(
								$elm_explorations$test$Expect$within,
								$elm_explorations$test$Expect$Absolute(0.0001),
								0.4,
								$elm_explorations$linear_algebra$Math$Vector3$getY(clamped.edgeColor));
						},
							function (_v13) {
							return A3(
								$elm_explorations$test$Expect$within,
								$elm_explorations$test$Expect$Absolute(0.0001),
								1,
								$elm_explorations$linear_algebra$Math$Vector3$getZ(clamped.edgeColor));
						},
							function (_v14) {
							return A2(
								$elm_explorations$test$Expect$notEqual,
								0,
								$elm_explorations$linear_algebra$Math$Vector3$length(clamped.lightDirection));
						}
						]),
					_Utils_Tuple0);
			}),
			A2(
			$elm_explorations$test$Test$test,
			'default style remains within expected range',
			function (_v15) {
				var style = $author$project$Render$Style$defaultStyle;
				return A2(
					$elm_explorations$test$Expect$all,
					_List_fromArray(
						[
							function (_v16) {
							return A2($elm_explorations$test$Expect$atMost, 1, style.ambientStrength);
						},
							function (_v17) {
							return A2($elm_explorations$test$Expect$atLeast, 0, style.ambientStrength);
						},
							function (_v18) {
							return A2($elm_explorations$test$Expect$atMost, 0.5, style.vibrance);
						},
							function (_v19) {
							return A2($elm_explorations$test$Expect$atLeast, -0.5, style.vibrance);
						}
						]),
					_Utils_Tuple0);
			})
		]));
var $author$project$Test$Generated$Main$main = A2(
	$author$project$Test$Runner$Node$run,
	{
		globs: _List_Nil,
		paths: _List_fromArray(
			['/workspaces/bricks/packages/bricks-simulator/tests/Gear/ComponentsTest.elm', '/workspaces/bricks/packages/bricks-simulator/tests/Gear/DetectTest.elm', '/workspaces/bricks/packages/bricks-simulator/tests/Gear/PhysicsTest.elm', '/workspaces/bricks/packages/bricks-simulator/tests/LDraw/ColorsTest.elm', '/workspaces/bricks/packages/bricks-simulator/tests/LDraw/GeometryTest.elm', '/workspaces/bricks/packages/bricks-simulator/tests/LDraw/ParserTest.elm', '/workspaces/bricks/packages/bricks-simulator/tests/PackageSmokeTest.elm', '/workspaces/bricks/packages/bricks-simulator/tests/Render/StyleTest.elm']),
		processes: 12,
		report: $author$project$Test$Reporter$Reporter$ConsoleReport($author$project$Console$Text$Monochrome),
		runs: 100,
		seed: 224645066583449
	},
	_List_fromArray(
		[
			_Utils_Tuple2(
			'Gear.ComponentsTest',
			_List_fromArray(
				[
					$author$project$Test$Runner$Node$check($author$project$Gear$ComponentsTest$suite)
				])),
			_Utils_Tuple2(
			'Gear.DetectTest',
			_List_fromArray(
				[
					$author$project$Test$Runner$Node$check($author$project$Gear$DetectTest$suite)
				])),
			_Utils_Tuple2(
			'Gear.PhysicsTest',
			_List_fromArray(
				[
					$author$project$Test$Runner$Node$check($author$project$Gear$PhysicsTest$suite)
				])),
			_Utils_Tuple2(
			'LDraw.ColorsTest',
			_List_fromArray(
				[
					$author$project$Test$Runner$Node$check($author$project$LDraw$ColorsTest$suite)
				])),
			_Utils_Tuple2(
			'LDraw.GeometryTest',
			_List_fromArray(
				[
					$author$project$Test$Runner$Node$check($author$project$LDraw$GeometryTest$suite)
				])),
			_Utils_Tuple2(
			'LDraw.ParserTest',
			_List_fromArray(
				[
					$author$project$Test$Runner$Node$check($author$project$LDraw$ParserTest$suite)
				])),
			_Utils_Tuple2(
			'PackageSmokeTest',
			_List_fromArray(
				[
					$author$project$Test$Runner$Node$check($author$project$PackageSmokeTest$suite)
				])),
			_Utils_Tuple2(
			'Render.StyleTest',
			_List_fromArray(
				[
					$author$project$Test$Runner$Node$check($author$project$Render$StyleTest$suite)
				]))
		]));
_Platform_export({'Test':{'Generated':{'Main':{'init':$author$project$Test$Generated$Main$main($elm$json$Json$Decode$int)(0)}}}});}(this));
return this.Elm;
})({});
var pipeFilename = "/tmp/elm_test-304695.sock";
var net = require('net'),
  client = net.createConnection(pipeFilename);

client.on('error', function (error) {
  console.error(error);
  client.end();
  process.exit(1);
});

client.setEncoding('utf8');
client.setNoDelay(true);

// Run the Elm app.
var app = Elm.Test.Generated.Main.init({ flags: Date.now() });

client.on('data', function (msg) {
  app.ports.elmTestPort__receive.send(JSON.parse(msg));
});

// Use ports for inter-process communication.
app.ports.elmTestPort__send.subscribe(function (msg) {
  // We split incoming messages on the socket on newlines. The gist is that node
  // is rather unpredictable in whether or not a single `write` will result in a
  // single `on('data')` callback. Sometimes it does, sometimes multiple writes
  // result in a single callback and - worst of all - sometimes a single read
  // results in multiple callbacks, each receiving a piece of the data. The
  // horror.
  client.write(msg + '\n');
});