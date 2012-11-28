function a() {
	var x = 2
	var y = 4
	var z = x
	var x = 3
	var t = 2
	return y	
}

function b(x) {
	return x + a();
}

function c(x,y) {
	return y + b(x);
}

c(3,4);
var g=5
var z=7