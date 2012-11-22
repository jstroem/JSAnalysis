function a() {
	1 + 2;
}

function b(x) {
	x + a();
}

function c(x,y) {
	y + b(x);
}

c(3,4);