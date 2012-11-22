function a() {
	return 1 + 2;
}

function b(x) {
	return x + a();
}

function c(x,y) {
	return y + b(x);
}

c(3,4);