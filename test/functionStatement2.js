function c(x,y) {
	function b(x) {
		function a() {
			1 + 2;
		}
		x + a();
	}
	y + b(x);
}
c(3,4);