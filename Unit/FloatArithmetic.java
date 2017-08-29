/*
	Expected Output:

		x: 1.2345
		y: 6.7891
		z: 11.121314
		x + y + z = 19.144915
		x - y - z = -16.675915
		x * y * z = 93.20934
		x / y / z = 0.01635019
*/

class FloatArithmetic {
	public static void main(String[] args) {
		float x = 1.2345f;
		float y = 6.78910f;
		float z = 11.12131415f;

		System.out.println("x: " + x);
		System.out.println("y: " + y);
		System.out.println("z: " + z);
		System.out.println("x + y + z = " + (x + y + z));
		System.out.println("x - y - z = " + (x - y - z));
		System.out.println("x * y * z = " + (x * y * z));
		System.out.println("x / y / z = " + (x / y / z));
	}
}