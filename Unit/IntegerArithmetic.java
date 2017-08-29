/*
	Expected Output:

		x: 1
		y: 2
		z: 3
		x + y + z = 6
		x - y - z = -4
		x * y * z = 6
		x / y / z = 0
*/

class IntegerArithmetic {
	public static void main(String[] args) {
		int x = 1;
		int y = 2;
		int z = 3;

		System.out.println("x: " + x);
		System.out.println("y: " + y);
		System.out.println("z: " + z);
		System.out.println("x + y + z = " + (x + y + z));
		System.out.println("x - y - z = " + (x - y - z));
		System.out.println("x * y * z = " + (x * y * z));
		System.out.println("x / y / z = " + (x / y / z));
	}
}