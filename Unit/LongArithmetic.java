/*
	Expected Output:

		x: 1000000000000
		y: 2000000000000
		z: 3000000000000
		x + y + z = 6000000000000
		x - y - z = -4000000000000
		x * y * z = 3730592787825950720
		x / y / z = 0
*/

class LongArithmetic {
	public static void main(String[] args) {
		long x = 1000000000000L;
		long y = 2000000000000L;
		long z = 3000000000000L;

		System.out.println("x: " + x);
		System.out.println("y: " + y);
		System.out.println("z: " + z);
		System.out.println("x + y + z = " + (x + y + z));
		System.out.println("x - y - z = " + (x - y - z));
		System.out.println("x * y * z = " + (x * y * z));
		System.out.println("x / y / z = " + (x / y / z));
	}
}