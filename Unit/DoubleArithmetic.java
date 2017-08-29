/*
	Expected Output:

		x: 1.2345
		y: 6.7891
		z: 11.12131415
		x + y + z = 19.144914149999998
		x - y - z = -16.67591415
		x * y * z = 93.2093348043219
		x / y / z = 0.016350189100687973
*/

class DoubleArithmetic {
	public static void main(String[] args) {
		double x = 1.2345;
		double y = 6.78910;
		double z = 11.12131415;

		System.out.println("x: " + x);
		System.out.println("y: " + y);
		System.out.println("z: " + z);
		System.out.println("x + y + z = " + (x + y + z));
		System.out.println("x - y - z = " + (x - y - z));
		System.out.println("x * y * z = " + (x * y * z));
		System.out.println("x / y / z = " + (x / y / z));
	}
}