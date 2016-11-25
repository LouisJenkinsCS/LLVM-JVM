/* Name of the class has to be "Main" only if the class is public. */
class ArithmeticTest
{
	public static void main (String[] args) {
		// Simple math example, you can alter this as you wish.
		int x = 100;
		System.out.println("X = " + x);
		int y = 1000;
		System.out.println("Y = " + y);
		int z = 50;
		System.out.println("Z = " + z);

		int addResult = x + y + z;
		int subResult = x - y - z;
		int multResult = x * y * z;
		int divResult = x / y / z;
		int mixedResult = x + y / z;
		System.out.println("X + Y + Z = " + addResult);
		System.out.println("X - Y = Z = " + subResult);
		System.out.println("X * Y * Z = " + multResult);
		System.out.println("X / Y / Z = " + divResult);
		System.out.println("X + Y / Z = " + mixedResult);
	}
}
