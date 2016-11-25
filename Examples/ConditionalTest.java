/* Name of the class has to be "Main" only if the class is public. */
class ConditionalTest
{
	public static void main (String[] args) {
    int x = 0;
    for (int i = 0; i < 100; i++)
			System.out.println("Idx: " + i + ";Val: " + x++);
	}
}
