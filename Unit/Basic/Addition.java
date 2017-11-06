class Addition {

  public static int result;

  public static int RunMe() {
    int x = 0x1000;
    int y = 0x2000;
    int z = 0x3000;
    int w = x + y + z;

    if (w > x) {
      result = w;
    } else if (y > w) {
      result = y;
    } else if (z > w) {
      result = z;
    } else {
      result = x;
    }

    return result;
  }

  public static void main(String[] args) {

  }
}
