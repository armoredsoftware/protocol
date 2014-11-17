class test {
	public static void main(String[] args) {
		int x = 1001;
		int y = 1002;
		foo();
	}
    	public static void foo() {
		int x = 1003;
		int y = 1004;
		while (true) {
			for (int i=0; i<10000000; i++)
			{
			}
			y++;
		}
	}	
}
