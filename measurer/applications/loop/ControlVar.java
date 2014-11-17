class ControlVar {
	public static void main(String[] args) {
		int i = 0;
		while (true)
		{
			i++;
			try {
				Thread.sleep(3000);
			}
			catch(Exception e) {

			}
			//			System.out.println(i);
		}
	}
}
