import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

class ControlVar {
	public static void main(String[] args) throws IOException {
		int i = 357893594;
		int j = 560146190;
		int k = 929611828;
		while (true)
		{
			System.out.println("Give me some input! : ");
			BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
			String input = br.readLine();
			if (input.compareTo("HACK0")==0) {
				i = 999999999;
			}
			if (input.compareTo("HACK1")==0) {
				j = 999999999;
			}
			if (input.compareTo("HACK2")==0) {
				k = 999999999;
			}
			else if (input.compareTo("RESTORE")==0) {
				i = 357893594;
				j = 560146190;
				k = 929611828;
			}
		}
	}
}
