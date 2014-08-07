import mdsj.MDSJ;
import mdsj.IO;
import mdsj.StressMinimization;
import java.util.Random;
import java.util.Scanner;
import java.io.File;

public class MDSJWrapper {
	public static void main(String[] args) {
		//assumes  args are: n, max_iterations, path_to_dist_matrix, (optional) path to seed coords, 
		int n = 0;
		try {
			n = Integer.parseInt(args[0]);
		} catch ( Exception e){
			System.out.println("unable to parse number of vertices in network:");
		}	
		
		int max_iter = 50;
		try {
			max_iter = Integer.parseInt(args[1]);
		} catch ( Exception e){
			System.out.println("unable to parse max iterations");
		}	
		
		
		double[][] distArray = null;
		double[][] newCoords = null;
		
		
		
		/*  no good 'cause only reads ints
		try {
			distArray=IO.read(args[0]);
		} catch (Exception e) {
			System.out.println(e.getStackTrace());
		}  */
		
		//parse the distance matrix
		if (args.length <3){
			System.out.println("Not enough parameters included for MDSJ, exiting.");
			System.exit(0);
		}
		try {
				distArray = new double[n][n];
				Scanner sc = new Scanner(new File(args[2]));
				//loop over rows and cols
				for (int r =0; r<n;r++){
					for (int c = 0; c<n; c++){
		      		if (sc.hasNextDouble()) {
		          		distArray[c][r] = sc.nextDouble();
		      		} else {
		      			System.out.println("distance matrix is missing entries?");	
		      		}
		      	}	
      		}
      		sc.close();
			} catch (Exception e) {
				System.out.println("Unable to parse distance matrix from file");
				e.printStackTrace();
				System.exit(0);
			}


		//check if any coords are passed in
		if ((args.length > 3) && (args[3] != null)) {
			try {
				newCoords = new double[2][n];
				Scanner sc = new Scanner(new File(args[3]));
				//loop over rows and cols
				for (int r =0; r<n;r++){
					for (int c = 0; c<2; c++){
		      		if (sc.hasNextDouble()) {
		          		newCoords[c][r] = sc.nextDouble();
		      		} else {
		      			System.out.println("coordinate array is missing entries?");	
		      		}
		      	}	
      		}
      		sc.close();
			} catch (Exception e) {
				System.out.println("Unable to parse seed coordinate array from file");
				e.printStackTrace();
			}
		} else {
			//create random initial positions
			Random generator = new Random();	
			newCoords = new double[2][n];
			for(int i=0; i<n; i++) {  // create new random coords
			    newCoords[0][i] = generator.nextDouble();
			    newCoords[1][i] = generator.nextDouble();
			}
		}
		
		//double[][] output=MDSJ.classicalScaling(input); // apply MDS
		
		

		//compute a weight matrix
		double[][] weights = StressMinimization.weightMatrix(distArray, -2);

		//do the minimization
		System.out.println("MDSJ starting stress: "+StressMinimization.stress(distArray, weights, newCoords));
		StressMinimization.majorize(newCoords, distArray, weights, max_iter);
		System.out.println("MDSJ ending stress: "+StressMinimization.stress(distArray, weights, newCoords));
		//newCoords = MDSJ.stressMinimization(distArray);

		for(int i=0; i<n; i++) {  // output all coordinates
		    System.out.println(newCoords[0][i]+" "+newCoords[1][i]);
		}
	}
}
