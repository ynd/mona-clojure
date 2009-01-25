import java.awt.image.BufferedImage;
import javax.imageio.ImageIO;
import java.io.File;

class Fitness {
	public static void main(String[] args) {
	  BufferedImage im = null;
	  
	  try {
      im = ImageIO.read(new File("../../mona_lisa_crop.jpg"));
    } catch (java.io.IOException e) {
      System.exit(-1);
    }
    
    LMSFitnessFunction f = new LMSFitnessFunction(im);
    
    for(int i = 0; i < 1000; i++) {
      f.evaluate(im);
    }
  }
}
