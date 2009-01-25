/*
 * Copyright 2008 Yann N. Dauphin. All Rights Reserved.
 * Distributed under the terms of the BSD License.
 */

import java.awt.image.BufferedImage;
import java.awt.Color;
import java.awt.image.PixelGrabber;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Computes the fitness of a program as the Least-Mean-Sqare distance between 
 * the image it generates and the target image.
 * 
 * @author lokee
 */
public class LMSFitnessFunction {
    private final int[] targetPixels;

    LMSFitnessFunction(BufferedImage target) {
        targetPixels = new int[target.getWidth() * target.getHeight()];
        PixelGrabber pg = new PixelGrabber(target, 0, 0, target.getWidth(),
                target.getHeight(), targetPixels, 0, target.getWidth());
        try {
            pg.grabPixels();
        } catch (InterruptedException ex) {
            Logger.getLogger(LMSFitnessFunction.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    protected double evaluate(BufferedImage target) {
      
        BufferedImage generated = new BufferedImage(target.getWidth(), target.getHeight(),
                BufferedImage.TYPE_INT_ARGB);
        final int[] generatedPixels = new int[generated.getWidth() * generated.getHeight()];
        PixelGrabber pg = new PixelGrabber(generated, 0, 0, generated.getWidth(),
                generated.getHeight(), generatedPixels, 0, generated.getWidth());
        try {
            pg.grabPixels();
        } catch (InterruptedException ex) {
            Logger.getLogger(LMSFitnessFunction.class.getName()).log(Level.SEVERE, null, ex);
        }

        long sum = 0;
        for (int i = 0; i < generatedPixels.length && i < targetPixels.length; i++) {
            Color c1 = new Color(targetPixels[i]);
            Color c2 = new Color(generatedPixels[i]);

            int r = c1.getRed() - c2.getRed();
            int g = c1.getGreen() - c2.getGreen();
            int b = c1.getBlue() - c2.getBlue();

            sum += r * r + g * g + b * b;
        }

        return Math.sqrt((double) sum);
    }
}
