package robocar;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Random;

import javax.swing.BoxLayout;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;

import com.ibm.able.AbleException;
import com.ibm.able.rules.AbleRuleSet;
import com.ibm.able.rules.AbleRuleSetImpl;

public class StreetlightFrame extends JFrame {
	private static final long serialVersionUID = 8911493498208303212L;
	private static AbleRuleSet ruleSet;

	public static void main(String[] args) throws Exception {
		ruleSet = new AbleRuleSetImpl();
		ruleSet.parseFromARL("rules/Lights.arl");
		ruleSet.init();
		
        javax.swing.SwingUtilities.invokeLater(new Runnable() {
            public void run() {
            	StreetlightFrame frame = new StreetlightFrame();             
                frame.setLocationRelativeTo(null);
                frame.setVisible(true);
            }
        });		
	}
	
	public StreetlightFrame(){
		super("Find your color!");
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        getContentPane().add(new StreetlightPanel());
        pack();	
	}
	
	private class StreetlightPanel extends JPanel {

		private static final long serialVersionUID = -1133218484398536763L;
		
		private ColorPanel cp;
		private JLabel label;
		
		public StreetlightPanel(){
			super();
			this.cp = new ColorPanel();
			this.label = new JLabel("");
			this.add(cp);
			this.add(label);
			this.setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));
			randomize();

			this.addMouseListener(new MouseAdapter() {			
				@Override
				public void mousePressed(MouseEvent e) {
					randomize();
				}
			});
		}
		
		private void randomize(){
			cp.randomizeColor();
			
			int r = cp.color.getRed();
			int g = cp.color.getGreen();
			int b = cp.color.getBlue();
			
			Object[] input = new Integer[]{r, g, b};
			String output;
			
			try {
				Object[] out = (Object[]) ruleSet.process(input);
				System.out.println(out.length);
				System.out.println(out[0]);
				output = (String) out[0];
			} catch(AbleException ae) {
				output = "Error!";
				if (ae.getExceptions() == null)
					System.err.println(ae.getLocalizedMessage());
				else
					for (Object o : ae.getExceptions()){
						Exception e = (Exception) o;
						System.err.println(e.getLocalizedMessage());
					}
			} catch(Exception e) {
				output = "Error!";
			}
			
			String result = "X"+r+"X"+g+"X"+b+"X";
			
			label.setText(result + " -> " + output);					
			repaint();
		}
	}
	
	private class ColorPanel extends JPanel {

		private static final long serialVersionUID = 1646488713969203757L;
		private static final int DEFAULT_WIDTH = 500;
		private static final int DEFAULT_HEIGHT = 400;

		public Color color;
		private Random rand;
		
		public ColorPanel(){
			super();
			this.color = Color.YELLOW;
			this.rand = new Random();
			setPreferredSize(new Dimension(DEFAULT_WIDTH, DEFAULT_HEIGHT));
		}
		
		private void randomizeColor(){
			color = new Color(rand.nextFloat(), rand.nextFloat(), rand.nextFloat());
		}

		@Override
		public void paintComponent(Graphics g){
			super.paintComponent(g);
			g.setColor(color);
			g.fillRect(0, 0, this.getWidth(), this.getHeight());
		}
	}
}
