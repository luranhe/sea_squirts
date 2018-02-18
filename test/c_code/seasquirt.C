/*                                              John Cain  04/14/2016

  Solves cable equation with two-current model with pacemakers at BOTH ends of the fiber to simulate
  competing pacemakers in a sea squirt heart.  Program allows stochastic variation in the stimulus times
  at both ends to see if, over time, reversal of propagation occurs.

  Exports v(x,t) versus x at various times as .txt files, creates a gnuplot
  script to create .jpg images from those .txt files (each using the same viewing window).
  Those .jpg images can then be concatenated into a movie showing the propagating
  action potential.

*/

#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cmath>
#include <iomanip>
#include <sstream>     //needed for stringstream command
#include <string>
#include <ctime>       //so I can seed the random number generator using the system clock

#include "parameters-tc.h"

using namespace std;

/* declarations of member functions in functions.h */

double hinf(double v);
double tau(double v);
double f1(double v, double h);
double f2(double v, double h);

#include "functions-tc.h"

double leftstimulus(double s);     //stimuli at left end of fiber
double rightstimulus(double s);    //stimuli at right end of fiber

/* constants for use in generating normal random variable */

#define p0  0.322232431088               
#define q0  0.099348462606
#define p1  1.0                
#define q1  0.588581570495
#define p2  0.342242088547     
#define q2  0.531103462366
#define p3  0.204231210245e-1  
#define q3  0.103537752850
#define p4  0.453642210148e-4  
#define q4  0.385607006340e-2

double uniformrv();                    //generate uniform [0,1]
double normalrv(double m, double s);   //normal with mean m and standard deviation s (NOT VARIANCE!!!) 


ofstream plotfile("squirtplot");  /* create a gnuplot script that will convert all .txt files to .jpg images */


/* -------------------------------------------------------------- */

int main() {

  /* Auto-generate the header for the gnuplot script that will convert all .txt files to .jpg files */

  plotfile << "set terminal jpeg" << endl;
  plotfile << "set nokey" << endl;
  plotfile << "set xrange[0:10]" << endl;
  plotfile << "set yrange[0:1]" << endl;


  /* Set up the generation of filenames of file####.txt files in which data will be dumped */

  stringstream ss;
  string front ("file");
  string extension (".txt");
  string jpegext (".jpg");
  string filename, jpegfilename;
  string digits;
  int framenumber = 0;

  const int nodes = 1000;       /* spatial nodes */
  double xl,xr;                 /* endpoints  */
  double t, dt;                 /* time, timestep */
  int counter;                  /* int or long? */
  double lefttstim = 0.0;       /* time to apply next stimulus on interval 0.0 < x < 0.1  */
  double righttstim = 0.0;      /* time to apply next stimulus on interval 9.9 < x < 10.0 */
  double blah;

  /* LEFT, RIGHT ENDPOINTS AND NODE WHERE MEASUREMENTS WILL BE TAKEN */
  xl = 0.0;
  xr = 10.0;                     /* right end of cable */
  double dx = (xr - xl)/nodes;

  dt = 0.01;
  double k = 0.001;                /* diffusion constant */
  double r = k*dt/(dx*dx);         /* for use in forward Euler scheme */
  int framespacing = int(30.0/dt);  /* frames will be 30 milliseconds apart */ 


  int howmany;
  cout << "Number of stimuli to apply at BCL = " << BCL <<":  ";
  cin >> howmany;
  const int numstims = howmany;
  double tfinal =  numstims*BCL;  

  t = 0.0;
  counter = 0;

  cout << "Length of cable = " << (xr - xl) << endl;
  cout << "dx = " << dx << endl;
  cout << "Timestep dt = " << dt << endl;

  double x[nodes+1], volt[nodes+1], gate[nodes+1];
  double vold[nodes+1], hold[nodes+1];

  /* -------------------initial data goes here-------------------- */


  for (int i = 0; i <= nodes; i++) {
    x[i] = xl + i*dx;
    vold[i] = leftstimulus(x[i]);    //start with stimulus at left boundary only (could switch this to stimulate both)
    hold[i] = 0.8;
  }

  srand(time(NULL));                   //seed random number generator using the system clock
  double R1, norm, m, s;               //random variables.  R1 is uniform in [0,1] and norm is normal RV with mean m and std dev s.
  m = 0.0;
  s = 15.0;                            //standard deviation of fluctuations in BCL (for now, assuming same for both pacemakers)
  R1 = uniformrv();
  norm = normalrv(m,s);
  lefttstim = lefttstim + BCL + norm;

  R1 = uniformrv();
  norm = normalrv(m,s);
  righttstim = righttstim + BCL + norm;   

  cout << "left stimulus applied at t = " << t << endl;
  cout << "right stimulus applied at t = " << t << endl;


  /* -------now start Forward Euler PDE solver ---------- */

  while(lefttstim < tfinal) {

    /* BEGIN PDE SOLVER */

    t = t + dt;
    counter++;

    /* apply Gaussian stimulus at x = 0 end of cable if appropriate */
    if ((t >= lefttstim) && (t < (lefttstim + dt))) {

      cout << "left stimulus applied at t = " << t << endl;

      for (int j = 0; j <= nodes; j++) {
	vold[j] = vold[j] + leftstimulus(x[j]);
      }

      R1 = uniformrv();
      norm = normalrv(m,s);
      lefttstim = lefttstim + BCL + norm;
//      lefttstim = lefttstim + BCL + 10.0*sin(2.0*M_PI*t/20000.0);
    }

    /* apply Gaussian stimulus at x = 10 end of cable if appropriate */
    if ((t >= righttstim) && (t < (righttstim + dt))) {

      cout << "right stimulus applied at t = " << t << endl;

      for (int j = 0; j <= nodes; j++) {
	vold[j] = vold[j] + rightstimulus(x[j]);
      }

      R1 = uniformrv();
      norm = normalrv(m,s);
      righttstim = righttstim + BCL + norm;
//      righttstim = righttstim + BCL - 10.0*sin(2.0*M_PI*t/20000.0);
    }

    /* now use Forward Euler solver to get updated volt, gate */

    volt[0] = vold[0] + 2.0*r*(vold[1] - vold[0]) + dt*f1(vold[0],hold[0]);
    gate[0] = hold[0] + dt*f2(vold[0],hold[0]);

    for (int i = 1; i <= (nodes - 1); i++) {
      volt[i] = vold[i] + r*(vold[i+1]-2.0*vold[i]+vold[i-1]) + dt*f1(vold[i],hold[i]);
      gate[i] = hold[i] + dt*f2(vold[i],hold[i]);
    }

    volt[nodes] = vold[nodes] + 2.0*r*(vold[nodes-1] - vold[nodes]) + dt*f1(vold[nodes],hold[nodes]);
    gate[nodes] = hold[nodes] + dt*f2(vold[nodes],hold[nodes]);

    if (counter%framespacing == 0) {    //export u(x,t) versus x into a .txt file at regularly-spaced time intervals

      ss.str("");                       //clear the stringstream operator ss
      ss.clear();
      ss << framenumber;
      digits = ss.str();
      if (framenumber < 10) {
        filename = front + "000" + digits + extension;
        jpegfilename = front + "000" + digits + jpegext;
      }
      else if ((framenumber>=10) && (framenumber < 100)) {
        filename = front + "00" + digits + extension;
        jpegfilename = front + "00" + digits + jpegext;
      }

      else if ((framenumber>=100) && (framenumber < 1000)) {
        filename = front + "0" + digits + extension;
        jpegfilename = front + "0" + digits + jpegext;
      }

      else if (framenumber >= 1000) {
        filename = front + digits + extension;
        jpegfilename = front + digits + jpegext;
      }


      plotfile << "set output \"" << jpegfilename << "\"" << endl;
      plotfile << "plot \"" << filename << "\" with lines lw 6 lt 1" << endl;

      ofstream ofs (filename);

      for (int i = 0; i <= nodes; i++) {
        ofs << x[i] << " " << volt[i] << endl;
      }

      framenumber++;

    }

    if ((counter%2000) == 0) {
      cout << t << endl;
    }

    /*  Now update the voltage after checking progress of wavefronts and backs  */

    for (int i = 0; i <= nodes; i++) {  
      vold[i] = volt[i];
      hold[i] = gate[i];
    }
    
  }


  
  system ("mv -f *.txt ./squirt/");
  system ("mv -f squirtplot ./squirt/");
  system ("gnuplot ./squirt/squirtplot");
  system ("ffmpeg -r 10 -pattern_type glob -i './squirt/file*.jpg' squirt.mp4");
  system ("open ./squirt/squirt.mp4 &");

  return 0;

}


/*--------------------------------------------------------------- */

double leftstimulus(double s) {

  if (s < 0.1) {  //stimulate a one-millimeter wide region at x = 0 boundary
    return 0.5;
  }

  else if (s >= 0.1) {
    return 0.0;
  }

  else {
    return 0.0;
  }

/* apply Gaussian stimuli centered at x = 0 */

  //  return (0.4*exp(-50.0*s*s));

}

/*--------------------------------------------------------------- */

double rightstimulus(double s) {

  if (s > 9.9) {  //stimulate a one-millimeter wide region at x = 10 boundary
    return 0.5;
  }

  else if (s <= 9.9) {
    return 0.0;
  }

  else {
    return 0.0;
  }

/* apply Gaussian stimuli centered at x = 0 */

  //  return (0.4*exp(-50.0*s*s));

}

/*--------------------------------------------------------------- */

double uniformrv() {

  return (double(rand())/double(RAND_MAX));         //uniform in [0,1)

}

/*--------------------------------------------------------------- */

double normalrv(double m, double s) {

  double u, t, p, q, z;

  u   = uniformrv();
  if (u < 0.5) {
    t = sqrt(-2.0 * log(u));
  }
  else if (u >= 0.5) {
    t = sqrt(-2.0 * log(1.0 - u));
  }
  p   = p0 + t * (p1 + t * (p2 + t * (p3 + t * p4)));
  q   = q0 + t * (q1 + t * (q2 + t * (q3 + t * q4)));
  if (u < 0.5) {
    z = (p / q) - t;
  }
  else if (u >= 0.5) {
    z = t - (p / q);
  }

  return (m + s * z);


}
