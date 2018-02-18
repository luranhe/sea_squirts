double hinf(double v) {

  if (v > vcrit) {
    return 0.0;
  }

  else {
    return 1.0;
  }

}

double tau(double v) {

  if (v > vcrit) {
    return tauclose;
  }

  else {
    return tauopen;
  }

}

double f1(double v, double h) {

  return ((h/tauin)*(v*v*(1.0-v)) - (v/tauout));

}

double f2(double v, double h) {

  return ((hinf(v) - h)/tau(v));

}
