const float M_PI = 3.14159265358979323846;


float ripple2_f(float x) 
{
  return 1.0 - sin((x*x)/M_PI);
}

float sawtooth_f(float x, float period) 
{
  return x - period*floor(x/period);
}

float ripple_f(float x) 
{
 return ripple2_f(sawtooth_f(x, M_PI));
}

float dune_f(float x, float z) 
{
  return (1.0 + 0.5*sin(x+z)) * ripple_f(x+0.5*sin(z + (1.0+0.8*sin(x))));
}

float dune2_f(float x, float z)
{
  const float phi = M_PI/4.0;
  float scaling= 12.0*2.0*M_PI/sqrt(2.0);

   return dune_f(
       scaling*(cos(phi)*x + sin(phi)*z),
       scaling*(-sin(phi)*x + cos(phi)*z));
}

void main(void)
{
  float x = dune2_f(gl_TexCoord[0].x, gl_TexCoord[0].y);
  gl_FragColor = vec4 (0.9*x, 0.6*x, 0.2*x, 1);
}
