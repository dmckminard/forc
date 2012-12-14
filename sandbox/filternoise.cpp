// filterNoise.cpp - filtered white noise example 
// Tested with STK 4.4.2

#include "Noise.h"  // Synthesis Tool Kit (STK) class
#include "Iir.h"    // STK class
#include "FileWvOut.h"  // STK class
#include <cmath>   // for pow()
#include <vector>

using namespace stk;

int main()
{
  Noise *theNoise = new Noise(); // Noise source

  /* Set up the filter */
  StkFloat bCoefficients[5] = {1,0,0,pow(0.5,3)}; 
  std::vector<StkFloat> b(bCoefficients, bCoefficients+5);
  StkFloat aCoefficients[7] = {1,0,0,0,0,pow(0.9,5)};
  std::vector<StkFloat> a(aCoefficients, aCoefficients+7);
  Iir *filter = new Iir;
  filter->setNumerator(b);
  filter->setDenominator(a);

  FileWvOut output("main");  /* write to main.wav */

  /* Generate one second of white noise */
  StkFloat amp = 0.1; // noise amplitude
  for (long i=0;i<SRATE;i++)   {
    output.tick(amp*theNoise->tick());
  }

  /* Generate filtered noise for comparison */
  for (long i=0;i<SRATE;i++)   {
    output.tick(filter->tick(amp*theNoise->tick()));
  }
}

/*
function [y] = filterslow(B,A,x) 
% FILTERSLOW: Filter x to produce y = (B/A) x .
%       Equivalent to 'y = filter(B,A,x)' using 
%       a slow (but tutorial) method.

NB = length(B);
NA = length(A);
Nx = length(x);

xv = x(:); % ensure column vector

% do the FIR part using vector processing:
v = B(1)*xv;
if NB>1
  for i=2:min(NB,Nx)
    xdelayed = [zeros(i-1,1); xv(1:Nx-i+1)];
    v = v + B(i)*xdelayed;
  end; 
end; % fir part done, sitting in v

% The feedback part is intrinsically scalar,
% so this loop is where we spend a lot of time.
y = zeros(length(x),1); % pre-allocate y
ac = - A(2:NA); 
for i=1:Nx, % loop over input samples
  t=v(i);   % initialize accumulator
  if NA>1, 
    for j=1:NA-1
      if i>j, 
        t=t+ac(j)*y(i-j); 
      %else y(i-j) = 0
      end; 
    end; 
  end; 
  y(i)=t; 
end; 

y = reshape(y,size(x)); % in case x was a row vector
*/