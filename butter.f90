!*****************************************************************
!*   Numerical Bandpass Filter (see demo program test_fil.f90)   *
!* ------------------------------------------------------------- *
!* References:                                                   * 
!*                                                               *
!* http://en.wikipedia.org/wiki/Digital_biquad_filter            *  
!* http://www.musicdsp.org/archive.php?classid=3#225             *  
!* http://www.musicdsp.org/showone.php?id=197                    *
!* http://www.musicdsp.org/files/Audio-EQ-Cookbook.txt           *
!* http://www.musicdsp.org/archive.php?classid=3#225             *
!* http://www.musicdsp.org/showArchiveComment.php?ArchiveID=225  *
!*                                                               *
!*                             F90 Release By J-P Moreau, Paris. *
!***************************************************************** 
!
! Sourced from http://jean-pierre.moreau.pagesperso-orange.fr/f_signal.html
!
!
!   Methods of filter:    
!   Subroutine InitFilter(filter,SampleRate, MaxBlockSize)
!   Subroutine SetQ(filter,NewQ)
!   Subroutine CalcFilterCoeff(filter)
!   Subroutine CalcFilterCoeffs(filter,pFilterType, pFreq, float pQ, pDBGain, pQIsBandWidth)
!   Subroutine Process(filter,Input,sampleframes)
!   real Function Process1(filter,input)
!   
!   *****
!   Note:
!   *****
!   use Process1(input:single):single;
!   for per sample processing
!   use Process(Input:psingle;sampleframes:integer);
!   for block processing. The input is a pointer to
!   the start of an array of single which contains
!   the audio data.
!   i.e.
!   RBJFilter.Process(@WaveData[0],256);
!

Module butter

integer, parameter ::  &
      kLowPass=0,      &  !-LowPass
      kHighPass=1,     &  !-HiPass
      kBandPassCSG=2,  &  !-BandPass CSG
      kBandPassCZPG=3, &  !-BandPass CZPG
      kNotch=4,        &  !-Notch
      kAll=5,          &  !-AllPass
      kPeaking=6,      &  !-Peaking
      kLowShelf=7,     &  !-LowShelf
      kHighShelf=8        !-HiShelf

                                                            
  !Butterworth all-purpose filter
  Type TRbjEqFilter
    real b0a0,b1a0,b2a0,a1a0,a2a0
    real in1,in2,ou1,ou2
    real fSampleRate
    integer fMaxBlockSize
    integer fFilterType
    real fFreq,fQ,fDBGain
    logical fQIsBandWidth
    real, pointer :: out1(:)
  End Type TRbjEqFilter

contains

!Initialize filter
Subroutine InitFilter(filter, SampleRate, MaxBlockSize)

  type(TRbjEqFilter) filter
  real SampleRate
  integer MaxBlockSize

  filter%fMaxBlockSize = MaxBlockSize
  filter%fSampleRate = SampleRate

  filter%fFilterType=0
  filter%fFreq=500
  filter%fQ=0.3
  filter%fDBGain=0
  filter%fQIsBandWidth=.TRUE.

  filter%in1=0
  filter%in2=0
  filter%ou1=0
  filter%ou2=0

  return

End Subroutine InitFilter

Subroutine SetQ(filter, NewQ)
  type(TRbjEqFilter) filter
  real NewQ
  filter%fQ = (1-NewQ)*0.98
  return
End Subroutine SetQ

Subroutine CalcFilterCoeffs(filter, pFilterType, pFreq, pQ, pDBGain, pQIsBandWidth)
  type(TRbjEqFilter) filter
  integer pFilterType
  real pFreq, pQ, pDBGain
  logical pQIsBandWidth

  filter%fFilterType=pFilterType
  filter%fFreq=pFreq
  filter%fQ=pQ
  filter%fDBGain=pDBGain
  filter%fQIsBandWidth=pQIsBandWidth

  call CalcFilterCoeff(filter)

  return

End Subroutine CalcFilterCoeffs

Subroutine CalcFilterCoeff(filter)
  type(TRbjEqFilter) filter
  real alpha,a0,a1,a2,b0,b1,b2
  real A,beta,omega,pi,tsin,tcos

  pi = 3.1415926535

  a0=0.0;a1=0.0;a2=0.0;b0=0.0;b1=0.0;b2=0.0
  
  ! peaking, LowShelf or HiShelf
  if (filter%fFilterType>=6) then
    A = 10.0**(filter%fDBGain/40.0)
    omega = 2.0*filter%fFreq/filter%fSampleRate
    tsin = sin(omega)
    tcos = cos(omega)

    if (filter%fQIsBandWidth .eqv. .true.) then
      alpha = tsin*sinh(log(2.0)/2.0*filter%fQ*omega/tsin)
    else
      alpha = tsin/(2.0*filter%fQ)
    end if

    beta = sqrt(A)/filter%fQ

    ! peaking
    if (filter%fFilterType==6) then
      b0=1.0+alpha*A
      b1=-2.0*tcos
      b2=1.0-alpha*A
      a0=1.0+alpha/A
      a1=-2.0*tcos
      a2=1.0-alpha/A
    else if (filter%fFilterType==7) then !lowshelf
      b0=(A*((A+1.0)-(A-1.0)*tcos+beta*tsin))
      b1=(2.0*A*((A-1.0)-(A+1.0)*tcos))
      b2=(A*((A+1.0)-(A-1.0)*tcos-beta*tsin))
      a0=((A+1.0)+(A-1.0)*tcos+beta*tsin)
      a1=(-2.0*((A-1.0)+(A+1.0)*tcos))
      a2=((A+1.0)+(A-1.0)*tcos-beta*tsin)
    else if (filter%fFilterType==8) then !hishelf
      b0=(A*((A+1.0)+(A-1.0)*tcos+beta*tsin))
      b1=(-2.0*A*((A-1.0)+(A+1.0)*tcos))
      b2=(A*((A+1.0)+(A-1.0)*tcos-beta*tsin))
      a0=((A+1.0)-(A-1.0)*tcos+beta*tsin)
      a1=(2.0*((A-1.0)-(A+1.0)*tcos))
      a2=((A+1.0)-(A-1.0)*tcos-beta*tsin)
  else 
    continue
    end if
  else  !other filter types
    omega=2.0*pi*filter%fFreq/filter%fSampleRate
    tsin = sin(omega)
    tcos = cos(omega)
    if (filter%fQIsBandWidth .eqv. .true.) then
      alpha = tsin*sinh(log(2.0)/2.0*filter%fQ*omega/tsin)
    else
      alpha = tsin/(2.0*filter%fQ)
    end if
    if (filter%fFilterType==0) then !lowpass
      b0=(1.0-tcos)/2.0
      b1=1.0-tcos
      b2=(1.0-tcos)/2.0
      a0=1.0+alpha
      a1=-2.0*tcos
      a2=1.0-alpha
  else if (filter%fFilterType==1) then !highpass
      b0=(1.0+tcos)/2.0
      b1=-(1.0+tcos)
      b2=(1.0+tcos)/2.0
      a0=1.0+alpha
      a1=-2.0*tcos
      a2=1.0-alpha
    else if (filter%fFilterType==2) then !bandpass CSG
      b0=tsin/2.0
      b1=0.0
      b2=-tsin/2.0
      a0=1.0+alpha
      a1=-1.0*tcos
      a2=1.0-alpha
    else if (filter%fFilterType==3) then !bandpass CZPG
      b0=alpha
      b1=0.0
      b2=-alpha
      a0=1.0+alpha
      a1=-2.0*tcos
      a2=1.0-alpha
  else if (filter%fFilterType==4) then !notch
      b0=1.0
      b1=-2.0*tcos
      b2=1.0
      a0=1.0+alpha
      a1=-2.0*tcos
      a2=1.0-alpha
  else if (filter%fFilterType==5) then !allpass
      b0=1.0-alpha
      b1=-2.0*tcos
      b2=1.0+alpha
      a0=1.0+alpha
      a1=-2.0*tcos
      a2=1.0-alpha
  else 
    continue
    end if
  end if

  filter%b0a0=b0/a0
  filter%b1a0=b1/a0
  filter%b2a0=b2/a0
  filter%a1a0=a1/a0
  filter%a2a0=a2/a0
 
  return

End Subroutine CalcFilterCoeff


real Function Process1(filter, input)
  type(TRbjEqFilter) filter
  real input, LastOut

  !filter
  LastOut = (filter%b0a0)*input + (filter%b1a0)*(filter%in1) + &
            (filter%b2a0)*(filter%in2) - (filter%a1a0)*(filter%ou1) - &
      (filter%a2a0)*(filter%ou2)

  ! push in/out buffers
  filter%in2=filter%in1
  filter%in1=input
  filter%ou2=filter%ou1
  filter%ou1=LastOut

  Process1 = LastOut

  return

End Function Process1

Subroutine Process(filter, input, Sampleframes)
  type(TRbjEqFilter) filter
  real input(*)  
  integer Sampleframes, i
  real LastOut

  ! Allocate memory for vector filter%out1
  allocate(filter%out1(0:Sampleframes),stat=ialloc)

  do i=0, Sampleframes-1
    ! filter
    LastOut = filter%b0a0*(input(i)) + filter%b1a0*filter%in1 + &
            filter%b2a0*filter%in2 - filter%a1a0*filter%ou1 - &
        filter%a2a0*filter%ou2
    ! LastOut=input[i];
    ! push in/out buffers
    filter%in2=filter%in1
    filter%in1=input(i)
    filter%ou2=filter%ou1
    filter%ou1=LastOut
    filter%out1(i)=LastOut

  end do

  return

End Subroutine Process


End Module butter
