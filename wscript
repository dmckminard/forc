#! /usr/bin/env python
# encoding: utf-8
#
# Mason A. Green (mason dot green at gmail)

import sys

top = '.'
out = 'build'

sys_libs = ['sndfile', 'fftw3']
source_files = ['sndfile_wrap.c', 'forc.f90','sndfile.f90', 'dft.f90', 
'utils.f90', 'min_phase.f90', 'filter.f90', 'butter.f90']

#Platform specific 
if sys.platform == 'win32':
  # MS Windows
  from waflib.Tools.compiler_c import c_compiler
  c_compiler['win32'] = ['gcc']
  include_dir = '-Ic:\MinGW32\include'
else:
  # GNU/Linux, BSD, OSX, etc
  include_dir = '-I/usr/include'
  
def options(opt):
	opt.load('compiler_fc')
	opt.load('compiler_c')

def configure(conf):
	
  conf.load('compiler_fc')
  conf.load('compiler_c')
  
  if conf.env.FC_NAME == 'IFORT':
    conf.env['FCFLAGS'] = ['-warn']
  else:
    # assume gfortran
    conf.env['FCFLAGS'] = ['-Wall', '-W', '-O3', include_dir]
  
  conf.env['CFLAGS'] = ['-std=c99'];
  #conf.env['INCLUDE'] = ['']
  
  conf.check_fortran()
  conf.check_fortran_verbose_flag()
  conf.check_fortran_clib()
  conf.check_fortran_dummy_main()
  conf.check_fortran_mangling()
  
  conf.check(features='fc fcprogram', lib=sys_libs, uselib_store='SNDFILE')

def build(bld):
		
	bld(
		features = 'c fc fcprogram',
		source   = source_files,
		target   = 'forc',
		use = 'SNDFILE')
