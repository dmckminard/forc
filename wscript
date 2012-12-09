#! /usr/bin/env python
# encoding: utf-8
# DC 2008
# Thomas Nagy 2010 (ita)

top = '.'
out = 'build'

sys_libs = ['sndfile']

def options(opt):
	opt.load('compiler_fc')
	opt.load('compiler_c')

def configure(conf):
	
	conf.load('compiler_fc')
	conf.load('compiler_c')
	
	if conf.env.FC_NAME == 'IFORT':
		conf.env['FCFLAGS'] = ['-warn']
	elif conf.env.FC_NAME == 'GFORTRAN':
		conf.env['FCFLAGS'] = ['-Wall', '-W', '-O3','-I/usr/include']
	
	conf.env['CFLAGS'] = ['-std=c99'];
	#conf.env['INCLUDE'] = ['']
	
	conf.check_fortran()
	conf.check_fortran_verbose_flag()
	conf.check_fortran_clib()
	conf.check_fortran_dummy_main()
	conf.check_fortran_mangling()
	
	conf.check(features='fc fcprogram', lib=['sndfile', 'fftw3'], uselib_store='SNDFILE')

def build(bld):
		
	bld(
		features = 'c fc fcprogram',
		source   = ['sndfile_wrap.c', 'forc.f90','sndfile.f90', 'dft.f90'],
		target   = 'forc',
		use = 'SNDFILE')
