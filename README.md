# idl2matlab
an IDL to Matlab/Scilab translator

version 1.4.1 (March 20th 2006)

(c) Institut Laue-Langevin, Grenoble, France.
--------------------------------------------------------------------------------
This is IDL2Matlab / IDL2Scilab translator
Initial Release 0.0 (28 jan 2003) dev (alpha)
        Release 1.0 (31 jul 2003) dev (alpha)
        Release 1.1 (17 Nov 2003) dev (alpha)
        Release 1.2 (11 Mar 2004) dev (alpha)
        Release 1.3 (20 Mar 2006) dev (alpha)
        Release 1.4 (13 Jun 2007) beta

This project was designed by the Institut Laue-Langevin <http://www.ill.fr>.
This version may be redistributed as long as you also give this README file.

SCOPE:
------

  This is IDL2Matlab (13 June 2007).

  This tool translates automatically an IDL code into Matlab code.

  This software is the property of the
    Computing for Science group
    Institut Laue langevin, BP 156
    F-38042 Grenoble cedex 9, FRANCE
  It comes with ABSOLUTELY NO WARRANTY
  Direct Commercial use is STRICTLY forbidden.
  You are allowed to redistribute this software but should then contact us,
  distribute its source code and cite all the authors.

  Contact: richard@ill.fr or farhi@ill.fr for more informations

  Restrictions:
    Graphic functions (plot, surf, widgets) are incomplete
    Objects are not translated

Usage : idl2matlab [-options] inputFile [outputDirectory]

Options :
  -s for script files translation
  -S for string translation
  -q to hide messages
  -w to stop warnings writing
  -t to print the abstract tree
  -V to get idl2Matlab version
  -Tx to get x spaces for a tabulation x<10
  -f to translate only one function
  -C to translate in Scilab (default : Matlab)

Examples :
  idl2matlab -qw essai.pro outDir/
  idl2matlab -t essai.pro outDir/ > out
  idl2matlab -S "print,'IDL2Matlab'"
  idl2matlab -T4 essai.pro outDir/ > out
  idl2matlab -f essai.pro functionName outDir/ > out

COMPILATION:
------------
This step requires a C compiler, and optionally lex/yacc (or flex/bison).
We recommand gcc on Linux systems and Dev-Cpp on Windows systems.

On Unix/Linux/BSD systems:
  # ./configure; make (if you have super-user rights)
  or
  # ./configure --prefix=/home/joe; make (if you are NOT super-user, single user installation)

On windows systems, use:
  # build.bat

The step-by-step procedure for compilation is something like:
  bison -v -d idl2matlab.y --output-file=idl2matlab.c
  flex -i idl2matlab.l
  gcc -c -g -O2   idl2matlab.c
  gcc -c -g -O2 main.c
  gcc -c -g -O2 tree.c
  gcc -c -g -O2 code.c
  gcc -c -g -O2 rename.c
  gcc -c -g -O2 lib.c
  gcc -c -g -O2 hashtable.c
  gcc -c -g -O2 table_symb.c
  gcc -c -g -O2 lex.yy.c
  gcc -g -O2 main.o idl2matlab.o tree.o hashtable.o table_symb.o rename.o lex.yy.o lib.o code.o -o idl2matlab

If you can not compile the program, you may use pre-built executables in the 'binaries' directory:
The 'idl2matlab.x86' executable was built for Linux (x86) machines.
The 'idl2matlab.exe' binary is for Windows systems.

INSTALLATION:
-------------

On Unix/Linux/BSD systems :
  # make install

If you are NOT superuser (single user install), define the local installation path
   (may be automated at the end of the ~/.cshrc, ~/.bashrc,
    /etc/csh.cshrc or /etc/bash.bashrc)
#tcsh: setenv IDL2MATLAB ~/lib/idl2matlab/
#bash: export IDL2MATLAB=~/lib/idl2matlab/

For Windows systems:
1- copy the 'binaries/idl2matlab.exe' file into C:\IDL2MATLAB, or somewhere accessible from the PATH (e.g. C:\WINDOWS)
2- copy the *content* of the 'lib' directory into C:\IDL2MATLAB
3- define the Environment variable IDL2MATLAB = C:\IDL2MATLAB

Package comes with the full compatibility library translation.

USAGE:
------

You may try this idl2matlab demo version with

# idl2matlab test.pro

will generate the test.m Matlab function.


Within Matlab, type at the prompt (from distribution directory):

>>  p=pwd;
>>  cd /usr/local/lib/idl2matlab/ % (or your local '~/lib/idl2matlab')
>>  i2m_install;
>>  cd(p);

Within Scilab, type at the prompt (from distribution directory):

>  chdir('/usr/local/lib/idl2scilab') # (or your local '~/lib/idl2matlab')
>  exec('i2s_init',-1);

These may be automatically executed when starting Matlab/Scilab. Refer to startup scripts
for these languages.

--------------------------------------------------------------------------------
Please visit the URL <http://sourceforge.net/projects/idl2matlab>
