# idl2matlab
an IDL to Matlab/Scilab translator

version 1.3 (March 11th 2004)

(c) Institut Laue-Langevin, Grenoble, France.
--------------------------------------------------------------------------------
This is IDL2Matlab / IDL2Scilab translator
Initial Release 0.0 (28 jan 2003) dev (alpha)
        Release 1.0 (31 jul 2003) dev (alpha)
        Release 1.1 (17 Nov 2003) dev (alpha)
        Release 1.2 (11 Mar 2004) dev (alpha)
        Release 1.3 (20 Mar 2006)

This project is designed by the Institut Laue-Langevin <http://www.ill.fr>.
This version may be redistributed as long as you also give this README file.

The 'idl2matlab' executable was built for Linux (x86) machines. It comes with the basic
library translation.

This demo translates essentially the IDL grammar to Matlab/Scilab code. It comes with 'pcode' functions for matlab and scilab compatibility functions.

SCOPE:
------

  This is IDL2Matlab (March 20, 2006).
  This software is the property of the
    Computing for Science group
    Institut Laue langevin, BP 156
    F-38042 Grenoble cedex 9, FRANCE
  It comes with ABSOLUTELY NO WARRANTY
  Commercial use is STRICTLY forbidden
  You are allowed to redistribute this software but should then contact us

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

Examples :
  idl2matlab -qw essai.pro outDir/
  idl2matlab -t essai.pro outDir/ > out
  idl2matlab -S "print,'IDL2Matlab'"
  idl2matlab -T4 essai.pro outDir/ > out
  idl2matlab -f essai.pro functionName outDir/ > out

INSTALLATION:
-------------

We only provide executable for Linux/Intel machines.

If you are superuser:

1- Extract/copy the distribution into the /usr/local/idl2matlab directory
# mkdir /usr/local/idl2matlab
# cd /usr/local/idl2matlab
# tar xzf ~/idl2matlab-archive.tgz
2- make a link
# ln -s /usr/local/idl2matlab/idl2matlab /usr/local/bin/idl2matlab

If you are not superuser:

1- Extract/copy the distribution into your 'idl2matlab' directory (create it if required)
# mkdir ~/idl2matlab
# cd ~/idl2matlab
# tar xzf ~/idl2matlab-archive.tgz
2- define the local installation path
   (to be automated at the end of the ~/.cshrc, ~/.bashrc,
    /etc/csh.cshrc or /etc/bash.bashrc)
#tcsh: setenv IDL2MATLAB ~/idl2matlab
#bash: export IDL2MATLAB=~/idl2matlab
3- create a link
# ln -s ~/idl2matlab/idl2matlab ~/bin/idl2matlab

USAGE:
------

You may try this idl2matlab demo version with

# idl2matlab essai.pro

will generate the essai.m Matlab function.


Within Matlab, type at the prompt (from distribution directory):

>>  p=pwd;
>>  cd /usr/local/idl2matlab/ % (or your local '~/idl2matlab')
>>  i2m_install;
>>  cd(p);

Within Scilab, type at the prompt (from distribution directory):

>  chdir('/usr/local/idl2scilab') # (or your local '~/idl2matlab')
>  exec('i2s_init',-1);

These may be automatically executed when starting Matlab/Scilab. Refer to startup scripts
for these languages.

--------------------------------------------------------------------------------
Please visit the URL <http://www.ill.fr/Computing/IDL2Matlab>

