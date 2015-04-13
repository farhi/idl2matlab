@rem
@rem   This file is part of the IDL2Matlab translator
@rem   Copyright (C) 2003-2009, All rights reserved
@rem   Institut Laue Langevin, Grenoble, France
@rem
@rem   This program is free software; you can redistribute it and/or modify
@rem   it under the terms of the GNU General Public License as published by
@rem   the Free Software Foundation; version 2 of the License.
@rem
@rem   This program is distributed in the hope that it will be useful,
@rem   but WITHOUT ANY WARRANTY; without even the implied warranty of
@rem   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
@rem   GNU General Public License for more details.
@rem
@rem   You should have received a copy of the GNU General Public License
@rem   along with this program; if not, write to the Free Software
@rem   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
@echo off
@rem Simple batch script for installation of IDL2Matlab on Win32 systems,
@rem
@rem Please modify the path below for installing mcstas in non-standard
@rem location

@echo ** IDL2Matlab install.bat for Win32...
@echo ...
@echo Requirements:
@echo   * Dec-Cpp from http://www.bloodshed.net/dev/devcpp.html
@echo       Have it installed e.g. in C:\Dev-Cpp
@echo       When installed, add the C:\Dev-Cpp\bin directory to your PATH
@echo       Select from the Windows menu:
@echo       Start/Settings/Control Panel/System/Advanced/Environment Variables
@echo ...
@echo Use Ctrl-C if you want to break this script to install these packages or
@pause
@set CC=c:\Dev-Cpp\bin\gcc.exe
@set /P CC=Set CC compiler variable (default is %CC%, use quotes if path contains spaces):

@if exist %CC% goto cc_ok
@echo ERROR: the C compiler %CC% can not be found. Aborting.
pause
exit
:cc_ok
@set INCLUDE="C:\Dev-Cpp\include"
@echo ...
@echo NOTE: Set the C include and lib paths using / or \\ - NOT single \
@echo ...
@set /P INCLUDE=Set C INCLUDE variable (default is %INCLUDE%, use quotes if path contains spaces):
@if exist %INCLUDE% goto inc_ok
@echo ERROR: the INCLUDE %INCLUDE% can not be found. Aborting.
pause
exit
:inc_ok
@set LIB="C:\Dev-Cpp\lib"
@set /P LIB=Set C LIB variable (default is %LIB%, use quotes if path contains spaces):
@if exist %LIB% goto lib_ok
@echo ERROR: the LIB %LIB% can not be found. Aborting.
pause
exit
:lib_ok

@if "%IDL2MATLAB_SITE%"=="" set IDL2MATLAB_SITE=c:\idl2matlab
@SET /P IDL2MATLAB_SITE=Set IDL2MATLAB base directory (default is %IDL2MATLAB_SITE%):

@echo Installing in IDL2MATLAB_SITE=%IDL2MATLAB_SITE%
@if exist %IDL2MATLAB_SITE% move %IDL2MATLAB_SITE% "%IDL2MATLAB_SITE%.%DATE%"
@echo Creating directory %IDL2MATLAB_SITE%
@mkdir %IDL2MATLAB_SITE%
:bin
@if exist %IDL2MATLAB_SITE%\bin goto compile
@echo Creating directory %IDL2MATLAB_SITE%\bin
@mkdir %IDL2MATLAB_SITE%\bin

:compile
%CC% -c -g idl2matlab.c -I%INCLUDE%
%CC% -c -g main.c -I%INCLUDE%
%CC% -c -g tree.c -I%INCLUDE%
%CC% -c -g code.c -I%INCLUDE%
%CC% -c -g rename.c -I%INCLUDE%
%CC% -c -g lib.c -I%INCLUDE%
%CC% -c -g hashtable.c -I%INCLUDE%
%CC% -c -g table_symb.c -I%INCLUDE%
%CC% -c -g lex.yy.c -I%INCLUDE%
%CC% -g main.o idl2matlab.o tree.o hashtable.o table_symb.o rename.o lex.yy.o lib.o code.o -o idl2matlab -I%INCLUDE%

:inst
@echo Copying in the files...
@copy idl2matlab.exe %IDL2MATLAB_SITE%\bin
@xcopy /e /i /y /q lib C:\IDL2MATLAB
@reg add HKCU\Environment /v IDL2MATLAB /d "%IDL2MATLAB_SITE%" /f
@reg add HKCU\Environment /v CC /d "%CC%" /f
@reg add "HKLM\SYSTEM\CurrentControlSet\Control\Session Manager\Environment" /v PATH /d "%IDL2MATLAB_SITE%\bin;%PATH%" /f /t REG_EXPAND_SZ

@if exist %IDL2MATLAB_SITE%\bin\idl2matlab.exe goto inst_ok
@echo ERROR: the installation seems to have failed. Please check.
pause
exit

:inst_ok
@echo Please log off and on again to finish the IDL2Matlab setup!
@echo .
@echo Thanks for using IDL2Matlab. End of the installation.
@echo To use idl2matlab, start a Command Prompt (Programs/Accessories)
@echo   and type: 'idl2matlab <filename.pro>'
@pause

