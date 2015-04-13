% idl2matlab(inputFile, outputDirectory, options, ...) translates IDL source code into Matlab
% input arguments must be of type char (strings). 
% There is no output parameter (translated code is generated as separate files).
%
% Options :
%   -s for script files translation
%   -S for string translation
%   -q to hide messages (quiet mode)
%   -w to stop warnings writing
%   -t to print the abstract tree (long output)
%   -V to get idl2Matlab version
%  -A to get idl2Matlab authors
%  -Tx to get x spaces for a tabulation x<10
%  -f to translate only one function
%  -C to translate in Scilab (default : Matlab)
%
%Examples :
%  idl2matlab -qw essai.pro outDir/
%  idl2matlab -t essai.pro outDir/ > out
%  idl2matlab -S "print,'IDL2Matlab'"
%  idl2matlab -T4 essai.pro outDir/
%  idl2matlab -f essai.pro functionName outDir/ > out
%  idl2matlab -C essai.pro outDir/

% idl2matlab version 1.5 090120

function idl2matlab(args)

if length(getenv('IDL2MATLAB'))
	addpath(genpath(getenv('IDL2MATLAB')));
end

if nargin == 0, args=''; end
if iscellstr(args)
  for index=1:length(args)
    idl2matlab(args{index});
  end
  return
end

if ispc
	idl2matlab_exe = 'idl2matlab.exe';
else     
	idl2matlab_exe = 'idl2matlab';
	addpath(genpath('/usr/local/lib/idl2matlab'));
end

% search executable 

while 1
  % try with system wide command
	exec = idl2matlab_exe;
	[s,w] = system(exec);
	if ~s, break; end
	% other locations
	exec = [ getenv('IDL2MATLAB') filesep idl2matlab_exe ];
	if isexec(exec), break; end
	exec = [ getenv('IDL2MATLAB') filesep 'bin' filesep idl2matlab_exe ];
	if isexec(exec), break; end
	exec = [ getenv('HOME') filesep 'bin' filesep idl2matlab_exe ];
	if isexec(exec), break; end
	exec = [ fileparts(which(mfilename)) filesep 'bin' filesep idl2matlab_exe ]; 
	if isexec(exec), break; end
	exec = [ fileparts(which(mfilename)) filesep idl2matlab_exe ]; 
	if isexec(exec), break; end
	exec = [ fileparts(which(mfilename)) filesep '..' filesep idl2matlab_exe ]; 
	if isexec(exec), break; end
	exec = [ fileparts(which(mfilename)) filesep '..' filesep 'bin' filesep idl2matlab_exe ]; 
	if isexec(exec), break; end
	exec = [ fileparts(which(mfilename)) filesep '..' filesep '..' filesep 'bin' filesep idl2matlab_exe ]; 
	if isexec(exec), break; end
	exec = '';
	break;
end
% execute command
if ~isempty(exec)
	exec = [ exec ' ' args ];
	disp(exec);
	[s,w] = system(exec);
else s=1; w='executable not found';
end
disp(w)

if s
	error('idl2matlab: failed to exectute IDL2Matalab');
else
	i2m_init;
end

% -----------------------------------------------------------------------------

function ret=isexec(file)
% test if single file exists
	ret = 0;
	d = dir(file);
	if length(d) ~= 1, return; end
	if d.isdir, return; end
	ret = 1;
	return

