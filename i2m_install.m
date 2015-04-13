function i2m_install

if ~exist('i2m_init')
  basename = [ fileparts(which(mfilename)) filesep ];
  p = genpath(fullfile(basename, 'lib', 'matlablib'));

  addpath(p);

  disp('Welcome to IDL2Matlab Library (March 20, 2006).');
  fprintf(1,'\n');
  disp('This software is the property of the');
  disp('  Computing for Science group');
  disp('  Institut Laue langevin, BP 156');
  disp('  F-38042 Grenoble cedex 9, FRANCE');
  disp('It comes with ABSOLUTELY NO WARRANTY');
  disp('Commercial use is STRICTLY forbidden');
  disp('You are allowed to redistribute this software but should then contact us.');
  disp('Contact richard@ill.fr or farhi@ill.fr for more informations');
  fprintf(1,'\n');
  disp('Loading IDL2Matlab compatibility functions');
  disp('Restrictions:');
  disp('  Graphic functions (plot, surf, widgets) are incomplete');
  disp('  Objects are not translated');
  rehash
  disp('[Press any key to continue]');
  pause
  disp('Executing "i2m_init" to initialize the IDL mode');
  i2m_init;
  disp('Launch the translated program using its name at the prompt.');

end
