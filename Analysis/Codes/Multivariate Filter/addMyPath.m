% restore MATLAB default setting
restoredefaultpath 

% add IRIS package to MATLAB path
% only version IRIS_Tbx_20130523 is tested, 
% other untested versions may or may not work with this code.
addpath('C:\Users\andra\Documents\MHCP\IRIS-Toolbox-Release-20210802');

irisstartup;

% add utilities
addpath('func'); %utility functions