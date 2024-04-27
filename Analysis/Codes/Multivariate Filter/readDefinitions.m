%% Potential Output Model 
clear all
close all
clc

% set countries
countrycode = {'COL','MEX'};

% set dates
startyear = yy(2000); %% yy(1990); % filter start date

startest  = yy(1984); %% yy(1990); % estimation start date

endyear   = yy(2022); %% yy(2016); % estimation/filter end date; ie. end date of information provided

endhist   = yy(2021); %% yy(2014); % end of historical data

% set model file
modfile   = 'gap_model.model';

% set initial parameters values 
P = struct('a1',                0.05,...    % 0.25,...
           'a2',                0.25,...    % 0.25,...
           'a3',                0.5,...     % 0.6,...
           'a4',                0.5,...     % 0.1,...
           'a5',                0.1,...     % 0.2,...
           'b1',                0.25,...     % 0.2,...
           'b2',                0.75,...       % 0.8,...
           'b3',                0.5,...    % Gap vs Pass through
           'c1',                0.3,...     % AR
           'c2',                0.3,...     % GAP (Okun's Law)
           'd1',                0.3,...     % Wait-and-see
           'd2',                1.5,...     % Taylor Principle
           'd3',                0.5,...     % Output gap weigth
           'growth_ss',         3.26,...  % 3.3   % PREGUNTAR % 2.44,... % steady state growth rate
           'unr_ss',            10.36408);     % 10.36408 % 11);%12); %12        % 9 % PREGUNTAR % 5.75);   % steady state unemployment rate

% set parameter priors for those to be estimated
% (parameter estimates from this prior will overwrite what's specified above)
E.lambda={.25,0.05,0.99,logdist.normal(.25,.1)};
E.beta  ={.25,0.05,3,logdist.normal(.25,.1)};
E.phi   ={.6,0.1,0.99,logdist.normal(.6,.001)}; %0.58
E.theta ={.25,0.05,0.9,logdist.normal(.25,.01)};%E.theta ={.25,0.05,0.9,logdist.normal(.25,.01)};

E.std_RES_LGDP_BAR={.2,0.005,3,logdist.normal(.2,.01)};
E.std_RES_G={.2,0.005,3,logdist.normal(.2,.01)};
E.std_RES_Y={.8,0.005,3,logdist.normal(.8,.01)};

E.std_RES_PIE   ={.25,0.005,3,logdist.normal(.25,.2)};

E.std_RES_UNR_GAP   ={.5,0.005,3,logdist.normal(.5,.01)};
E.std_RES_UNR_BAR   ={.1,0.005,3,logdist.normal(.1,.01)};
E.std_RES_G_UNR_BAR ={.1,0.005,3,logdist.normal(.1,.01)};

E.tau1  ={.35,0.05,0.99,logdist.normal(.35,.1)};
E.tau2  ={.3,0.05,0.99,logdist.normal(.3,.1)};
E.tau3  ={.1,0.05,0.99,logdist.normal(.1,.01)};
E.tau4  ={.22,0.05,0.99,logdist.normal(.22,.005)};


% set data file
%% Base original
% datafile = ['data',filesep,'COL_data.csv']; % including historical data, forecasts, and judgments (if any)
%% Base contrafactual
datafile = ['data',filesep,'COL.xslx'] % including historical data, forecasts, and judgments (if any)

% impose judgment (for potential, output gap and NAIRU, at the end of
% sample)
% 1: on; 0: off
judgment = 1;
