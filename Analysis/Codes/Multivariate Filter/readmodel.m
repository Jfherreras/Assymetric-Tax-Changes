%% 2022-10-12 readmodel
%%%%%%%%
%%% CALIBRATION
%%%%%%%%
function [m,p,mss] = readmodel(filter)

%% Country list settings
country_list = {'AUS','AUT'};
country_list = 'AUS';
%% Filtration on/off
% filter = true - Kalman filter ON
% filter = false - Kalman filter OFF
p.filter = filter;

%% Typical and specific parameter values be used in calibrations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 1. Aggregate demand equation (the IS curve)
%output expectations
p.a1 = {.05,0.05,0.99,logdist.normal(.05,.1)};

%output persistence
p.a2 = {.5,0.05,0.99,logdist.normal(.5,.1)};
    
%MCI
p.a3 = {.5,0.05,0.99,logdist.normal(.5,.1)};

%save-imported consumption
p.a4 = {.75,0.05,0.99,logdist.normal(.75,.1)};

%world demand
p.a5 = {.2,0.05,0.99,logdist.normal(.2,.1)};

%% 2. Aggregate supply (Phillips curve)
%persistence - expectations
p.b1 = {.25,0.05,0.99,logdist.normal(.25,.1)};

%RMC
p.b2 = {.75,0.05,0.99,logdist.normal(.75,.1)};

%Output gap vs imported inputs
p.b3 = {.5,0.05,0.99,logdist.normal(.5,.1)};

%% 3. Unemployment (Okun's law)
% persistence; 
p.c1 = {.3,0.05,0.99,logdist.normal(.3,.1)};     

% output gap weight (okun's law)
p.c2 = {.3,0.05,0.99,logdist.normal(.3,.1)};

% convergence speed to steady state
p.c3 = {.5,0.05,0.99,logdist.normal(.5,.1)};
%% 4. Monetary policy (Taylor's rule)
% Wait and see - persistence
p.d1 = {.5,0.05,0.99,logdist.normal(.5,.1)};

% Taylor's principle - inflation
p.d2 = {1.5,0.05,2,logdist.normal(1.5,.1)};

% Output gap weight
p.d3 = {.25,0.05,0.99,logdist.normal(.25,.1)};

%% 5. External block
% Persistences
p.rho_DL_CPI_TAR = 0.3;
p.rho_PREM_BAR = 0.3;
p.rho_L_GDP_RW_GAP = 0.3;
p.rho_RS_RW = 0.3;
p.rho_RR_RW_BAR = 0.3;
p.rho_DL_CPI_RW = 0.3;

%% 6. Steady states
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The inflation target and other observed economic trends
% These "steady-state" values are all calibrated

% Foreign trend inflation or inflation target
p.ss_DL_CPI_RW  = 2;

% Trend level of domestic real interest rate 
p.ss_RR_BAR      = 3;%2; %3;2;((1+2/100)^0.25 -1)*100;
if country_list == "AUS"
    p.ss_RR_BAR      = 3;
end
if country_list == "AUT"
    p.ss_RR_BAR      = 2;
end

% Trend change in the real ER (negative number = real appreciation)
p.ss_DLA_Z_BAR   = 0;%-1; %1.1;0; ((1+1.1/100)^0.25 -1)*100; -1;0; % El supuesto de los externos es depreciación real de 1.1% consistente con el histórico

% Potential output growth
p.ss_DLA_GDP_BAR = 3.3;((1+3.3/100)^0.25 -1)*100; % Crecimiento anual 3.3

% Trend level of foreign real interest rate
p.ss_RR_RW_BAR   = 0.5;
    
% Domestic inflation target
p.ss_D4L_CPI_TAR = 3;

% commodity price trends
p.ss_DLA_RWOIL_BAR  = 0;  2;%0;% 4GM tiene 0 % 2;
p.ss_DLA_RWFOOD_BAR = 0; 1.8232;

% relative price trends
p.ss_DLA_RPXFE_BAR = -0.5267;-0.7/4;-0.1174;-0.35;-0.7/4;-0.1174;      %-0.7340;-1.49;-0.7;
p.ss_DLA_RPF_BAR   = 1.0183;1.8/3;0.1637;0.85;1.8/3;0.1637	;      %1;1.4832;0.7;1.3;
% p.ss_DLA_RPE_BAR   = 0.6;%(-(1-p.w_CPIE-p.w_CPIF)*p.ss_DLA_RPXFE_BAR-p.w_CPIF*p.ss_DLA_RPF_BAR)/p.w_CPIE;1.4/4;1.1764;1;1.6;1.4/4;1.1764	;      %1;

p.ss_PREM_BAR      = -(p.ss_DLA_Z_BAR - p.ss_RR_BAR + p.ss_RR_RW_BAR);2.5;%mean(d.PREM(qq(2007,2):qq(2019,4))); %2.5;2;1.5;


%%
% p.rho_WOIL_BAR = 0.8;
% p.ss_DLA_RWOIL_BAR = 2;

% p.rho_L_WOIL_GAP = 0.8; 

p.kappa_rho_DLA_GDP_BAR = 0.05;
p.kappa_rho_PREM_BAR    = -0.07;0;0.0489;0.05;0.2; % Pensar en si este parámetro debería ser negativo (cuadra bien con el inicio de muestra), teóricamente debería ser positivo
p.nu_DLA_Z_BAR = 0.1;0.5;0.15;0.5;0.3648;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% STDs of the shocks
% Agregate demand STDs shocks
    p.std_SHK_L_GDP_GAP     = 4;3.87;4.95;5.25;5.6;6.3;5.7;4.95;3.7;%3.5;
    p.std_SHK_DLA_GDP_BAR   = 0.5;0.19;0.15;%0.15;
    p.std_SHK_L_GDP_BAR     = 1.3;1.5;1.741;0.75;0.9;0.7;0.75;0.8;%0.8;%21.1332/4;%0.0001;
    
% Inflation STDs shocks
    p.std_SHK_L_CPI         = 0.355;0.338;0.4;0.35;0.3;%0.25;0.2;         %0.025*6.2708; p.std_SHK_DLA_CPI       = 0.9757;
    p.std_SHK_DLA_CPIXFE    = 2.55;2.5;0.5;3;%1.8;3.3;1.5;%1.2*3*0.6865/4;
    p.std_SHK_DLA_CPIF      = 13.2;13;12.5;6.5; %        %3*2.8786/4;
    p.std_SHK_DLA_CPIE      = 6.4;6.9;6.2;4;7;6.5;%1.5*1.5;          %3*1.4130/4;

% Relative price STDs shocks
    p.std_SHK_DLA_RPXFE_BAR = 1.6;2;5;1;2.5;0.3;1;0.83;%1.5;
    p.std_SHK_DLA_RPF_BAR   = 7.7;7.4;8.5;7.5;0.95;1.2;0.5;5;1;18;0.85;0.85;0.5;%0.75;1.5*0.5*1.0432;
    p.std_SHK_DLA_RPE_BAR   = 0.1;1.0618;0.35;%std(d.DLA_RPE_BAR);0.35;4.1;%0.5*0.7086; % Por qué no aparece??
    
% Nominal interest STDs shocks
    p.std_SHK_RS            =  2.35;3.21;3.85;3.5;4;7.5;10;7.5;3.5;3;2.85;2.75;%2.5;1.5;%2*1.5*0.5742/2;

% Inflation targeting YoY STDs shocks
    p.std_SHK_D4L_CPI_TAR   = 2.85;2.55;3;2.8;2.7;3;%1;2.7; 5*0.3740;

% UIP condition STDs shocks
    p.std_SHK_L_S           = 10;10.4;9.5;7;10;%16.5;0.8*11.5933;
    p.std_SHK_DLA_Z_BAR     = 1.55;3.81;3.4;1.2;1.25;2.5;1.275;0.2;0.15;0.4*5.4319;
%     p.std_SHK_DLA_Z_GAP     = 0;

% Premium risk STDs shocks
    p.std_SHK_PREM          = 4*0.4124;
    p.std_SHK_PREM_BAR      = 0.8655;0.5;%4*0.2062;

% External STDs shocks
    p.std_SHK_L_GDP_RW_GAP  = 4.2044;  
    p.std_SHK_RS_RW         = 0.3764;  
    p.std_SHK_RR_RW_BAR     = 0.2;%0.4378;
    p.std_SHK_DLA_CPI_RW    = 2.7423;
    
    p.std_SHK_DLA_RWFOOD_BAR = 0.2507;
    p.std_SHK_L_RWFOOD_GAP   = 0.5014;
    p.std_SHK_DLA_RWOIL_BAR  = 8.7711;
    p.std_SHK_L_RWOIL_GAP    = 17.5421;
    
    p.std_SHK_L_Z_BAR        = 18.213;30;12;14;16;13;3;3.5;%18.9262/4;%0.0001;
    p.std_SHK_L_RWOIL_BAR    = 1.5;
    
    
    p.std_SHK_CR_PREM        = 0.1;%40; % Este valor toca hacerlo muy grande y no hace sentido, mejor sacarlo de las variables

% Anticipated shock
p.std_SHK_A_DLA_GAP = 0.0001;
% COVID
p.std_SHK_COVID = 0.0001;

% test;

%%% 
%% Model solving--a brief description of commands
% Command 'Model.fromFile' reads the text file 'model.mod' (contains the model's
% equations), assigns the parameters and trend values preset in the database
% 'p' (see readmodel) and transforms the model for the matrix algebra. 
% Transformed model is written in the object 'm'. 
m = Model.fromFile('model.model','assign', p,'linear',true);

% Command 'solve' takes the model saved in object 'm' and solves the model
% for its reduced form (Blanchard-Kahn algorithm). The reduced form is again  
% written in the object 'm'
m = solve(m);

% Command 'sstate' takes the transformed model in object 'm', calculates the model's
% steady-state and writes everything back in the object 'm'. Typing 'mss' in
% Matlab command window provides the steady-state values.
m = sstate(m);

% iris version 20211222 - for nonlinear model
% m = Model.fromFile('model.model',"Growth",true,'assign', p);  % replace with new code -- Model.fromFile
% m = sstate(m,'growth',true,'MaxFunEvals',2000);
% m = solve(m);


%% Check steady state
mss = get(m,'sstate');

[flag,discrep,eqtn] = chksstate(m);

if ~flag
  error('Equation fails to hold in steady state: "%s"\n', eqtn{:});
end

if mss.L_GDP_GAP~=0 | mss.L_Z_GAP~=0 | mss.L_RPXFE_GAP~=0 | mss.L_RPF_GAP~=0
    disp('WARNING')
end