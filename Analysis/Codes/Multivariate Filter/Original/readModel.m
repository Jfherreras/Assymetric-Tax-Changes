%% Build model

% build model file
m=model(modfile,'linear=',true,...
    'unobs_condition=',judgment);

% assign calibration
m = assign(m,P);

% solve and compute steady state
m = solve(m);
m = sstate(m,'growth=',true);


