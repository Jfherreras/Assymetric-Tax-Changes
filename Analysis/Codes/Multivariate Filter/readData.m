%% Read data/forecast/judgment (if applicable)

H = dbload(datafile);

% keep only one year of LGDP, rest will be computed using growth rate

if ~isnan(H.LGDP_(min(startyear,startest)))
    H.LGDP_ = H.LGDP_{min(startyear,startest)};
else
    lrng = get(H.LGDP_,'range'); 
    H.LGDP_ = H.LGDP_{lrng(1)};
end

% log-linearize percentage points
varlist = {'GROWTH_','GROWTH_CONS1_','GROWTH_CONS2_','GROWTH_CONS3_','GROWTH_CONS4_','GROWTH_CONS5_','Y_','GROWTH_BAR_'};
H = dbfun(@actual2log, H, 'namelist=',varlist);

% Compute RMSE for consensus data
for ilag = 1:5
    ll = num2str(ilag);
    P.(['std_RES_GROWTH_CONS' ll]) = rmse(H.GROWTH_, H.(['GROWTH_CONS' ll '_']){-ilag});
end

for ilag = 1
    ll = num2str(ilag);
    P.(['std_RES_PIE_CONS' ll]) = rmse(H.PIE_, H.(['PIE_CONS' ll '_']){-ilag});
end
 
% update parameters
m = assign(m,P);

