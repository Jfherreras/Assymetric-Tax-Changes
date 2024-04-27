%% Estimating Parameters

    
% Run Bayesian estimation
        
[pEst,pos,~,~,mest] =estimate(m,H,startest:endyear,E,...
    'tolx',1e-8,'tolfun',1e-8,'nosolution=','penalty',...
    'filter=',{'relative',false});

%save all estimated models in database
resultfolder = countrycode;
if ~isdir(resultfolder)
    mkdir(resultfolder)
end
datstr = datestr(now(),'yyyymmdd');
save([resultfolder,filesep,'estModel_', datstr, '.mat'],'mest','pEst','pos');

