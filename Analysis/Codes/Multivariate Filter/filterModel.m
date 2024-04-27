%% ========================
%% filter the unobservables
%% ========================

% smoothing/filtering using the estimated model    
filtrng = startyear:endyear;

[~,f,v,~,pe,scov] = filter(mest,H,filtrng, ...
        'output=','smooth,filter,predict','ahead=',12,'returnMse=',false,'returnCont=',true);
    
fout = f.smooth.mean;

%save results
datstr = datestr(now(),'yyyymmdd');
save([resultfolder,filesep,'filtModel_',datstr,'.mat'],'f','scov','v','pe');
