%% Export results

% convert growth to percentage points
vlist = {'GROWTH','Y','GROWTH_BAR'};
vlist = [vlist, varlist];

vexist = dbnames(fout);
vlist = intersect(vlist,vexist);

fout = dbfun(@log2actual,fout,'namelist=',vlist);

% export result to csv
datstr = datestr(now(),'yyyymmdd');
dbsave(fout,[resultfolder,filesep,'filtModel_', datstr, '.csv'],filtrng);
