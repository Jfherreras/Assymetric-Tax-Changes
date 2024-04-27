function D=actual2log(G)
% Convert from actual growth rates to log aprroximation

D=100.*log(1+G./100);
end