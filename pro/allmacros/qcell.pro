pro qcell,s,cell,index=index,counts=counts

index=indgen(N_ELEMENTS(s(0,*)))+cell
counts=s(cell,*)
end
