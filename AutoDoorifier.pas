{
  New script template, only shows processed records
  Assigning any nonzero value to Result will terminate script
}
unit AutoDoorifier;

    uses praUtil;
    
    var
        setupDone: boolean;
        autoDoorFile: IwbFile;
        ToFile: IwbFile;
        autoDoorKeyword: IInterface;
        
    function doSetup(e: IInterface): boolean;
    begin
        Result := false;
        
        if not Assigned(ToFile) then begin
            ToFile := ShowFileSelectDialog('Select file to create overrides in');

            if not Assigned(ToFile) then begin
                Exit;
            end;
        end;
        
        autoDoorFile := findFile('AutoDoors.esp');
        if(not assigned(autoDoorFile)) then begin
            AddMessage('AutoDoors.esp not found!');
            exit;
        end;
        autoDoorKeyword := MainRecordByEditorID(GroupBySignature(autoDoorFile, 'KYWD'), 'AD_CrossModUniversalDoorSupport');
        setupDone := true;
        Result := true;
    end;
    
    function getDisplayString(e: IInterface): string;
    var 
        dName: string;
    begin
        dName := DisplayName(e);
        if(dName = '') then begin
            dName := '<no name set>';
        end else begin
            dName := ''''+dName+'''';
        end;
        Result := GetElementEditValues(e, 'EDID') + ' '+dName;
    end;


    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    var
        sig: string;
        dName: string;
        newElem: IInterface;
    begin
        Result := 0;
        
        if(not setupDone) then begin
            if(not doSetup(e)) then begin
                Result := 1;
                exit;
            end;
        end;

        sig :=  signature(e);
        
        if(sig = 'DOOR') then begin
            if(not hasKeywordByPath(e, autoDoorKeyword, 'KWDA')) then begin 
            
                // check if we have this one already
                newElem := MainRecordByEditorID(GroupBySignature(ToFile, 'DOOR'), GetElementEditValues(e, 'EDID'));
                dName := getDisplayString(e);
                if(assigned(newElem)) then begin 
                    if(hasKeywordByPath(newElem, autoDoorKeyword, 'KWDA')) then begin
                        AddMessage(dName+' is already done');
                        exit;
                    end else begin
                        AddMessage('Adding keyword to existing override of '+dName);
                    end;
                end else begin 
                    AddMessage('Creating override with keyword for '+dName);
                    // addRequiredMastersSilent(e, ToFile);
                    newElem := getOrCreateElementOverride(e, ToFile);
                end;
                
                addRequiredMastersSilent(autoDoorKeyword, ToFile);
                addKeywordByPath(newElem, autoDoorKeyword, 'KWDA');
            end;
        end;
    end;
end.
