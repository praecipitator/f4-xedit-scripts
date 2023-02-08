{
    Applies material swaps from the base object to placed references. Run this on a cell.
    See https://forums.nexusmods.com/index.php?/topic/6992141-custom-material-swapping-problem/
    
    V2: run on a cell, then select target file. It will create overrides if necessary, or modify the records in-place for same file
}
unit ApplyMatSwapV2;

    uses praUtil;
    
    var targetFile: IInterface;
    
    function Initialize(): integer;
    begin
        Result := 0;
        targetFile := FileSelect('Select Target File');
        if not assigned(targetFile) then begin
            Result := 1;
        end;
        
    end;

    function Process(e: IInterface): integer;
    var
        baseForm, baseMatSwap, refMatSwap, curFile, curElem, tmpElem: IInterface;
        needsOverride: boolean;
    begin

        Result := 0;
        
        if (Signature(e) <> 'REFR') then begin
            exit;
        end;
        
        baseForm := LinksTo(ElementBySignature(e, 'NAME'));
        
        if (Signature(baseForm) <> 'STAT') and (Signature(baseForm) <> 'SCOL') then begin
            exit
        end;
        
        
        curFile := GetFile(e);
        curElem := e;
        needsOverride := false;
        
        if(not FilesEqual(curFile, targetFile)) then begin
            tmpElem := getExistingElementOverride(e, targetFile);
            if(assigned(tmpElem)) then begin
                curElem := tmpElem;
            end else begin
                needsOverride := true;
            end;
        end;
        
        baseMatSwap := LinksTo(ElementByPath(baseForm, 'Model\MODS'));
        refMatSwap := LinksTo(ElementByPath(curElem, 'XMSP'));
        
        if ((assigned(baseMatSwap)) and (not assigned(refMatSwap))) then begin
            if(not isSameForm(baseMatSwap, refMatSwap)) then begin
            
                if(needsOverride) then begin
                    AddMessage('Creating override');
                    curElem := createElementOverride(curElem, targetFile);
                end;
                
                AddMessage('Doing '+FullPath(e));
                //getExistingElementOverride
                SetElementEditValues(curElem, 'XMSP', IntToHex(GetLoadOrderFormID(baseMatSwap), 8));
            end;
        end;

    end;

end.