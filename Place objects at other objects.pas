{
    Run on something within your target file. Put in the EDIDs into the const down there.
    Script will search for all refs of the orig base object, and put an instance of the target object there
}
unit userscript;
    uses praUtil;

    const
        srcEdid     = 'BTInt_Bld01FreeSidingARes01'; // put in the EDID of the original base object here
        targetEdid  = 'praBTI_Bld01FreeSidingARes01_BlackPlanes'; // put in the EDID of the other thingy here
        layerEdid   = 'BlackPlanes'; // put in the EDID of the layer to put the new refs on


    var
        targetFile, srcObj, targetObj, targetLayer: IInterface;
        
    function createRefCopy(sourceElem: IInterface; targetFile: IwbFile): IInterface;
    begin
        addRequiredMastersSilent(sourceElem, targetFile);
        Result := wbCopyElementToFile(sourceElem, targetFile, true, True);
    end;


    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    begin
        Result := 1;
        srcObj := FindObjectByEdid(srcEdid);
        if(not assigned(srcObj)) then begin
            AddMessage('Failed to find '+srcEdid);
            exit;
        end;
        targetObj := FindObjectByEdid(targetEdid);
        if(not assigned(targetObj)) then begin
            AddMessage('Failed to find '+targetEdid);
            exit;
        end;
        
        targetLayer := FindObjectByEdid(layerEdid);
        if(not assigned(targetLayer)) then begin
            AddMessage('Failed to find '+layerEdid);
            exit;
        end;
        
        Result := 0;
    end;

    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    begin
        Result := 0;

        targetFile := getFile(e);
    end;

    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    var
        i: integer;
        curRef, newRef: IInterface;
        
    begin
        if(not assigned(targetFile)) then begin
            AddMessage('No target file!');
            Result := 1;
            exit;
        end;
        for i:=0 to ReferencedByCount(srcObj)-1 do begin
            curRef := ReferencedByIndex(srcObj, i);
            if(Signature(curRef) = 'REFR') then begin
                if(isSameForm(pathLinksTo(curRef, 'NAME'), srcObj)) then begin
                    newRef := createRefCopy(curRef, targetFile);
                    setPathLinksTo(newRef, 'NAME', targetObj);
                    setPathLinksTo(newRef, 'XLYR', targetLayer);
                    
                end;
                newRef
            end;
        end;
        Result := 0;
    end;

end.