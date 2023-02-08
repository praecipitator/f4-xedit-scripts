{
    Run on something within your target file. Put in the EDIDs into the const down there.
    Script will check if the refs you run it on have the given baseform, and if yes, place a new instance of the target object at them
}
unit userscript;
    uses praUtil;

    const
        srcEdid     = 'BTInt_Bld02FrontSidingACom04'; // put in the EDID of the original base object here
        targetEdid  = 'praBTI_Bld02CornerBrickACom02_BlackPlanes'; // put in the EDID of the other thingy here
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
    var
        i: integer;
        curRef, newRef: IInterface;
    begin
        Result := 0;
        if(not assigned(targetFile)) then begin
            targetFile := getFile(e);
        end;
        
        if(Signature(e) <> 'REFR') then begin
            exit;
        end;

        if(isSameForm(pathLinksTo(e, 'NAME'), srcObj)) then begin
            newRef := createRefCopy(e, targetFile);
            AddMessage('Processing ' + FullPath(e));
            setPathLinksTo(newRef, 'NAME', targetObj);
            setPathLinksTo(newRef, 'XLYR', targetLayer);
        end;
    end;

    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    var
        i: integer;
        curRef, newRef: IInterface;

    begin
        Result := 0;
    end;

end.